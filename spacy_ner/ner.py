# Rôle du fichier: ner.py porte une partie du pipeline d'analyse Rainette.
# Ce script centralise une responsabilité métier/technique utilisée par l'application.
# ner.py

"""
Extraction NER spaCy FR pour Rainette
Entrée : TSV (doc_id, text)
Sortie : TSV (doc_id, ent_text, ent_label, start_char, end_char)

Support optionnel d'un dictionnaire JSON:
{
  "exclude_texts": ["ça", "«"],
  "exclude_labels": ["MISC"],
  "include": [
    {"text": "OpenAI", "label": "ORG"}
  ]
}
"""

import argparse
import csv
import json
import re
import sys
from typing import Dict, List, Set, Tuple

import spacy


def lire_tsv(chemin: str) -> Tuple[List[str], List[str]]:
    doc_ids: List[str] = []
    textes: List[str] = []
    with open(chemin, "r", encoding="utf-8", newline="") as f:
        lecteur = csv.DictReader(f, delimiter="\t")
        if lecteur.fieldnames is None:
            raise ValueError("TSV invalide : en-tête manquant.")
        if "doc_id" not in lecteur.fieldnames or "text" not in lecteur.fieldnames:
            raise ValueError("Le TSV doit contenir les colonnes 'doc_id' et 'text'.")
        for row in lecteur:
            doc_ids.append((row.get("doc_id", "") or "").strip())
            textes.append(row.get("text", "") or "")
    return doc_ids, textes


def ecrire_tsv(chemin: str, lignes: List[dict]) -> None:
    with open(chemin, "w", encoding="utf-8", newline="") as f:
        champs = ["doc_id", "ent_text", "ent_label", "start_char", "end_char"]
        ecrivain = csv.DictWriter(f, fieldnames=champs, delimiter="\t")
        ecrivain.writeheader()
        for row in lignes:
            ecrivain.writerow(row)


STOPWORDS_BRUIT_FR = {
    "ca",
    "ça",
    "ce",
    "cet",
    "cette",
    "ces",
    "c",
    "j",
    "je",
    "tu",
    "il",
    "elle",
    "on",
    "nous",
    "vous",
    "ils",
    "elles",
    "me",
    "te",
    "se",
    "moi",
    "toi",
    "lui",
    "leur",
    "leurs",
}

DETERMINANTS_FR = {
    "un",
    "une",
    "des",
    "du",
    "de",
    "le",
    "la",
    "les",
    "ce",
    "cet",
    "cette",
    "ces",
    "mon",
    "ma",
    "mes",
    "ton",
    "ta",
    "tes",
    "son",
    "sa",
    "ses",
    "notre",
    "nos",
    "votre",
    "vos",
    "leur",
    "leurs",
}


def charger_dictionnaire_ner(chemin: str) -> Dict[str, object]:
    with open(chemin, "r", encoding="utf-8") as f:
        brut = json.load(f)

    if not isinstance(brut, dict):
        raise ValueError("Le dictionnaire JSON NER doit être un objet JSON.")

    exclude_texts_raw = brut.get("exclude_texts", [])
    exclude_labels_raw = brut.get("exclude_labels", [])
    include_raw = brut.get("include", [])

    if not isinstance(exclude_texts_raw, list) or not isinstance(exclude_labels_raw, list) or not isinstance(include_raw, list):
        raise ValueError("Le dictionnaire JSON NER doit contenir des listes pour 'exclude_texts', 'exclude_labels' et 'include'.")

    exclude_texts: Set[str] = {
        " ".join(str(x).split()).lower() for x in exclude_texts_raw if str(x).strip()
    }
    exclude_labels: Set[str] = {str(x).strip().upper() for x in exclude_labels_raw if str(x).strip()}

    include: List[Dict[str, str]] = []
    erreurs_include: List[str] = []
    for i, row in enumerate(include_raw, start=1):
        if not isinstance(row, dict):
            erreurs_include.append(f"include[{i}] doit être un objet JSON.")
            continue

        champs_inconnus = sorted(k for k in row.keys() if k not in {"text", "label"})
        if champs_inconnus:
            erreurs_include.append(
                f"include[{i}] contient des clés non supportées: {', '.join(champs_inconnus)} (attendu: text, label)."
            )
            continue

        txt = " ".join(str(row.get("text", "")).split())
        lbl = str(row.get("label", "MISC")).strip().upper() or "MISC"

        if not txt:
            erreurs_include.append(f"include[{i}].text est vide.")
            continue

        include.append({"text": txt, "label": lbl})

    if erreurs_include:
        raise ValueError(
            "Entrées 'include' invalides dans le dictionnaire NER JSON\n- "
            + "\n- ".join(erreurs_include)
        )

    return {
        "exclude_texts": exclude_texts,
        "exclude_labels": exclude_labels,
        "include": include,
    }


def est_entite_bruyante(entite_texte: str, entite_label: str, dico: Dict[str, object]) -> bool:
    txt = " ".join((entite_texte or "").split())
    if not txt:
        return True

    txt_lower = txt.lower()
    label_upper = (entite_label or "").upper()
    tokens = [t for t in re.split(r"\s+", txt_lower) if t]
    if not tokens:
        return True

    if txt_lower in dico["exclude_texts"]:
        return True

    if label_upper in dico["exclude_labels"]:
        return True

    if len(tokens) == 1 and tokens[0] in STOPWORDS_BRUIT_FR:
        return True

    # Rejette ponctuation/quotes/emoji seuls (ex: «, », "", ...).
    if not any(ch.isalnum() for ch in txt):
        return True

    # Rejette aussi les entités d'un seul caractère non alphanumérique utile.
    if len(txt) == 1 and not txt.isalnum():
        return True

    if tokens[0] in DETERMINANTS_FR and label_upper in {"PER", "LOC", "ORG"}:
        return True

    # Cas fréquent de faux positifs NER: mot unique tout en minuscules.
    if (
        len(tokens) == 1
        and label_upper in {"PER", "LOC", "ORG"}
        and txt == txt_lower
        and not any(ch.isdigit() for ch in txt)
    ):
        return True

    return False


def ajouter_entites_dictionnaire(doc_id: str, texte: str, dico: Dict[str, object], deja_vues: Set[Tuple[int, int, str]]) -> List[dict]:
    out: List[dict] = []
    for ent in dico["include"]:
        term = ent["text"]
        label = ent["label"]
        motif = re.compile(rf"\b{re.escape(term)}\b", flags=re.IGNORECASE)
        for m in motif.finditer(texte or ""):
            start, end = m.span()
            cle = (start, end, label)
            if cle in deja_vues:
                continue
            deja_vues.add(cle)
            out.append(
                {
                    "doc_id": doc_id,
                    "ent_text": " ".join((m.group(0) or "").split()),
                    "ent_label": label,
                    "start_char": start,
                    "end_char": end,
                }
            )
    return out


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--input", required=True, help="Chemin TSV d'entrée (doc_id, text).")
    parser.add_argument("--output", required=True, help="Chemin TSV de sortie.")
    parser.add_argument("--modele", default="fr_core_news_md", help="Nom du modèle spaCy FR.")
    parser.add_argument(
        "--filtrer-bruit",
        choices=["0", "1"],
        default="1",
        help="Applique un post-filtrage léger pour réduire les faux positifs évidents.",
    )
    parser.add_argument(
        "--dictionnaire-json",
        default="",
        help="Chemin optionnel vers un dictionnaire JSON NER (exclude/include).",
    )
    args = parser.parse_args()

    dico = {"exclude_texts": set(), "exclude_labels": set(), "include": []}
    if str(args.dictionnaire_json).strip():
        try:
            dico = charger_dictionnaire_ner(args.dictionnaire_json)
        except Exception as e:
            sys.stderr.write(f"Erreur chargement dictionnaire JSON NER '{args.dictionnaire_json}' : {e}\n")
            return 6

    try:
        nlp = spacy.load(args.modele)
    except Exception as e:
        sys.stderr.write(f"Erreur chargement modèle spaCy '{args.modele}' : {e}\n")
        return 2

    # On ne garde que tok2vec + ner pour accélérer et stabiliser
    keep = {"tok2vec", "ner"}
    disable = [p for p in nlp.pipe_names if p not in keep]
    if disable:
        nlp.disable_pipes(*disable)

    try:
        doc_ids, textes = lire_tsv(args.input)
    except Exception as e:
        sys.stderr.write(f"Erreur lecture TSV : {e}\n")
        return 3

    lignes: List[dict] = []
    try:
        for did, texte, doc in zip(doc_ids, textes, nlp.pipe(textes)):
            deja_vues: Set[Tuple[int, int, str]] = set()
            for ent in doc.ents:
                txt = (ent.text or "").strip()
                if not txt:
                    continue
                if args.filtrer_bruit == "1" and est_entite_bruyante(txt, ent.label_, dico):
                    continue

                cle = (ent.start_char, ent.end_char, (ent.label_ or "").upper())
                if cle in deja_vues:
                    continue
                deja_vues.add(cle)

                lignes.append(
                    {
                        "doc_id": did,
                        "ent_text": " ".join(txt.split()),
                        "ent_label": ent.label_,
                        "start_char": ent.start_char,
                        "end_char": ent.end_char,
                    }
                )

            lignes.extend(ajouter_entites_dictionnaire(did, texte, dico, deja_vues))
    except Exception as e:
        sys.stderr.write(f"Erreur traitement NER : {e}\n")
        return 4

    try:
        ecrire_tsv(args.output, lignes)
    except Exception as e:
        sys.stderr.write(f"Erreur écriture TSV : {e}\n")
        return 5

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
