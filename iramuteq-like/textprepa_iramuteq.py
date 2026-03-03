#!/usr/bin/env python3
"""
Préparation de corpus IRaMuTeQ-like (branche lexique_fr) pour audit de reproductibilité.

Entrée  : TSV (doc_id, text)
Sortie  : TSV (doc_id, text_prepared)
Optionnel: TSV de tokens (doc_id, token)

Objectif:
- Reproduire au plus près le pré-nettoyage de `nettoyage.R`
- Offrir un point d'audit unique pour comparer les formes avec IRaMuTeQ desktop
"""

from __future__ import annotations

import argparse
import csv
import re
from typing import Iterable, List, Tuple

ALLOWED_CHARS = "a-zA-Z0-9àÀâÂäÄáÁåÅãéÉèÈêÊëËìÌîÎïÏíÍóÓòÒôÔöÖõÕøØùÙûÛüÜúÚçÇßœŒ’ñÑ\\.:,;!\\?'"
RE_REMOVE_DISALLOWED = re.compile(rf"[^{ALLOWED_CHARS}]")
RE_MULTI_SPACES = re.compile(r"\s+")
RE_NUMBERS = re.compile(r"[0-9]+")
# Équivalent pratique de (?i)\b(?:[cdjlmnst]|qu)['’`´ʼʹ](?=\p{L})
RE_FR_ELISIONS = re.compile(r"(?i)\b(?:[cdjlmnst]|qu)['’`´ʼʹ](?=[A-Za-zÀ-ÖØ-öø-ÿŒœ])")
# Token "mot" avec apostrophe interne possible
RE_WORD = re.compile(r"[A-Za-zÀ-ÖØ-öø-ÿŒœ0-9]+(?:['’][A-Za-zÀ-ÖØ-öø-ÿŒœ0-9]+)*")


def read_tsv(path: str) -> Tuple[List[str], List[str]]:
    ids: List[str] = []
    texts: List[str] = []
    with open(path, "r", encoding="utf-8", newline="") as f:
        reader = csv.DictReader(f, delimiter="\t")
        if reader.fieldnames is None or "doc_id" not in reader.fieldnames or "text" not in reader.fieldnames:
            raise ValueError("TSV invalide: colonnes attendues 'doc_id' et 'text'.")
        for row in reader:
            ids.append((row.get("doc_id") or "").strip())
            texts.append(row.get("text") or "")
    return ids, texts


def write_tsv(path: str, ids: Iterable[str], texts: Iterable[str]) -> None:
    with open(path, "w", encoding="utf-8", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=["doc_id", "text"], delimiter="\t")
        writer.writeheader()
        for did, txt in zip(ids, texts):
            writer.writerow({"doc_id": did, "text": txt})


def write_tokens(path: str, rows: List[Tuple[str, str]]) -> None:
    with open(path, "w", encoding="utf-8", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=["doc_id", "token"], delimiter="\t")
        writer.writeheader()
        for did, tok in rows:
            writer.writerow({"doc_id": did, "token": tok})


def prepare_text(
    text: str,
    nettoyer_caracteres: bool,
    lower: bool,
    remove_numbers: bool,
    strip_fr_elisions: bool,
) -> str:
    out = text.replace("\u00A0", " ")

    if remove_numbers:
        out = RE_NUMBERS.sub(" ", out)

    if strip_fr_elisions:
        out = RE_FR_ELISIONS.sub("", out)

    if nettoyer_caracteres:
        out = RE_REMOVE_DISALLOWED.sub(" ", out)

    out = RE_MULTI_SPACES.sub(" ", out).strip()

    if lower:
        out = out.lower()

    return out


def tokenize(prepared: str, remove_numbers: bool, lower_tokens: bool) -> List[str]:
    tokens = RE_WORD.findall(prepared)
    if remove_numbers:
        tokens = [t for t in tokens if not t.isdigit()]
    if lower_tokens:
        tokens = [t.lower() for t in tokens]
    return tokens


def main() -> int:
    p = argparse.ArgumentParser()
    p.add_argument("--input", required=True, help="TSV entrée (doc_id, text)")
    p.add_argument("--output", required=True, help="TSV sortie (doc_id, text préparé)")
    p.add_argument("--nettoyage_caracteres", default="0", help="1 pour activer le nettoyage des caractères")
    p.add_argument("--forcer_minuscules_avant", default="0", help="1 pour forcer les minuscules")
    p.add_argument("--supprimer_chiffres", default="0", help="1 pour supprimer les chiffres")
    p.add_argument("--supprimer_apostrophes", default="0", help="1 pour retirer les élisions FR (c', d', l', qu', ...)")
    p.add_argument("--output_tokens", default="", help="TSV optionnel de tokens (doc_id, token)")
    args = p.parse_args()

    nettoyer_caracteres = str(args.nettoyage_caracteres).strip() == "1"
    lower = str(args.forcer_minuscules_avant).strip() == "1"
    remove_numbers = str(args.supprimer_chiffres).strip() == "1"
    strip_fr_elisions = str(args.supprimer_apostrophes).strip() == "1"

    ids, texts = read_tsv(args.input)

    prepared = [
        prepare_text(
            t,
            nettoyer_caracteres=nettoyer_caracteres,
            lower=lower,
            remove_numbers=remove_numbers,
            strip_fr_elisions=strip_fr_elisions,
        )
        for t in texts
    ]

    write_tsv(args.output, ids, prepared)

    if args.output_tokens:
        rows: List[Tuple[str, str]] = []
        for did, txt in zip(ids, prepared):
            for tok in tokenize(txt, remove_numbers=remove_numbers, lower_tokens=True):
                rows.append((did, tok))
        write_tokens(args.output_tokens, rows)

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
