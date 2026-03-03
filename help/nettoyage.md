# Audit des options de nettoyage (spaCy vs `lexique_fr`)

Contexte analysé : pipeline Rainette (`R/server_events_lancer.R`, `nettoyage.R`, `R/pipeline_spacy_analysis.R`, `R/pipeline_lexique_analysis.R`, `R/nlp_spacy.R`, `rainette/spacy_preprocess.py`, `R/chd_afc_pipeline.R`, `R/nlp_language.R`).

## Résumé rapide

- Toutes les options UI sont bien **lues** par le pipeline.
- Mais en mode **spaCy**, certaines options sont **redondantes** ou **sans effet réel** sur la sortie finale.
- Ton log debug (`Diagnostic pipeline: ...`) est normal : il n'affiche qu'un sous-ensemble des options.

## Vérification option par option

| Option UI | `lexique_fr` | `spaCy` | Verdict |
|---|---|---|---|
| `nettoyage_caracteres` | Appliquée avant pipeline via `appliquer_nettoyage_et_minuscules()` | Appliquée avant pipeline via `appliquer_nettoyage_et_minuscules()` | ✅ OK des deux côtés |
| `forcer_minuscules_avant` | Appliquée dans `appliquer_nettoyage_et_minuscules()` | Appliquée dans `appliquer_nettoyage_et_minuscules()` + passée à Python (`--lower_input`) | ⚠️ Redondante en spaCy |
| `supprimer_chiffres` | Appliquée dans `appliquer_nettoyage_et_minuscules()` + `tokens(..., remove_numbers=TRUE/FALSE)` | Appliquée dans `appliquer_nettoyage_et_minuscules()` + spaCy Python (`--remove_numbers`) + `tokens(..., remove_numbers=...)` | ⚠️ Très redondante en spaCy |
| `supprimer_apostrophes` | Appliquée dans `appliquer_nettoyage_et_minuscules()` | Appliquée dans `appliquer_nettoyage_et_minuscules()` + spaCy Python (`--strip_fr_elisions`) | ⚠️ Redondante en spaCy |
| `supprimer_ponctuation` | Appliquée à la tokenisation quanteda (`tokens(remove_punct=...)`) | **Déjà supprimée** dans `rainette/spacy_preprocess.py` (`tok.is_punct` ignoré), puis retokenisation quanteda | ⚠️ En spaCy, case quasi sans effet |
| `retirer_stopwords` | Oui (`obtenir_stopwords_analyse(..., source_dictionnaire='lexique_fr')` → quanteda FR) | Oui (`source_dictionnaire='spacy'` → stopwords spaCy) | ✅ OK des deux côtés |
| `filtrage_morpho` | Oui via `filtrer_textes_lexique_par_cgram()` (`c_morpho`) | Oui via `--pos_keep` et filtrage POS dans Python | ✅ OK des deux côtés |

## Point important pour ton test (`lexique_fr` + pas de case minuscules)

Même si tu ne coches pas « Passage en minuscule », la chaîne de traitement fait ensuite un `tokens_tolower(...)` lors de la construction du DFM (avec ou sans stopwords). Donc la représentation finale utilisée pour l'analyse passe en minuscules dans tous les cas.

➡️ En clair : pour l'analyse statistique, l'absence de coche « minuscules » n'empêche pas une normalisation en minuscules plus tard.

## Pourquoi ton log debug ne montre pas tout

La ligne :

`Diagnostic pipeline: dictionnaire=... | langue UI=... | filtrage_morpho=... | retirer_stopwords=...`

est volontairement courte et n'inclut pas `forcer_minuscules_avant`, `supprimer_ponctuation`, `supprimer_chiffres`, `supprimer_apostrophes`, `nettoyage_caracteres`.

## Conclusion

- ✅ **Oui**, les options principales sont prises en compte pour `lexique_fr` et spaCy.
- ⚠️ **Non**, elles ne sont pas toutes discriminantes en spaCy (certaines sont doublées, et `supprimer_ponctuation` n'a presque pas d'effet car spaCy retire déjà la ponctuation en amont).
- ⚠️ La case « Passage en minuscule » n'est pas un indicateur fiable du casing final du DFM, car le pipeline applique `tokens_tolower(...)` ensuite.

## Recommandations (si tu veux un comportement plus lisible)

1. Ajouter dans le log debug l'état de toutes les cases de nettoyage.
2. En mode spaCy, clarifier dans l'UI que la ponctuation est déjà retirée côté Python.
3. Décider d'un **seul** niveau pour la mise en minuscules (avant spaCy OU juste avant DFM), pour éviter les ambiguïtés de debug.
4. Éviter les doubles suppressions chiffres/apostrophes (R + Python) sauf besoin explicite.
