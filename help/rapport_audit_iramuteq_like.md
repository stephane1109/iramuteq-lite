# Audit technique détaillé — pipeline IRaMuTeQ-like (tokenisation + découpage)

## 1) Ce que fait exactement le pipeline actuel

### 1.1 Entrée corpus et segmentation initiale
- Le serveur lance d'abord l'import du corpus via `import_corpus_iramuteq(chemin_fichier)`, puis applique la segmentation via `split_segments(corpus, segment_size = input$segment_size)`.
- Le nombre de segments affiché juste après cette étape est `ndoc(corpus)` après `split_segments`.
- **Important**: les fonctions `import_corpus_iramuteq` et `split_segments` ne sont pas redéfinies dans ce dépôt (elles viennent d'une dépendance, typiquement `rainette`), donc leur comportement exact dépend de la version installée.

### 1.2 Pré-nettoyage texte (avant tokenisation)
- Le texte segmenté passe ensuite par `appliquer_nettoyage_et_minuscules(...)`.
- Transformations possibles:
  - normalisation espace insécable `\u00A0` -> espace,
  - suppression des chiffres,
  - suppression des préfixes d'élision FR (`c'`, `d'`, `l'`, `n'`, `t'`, `s'`, `j'`, `qu'`),
  - suppression de caractères hors regex autorisée,
  - normalisation des espaces,
  - passage en minuscules.

### 1.3 Tokenisation réellement utilisée

#### Chemin Lexique FR
- La tokenisation est faite avec `quanteda::tokens(...)` sur les textes prétraités.
- Les options `remove_punct` et `remove_numbers` dépendent de l'UI.
- Ensuite, stopwords optionnels puis `dfm` + `dfm_trim(min_docfreq=...)`.

#### Chemin spaCy
- Le script Python `rainette/spacy_preprocess.py` lit `doc_id,text`, puis pour chaque token spaCy:
  - ignore espaces et ponctuation,
  - optionnellement ignore nombres (`tok.like_num`),
  - optionnellement retire préfixes d'élision FR,
  - filtre POS (si demandé),
  - prend surface ou lemme, en minuscules,
  - recompose le document par `" ".join(tokens_sortie)`.
- Ensuite côté R, **il y a une seconde tokenisation** quanteda sur ce texte reconstruit.

### 1.4 CHD IRaMuTeQ-like
- Quand `modele_chd == "iramuteq"`, le pipeline force `source_dictionnaire = "lexique_fr"`.
- Les stats classes sont calculées via `construire_stats_classes_iramuteq(...)` (contingence présence/absence doc-terme), avec exclusion des classes 0.
- Le moteur IRaMuTeQ-like (`lancer_moteur_chd_iramuteq`) s'appuie sur les scripts historiques `CHD.R`, `anacor.R`, `chdtxt.R`.

---

## 2) Où une divergence de nombre de segments peut apparaître

## 2.1 Niveau segmentation primaire (le plus probable)
1. **Version de dépendance différente** pour `split_segments`.
2. **Normalisation des retours ligne** avant import non identique (`CRLF` vs `LF`) si la dépendance n'harmonise pas strictement.
3. **Docnames invalides/dupliqués**: le pipeline renomme avec `make.unique`, ce qui peut modifier les correspondances ultérieures.

## 2.2 Niveau filtrage post-segmentation
Même si la segmentation est identique au départ, le nombre final peut diverger car:
1. `dfm_trim(min_docfreq)` peut supprimer des termes et rendre certains documents vides.
2. `supprimer_docs_vides_dfm(...)` retire explicitement les segments à somme nulle.
3. En CHD, les segments non assignés (`classe 0/NA`) sont exclus des stats et de l'AFC.

## 2.3 Niveau tokenisation
1. **Différence quanteda vs spaCy**: la définition d'un token n'est pas identique.
2. **Double tokenisation en chemin spaCy** (spaCy puis quanteda) -> pertes supplémentaires possibles.
3. **Option élisions FR** activée à un endroit et pas l'autre (R nettoyage vs Python spaCy).
4. **Suppression des chiffres** appliquée à deux niveaux possibles.

---

## 3) Fonctions IRaMuTeQ-like auditées et risques par fonction

### 3.1 `preparer_entrees_chd_iramuteq(...)`
- Fait: nettoyage + tokenisation quanteda + stopwords + DFM.
- Risques:
  - incohérence si ce prétraitement n'est pas exactement celui utilisé en pipeline principal,
  - variation stopwords selon langue/source.

### 3.2 `calculer_chd_iramuteq(...)`
- Fait: conversion DFM -> matrice, binarisation optionnelle, appel `CHD(...)` historique.
- Risques:
  - DFM trop pauvre (<2 lignes/colonnes),
  - effet de la binarisation sur la stabilité des classes.

### 3.3 `reconstruire_classes_terminales_iramuteq(...)`
- Fait: reconstruit classes terminales avec `find.terminales` + propagation descendants.
- Risques:
  - classes 0 (non assignées) nombreuses si `mincl` trop strict,
  - conflit entre `nb_classes_cible` et terminales reconstruites.

### 3.4 `construire_stats_classes_iramuteq(...)`
- Fait: chi² signé doc-terme, exclut explicitement classes 0.
- Risques:
  - impression de "perte" de segments côté utilisateur (en réalité exclus post-classement).

---

## 4) Réponse précise à "comment est tokenisé ?"

### En mode Lexique FR
- `quanteda::tokens(textes_lexique, remove_punct=..., remove_numbers=...)`.
- Les formes sont ensuite potentiellement abaissées en minuscules, stopwords retirés, puis DFM.

### En mode spaCy
1. spaCy segmente selon son tokenizer modèle langue.
2. Filtrage optionnel POS + nombres + élisions.
3. Lemme (ou surface) en minuscules.
4. Reconstruction du texte par espaces.
5. Nouvelle tokenisation quanteda pour DFM.

**Conséquence**: le mode spaCy n'est pas équivalent byte-à-byte au mode lexique, même avec "mêmes paramètres UI".

---

## 5) Réponse précise à "comment est découpé le texte ?"

1. Découpage en segments d'abord par `split_segments(corpus, segment_size=...)` (fonction externe).
2. Ensuite, il existe un **découpage analytique secondaire**: les segments peuvent être retirés du jeu analysé si vidés par les filtres (`dfm_trim`, suppression docs vides, classes 0).

Donc il faut distinguer:
- **segments bruts après split**,
- **segments conservés pour CHD**,
- **segments classés (hors 0/NA)**.

---

## 6) Erreurs possibles (checklist de diagnostic)

1. Version package `rainette` différente -> comportement `split_segments` différent.
2. `segment_size` identique mais options de nettoyage non alignées.
3. `supprimer_apostrophes` active d'un côté, inactive de l'autre.
4. `supprimer_chiffres` active à la fois en nettoyage R et en spaCy.
5. `remove_punct`/`remove_numbers` quanteda différents.
6. `min_docfreq` trop élevé -> segments vidés.
7. POS trop restrictif en spaCy -> segments vidés.
8. Stopwords retirés puis fallback partiel.
9. Docnames renommés (`make.unique`) perturbant certains alignements aval.
10. Exclusion classes 0 interprétée comme erreur de découpage.

---

## 7) Recommandations immédiates (très concrètes)

1. Logger séparément 3 compteurs à chaque run:
   - `N_split = ndoc(corpus)` après `split_segments`,
   - `N_non_vide = ndoc(dfm_obj)` après `supprimer_docs_vides_dfm`,
   - `N_classes = ndoc(filtered_corpus_ok)` après exclusion classes 0/NA.
2. Afficher ces 3 nombres dans l'UI, pas un seul "nombre de segments".
3. Figer version `rainette` dans l'environnement pour reproductibilité.
4. Pour audit comparatif, désactiver temporairement: POS filter, stopwords, `min_docfreq > 1`, suppression apostrophes/chiffres.

---

## 8) Conclusion opérationnelle

Le script ne fait pas qu'un découpage unique. Il y a:
1. une segmentation primaire (`split_segments`),
2. un prétraitement/tokenisation,
3. des filtrages qui peuvent éliminer des segments,
4. une exclusion finale des segments non classés.

Si vous observez un "nombre de segments" différent malgré corpus + segment_size identiques, l'écart provient très probablement de (a) version de `split_segments` ou (b) filtrages post-segmentation, et non du seul paramètre de segmentation UI.
