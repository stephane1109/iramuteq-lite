# Rapport ciblé — chaîne de tokenisation `iramuteq_clone_v3` et pipeline `iramuteq-like`

## Contexte

Ce rapport répond à deux points :
1. Auditer **la chaîne de tokenisation** du dossier `iramuteq_clone_v3`.
2. Vérifier, côté application actuelle, le mode **`iramuteq-like`** (qui force `lexique_fr`) sans analyse spaCy.

---

## 1) Résultat principal sur `iramuteq_clone_v3`

### 1.1 Limite structurelle du dépôt cloné

Dans ce dépôt, le cœur de préparation du corpus texte (module `corpus`) est référencé mais **n'est pas présent** dans l'arborescence locale fournie.

- `iramuteq.py` importe `from corpus import Builder, SubBuilder, MergeClusters`. Cela indique que la chaîne de tokenisation principale est attendue dans `corpus.py`/module `corpus`.【F:iramuteq_clone_v3/iramuteq.py†L43-L44】
- `textcheckcorpus.py` importe aussi `Corpus` depuis `corpus`, confirmant la dépendance centrale à ce module absent localement.【F:iramuteq_clone_v3/textcheckcorpus.py†L16-L17】

👉 Conséquence : on peut auditer les options visibles en amont/aval, mais **pas reconstruire à 100% la tokenisation interne historique** sans ce module.

### 1.2 Ce qui est explicitement vérifiable dans `iramuteq_clone_v3`

#### a) Contrôle du format corpus (pas de tokenisation lexicale ici)
`textcheckcorpus.py` vérifie surtout la syntaxe des lignes/étoiles (`****`, variables `*`, thématiques `-*`, espaces interdits, etc.), mais pas un découpage lexical type tokenizer moderne.【F:iramuteq_clone_v3/textcheckcorpus.py†L19-L39】

#### b) Options de CHD (aval de la tokenisation)
`textreinert.py` expose des paramètres de classification (pas la tokenisation elle-même), notamment :
- `classif_mode` (simple/double/uci),
- `tailleuc1`, `tailleuc2` (taille des unités de contexte),
- `mincl`, `minforme`,
- `nbcl_p1`,
- `max_actives`,
- `svdmethod`,
- `mode.patate`.【F:iramuteq_clone_v3/textreinert.py†L70-L81】

Il génère ensuite des matrices clairsemées à partir des UC/UCE/UCI via des méthodes de `corpus` (`make_and_write_sparse_matrix_from_uc`, `..._from_uces`, `..._from_uci`).【F:iramuteq_clone_v3/textreinert.py†L33-L41】

#### c) Règle mincl auto côté scripts R historiques
Dans `Rscripts/chdtxt.R`, si `mincl == 0`, la formule automatique est :
- `ind = nbcl * 2` en mode double, sinon `ind = nbcl` ;
- `mincl = round(nrow(classeuce1)/ind)`.【F:iramuteq_clone_v3/Rscripts/chdtxt.R†L278-L281】

---

## 2) Pipeline **réel** utilisé par ton application `iramuteq-like` (sans spaCy)

Tu as raison : en mode `iramuteq-like`, la source est forcée sur `lexique_fr`.

### 2.1 Forçage `lexique_fr`
Dans `server_events_lancer.R`, si `modele_chd == "iramuteq"`, la source dictionnaire est forcée à `lexique_fr` avec log explicite.【F:R/server_events_lancer.R†L314-L317】

### 2.2 Chaîne de prétraitement effectivement appliquée
Avant tokenisation, la pipeline applique :
- nettoyage caractères,
- minuscules (option),
- suppression chiffres (option),
- suppression apostrophes/élisions (option).【F:R/server_events_lancer.R†L295-L301】

Ensuite, en branche `lexique_fr`, la pipeline passe par `executer_pipeline_lexique(...)`.【F:R/server_events_lancer.R†L342-L347】

### 2.3 Tokenisation et options actives en branche `lexique_fr`
Dans `executer_pipeline_lexique` :
- tokenisation `quanteda::tokens(...)` sur `textes_lexique`,
- options actives : `remove_punct = input$supprimer_ponctuation`, `remove_numbers = input$supprimer_chiffres`.
- puis construction DFM avec logique stopwords/min_docfreq via `construire_dfm_avec_fallback_stopwords(...)`.【F:R/pipeline_lexique_analysis.R†L67-L82】

### 2.4 Option de normalisation en minuscules au niveau DFM
La fonction `construire_dfm_avec_fallback_stopwords(...)` applique `tokens_tolower(...)` (avec ou sans retrait stopwords), donc la représentation finale est normalisée en minuscules côté DFM.【F:R/chd_afc_pipeline.R†L63-L70】

### 2.5 Option de préparation spécifique `iramuteq-like`
Le module `iramuteq-like/chd_iramuteq.R` normalise explicitement les options de nettoyage suivantes :
- `nettoyage_caracteres`,
- `forcer_minuscules_avant`,
- `supprimer_chiffres`,
- `supprimer_apostrophes`,
- `supprimer_ponctuation`,
- `retirer_stopwords`.
Puis tokenise avec `quanteda::tokens(remove_punct, remove_numbers)` et option stopwords quanteda selon langue.【F:iramuteq-like/chd_iramuteq.R†L26-L33】【F:iramuteq-like/chd_iramuteq.R†L80-L89】

---

## 3) Rapport des options actives (mode `iramuteq-like`)

### 3.1 Options effectivement actives dans ta chaîne (et où)

- `nettoyage_caracteres` : actif avant tokenisation via `appliquer_nettoyage_et_minuscules(...)`.【F:R/server_events_lancer.R†L295-L299】
- `forcer_minuscules_avant` : actif avant tokenisation via `appliquer_nettoyage_et_minuscules(...)`.【F:R/server_events_lancer.R†L298-L299】
- `supprimer_chiffres` : actif avant tokenisation + actif dans `tokens(remove_numbers=...)`.【F:R/server_events_lancer.R†L299-L300】【F:R/pipeline_lexique_analysis.R†L69-L71】
- `supprimer_apostrophes` : actif avant tokenisation via nettoyage texte.【F:R/server_events_lancer.R†L300-L301】
- `supprimer_ponctuation` : actif dans `tokens(remove_punct=...)` en branche `lexique_fr`.【F:R/pipeline_lexique_analysis.R†L69-L70】
- `retirer_stopwords` : actif dans `construire_dfm_avec_fallback_stopwords(...)` (avec fallback si DFM trop pauvre).【F:R/chd_afc_pipeline.R†L55-L66】【F:R/chd_afc_pipeline.R†L87-L93】
- `min_docfreq` : actif au `dfm_trim(min_docfreq=...)`.【F:R/chd_afc_pipeline.R†L77-L77】
- `filtrage_morpho` + `lexique_utiliser_lemmes` : actifs en branche lexique via filtrage cgram/lemmatisation forme→lemme si demandé.【F:R/pipeline_lexique_analysis.R†L31-L36】【F:R/pipeline_lexique_analysis.R†L48-L61】

### 3.2 Options non pertinentes pour ce mode
- Les options spaCy ne sont pas utilisées quand `modele_chd="iramuteq"` puisque la source est forcée `lexique_fr`.【F:R/server_events_lancer.R†L314-L317】

---

## 4) Pourquoi l'écart de formes peut persister malgré “même paramétrage”

Même en excluant spaCy, un écart peut persister entre IRaMuTeQ desktop et ton app si la tokenisation historique interne d'IRaMuTeQ (module `corpus` absent ici) diffère de ta chaîne `quanteda + lexique_fr + trim`.

En pratique, les points sensibles sont :
- règles de segmentation des unités de contexte (UC/UCE/UCI) et calcul d'actives/supplémentaires dans `corpus` historique (non auditable dans ce clone incomplet),
- ordre exact nettoyage → lemmatisation → tokenisation,
- traitement des élisions/apostrophes,
- seuil `min_docfreq`, suppression stopwords, fallback, et suppression segments vides avant CHD.

---

## 5) Recommandation opérationnelle

Pour une comparaison “iso-IRaMuTeQ” stricte, je recommande un mode audit en 2 passes :

1. **Pass A (référence max proche IRaMuTeQ)**
   - `retirer_stopwords = FALSE`
   - `min_docfreq = 1`
   - pas de filtrage morpho
   - pas de suppression apostrophes/chiffres supplémentaire

2. **Pass B (tes réglages actuels)**
   - comparer : nb tokens bruts, nb formes DFM, nb hapax, nb segments non vides.

Et ajouter un export intermédiaire (tokens par segment) pour expliquer où apparaissent les +194 formes.


---

## 6) Corrections concrètes recommandées pour rapprocher les scores

1. **Figer une prépa corpus unique avant `quanteda::tokens`**
   - Utiliser `iramuteq-like/textprepa_iramuteq.py` pour générer un texte préparé stable et auditable (mêmes règles à chaque run), puis tokeniser ce texte côté R.

2. **Éviter les doubles effets de nettoyage**
   - Si `iramuteq-like/textprepa_iramuteq.py` est activé, neutraliser le nettoyage redondant en amont pour ne pas supprimer deux fois chiffres/élisions.

3. **Comparer les formes à 3 niveaux**
   - Niveau A: formes post-prépa (sortie `output_tokens` de `iramuteq-like/textprepa_iramuteq.py`),
   - Niveau B: formes post-`quanteda::tokens`,
   - Niveau C: formes post-`dfm_trim`.

4. **Conserver un mode “audit IRaMuTeQ”**
   - `min_docfreq=1`, `retirer_stopwords=FALSE`, pas de filtrage morpho, pour isoler l'écart de tokenisation.

5. **Comparer sur un sous-corpus fixe**
   - Exporter 50–100 segments identiques entre IRaMuTeQ desktop et app, comparer les listes de formes exactes segment par segment.
