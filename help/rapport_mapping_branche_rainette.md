# Rapport de mapping — pipeline CHD/AFC « rainette »

## 1) Constat de branche et périmètre
- Le dépôt local ne contient qu'une branche Git active `work` (aucune branche nommée `rainette`).
- Le périmètre « rainette » est néanmoins bien présent via le dossier `rainette/` et son intégration dans `app.R`.

## 2) Point d'entrée et orchestration globale
- `app.R` source tous les modules Rainette/IRaMuTeQ-like (nettoyage, NLP, stats, CHD/AFC, concordancier, affichage), puis crée les `reactiveValues` partagés.
- `app.R` délègue l'essentiel de l'exécution à :
  - `register_events_lancer(...)` pour le pipeline de calcul.
  - `register_rainette_explor_affichage(...)` pour l'onglet Explore.
  - `register_outputs_status(...)` pour les sorties de statut.

## 3) Mapping fonctionnel de bout en bout (du concordancier aux stats/nuages/points)

### 3.1 Import corpus, segmentation, stats corpus
- **Fichier**: `rainette/server_events_lancer_rainette.R`
  - Importe le corpus (`import_corpus_iramuteq`), segmente (`split_segments`), puis calcule les stats globales (`calculer_stats_corpus`).
- **Fichier**: `rainette/stats_rainette.R`
  - `calculer_stats_corpus(...)` produit:
    - tableau des métriques (textes, mots, formes, segments, hapax),
    - données log-log Zipf (`zipf_df`) utilisées pour le nuage de points/dispersion de Zipf.
- **Affichage UI**:
  - `app.R` expose `ui_table_stats_corpus` et `plot_stats_zipf`.
  - `ui.R` affiche ces sorties dans l'onglet **Analyse**.

### 3.2 Prétraitement linguistique et DFM
- **Fichier**: `rainette/server_events_lancer_rainette.R`
  - Orchestration du choix modèle (`rainette` vs `iramuteq`) et dictionnaire (`spacy` vs `lexique_fr`).
  - Appelle pipeline spaCy ou lexique.
- **Fichiers**:
  - `rainette/pipeline_spacy_analysis_rainette.R` : exécution spaCy, filtrage POS, lemmatisation, tokens -> DFM.
  - `rainette/pipeline_lexique_analysis_rainette.R` : filtrage/lemmes lexique_fr, tokens -> DFM.
  - `rainette/chd_afc_pipeline_rainette.R` : helpers DFM (trim, stopwords, suppression docs vides, validation, ajustement k).

### 3.3 Calcul CHD (classification)
- **Fichier**: `rainette/server_events_lancer_rainette.R`
  - Branche Rainette:
    - `rainette(...)` (simple),
    - `rainette2(...)` (double),
    - stockage dans `rv$res_chd`, `rv$dfm_chd`, `rv$clusters`.
  - Branche IRaMuTeQ-like:
    - `lancer_moteur_chd_iramuteq(...)`, résultats dans `rv$res`/classes.
- **Fichier**: `rainette/chd_afc_pipeline_rainette.R`
  - Ajuste `k` (`calculer_k_effectif`), sécurise les docvars, et génère artefacts CHD Explore (`generer_chd_explor_si_absente`, HTML associé).

### 3.4 Statistiques CHD par classe
- **Fichier**: `rainette/server_events_lancer_rainette.R`
  - Mode Rainette: `rainette_stats(...)` avec mesures `chi2`, `lr`, `frequency`, `docprop`.
  - Mode IRaMuTeQ-like: `construire_stats_classes_iramuteq(...)`.
  - Normalise/sérialise vers `rv$res_stats_df` + export `stats_par_classe.csv`.
- **Affichage UI**:
  - Explore: `rainette/rainette_explor_affichage.R` -> `table_stats_classe` via `extraire_stats_chd_classe(...)`.
  - IRaMuTeQ-like: panneaux dédiés (dans `app.R` + `iramuteq-like/affichage_iramuteq-like.R`).

### 3.5 Concordancier HTML (pivot stats -> segments)
- **Fichier**: `rainette/server_events_lancer_rainette.R`
  - Construit `segments_by_class`, prépare les arguments, appelle dynamiquement :
    - `generer_concordancier_spacy_html(...)` ou
    - `generer_concordancier_lexique_html(...)` ou
    - `generer_concordancier_iramuteq_html(...)`.
- **Fichiers concordancier rainette**:
  - `rainette/concord_spacy_rainette.R`
  - `rainette/concord_lexique_rainette.R`
  - Les deux filtrent les termes de classe à partir de `res_stats_df` (p-value optionnelle), détectent les segments pertinents via motifs Unicode, surlignent et exportent le HTML.
- **Affichage UI**:
  - `rainette/rainette_explor_affichage.R` charge l'HTML via iframe (`ui_concordancier_explore`).

### 3.6 Nuages de mots et cooccurrences
- **Fichier**: `rainette/server_events_lancer_rainette.R`
  - Génère des PNG de nuages par classe depuis `res_stats_df`.
  - Génère cooccurrences (mode Rainette) via `generer_cooccurrences_par_classe(...)`.
- **Fichier**: `rainette/cooccurrences_rainette.R`
  - Construit FCM par classe, projection en graphe igraph, export PNG réseau.
- **Affichage UI**:
  - `rainette/rainette_explor_affichage.R`:
    - `ui_wordcloud` affiche les nuages,
    - `ui_cooc` affiche les réseaux.

### 3.7 Nuages de points AFC (classes/termes/modalités) et tables
- **Fichier**: `rainette/server_events_lancer_rainette.R`
  - Lance `executer_afc_classes(...)` puis `executer_afc_variables_etoilees(...)`.
  - Enrichit `termes_stats` (merge stats CHD + segments exemples), produit PNG/csv AFC.
- **Fichiers AFC**:
  - `rainette/afc_rainette.R`
    - Table contingence classes×termes,
    - CA FactoMineR,
    - tracés type nuage de points (`tracer_afc_classes_seules`, `tracer_afc_classes_termes`, `tracer_afc_variables_etoilees`).
  - `rainette/afc_helpers_rainette.R`
    - `construire_segments_exemples_afc(...)` (segment illustratif par terme-classe).
- **Affichage UI**:
  - `ui.R` déclare `plot_afc_classes`, `plot_afc`, `plot_afc_vars`, tables AFC.
  - `app.R` rend effectivement ces sorties (messages d'erreur, tables, valeurs propres).

## 4) Mapping fichier -> responsabilité (synthèse)
- `app.R`: composition et wiring global des modules + sorties UI server.
- `ui.R`: structure des onglets, contrôles, zones d'affichage CHD/stats/concordancier/nuages/AFC.
- `rainette/server_events_lancer_rainette.R`: pipeline métier principal à l'événement « lancer ».
- `rainette/chd_afc_pipeline_rainette.R`: briques utilitaires CHD/DFM/export Explore.
- `rainette/pipeline_spacy_analysis_rainette.R`: prétraitement spaCy -> DFM.
- `rainette/pipeline_lexique_analysis_rainette.R`: prétraitement lexique_fr -> DFM.
- `rainette/stats_rainette.R`: stats corpus + Zipf.
- `rainette/concord_spacy_rainette.R`: concordancier HTML source spaCy.
- `rainette/concord_lexique_rainette.R`: concordancier HTML source lexique_fr.
- `rainette/cooccurrences_rainette.R`: graphes de cooccurrences par classe.
- `rainette/rainette_explor_affichage.R`: rendu UI onglet Explore rainette.
- `rainette/afc_rainette.R`: calcul + tracés AFC (nuages de points).
- `rainette/afc_helpers_rainette.R`: enrichissements contextuels AFC.

## 5) Points de vigilance identifiés
- `app.R` source `rainette/pipeline_iramuteq_analysis_rainette.R`, mais ce fichier est absent du dossier `rainette/` dans l'état actuel du dépôt.
- `rainette/start.R` contient un bloc dupliqué de bootstrap de l'app (doublon exact de chargement `app.R`/construction `app_obj`).

