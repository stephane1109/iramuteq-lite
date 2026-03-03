# Rapport de mapping IRaMuTeQ-like (CHD)

## 1) État de la branche demandée

- La branche Git locale `iramuteq-like` n'existe pas dans ce dépôt au moment de l'audit (`git branch -a` ne retourne que `work`).
- L'analyse a donc été réalisée sur le **module fonctionnel `iramuteq-like/`** présent dans la branche courante.

## 2) Mapping bout-en-bout: du concordancier au dendrogramme, à l'AFC (nuage de points) et à l'UI

### 2.1 Concordancier

- **Calcul des termes par classe**: `iramuteq-like/concordancier-iramuteq.R`
  - `.generer_concordancier_iramuteq_termes()`:
    - filtre par classe,
    - filtre p-value (`p` / `p_value`) selon `max_p`,
    - conserve chi2 positifs,
    - trie par chi2 décroissant.
- **Génération HTML**: `generer_concordancier_iramuteq_html()`:
  - prend `segments_by_class`, `res_stats_df`, `textes_indexation`,
  - applique fallback top chi2 si aucun terme filtré,
  - détecte segments contenant les termes,
  - surligne et écrit le HTML final.

### 2.2 Calcul CHD IRaMuTeQ-like

- **Moteur CHD**: `iramuteq-like/chd_iramuteq.R`
  - `calculer_chd_iramuteq()`:
    - charge scripts historiques (`anacor.R`, `CHD.R`, `chdtxt.R`),
    - binarise la matrice documentaire,
    - lance `CHD(...)`.
  - `reconstruire_classes_terminales_iramuteq()`:
    - reconstruit classes documentaires finales depuis `n1`, `list_mere`, `list_fille`,
    - applique logique `mincl` auto/manuel,
    - mappe docs vers classes terminales.
  - `construire_stats_classes_iramuteq()`:
    - calcule stats par classe/terme,
    - chi2 signé et p-value (présence/absence doc),
    - structure compatible avec le concordancier et les sorties UI.

### 2.3 Dendrogramme CHD

- **Entrée UI du dendrogramme**: `iramuteq-like/dendogramme_iramuteq.R`
  - `tracer_dendogramme_iramuteq_ui()` récupère l'objet CHD (`rv$res$chd`, `rv$res_chd`, fallback) et appelle le traceur principal.
- **Traceur principal**: `iramuteq-like/chd_iramuteq.R`
  - `tracer_dendrogramme_chd_iramuteq()`:
    - reconstruit la topologie depuis `list_fille`,
    - identifie racine/feuilles terminales,
    - calcule layout arborescent,
    - annote classes + pourcentages + termes top chi2,
    - gère orientation `vertical` / `horizontal`.

### 2.4 Nuage de points (AFC classes × termes / variables)

- **Calcul AFC**: `iramuteq-like/afc_iramuteq.R`
  - `calculer_afc_classes_termes()` construit la table classes × termes,
  - `executer_afc_classes_termes()` exécute l'AFC,
  - `tracer_afc_classes_seules()` affiche classes,
  - `tracer_afc_classes_termes()` affiche classes + termes.
- **AFC variables étoilées**: même fichier
  - `calculer_afc_classes_variables()`, `executer_afc_classes_variables()`, `tracer_afc_classes_variables()`.

### 2.5 Nuage de mots par classe

- **Génération assets PNG**: `iramuteq-like/wordcloud_iramuteq.R`
- **Exposition UI (iframe/img)**: `iramuteq-like/server_events_lancer_iramuteq.R` via `output$ui_wordcloud_iramuteq`.

### 2.6 Orchestration serveur (pipeline)

- **Pipeline principal au clic Lancer**: `iramuteq-like/server_events_lancer_iramuteq.R`
  - préparation corpus,
  - exécution CHD,
  - calcul stats,
  - calcul AFC,
  - génération wordclouds,
  - génération concordancier HTML,
  - alimentation de `rv$*` pour l'UI.

### 2.7 Couches UI

- **Panneau résultats IRaMuTeQ-like**: `iramuteq-like/affichage_iramuteq-like.R`
  - sous-onglets Dendrogramme / Stats CHD / Concordancier / Nuage de mots.
- **Rendu serveur principal Shiny**: `app.R`
  - `output$plot_chd_iramuteq_dendro`,
  - `output$ui_tables_stats_chd_iramuteq`,
  - `output$plot_afc_classes`, `output$plot_afc`, `output$plot_afc_vars`,
  - intègre `register_events_lancer(...)`.

## 3) Changement réalisé: méthode d'affichage du dendrogramme

### 3.1 Objectif

Réduire la surcharge visuelle quand les annotations de termes par classe se chevauchent.

### 3.2 Implémentation

- **Nouvelle option UI** dans l'onglet Dendrogramme:
  - `Méthode d'affichage`
  - `standard` (comportement historique: termes près des classes)
  - `compact` (nouveau défaut: termes regroupés en légende).
- **Propagation des paramètres**:
  - `app.R` transmet `input$iramuteq_dendro_display_method` au traceur.
  - `tracer_dendogramme_iramuteq_ui()` accepte `display_method` et le passe à `tracer_dendrogramme_chd_iramuteq()`.
- **Rendu compact dans le traceur**:
  - en `vertical`: légende textuelle regroupée en bas (`mtext`).
  - en `horizontal`: légende compacte en bas à droite (`legend`).
  - titres ajustés pour expliciter le mode `compact`.

### 3.3 Impact

- Pas de changement du calcul CHD ni des classes.
- Changement purement visuel du dendrogramme côté affichage.
- Le mode `standard` reste disponible.
