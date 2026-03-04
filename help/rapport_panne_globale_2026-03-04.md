# Rapport d'audit immédiat — pannes CHD / stats / dendrogramme / nuages

## Contexte
Audit statique du code (lecture des scripts) pour identifier les causes probables du blocage global signalé.

## Causes racines identifiées

### 1) Crash probable du pipeline sur `input$top_n` (cause critique)
- Le pipeline serveur utilise `input$top_n` pour générer les nuages.
- Or aucun `numericInput("top_n", ...)` n'existe dans `ui.R`.
- Quand `top_n` est absent, `as.integer(input$top_n)` retourne une longueur 0, puis le test `if (!is.finite(top_n_demande) || is.na(top_n_demande))` peut lever l'erreur classique `argument is of length zero`.
- Cette erreur survient en fin de pipeline et peut interrompre l'analyse complète (d'où impression que « rien ne fonctionne »).

### 2) Dépendances R bloquantes au démarrage
- Les packages critiques (`quanteda`, `wordcloud`, `RColorBrewer`, `igraph`, `dplyr`) sont chargés au démarrage.
- Si un package manque, l'application refuse explicitement de lancer l'analyse.
- Effet utilisateur: aucun calcul CHD / stats / AFC / nuage possible tant que l'environnement n'est pas complet.

### 3) Paramètres AFC avancés référencés mais non exposés en UI
- Le serveur lit `input$afc_top_termes` et `input$afc_top_modalites`.
- Ces entrées ne sont pas définies dans l'UI actuelle.
- Ici ce n'est pas fatal (des garde-fous `is.null` existent), mais cela montre une dérive UI/serveur et augmente le risque de bugs masqués.

### 4) Chemin d'aide incohérent (non bloquant mais révélateur)
- Une branche vérifie `file.exists("help.md")` à la racine.
- L'inclusion réelle est `help/help.md` ailleurs.
- Ce n'est pas la cause des pannes CHD, mais c'est un symptôme de cohérence incomplète.

## Impact fonctionnel par bloc

- **Stats corpus**: peuvent être calculées mais deviennent invisibles si le pipeline casse ensuite.
- **CHD**: peut démarrer puis échouer avant finalisation/export selon le point de rupture.
- **Dendrogramme**: dépend d'objets CHD finalisés; indisponible si étape précédente interrompue.
- **Nuages de mots**: fortement impactés (zone exacte du bug critique `top_n`).

## Actions correctives prioritaires

### P0 (immédiat)
1. Ajouter dans l'UI un contrôle:
   - `numericInput("top_n", "Top N mots par classe", value = 20, min = 5, step = 1)`
2. Sécuriser serveur:
   - Remplacer les tests scalaires fragiles par un parseur robuste:
     - `top_n_demande <- suppressWarnings(as.integer(input$top_n))`
     - `if (length(top_n_demande) != 1 || is.na(top_n_demande) || !is.finite(top_n_demande)) top_n_demande <- 20L`

### P1
3. Ajouter un écran de préflight lisible listant les packages manquants + commandes d'installation.
4. Centraliser la lecture des inputs (UI contract) pour détecter automatiquement les `input$...` non définis.

### P2
5. Harmoniser les chemins d'aide (`help.md` vs `help/help.md`).
6. Ajouter un mode diagnostic qui continue à afficher les résultats partiels même si une étape aval échoue.

## Validation minimale à exécuter après correctif
1. Lancer l'analyse avec corpus minimal (2-3 documents) → vérifier statut final "terminé".
2. Vérifier l'onglet Stats (table + Zipf).
3. Vérifier l'onglet Dendrogramme.
4. Vérifier présence des PNG de nuages dans `exports/wordclouds`.
5. Vérifier que l'erreur `argument is of length zero` n'apparaît plus dans les logs.
