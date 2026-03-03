# Rapport — construction du dendrogramme IRaMuTeQ-like

## Constat
Le dendrogramme pouvait afficher plus de classes que les classes finales réellement exploitées (ex: 16 affichées vs 6 classes finales).

## Vérification de `iramuteq_clone_v3`
Dans `iramuteq_clone_v3/tabchddist.py`, la découpe de l'arbre est pilotée explicitement par le nombre de classes cible (`clnb`) via :

- `classes<-as.data.frame(cutree(as.hclust(chd), k=clnb))[,1]`

Cette logique borne donc l'affichage/les statistiques à un nombre de classes final choisi.

## Correctif appliqué
Dans `tracer_dendrogramme_chd_iramuteq` :

1. Source de vérité prioritaire = classes présentes dans `res_stats_df$Classe` (résultat final affiché côté UI).
2. Sinon repli sur `classes` documentaires.
3. Projection sur `terminales` par index de classe (classe `i` -> `terminales[i]`) avec filtrage des indices invalides.
4. Pas de repli "toutes les feuilles" tant qu'une liste de classes finales utiles existe.

## Effet attendu
Le dendrogramme affiche uniquement les classes finales réellement présentes dans les résultats CHD (et donc alignées avec l'analyse affichée).
