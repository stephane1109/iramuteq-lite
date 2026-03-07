# Rapport — rendu dendrogramme sans camemberts

## Demande appliquée
- Pas de camemberts.
- Dendrogramme avec répartition des classes (%).
- Sous chaque classe : 10 mots-clés basés sur le chi².
- Couleur différente pour chaque classe.

## Implémentation
Dans `iramuteq-lite/chd_iramuteq.R` :

1. Le tracé reste basé sur `stats::hclust` + `plot` (base R).
2. Les labels de feuilles sont reconstruits au format :
   - `Classe X (Y %)`
   - puis la liste des 10 termes les plus discriminants (triés par `chi2`).
3. Une palette de couleurs est appliquée par classe et utilisée pour colorer les labels.
4. Aucun package externe de type `ape`/`plotrix` n'est requis pour ce rendu.

Dans `iramuteq-lite/dendrogramme_iramuteq.R` :
- la valeur par défaut `top_n_terms` passe à 10.

## Docker
Aucun dépôt GitHub à ajouter pour ce besoin : le rendu est réalisé avec les fonctions de base R déjà présentes.
