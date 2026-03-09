# Vectorisation du calcul du χ² (CHD IRaMuTeQ-like)

Ce document explique la logique du mode **vectorisé** ajouté pour calculer `chi2` et `p.value` plus rapidement dans la table des statistiques CHD.

## 1) Contexte

Dans la version classique, pour chaque terme et chaque classe on construit un tableau de contingence 2x2 puis on appelle :

- `chisq.test(tb, correct = FALSE)`

Cette approche est correcte, mais coûteuse sur de gros corpus (beaucoup de termes × classes), car elle fait un grand nombre d'appels R/fonctions statistiques.

## 2) Idée de la vectorisation

La vectorisation consiste à calculer **en bloc** les mêmes quantités pour tous les termes d'une classe, au lieu de les traiter un par un.

Concrètement, on fabrique les 4 cellules du 2x2 sous forme de **vecteurs** :

- `n11` : occurrences du terme dans la classe
- `n12` : occurrences du terme hors classe
- `n21` : autres occurrences dans la classe
- `n22` : autres occurrences hors classe

Puis on applique la formule fermée du χ² (ddl=1) sur ces vecteurs :

\[
\chi^2 = \frac{n(ad-bc)^2}{(a+b)(c+d)(a+c)(b+d)}
\]

avec `a=n11`, `b=n12`, `c=n21`, `d=n22`, `n=a+b+c+d`.

## 3) Signe du χ² (sur/sous-représentation)

Le χ² standard est positif. Pour conserver la sémantique historique IRaMuTeQ-like, on applique un signe :

- on calcule l'attendu `E11`
- si `n11 >= E11` → signe positif
- sinon → signe négatif

On obtient ainsi un `chi2` signé compatible avec les usages existants (tri, interprétation, filtres).

## 4) Calcul de la p-value

En mode vectorisé, la p-value est obtenue via la loi du χ² :

- `p = pchisq(chi2_abs, df = 1, lower.tail = FALSE)`

où `chi2_abs` est le χ² non signé.

Cela reproduit la logique de `chisq.test(..., correct=FALSE)` pour des tableaux 2x2 sans correction de continuité.

## 5) Robustesse numérique

Le mode vectorisé inclut des garde-fous :

- dénominateur nul/non fini → χ² forcé à 0
- valeurs non finies/NA → p-value forcée à 1
- attendu non fini/NA → repli sécurisé

Objectif : éviter les crashs et garder un comportement stable sur des données extrêmes/rares.

## 6) Compatibilité : mode "classique"

Un mode de calcul "classique" est conservé :

- boucle terme par terme
- `chisq.test(..., correct = FALSE)`

Ce mode sert de référence de compatibilité. Le mode vectorisé est recommandé pour la performance.

## 7) Pourquoi c'est plus rapide ?

- moins d'appels de fonctions R coûteux
- opérations arithmétiques vectorielles optimisées
- réduction de l'overhead interpréteur sur de grands volumes

Résultat attendu : même logique statistique, temps de calcul réduit sur gros corpus.
