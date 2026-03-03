# CHD — Clarification de `mincl` et de la sélection des classes terminales

## 1) À quoi sert `mincl` ?

`mincl` est le **seuil minimal d'effectif d'une classe** (en pratique : nombre d'UCE/segments) utilisé au moment de la **sélection finale des classes terminales**.

- Si une classe terminale a un effectif `< mincl`, elle peut être écartée telle quelle.
- Dans la logique IRaMuTeQ, l'algorithme peut alors remonter dans l'arbre vers une classe mère pour garder une partition interprétable.

Autrement dit, `mincl` ne règle pas directement la segmentation du texte : il intervient surtout dans le **post-traitement de l'arbre CHD**.

---

## 2) Comment IRaMuTeQ applique `mincl` ?

Dans les scripts historiques d'IRaMuTeQ :

- `find.terminales(...)` conserve d'abord les classes terminales dont l'effectif est `>= mincl`.
- Pour les classes trop petites, il existe une logique de remontée vers la classe mère (via les liens mère/filles) avant de figer la solution.
- `make.classes(...)` reconstruit ensuite les classes finales et l'arbre filtré.

Cette étape explique pourquoi, à corpus identique, les classes finales peuvent différer d'une implémentation CHD qui n'a pas ce post-traitement.

---

## 3) Valeur "auto" de `mincl` dans IRaMuTeQ

### 3.1 CHD texte (`Rchdtxt`)

Convention utilisée :

- `mincl = 0`  
  → mode automatique

Formule auto :

\[
mincl = round(nrow(classeuce1) / ind)
\]

avec :

- `ind = nbcl * 2` si `classif_mode == 0` (double classification)
- sinon `ind = nbcl`

et `nbcl = nbt + 1`.

### 3.2 CHD questionnaire (`Rchdquest`)

Convention différente :

- `mincl = 2`  
  → mode automatique

Formule auto :

\[
mincl = round(nrow(classeuce1) / (nbt + 1))
\]

Puis un plancher est imposé :

- si `mincl < 3` alors `mincl = 3`.

---

## 4) Différence avec le script Rainette de ce projet

Dans ce projet, la classification est obtenue via `rainette(...)` / `rainette2(...)`, puis les groupes sont utilisés directement comme classes.

- Le paramètre `min_split_members` sert principalement à contraindre `k` (nombre de classes faisable).
- Ce n'est **pas** l'équivalent exact de `mincl` d'IRaMuTeQ.

Conséquence : sans post-traitement "classes terminales" à la manière IRaMuTeQ, le nombre et l'identité des classes finales peuvent varier.

---

## 5) Recommandation d'implémentation (optionnelle)

Pour rapprocher le comportement d'IRaMuTeQ sans casser l'existant :

1. Ajouter un réglage `mode_mincl` :
   - `manuel`
   - `auto_iramuteq`
2. Ajouter `mincl_manuel` (actif seulement en mode manuel).
3. En mode `auto_iramuteq`, calculer `mincl` avec la formule texte ci-dessus.
4. Appliquer ensuite un post-traitement de classes terminales (inspiré de `find.terminales`/`make.classes`).

Ainsi, l'utilisateur peut choisir entre :

- un mode Rainette "direct" (plus simple),
- un mode "IRa-like" (plus proche des sorties IRaMuTeQ).

---

## 6) Vocabulaire rapide

- **UCE / segment** : unité de texte classée.
- **Classe terminale** : classe feuille dans l'arbre CHD.
- **Classe mère / filles** : relation hiérarchique dans l'arbre de partition.
- **`mincl`** : effectif minimal exigé pour conserver une classe terminale telle quelle.
