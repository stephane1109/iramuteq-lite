# CHD IRaMuTeQ-like — fonctionnement et paramètres utilisateur

Ce document explique le fonctionnement du mode **CHD IRaMuTeQ-like** dans l’application, les paramètres disponibles, et l’interprétation des sorties (dendrogramme et tableaux de statistiques).

---

## 1) Objectif de la CHD IRaMuTeQ-like

La CHD (Classification Hiérarchique Descendante) cherche à partitionner les segments de texte en classes lexicalement homogènes et distinctes.

Dans ce mode IRaMuTeQ-like, l’algorithme suit la logique historique IRaMuTeQ :
- découpage hiérarchique en classes,
- sélection de classes terminales,
- calcul des termes caractéristiques par classe (chi2 signé, p-value, fréquence, proportion documentaire),
- visualisation via un dendrogramme de style phylogramme.

---

## 2) Pipeline simplifié

1. **Préparation du DFM** (matrice documents/termes) à partir des segments.
2. **Lancement du moteur CHD** avec un nombre de classes cible (paramètre `k`).
3. **Reconstruction des classes terminales** avec un seuil `mincl` (auto ou manuel).
4. **Calcul des statistiques par classe**:
   - chi2 signé,
   - p-value,
   - fréquence (`frequency`),
   - proportion documentaire (`docprop`),
   - ratio de vraisemblance (`lr`).
5. **Rendu visuel**:
   - dendrogramme CHD IRaMuTeQ-like,
   - tableaux de stats par classe,
   - vues AFC (si activées).

---

## 3) Paramètres utilisateur (côté interface)

> Les noms affichés peuvent légèrement varier selon l’écran, mais la logique fonctionnelle est la suivante.

### 3.1 Nombre de classes (`k`)
- **Libellé dans l’interface**: « Nombre de classes terminales de la phase 1 ».
- **Valeur par défaut**: `10`.
- **Rôle**: fixe le niveau de découpage attendu.
- **Effet**: plus `k` est élevé, plus la partition est fine (classes plus nombreuses, parfois moins stables).
- **Valeurs recommandées**:
  - petits corpus: 3 à 6,
  - corpus moyens/grands: 4 à 10.

### 3.2 Mode de classification (`classif_mode`)
- **Valeurs**: `simple` ou `double`.
- **Rôle**:
  - `simple`: classification standard,
  - `double`: logique de classification croisée (plus stricte, selon configuration).

### 3.3 Seuil minimal de classe (`mincl`) + mode auto/manuel (`mincl_mode`)
- **`mincl_mode = auto`**: le système calcule automatiquement un minimum d’effectif par classe.
- **`mincl_mode = manuel`**: l’utilisateur impose `mincl`.
- **Effet**:
  - `mincl` plus grand = classes plus robustes, mais potentiellement moins nombreuses,
  - `mincl` plus petit = classes plus fines, mais plus sensibles au bruit.

### 3.4 Seuil de p-value (`max_p`)
- **Rôle**: filtre des termes dans certaines vues statistiques.
- **Exemples**:
  - `0.05` pour les termes statistiquement marqués,
  - `1` pour ne pas filtrer (vue exhaustive).

### 3.5 Binarisation (`binariser`)
- **Rôle**: transforme les fréquences en présence/absence avant CHD.
- **Usage**: en général activée pour rester proche des pratiques historiques CHD lexicales.

### 3.6 Méthode SVD (`svd_method`)
- **Valeurs**: `irlba`, `svdR`.
- **Rôle**: méthode de décomposition utilisée dans l’étape factorielle interne.
- **Interprétation des méthodes**:
  - `irlba` : appelle `irlba::irlba(...)`, une SVD tronquée itérative adaptée aux matrices creuses/volumineuses ; souvent plus rapide et plus robuste en mémoire pour la CHD,
  - `svdR` : appelle `svd(...)` de base R ; calcul exact complet, utile comme référence de reproductibilité mais potentiellement plus coûteux.
- **Par défaut**: `irlba`.

### 3.7 Mode patate (`mode_patate`)
- **Rôle**: active/désactive l’étape de reclassement itératif des individus selon le moteur historique.
- **Effet**:
  - désactivé (`FALSE`) = reclassement plus complet,
  - activé (`TRUE`) = comportement simplifié/rapide.

---

## 4) Sorties et interprétation

### 4.1 Dendrogramme CHD (IRaMuTeQ-like)
- Représentation en **phylogramme**:
  - axe x: profondeur de partition,
  - axe y: ordre des classes terminales.
- Les classes terminales peuvent être colorées différemment.
- Si la structure est incomplète (données trop pauvres ou sortie CHD invalide), un message explicite est affiché.

### 4.2 Tableau statistique par classe
Colonnes principales:
- **Terme**: forme lexicale.
- **chi2**: chi2 signé (positif = sur-représenté dans la classe; négatif = sous-représenté).
- **p**: p-value associée.
- **frequency**: fréquence du terme dans la classe.
- **docprop**: proportion de segments de la classe contenant le terme.
- **lr**: ratio de vraisemblance (indicateur complémentaire).

---

## 5) Bonnes pratiques pour l’utilisateur

1. Commencer avec un `k` modéré (4–6).
2. Vérifier l’équilibre des classes (éviter des classes trop petites).
3. Ajuster `mincl` si beaucoup de classes instables apparaissent.
4. Utiliser `max_p = 0.05` pour une lecture interprétative, puis `max_p = 1` pour audit complet.

---

## 6) Dépannage rapide

### Problème: pas de dendrogramme / message d’erreur
- Vérifier que le corpus contient suffisamment de segments et de vocabulaire.
- Réduire `k`.
- Revenir à `mincl_mode = auto`.

### Problème: tableau de stats vide
- Vérifier les filtres (`max_p` trop strict).
- Vérifier que les classes reconstruites ne sont pas nulles.

### Problème: résultats instables entre exécutions
- Éviter de changer simultanément plusieurs paramètres (`k`, `mincl`, `svd_method`).

---

## 7) Paramètres “utilisateur” recommandés (profil de départ)

- `k = 5`
- `classif_mode = simple`
- `mincl_mode = auto`
- `max_p = 0.05`
- `binariser = TRUE`
- `svd_method = irlba`
- `mode_patate = FALSE`

Ce profil donne généralement un bon compromis entre lisibilité des classes, stabilité et interprétabilité.
