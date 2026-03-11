### codeandcortex.fr - Stéphane Meurisse - version beta 0.1 - 09-03-2026
- <a href="https://www.codeandcortex.fr" target="_blank" rel="noopener noreferrer">codeandcortex.fr</a>
- <a href="https://www.codeandcortex.fr/comprendre-chd-methode-reinert/" target="_blank" rel="noopener noreferrer">Comprendre la CHD</a>


### IRaMuTeQ
IRaMuTeQ, développé par Pierre Ratinaud, est un logiciel libre devenu une référence pour l’analyse textuelle en sciences humaines et sociales. Il met en œuvre la méthode de Reinert (CHD), l’AFC, ainsi que l’analyse de similitudes de Vergès, et propose de nombreux traitements complémentaires pour explorer la structure lexicale d’un corpus. Un atout est son dictionnaire de lemmes, plus précis et performant que beaucoup d’alternatives, ce qui améliore la stabilité des classes. Le dictionnaire NLP est celui de **IRaMuTeQ - lexique_fr** (uniquement fr)

- <a href="https://pratinaud.gitpages.huma-num.fr/iramuteq-website/" target="_blank" rel="noopener noreferrer">IRaMuTeQ</a>


### Méthode Reinert - CHD

La méthode de Reinert est une approche statistique d’analyse lexicale conçue pour dégager des « mondes lexicaux » dans un corpus. 
L’idée est de repérer des ensembles de segments de texte qui partagent des vocabulaires proches. 

La CHD, pour "Classification Hiérarchique Descendante", est l’algorithme de partitionnement associé à cette méthode. 
Il procède par divisions successives : on prend l’ensemble des segments, puis on le coupe en deux groupes maximisant leur différenciation lexicale. 
Ensuite, chaque groupe peut être à nouveau subdivisé, et ainsi de suite, jusqu’à obtenir un nombre de classes jugé pertinent ou une limite imposée par les paramètres.


### Moteur de classification IRaMuTeQ-lite

L'application utilise un moteur de CHD compatible IRaMuTeQ-lite, intégré au dépôt.
Il réalise la segmentation, la classification hiérarchique descendante et les exports d'analyse.

### Pourquoi vos fichiers peuvent disparaître sur Hugging Face

Sur Hugging Face Spaces, le stockage local de ce conteneur est temporaire : si le serveur redémarre, ou si la page est rechargée après une déconnexion, les fichiers générés pendant une analyse précédente peuvent ne plus être disponibles.

Conseil : télécharge l’archive ZIP des exports juste après la fin de l’analyse.


### DFM (définition et construction)

- **DFM (Document-Feature Matrix)** : matrice où chaque ligne = un segment, chaque colonne = un terme, chaque cellule = nombre d’occurrences du terme dans le segment.
- Construction (version courte) : segmentation → tokenisation/nettoyage → retrait optionnel des stopwords → filtrage `min_docfreq` (`dfm_trim`) pour retirer les termes trop rares.

### Segments vides (dans la DFM)

- Un **segment vide** est un segment dont la somme de ligne vaut 0 dans la DFM.
- En clair : c'est un segment de texte pour lequel **aucun terme ne survit** après les filtres (stopwords, fréquence minimale `min_docfreq`, nettoyage, etc.).
- Donc "vide" signifie ici : **vide de termes conservés dans la DFM**, pas forcément vide dans le texte brut.
- Ces segments sont supprimés avant la CHD.

### Définitions IRaMuTeQ des effectifs (table CHD)

- **Eff. s.t. (effectif des segments de texte)** : nombre de segments de texte de la classe qui contiennent au moins une fois la forme.
- **Eff. total (effectif total des segments de texte)** : nombre de segments de texte dans tout le corpus classé qui contiennent au moins une fois la forme.
- **Important** : ces deux colonnes sont des comptages de **segments contenant la forme** (présence/absence), pas des comptages d'occurrences.

### Segments non classés (Classe 0 / NA)

- Un **segment non classé** est un segment qui n'est pas affecté à une classe terminale à l'issue de la CHD (valeur de classe `0` ou `NA`).
- Dans l'application, ces segments sont **exclus des calculs CHD/AFC finaux** et des statistiques de classes.
- Conséquence pratique : les effectifs `Eff. s.t.` et `Eff. total` sont calculés sur le **corpus classé** (segments avec classe `> 0`) et non sur l'ensemble brut des segments importés.
- L'application journalise le nombre de segments non classés pour audit/reproductibilité.

### Paramètres de l’analyse (appliqués au calcul IRaMuTeQ-like)

- **segment_size** : taille des segments pour la segmentation simple (valeur UI par défaut: 40).
- **Fréquence minimale des termes (`min_docfreq`)** : valeur recommandée **3**. Une forme doit apparaître dans au moins 3 segments pour être conservée; plus la valeur augmente, plus les termes rares sont exclus.
- **max_p (p-value)** + **Filtrer l'affichage par p-value** : ce seuil filtre surtout l'affichage des tableaux/stats/concordancier/nuages (le calcul CHD est lancé sur le DFM préparé en amont).
- **top_n (wordcloud)** : nombre de termes affichés dans les nuages de mots par classe.

#### Paramètres CHD spécifiques IRaMuTeQ-lite

- **Nombre de classes terminales de la phase 1 (`k_iramuteq`)** : nombre de classes cibles pour la phase de partition.
- **mincl (auto/manuel)** : seuil minimal d'UCE pour conserver une classe terminale (mode automatique ou valeur manuelle).
- **Type de classification terminale** :
  - `simple` : segmentation avec `segment_size`.
  - `double` : segmentation en deux passes avec **rst1** puis **rst2**.
- **Méthode SVD (`iramuteq_svd_method`)** : `irlba` (défaut) ou `svdR`.
- **Nombre maximum de formes analysées (`iramuteq_max_formes`)** : limite le nombre de termes conservés pour la CHD.
- **Calcul des statistiques CHD (`iramuteq_stats_mode`)** : choix du mode de calcul des stats (vectorisé/classique) sans changer la logique métier des sorties.

### Options de nettoyage du texte

Ces options agissent surtout sur la **préparation linguistique** (tokenisation, DFM, CHD, stats), pas sur l’affichage "brut" des segments.

- **Nettoyage caractères (regex)** (`nettoyage_caracteres`) : supprime les caractères non autorisés par la regex interne (ex : @).
- **Supprimer la ponctuation** (`supprimer_ponctuation`) : active `remove_punct` lors de la tokenisation quanteda. La ponctuation est retirée des tokens utilisés pour les analyses (CHD, stats).
- **Supprimer les chiffres (0-9)** (`supprimer_chiffres`) : supprime les chiffres avant tokenisation.
- **Traiter les élisions FR** (`supprimer_apostrophes`) : enlève les élisions en début de mot (`c'`, `j'`, `l'`, `m'`, `n'`, `s'`, `t'`, `d'`, `qu'`) pour ramener par ex. `c'est` vers `est`.
- **Remplacer les tirets par des espaces** (`remplacer_tirets_espaces`) : transforme `mot-compose` en `mot compose` avant tokenisation.
- **Retirer les stopwords** (`retirer_stopwords`) : enlève les mots-outils français via la liste `quanteda::stopwords("fr")`.
- **Passage en minuscules** : appliqué automatiquement avant la construction des tokens/termes (option non configurable).

#### Stopwords en mode IRaMuTeQ-lite

- En mode **IRaMuTeQ-like**, la source de lemmatisation est forcée sur **Lexique (fr)**.
- Donc, quand l'option **Retirer les stopwords** est activée, le filtrage se fait avec les stopwords **français de quanteda** (et non avec spaCy).
- Le filtrage stopwords via **spaCy** n'est pas utilisé dans cette version centrée sur Lexique (fr).

#### Effet sur le concordancier HTML

- Quand **Supprimer la ponctuation** est cochée, la ponctuation est bien retirée dans les **données d’analyse**.
- Le **concordancier HTML** continue d’afficher les segments issus du corpus, donc vous pouvez encore voir de la ponctuation dans le texte affiché.

### Dictionnaire et lemmatisation (calcul IRaMuTeQ-like)

- **Source de lemmatisation** : en mode IRaMuTeQ-lite, la source active est **Lexique (fr)**.
- **Lemmatisation via lexique_fr** (`lexique_utiliser_lemmes`) : remplace les formes par leur lemme (`forme → c_lemme`) avant la DFM.
- **Dictionnaire d'expressions** (`expression_utiliser_dictionnaire`) : applique les remplacements `dic_mot → dic_norm` avant l'analyse.

- <a href="https://openlexicon.fr/" target="_blank" rel="noopener noreferrer">OpenLexicon</a>

### Filtrage morphosyntaxique
- **Filtrage morphosyntaxique** (`filtrage_morpho`) : filtre les formes selon `c_morpho` du lexique_fr.
- **Catégories conservées** (`pos_lexique_a_conserver`) : sélection des étiquettes autorisées (ex: NOM, VER, ADJ, etc.).

### Exploration

- **Classe** : sélection de la classe pour afficher les images et la table de statistiques associées.
- **CHD** : affichage graphique de la CHD.
- **Type** : bar (barres) ou cloud (nuage) pour l’affichage des termes par classe.
- **Statistiques** : chi2, lr, frequency, selon le critère utilisé pour classer les termes.
- **AFC (limite 400 termes)** : quand le vocabulaire dépasse 400 termes, l'application conserve d'abord les 400 termes les plus fréquents (pas les meilleurs chi2), puis calcule les chi2 sur cette table réduite.
- **CHD (pas de plafond fixe type 400)** : la CHD ne coupe pas automatiquement à 400 termes ; elle travaille sur le vocabulaire restant après prétraitements et filtre `min_docfreq`.
- Dans les exports CSV de type (`measure = "chi2"`), les colonnes suivantes sont importantes :
  - **`n_target`** : nombre d’occurrences du terme dans la classe/cluster analysé.
  - **`n_reference`** : nombre d’occurrences du même terme dans (tout) le corpus de référence (le reste des classes).
  - **`chi2`** et **`p`** : test d’association entre cible et référence ; plus `chi2` est élevé et `p` petite, plus le terme est spécifiquement lié à la classe.
- **Nombre de termes** : nombre de termes affichés par classe dans la visualisation.
- **Afficher les valeurs négatives** : inclut les termes négativement associés à une classe.
