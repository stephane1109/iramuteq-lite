[//]: # (Rôle du fichier: help.md documente une partie de l'application Rainette.)
[//]: # (Ce document sert de référence fonctionnelle/technique pour l'équipe.)
[//]: # (Il décrit le comportement attendu afin de sécuriser maintenance et diagnostics.)
### codeandcortex.fr - Stéphane Meurisse - version beta 0.4 - 18-02-2026
- <a href="https://www.codeandcortex.fr" target="_blank" rel="noopener noreferrer">codeandcortex.fr</a>
- <a href="https://www.codeandcortex.fr/comprendre-chd-methode-reinert/" target="_blank" rel="noopener noreferrer">Comprendre la CHD</a>


### IRaMuTeQ
IRaMuTeQ, développé par Pierre Ratinaud, est un logiciel libre devenu une référence pour l’analyse textuelle en sciences humaines et sociales. Il met en œuvre la méthode de Reinert (CHD), l’AFC, ainsi que l’analyse de similitudes de Vergès, et propose de nombreux traitements complémentaires pour explorer la structure lexicale d’un corpus. Un atout est son dictionnaire de lemmes, plus précis et performant que beaucoup d’alternatives, ce qui améliore la stabilité des classes. Depuis la version 0.4 vous avez le choix avec le dictionnaire NLP de spaCy et celui de **IRaMuTeQ - lexique_fr** (uniquement fr)
Ce qui change à partir de la version O.4 c'est l'utilisation du **dictionnaire** utilisé par **IRaMuTeQ** (uniquement fr). **Ce dictionnaire est plus précis que spaCy**.

- <a href="https://pratinaud.gitpages.huma-num.fr/iramuteq-website/" target="_blank" rel="noopener noreferrer">IRaMuTeQ</a>


### Méthode Reinert - CHD

La méthode de Reinert est une approche statistique d’analyse lexicale conçue pour dégager des « mondes lexicaux » dans un corpus. 
L’idée est de repérer des ensembles de segments de texte qui partagent des vocabulaires proches. 

La CHD, pour "classification hiérarchique descendante", est l’algorithme de partitionnement associé à cette méthode. 
Il procède par divisions successives : on prend l’ensemble des segments, puis on le coupe en deux groupes maximisant leur différenciation lexicale. 
Ensuite, chaque groupe peut être à nouveau subdivisé, et ainsi de suite, jusqu’à obtenir un nombre de classes jugé pertinent ou une limite imposée par les paramètres.


### Rainette développé par Julien Barnier

Rainette est un package R qui réalise une CHD selon la méthode Reinert.
- <a href="https://github.com/juba/rainette/blob/main/vignettes/introduction_usage.Rmd" target="_blank" rel="noopener noreferrer">Doc Rainette</a>
- <a href="https://cran.r-project.org/web/packages/rainette/vignettes/introduction_usage.html" target="_blank" rel="noopener noreferrer">Utilisation de rainette</a>
- <a href="https://juba.r-universe.dev/builds" target="_blank" rel="noopener noreferrer">Builds r-universe</a>


### Pourquoi vos fichiers peuvent disparaître sur Hugging Face

Sur Hugging Face Spaces, le stockage local de ce conteneur est temporaire : si le serveur redémarre, ou si la page est rechargée après une déconnexion, les fichiers générés pendant une analyse précédente peuvent ne plus être disponibles.

Conseil : télécharge l’archive ZIP des exports juste après la fin de l’analyse.


# Logique générale de l’application

Uploadez un fichier texte au format IRaMuTeQ. L’app segmente, construit une matrice termes-documents (DTM), lance la CHD avec rainette, calcule les statistiques, génère un HTML surligné (concordancier), puis produit la CHD, AFC, NER, nuages de mots et réseaux de cooccurrences. L’onglet d’exploration (Explore_rainette) permet de visualiser la CHD.

### Choix de la langue du dictionnaire spaCy

Vous avez le choix entre 4 langues spaCy préinstallées : français, anglais, espagnol et allemand (modèles "large", lg). D’autres langues peuvent être ajoutées ensuite selon les besoins. Il existe quatre tailles de modèles : "sm", "md", "lg" et "trf" (basé sur la technologie "transformer"). Le script détecte la cohérence entre le choix du dictionnaire et votre corpus importé, sur la base des stopwords.

### Paramètres de l’analyse

- **segment_size** : taille des segments lors du découpage du corpus. Plus petit donne plus de segments, plus grand donne des segments plus longs.
- **k (nombre de classes)** : nombre de classes demandé pour la CHD.
- Nombre minimal de termes par segment : `min_segment_size` : Lors de la tokenisation et du calcul de la dtm, certaines formes (mots-outils, mots trop peu fréquents) ont été supprimées, les segments peuvent donc varier en taille. 
Avec `min_segment_size = 10`, les segments comportant moins de 10 formes sont regroupés avec le segment suivant ou précédent du même document jusqu'à atteindre la taille minimale souhaitée.
- Effectif minimal pour scinder une classe : **min_split_members**. Nombre minimal de documents pour qu'une classe soit scindée en deux à l'étape suivante de la classification.
- Fréquence minimale des termes : `dfm_trim min_docfreq` : fréquence minimale en nombre de segments pour conserver un terme dans le DFM. Plus "haut" enlève les termes rares. Par exemple si vous `dfm_trim = 3` cela supprime de la matrice les termes apparaissant dans moins de 3 segments.
- **max_p (p-value)** : seuil de p-value pour filtrer les termes mis en avant dans les statistiques.
- **top_n (wordcloud)** : nombre de termes affichés dans chaque nuage de mots.
- **window (cooccurrences)** : taille de la fenêtre glissante pour calculer les cooccurrences.
- **top_feat (cooccurrences)** : nombre de termes retenus pour construire le réseau de cooccurrences.

### Options de nettoyage du texte

Ces options agissent surtout sur la **préparation linguistique** (tokenisation, DFM, CHD, stats), pas sur l’affichage "brut" des segments.

- **Nettoyage caractères (regex)** (`nettoyage_caracteres`) : supprime les caractères non autorisés par la regex interne (ex : @).
- **Supprimer la ponctuation** (`supprimer_ponctuation`) : active `remove_punct` lors de la tokenisation quanteda. La ponctuation est retirée des tokens utilisés pour les analyses (CHD, stats).
- **Supprimer les chiffres (0-9)** (`supprimer_chiffres`) : supprime les chiffres avant tokenisation.
- **Traiter les élisions FR** (`supprimer_apostrophes`) : enlève les élisions en début de mot (`c'`, `j'`, `l'`, `m'`, `n'`, `s'`, `t'`, `d'`, `qu'`) pour ramener par ex. `c'est` vers `est`.
- **Forcer en minuscules avant analyse** (`forcer_minuscules_avant`) : convertit le texte en minuscules avant la construction des tokens/termes.

#### Stopwords en mode IRaMuTeQ-like

- En mode **IRaMuTeQ-like**, la source de lemmatisation est forcée sur **Lexique (fr)**.
- Donc, quand l'option **Retirer les stopwords** est activée, le filtrage se fait avec les stopwords **français de quanteda** (et non avec spaCy).
- Le filtrage stopwords via **spaCy** n'est utilisé que lorsque la source de dictionnaire est **spaCy** (mode Rainette).

#### Effet sur le concordancier HTML

- Quand **Supprimer la ponctuation** est cochée, la ponctuation est bien retirée dans les **données d’analyse**.
- Le **concordancier HTML** continue d’afficher les segments issus du corpus, donc vous pouvez encore voir de la ponctuation dans le texte affiché.

### Classification double (rainette2)

- **Classification double** : l’application combine deux classifications rainette (res1 et res2) via rainette2, puis découpe l’arbre final avec k.

### Lemmatisation (option)

- **Lemmatisation** : si activée, le texte est **lemmatisé avec Spacy ou le dictionnaire de lemme provenant du logiciel IRaMuTeQ - lexique_fr**. La lemmatisation semble (beaucoup) plus efficace avec le dictionnaire IRaMuTeQ provenant de **OpenLexicon (modifié)**.

- <a href="https://openlexicon.fr/" target="_blank" rel="noopener noreferrer">OpenLexicon</a>

### Filtrage Morphosyntaxique
- **Tokens à conserver** : filtre les tokens conservés selon leur catégorie grammaticale (ex : NOUN, ADJ, VERB, PROPN, ADV...).

### Paramètres SpaCy/NER
- Activer NER (spaCy) => Détections des entités nommées (NER) par spaCy (ex : "Paris" = "LOC"). Le modele spaCy "md" est un peu léger... pour cette tâche.

### Exploration "Explore_rainette"

- **Classe** : sélection de la classe pour afficher les images et la table de statistiques associées.
- **CHD** : affichage graphique de la CHD.
- **Type** : bar (barres) ou cloud (nuage) pour l’affichage des termes par classe.
- **Statistiques** : chi2, lr, frequency, selon le critère utilisé pour classer les termes.
- Dans les exports CSV de type (`measure = "chi2"`), les colonnes suivantes sont importantes :
  - **`n_target`** : nombre d’occurrences du terme dans la classe/cluster analysé.
  - **`n_reference`** : nombre d’occurrences du même terme dans (tout) le corpus de référence (le reste des classes).
  - **`chi2`** et **`p`** : test d’association entre cible et référence ; plus `chi2` est élevé et `p` petite, plus le terme est spécifiquement lié à la classe.
- **Nombre de termes** : nombre de termes affichés par classe dans la visualisation.
- **Afficher les valeurs négatives** : inclut les termes négativement associés à une classe.

### Démarrage

- L’application ne fait plus de mise à jour automatique de `rainette` au lancement.
- Si vous voyez un ancien message `AUTO_UPDATE_RAINETTE=true -> tentative de mise à jour`, vérifiez que l’image/conteneur a bien été reconstruit avec la dernière version du dépôt.
