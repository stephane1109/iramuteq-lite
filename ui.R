# Rôle du fichier: ui.R porte une partie du pipeline d'analyse IRaMuTeQ-like.
# ui.R

library(shiny)
library(htmltools)


if (!exists("ui_options_iramuteq", mode = "function", inherits = TRUE)) {
  app_dir <- tryCatch(shiny::getShinyOption("appDir"), error = function(e) NULL)
  if (is.null(app_dir) || !nzchar(app_dir)) app_dir <- getwd()
  chemin_options_iramuteq <- file.path(app_dir, "iramuteq-like", "ui_options_iramuteq.R")

  if (file.exists(chemin_options_iramuteq)) {
    source(chemin_options_iramuteq, encoding = "UTF-8", local = TRUE)
  }
}


if (!exists("ui_resultats_chd_iramuteq", mode = "function", inherits = TRUE)) {
  app_dir <- tryCatch(shiny::getShinyOption("appDir"), error = function(e) NULL)
  if (is.null(app_dir) || !nzchar(app_dir)) app_dir <- getwd()
  chemin_affichage_iramuteq <- file.path(app_dir, "iramuteq-like", "affichage_iramuteq-like.R")

  if (file.exists(chemin_affichage_iramuteq)) {
    source(chemin_affichage_iramuteq, encoding = "UTF-8", local = TRUE)
  }
}

if (!exists("ui_aide_huggingface", mode = "function")) {
  if (file.exists("help.md")) {
    ui_aide_huggingface <- function() {
      tagList(
        tags$h2("Aide"),
        includeMarkdown("help/help.md")
      )
    }
  } else {
    ui_aide_huggingface <- function() {
      tagList(
        tags$h2("Aide"),
        tags$p("Le fichier help.md est introuvable. Ajoute help.md à la racine du projet.")
      )
    }
  }
}

if (!exists("REGEX_CARACTERES_A_SUPPRIMER", inherits = TRUE)) {
  app_dir <- tryCatch(shiny::getShinyOption("appDir"), error = function(e) NULL)
  if (is.null(app_dir) || !nzchar(app_dir)) app_dir <- getwd()
  chemin_nettoyage <- file.path(app_dir, "iramuteq-like", "nettoyage_iramuteq.R")

  if (file.exists(chemin_nettoyage)) {
    source(chemin_nettoyage, encoding = "UTF-8", local = TRUE)
  }
}

if (!exists("REGEX_CARACTERES_A_SUPPRIMER", inherits = TRUE)) {
  # Fallback explicite : évite d'afficher un message d'erreur permanent dans l'UI
  # quand le fichier iramuteq-like/nettoyage_iramuteq.R n'a pas pu être sourcé dans cet environnement.
  REGEX_CARACTERES_AUTORISES <- "a-zA-Z0-9àÀâÂäÄáÁåÅãéÉèÈêÊëËìÌîÎïÏíÍóÓòÒôÔöÖõÕøØùÙûÛüÜúÚçÇßœŒ’ñÑ\\.:,;!\\?'"
  REGEX_CARACTERES_A_SUPPRIMER <- paste0("[^", REGEX_CARACTERES_AUTORISES, "]")
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      #shiny-modal .modal-dialog {
        width: 96vw !important;
        max-width: 96vw !important;
      }
      #shiny-modal .modal-body {
        max-height: 88vh !important;
        overflow-y: auto !important;
      }
      .sidebar-section-title {
        font-weight: 700;
        font-size: 18px !important;
        color: #1e5aa8 !important;
        margin-top: 12px;
        margin-bottom: 6px;
      }
      small {
        color: #842029 !important;
      }
    "))
  ),

  tags$h2(
    style = "color: #1e5aa8;",
    "IRaMuTeQ-Lite"
  ),
  tags$p(
    style = "font-size: 14px;",
    "Tentaive de reproduction de la CHD (Méthode Reinert) du logiciel IRaMuTeQ",
    tags$br(),
    "Pour plus d’informations, vous pouvez consulter mon site : www.codeandcortex.fr",
    tags$br(),
    "version beta 0.4 - 18-02-2026"
  ),

  sidebarLayout(
    sidebarPanel(
      fileInput("fichier_corpus", "Uploader un corpus IRaMuTeQ (.txt)", accept = c(".txt")),

      radioButtons(
        "modele_chd",
        "Méthode Iramuteq-like",
        choices = c(
          "IRaMuTeQ-like" = "iramuteq"
        ),
        selected = "iramuteq",
        inline = FALSE
      ),

      tags$div(class = "sidebar-section-title", "Paramètres généraux CHD"),
      numericInput("segment_size", "segment_size", value = 40, min = 5, step = 1),
      numericInput(
        "min_docfreq",
        "Fréquence minimale des termes (min_docfreq)",
        value = 2,
        min = 1,
        step = 1
      ),
      tags$p(
        style = "color: #d32f2f; font-size: 12px; margin-top: -8px;",
        "Plus la valeur est élevée, plus les termes rares sont exclus de l'analyse (risque de perdre des informations)."
      ),
      numericInput("max_p", "max_p (p-value)", value = 0.05, min = 0, max = 1, step = 0.01),
      checkboxInput(
        "filtrer_affichage_pvalue",
        "Filtrer l'affichage des résultats par p-value (p ≤ max_p)",
        value = TRUE
      ),

      conditionalPanel(
        condition = "input.modele_chd == 'iramuteq'",
        ui_options_iramuteq()
      ),

      tags$div(class = "sidebar-section-title", "Dictionnaire"),
      radioButtons(
        "source_dictionnaire",
        "Source de lemmatisation",
        choices = c("Lexique (fr)" = "lexique_fr"),
        selected = "lexique_fr",
        inline = FALSE
      ),
      conditionalPanel(
        condition = "input.source_dictionnaire == 'lexique_fr'",
        checkboxInput("lexique_utiliser_lemmes", "Lemmatisation via les lemmes de lexique_fr (forme → c_lemme)", value = TRUE)
      ),

      tags$div(class = "sidebar-section-title", "Nettoyage"),

      conditionalPanel(
        condition = "input.modele_chd == 'iramuteq'",
        tags$div(
          style = "margin: 0 0 8px 0; padding: 8px; background: #f7fbff; border-left: 3px solid #1e5aa8;",
          tags$strong("Options IRaMuTeQ-like (iramuteq-like/nettoyage_iramuteq.R)"),
          tags$br(),
          tags$small("Ces options pilotent la préparation du texte avant la tokenisation en mode IRaMuTeQ-like."),
          tags$br()
      ),

      checkboxInput("nettoyage_caracteres", "Nettoyage caractères (regex)", value = FALSE),
      checkboxInput("forcer_minuscules_avant", "Passage en minuscules avant tokenisation", value = FALSE),
      checkboxInput("supprimer_ponctuation", "Supprimer la ponctuation", value = FALSE),
      tags$small("Supprime la ponctuation à la tokenisation quanteda (remove_punct), par ex. . , ; : ! ? ' ’ \" - ( ) [ ] …"),
      checkboxInput("supprimer_chiffres", "Supprimer les chiffres (0-9)", value = FALSE),
      checkboxInput("supprimer_apostrophes", "Traiter les élisions FR (c'est→est, m'écrire→écrire)", value = FALSE),
      checkboxInput("retirer_stopwords", "Retirer les stopwords (liste française quanteda)", value = FALSE),
      tags$small("La normalisation en minuscules est appliquée automatiquement avant la construction du DFM."),
      checkboxInput("filtrage_morpho", "Filtrage morphosyntaxique", value = FALSE),
      tags$small("Le filtrage morphosyntaxique s'applique à lexique_fr."),
      conditionalPanel(
        condition = "input.filtrage_morpho == true",
        conditionalPanel(
          condition = "input.source_dictionnaire == 'lexique_fr'",
          selectizeInput(
            "pos_lexique_a_conserver",
            "Catégories c_morpho à conserver (lexique_fr)",
            choices = c(
              "NOM", "VER", "AUX", "ADJ", "ADV", "PRE", "CON", "ONO",
              "ADJ:NUM", "ADJ:POS", "ADJ:IND", "ADJ:INT", "ADJ:DEM",
              "PRO:PER", "PRO:POS", "PRO:DEM", "PRO:IND", "PRO:REL", "PRO:INT",
              "ART:DEF", "ART:IND"
            ),
            selected = c("NOM", "VER", "ADJ"),
            multiple = TRUE,
            options = list(plugins = list("remove_button"))
          )
        )
      ),
      tags$small("Regex appliquée quand “Nettoyage caractères (regex)” est activé :"),
      tags$pre(
        style = "white-space: pre-wrap; font-size: 11px; border: 1px solid #ddd; padding: 6px;",
        REGEX_CARACTERES_A_SUPPRIMER
      ),
      tags$small("Les caractères présents dans la liste entre crochets sont conservés ; tous les autres (ex. @ # & / emoji) sont remplacés par des espaces."),
      tags$small("L'option “Supprimer la ponctuation” pilote remove_punct, même si elle est autorisée par la regex ci-dessus."),
      tags$small("Cette option conserve les apostrophes lexicales (ex. aujourd'hui) et ne traite que les élisions en début de mot."),
      ),

      tags$hr(),

      tags$div(class = "sidebar-section-title", "Paramètres AFC"),

      checkboxInput("afc_reduire_chevauchement", "Réduire les chevauchements des mots (AFC)", value = FALSE),

      radioButtons(
        "afc_taille_mots",
        "Taille des mots (AFC termes)",
        choices = c("Fréquence" = "frequency", "Chi2" = "chi2"),
        selected = "frequency",
        inline = FALSE
      ),

      numericInput(
        "top_n",
        "Top N mots par classe (nuages)",
        value = 20,
        min = 5,
        step = 1
      ),

      tags$hr(),

      tags$div(
        style = "display: flex; gap: 8px; flex-wrap: wrap; align-items: center;",
        actionButton("lancer", "Lancer l'analyse")
      ),

      tags$hr(),

      downloadButton("dl_zip", "Télécharger exports (zip)"),
      downloadButton("dl_afc_zip", "Télécharger AFC (zip)")
    ),

    mainPanel(
      tabsetPanel(
        id = "onglets_principaux",

        tabPanel(
          "Analyse",
          tags$h3("Statut"),
          textOutput("statut"),
          tags$h3("Journal"),
          tags$pre(style = "white-space: pre-wrap;", textOutput("logs")),
          tags$h3("Analyse du corpus (mode debug)"),
          uiOutput("ui_table_stats_corpus"),
          tags$div(
            style = "width: 600px;",
            plotOutput("plot_stats_zipf", height = "600px", width = "600px")
          ),
          tags$h3("Répartition des classes"),
          tableOutput("table_classes")
        ),

        ui_resultats_chd_iramuteq(),

        tabPanel(
          "Prévisu corpus",
          tags$h3("Corpus importé"),
          uiOutput("ui_corpus_preview")
        ),

        
        tabPanel(
          "AFC",
          tags$h3("AFC"),
          uiOutput("ui_afc_statut"),
          uiOutput("ui_afc_erreurs"),

          tags$h4("AFC des classes (Représentation des classes)"),
          plotOutput("plot_afc_classes", height = "620px"),

          tags$h4("AFC des termes"),
          tags$p("Les mots sont colorés selon la classe où ils sont le plus surreprésentés (résidus standardisés) et leur taille est proportionnelle à leur fréquence globale ou chi2 (selon le choix)."),
          tags$div(
            style = "display:flex; gap:8px; align-items:center; margin-bottom:8px;",
            actionButton("afc_zoom_reset", "Réinitialiser le zoom AFC termes"),
            tags$small("Astuce: clique-glisse sur le graphique pour zoomer.")
          ),
          plotOutput("plot_afc", height = "720px", brush = "afc_brush"),
          tags$h4("Table des mots projetés (fréquence, chi2, p-value, segment exemple)"),
          uiOutput("ui_table_afc_mots_par_classe"),

          tags$h4("AFC des variables étoilées"),
          plotOutput("plot_afc_vars", height = "720px"),
          tags$h4("Table des modalités projetées"),
          tableOutput("table_afc_vars"),

          tags$h4("Valeurs propres"),
          tableOutput("table_afc_eig")
        ),
        
        tabPanel(
          "Aide",
          ui_aide_huggingface()
        )

      )
    )
  )
) # fin fluidPage
