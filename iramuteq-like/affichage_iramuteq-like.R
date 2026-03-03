# Rôle du fichier: centraliser l'affichage des résultats IRaMuTeQ-like avec des sous-onglets dédiés.

ui_resultats_chd_iramuteq <- function() {
  tabPanel(
    "Résultats CHD Iramuteq",
    tabsetPanel(
      id = "tabs_resultats_chd_iramuteq",
      tabPanel(
        "Dendrogramme",
        tags$h3("Dendrogramme CHD (IRaMuTeQ-like)"),
        radioButtons(
          "iramuteq_dendro_display_method",
          "Méthode d'affichage",
          choices = c(
            "Style IRaMuTeQ (barres + mots par classe)" = "iramuteq_blocks",
            "Standard (labels près des classes)" = "standard",
            "Compact (légende des termes en bas)" = "compact"
          ),
          selected = "iramuteq_blocks",
          inline = FALSE
        ),
        plotOutput("plot_chd_iramuteq_dendro", height = "420px")
      ),
      tabPanel(
        "Stats CHD",
        tags$h3("Tableaux statistiques CHD par classe"),
        uiOutput("ui_tables_stats_chd_iramuteq")
      ),
      tabPanel(
        "Concordancier IRaMuTeQ-like",
        tags$h3("Concordancier"),
        uiOutput("ui_concordancier_iramuteq")
      ),
      tabPanel(
        "Nuage de mots",
        tags$h3("Nuage de mots par classe"),
        selectInput("classe_viz_iramuteq", "Classe", choices = c("1"), selected = "1"),
        uiOutput("ui_wordcloud_iramuteq")
      )
    )
  )
}
