# Rôle du fichier: centraliser l'affichage des résultats IRaMuTeQ-lite avec des sous-onglets dédiés.

ui_resultats_chd_iramuteq <- function() {
  tabPanel(
    "Résultats CHD Iramuteq",
    tabsetPanel(
      id = "tabs_resultats_chd_iramuteq",
      tabPanel(
        "Dendrogramme",
        tags$h3("Dendrogramme CHD (IRaMuTeQ-lite)"),
        plotOutput("plot_chd_iramuteq_dendro", height = "420px")
      ),
      tabPanel(
        "Stats CHD",
        tags$h3("Tableaux statistiques CHD par classe"),
        uiOutput("ui_tables_stats_chd_iramuteq")
      ),
      tabPanel(
        "Concordancier IRaMuTeQ-lite",
        tags$h3("Concordancier"),
        uiOutput("ui_concordancier_iramuteq"),
        tags$hr(),
        tags$h4("Concordancier AFC (mots et segments)", style = "margin-top: 18px;"),
        uiOutput("ui_table_afc_mots_par_classe")
      ),
      tabPanel(
        "Nuage de mots",
        tags$h3("Nuages de mots par classe"),
        uiOutput("ui_wordcloud_iramuteq")
      )
    )
  )
}
