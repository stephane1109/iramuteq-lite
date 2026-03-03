# Rôle du fichier: iramuteq-like/ui_options_iramuteq.R définit les options UI
# spécifiques au mode IRaMuTeQ-like.

library(shiny)

ui_options_iramuteq <- function() {
  tagList(
    tags$div(class = "sidebar-section-title", "Paramètres CHD (IRaMuTeQ-like)"),
    numericInput("k_iramuteq", "Nombre de classes terminales de la phase 1", value = 10, min = 2, step = 1),
    radioButtons(
      "iramuteq_mincl_mode",
      "Nombre minimum d'UCE par classe terminale (mincl)",
      choices = c("Automatique" = "auto", "Manuel" = "manuel"),
      selected = "auto",
      inline = FALSE
    ),
    conditionalPanel(
      condition = "input.iramuteq_mincl_mode == 'manuel'",
      numericInput("iramuteq_mincl", "mincl (manuel)", value = 5, min = 1, step = 1)
    ),
    radioButtons(
      "iramuteq_classif_mode",
      "Type de classification terminale",
      choices = c("Simple" = "simple", "Double" = "double"),
      selected = "simple",
      inline = FALSE
    ),
    selectInput(
      "iramuteq_svd_method",
      "Méthode SVD",
      choices = c("irlba" = "irlba", "svdR" = "svdR"),
      selected = "irlba"
    ),
    checkboxInput("iramuteq_mode_patate", "Mode patate (moins précis, plus rapide)", value = FALSE)
  )
}
