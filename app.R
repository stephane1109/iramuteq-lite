# Rôle du fichier: app.R porte une partie du pipeline d'analyse Rainette.
# Ce script centralise une responsabilité métier/technique utilisée par l'application.

###############################################################################
#                    Script CHD - version beta 0.4 - 18-02-2026               #
#      A partir d'un corpus texte formaté aux exigences IRAMUTEQ              #
#                            Stéphane Meurisse                                #
#                           wwww.codeandcortex.fr                             #          
#                                                                             #
#      1.Réalise la CHD sur le corpus, sans rainette_explor                   #
#      2.Extrait chi2, lr, freq, docprop dans un CSV                          #
#      3.AFC                                                                  #
#      4.Recherche de NER avec Spacy (md)                                     #
#      5.Génère nuages de mots et graphes de cooccurrences par classe         #
#      6.Exporte les segments de texte par classe au format text              #
#      7.Creation d'un concordancier au format html                           #
#      8.Recherche de coocurrences                                            #
###############################################################################

library(shiny)
library(quanteda)
library(wordcloud)
library(RColorBrewer)
library(igraph)
library(dplyr)
library(htmltools)

options(shiny.maxRequestSize = 300 * 1024^2)
options(shinygadgets.viewer = shiny::browserViewer())
options(bspm.sudo = TRUE)

if (file.exists("help.md")) {
  ui_aide_huggingface <- function() {
    tagList(
      tags$h2("Aide"),
      includeMarkdown("help.md")
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


source("iramuteq-like/nettoyage_iramuteq.R", encoding = "UTF-8", local = TRUE)
source("iramuteq-like/concordancier-iramuteq.R", encoding = "UTF-8", local = TRUE)
source("spacy_ner/concordancier_ner.R", encoding = "UTF-8", local = TRUE)
source("iramuteq-like/afc_iramuteq.R", encoding = "UTF-8", local = TRUE)
source("iramuteq-like/ui_options_iramuteq.R", encoding = "UTF-8", local = TRUE)
source("iramuteq-like/affichage_iramuteq-like.R", encoding = "UTF-8", local = TRUE)
source("iramuteq-like/wordcloud_iramuteq.R", encoding = "UTF-8", local = TRUE)
source("ui.R", encoding = "UTF-8", local = TRUE)

source("iramuteq-like/chd_iramuteq.R", encoding = "UTF-8", local = TRUE)
source("iramuteq-like/dendogramme_iramuteq.R", encoding = "UTF-8", local = TRUE)
source("iramuteq-like/stats_chd.R", encoding = "UTF-8", local = TRUE)
source("iramuteq-like/chd_engine_iramuteq.R", encoding = "UTF-8", local = TRUE)

server <- function(input, output, session) {

  rv <- reactiveValues(
    logs = "",
    statut = "En attente.",
    progression = 0,

    base_dir = NULL,
    export_dir = NULL,
    segments_file = NULL,
    stats_file = NULL,
    html_file = NULL,
    ner_file = NULL,
    zip_file = NULL,

    res = NULL,
    res_chd = NULL,
    dfm_chd = NULL,
    dfm = NULL,
    filtered_corpus = NULL,
    res_stats_df = NULL,
    clusters = NULL,
    max_n_groups = NULL,
    max_n_groups_chd = NULL,

    res_type = "simple",

    exports_prefix = paste0("exports_", session$token),

    spacy_tokens_df = NULL,
    lexique_fr_df = NULL,
    textes_indexation = NULL,

    ner_df = NULL,
    ner_nb_segments = NA_integer_,

    afc_obj = NULL,
    afc_erreur = NULL,

    afc_vars_obj = NULL,
    afc_vars_erreur = NULL,

    afc_dir = NULL,
    afc_table_mots = NULL,
    afc_table_vars = NULL,
    afc_plot_classes = NULL,
    afc_plot_termes = NULL,
    afc_plot_vars = NULL,

    explor_assets = NULL,
    stats_corpus_df = NULL,
    stats_zipf_df = NULL
  )

  register_outputs_status(input, output, session, rv)

  output$ui_afc_statut <- renderUI({
    if (!is.null(rv$afc_erreur) && nzchar(rv$afc_erreur)) {
      return(tags$p("AFC : erreur (voir ci-dessous)."))
    }
    if (is.null(rv$afc_obj) || is.null(rv$afc_obj$ca)) {
      return(tags$p("AFC non calculée. Lance une analyse pour calculer l'AFC classes × termes."))
    }
    ncl <- nrow(rv$afc_obj$table)
    nt <- ncol(rv$afc_obj$table)
    tags$p(paste0("AFC calculée sur ", ncl, " classes et ", nt, " termes (table Classes × Termes)."))
  })

  output$ui_afc_erreurs <- renderUI({
    messages <- Filter(
      nzchar,
      list(
        rv$afc_erreur,
        rv$afc_vars_erreur
      )
    )

    if (length(messages) == 0) {
      return(NULL)
    }

    tags$div(
      style = "display: flex; flex-direction: column; gap: 8px; margin-bottom: 12px;",
      lapply(messages, function(msg) {
        tags$div(
          style = "border: 1px solid #f5c2c7; background: #f8d7da; color: #842029; border-radius: 4px; padding: 10px; white-space: pre-wrap;",
          msg
        )
      })
    )
  })

  output$ui_spacy_langue_detection <- renderUI({
    if (identical(input$source_dictionnaire, "lexique_fr")) {
      return(NULL)
    }

  output$ui_ner_lexique_incompatibilite <- renderUI({
    if (!isTRUE(input$activer_ner) || !identical(input$source_dictionnaire, "lexique_fr")) {
      return(NULL)
    }

  output$ui_corpus_preview <- renderUI({
    fichier <- input$fichier_corpus
    if (is.null(fichier) || is.null(fichier$datapath) || !file.exists(fichier$datapath)) {
      return(tags$p("Aucun corpus importé pour le moment."))
    }

    lignes <- tryCatch(
      readLines(fichier$datapath, encoding = "UTF-8", warn = FALSE),
      error = function(e) NULL
    )

    if (is.null(lignes) || length(lignes) == 0) {
      return(tags$p("Le corpus importé est vide ou illisible."))
    }

    max_lignes <- 250
    extrait <- lignes[seq_len(min(length(lignes), max_lignes))]
    texte <- paste(extrait, collapse = "\n")

    if (length(lignes) > max_lignes) {
      texte <- paste0(
        texte,
        "\n\n… Aperçu limité aux ", max_lignes,
        " premières lignes (", length(lignes), " lignes au total)."
      )
    }

    tags$div(
      tags$p(
        style = "margin-bottom: 8px;",
        paste0("Fichier : ", fichier$name)
      ),
      tags$pre(
        style = "white-space: pre-wrap; max-height: 70vh; overflow-y: auto; border: 1px solid #ddd; padding: 10px; background: #fafafa;",
        texte
      )
    )
  })

  output$ui_table_stats_corpus <- renderUI({
    req(rv$stats_corpus_df)

    definitions <- c(
      "Nom du corpus" = "Nom du fichier corpus importé.",
      "Nombre de textes" = "Nombre d'unités de texte détectées dans le corpus.",
      "Nombre de mots dans le corpus" = "Total des occurrences de mots (tokens).",
      "Nombre de formes" = "Nombre de formes lexicales distinctes (types), différent des hapax.",
      "Nombre de segments de texte" = "Nombre de segments après découpage pour l'analyse.",
      "Nombre d'Hapax" = "Nombre de formes apparaissant une seule fois dans le corpus.",
      "Loi de Zpif" = "Indicateur de conformité approximative à la loi de Zipf."
    )

    lignes <- lapply(seq_len(nrow(rv$stats_corpus_df)), function(i) {
      metrique <- as.character(rv$stats_corpus_df$Metrique[i])
      valeur <- as.character(rv$stats_corpus_df$Valeur[i])
      definition <- unname(definitions[[metrique]])
      if (is.null(definition) || !nzchar(definition)) definition <- ""

      tags$tr(
        tags$td(
          tags$div(metrique),
          if (nzchar(definition)) tags$div(
            style = "font-size: 0.85em; color: #c62828; margin-top: 2px;",
            definition
          )
        ),
        tags$td(valeur)
      )
    })

    tags$table(
      class = "table table-striped table-condensed",
      tags$thead(
        tags$tr(
          tags$th("Metrique"),
          tags$th("Valeur")
        )
      ),
      tags$tbody(lignes)
    )
  })


  output$plot_stats_zipf <- renderPlot({
    req(rv$stats_zipf_df)
    df <- rv$stats_zipf_df
    if (is.null(df) || nrow(df) < 2) {
      plot.new()
      text(0.5, 0.5, "Données insuffisantes pour tracer la loi de Zpif.", cex = 1.1)
      return(invisible(NULL))
    }

    x_lim <- range(df$log_rang, na.rm = TRUE)
    y_lim <- range(c(df$log_frequence, df$log_pred), na.rm = TRUE)

    plot(
      x = df$log_rang,
      y = df$log_frequence,
      pch = 16,
      cex = 0.8,
      col = grDevices::adjustcolor("#2C7FB8", alpha.f = 0.7),
      xlab = "log(rang)",
      ylab = "log(fréquence)",
      main = "Loi de Zpif",
      xlim = x_lim,
      ylim = y_lim,
      asp = 1
    )
    grid(col = "#E6E6E6", lty = "dotted")

    ord <- order(df$log_rang)
    lines(df$log_rang[ord], df$log_pred[ord], col = "#D7301F", lwd = 2.5)

    legend(
      "topright",
      legend = c("Données", "Régression log-log"),
      col = c("#2C7FB8", "#D7301F"),
      pch = c(16, NA),
      lty = c(NA, 1),
      lwd = c(NA, 2),
      bty = "n"
    )
  })

  output$ui_chd_statut <- renderUI({
    if (is.null(rv$res)) {
      return(tags$p("CHD non disponible. Lance une analyse."))
    }

    nb_classes <- NA_integer_
    if (!is.null(rv$clusters)) nb_classes <- length(rv$clusters)

    if (identical(rv$res_type, "iramuteq")) {
      return(tags$p(paste0("CHD disponible (moteur IRaMuTeQ-like) - classes détectées : ", nb_classes, ".")))
    }

    if (identical(rv$res_type, "double")) {
      return(tags$p("CHD disponible (classification double rainette2)."))
    }

    tags$p(paste0("CHD disponible (classification simple rainette) - classes détectées : ", nb_classes, "."))
  })

  register_events_lancer(input, output, session, rv)
  register_rainette_explor_affichage(input, output, session, rv)

  output$plot_afc_classes <- renderPlot({
    if (!is.null(rv$afc_erreur) && nzchar(rv$afc_erreur)) {
      plot.new()
      text(0.5, 0.5, "AFC indisponible (erreur).", cex = 1.1)
      return(invisible(NULL))
    }
    if (is.null(rv$afc_obj) || is.null(rv$afc_obj$ca)) {
      plot.new()
      text(0.5, 0.5, "AFC non disponible. Lance une analyse.", cex = 1.1)
      return(invisible(NULL))
    }
    tracer_afc_classes_seules(rv$afc_obj, axes = c(1, 2), cex_labels = 1.05)
  })

    panneaux <- lapply(classes, function(cl) {
      output_id <- paste0("table_stats_chd_iramuteq_cl_", cl)

      output[[output_id]] <- renderTable({
        extraire_stats_chd_classe(
          rv$res_stats_df,
          classe = cl,
          n_max = 100,
          show_negative = FALSE,
          max_p = if (isTRUE(input$filtrer_affichage_pvalue)) input$max_p else 1,
          seuil_p_significativite = input$max_p,
          style = "iramuteq_clone"
        )
      }, rownames = FALSE, sanitize.text.function = function(x) x)

      tabPanel(
        title = paste0("Classe ", cl),
        tableOutput(output_id)
      )
    })

    do.call(tabsetPanel, c(id = "tabs_stats_chd_iramuteq", panneaux))
  })


    if (is.null(rv$exports_prefix) || !nzchar(rv$exports_prefix)) {
      return(tags$div(
        style = "padding: 12px;",
        tags$p("Préfixe de ressources invalide."),
        tags$p("Relance l'analyse pour régénérer les exports.")
      ))
    }

    if (!(rv$exports_prefix %in% names(shiny::resourcePaths()))) {
      shiny::addResourcePath(rv$exports_prefix, rv$export_dir)
    }

    candidats_html <- c(
      rv$html_file,
      file.path(rv$export_dir, "segments_par_classe.html"),
      file.path(rv$export_dir, "concordancier.html")
    )
    candidats_dyn <- list.files(
      rv$export_dir,
      pattern = "(segments.*classe|concord).*\\.html$",
      ignore.case = TRUE,
      full.names = TRUE
    )
    candidats_html <- c(candidats_html, candidats_dyn)
    candidats_html <- unique(candidats_html[!is.na(candidats_html) & nzchar(candidats_html)])
    html_existant <- candidats_html[file.exists(candidats_html)]

    if (length(html_existant) == 0) {
      return(tags$div(
        style = "padding: 12px;",
        tags$p("Le fichier du concordancier HTML n'est pas disponible pour cette analyse."),
        tags$p("Relance l'analyse puis vérifie les logs si le problème persiste.")
      ))
    }

    src_html <- html_existant[[1]]
    nom_html <- basename(src_html)
    src_dans_exports <- file.path(rv$export_dir, nom_html)

    if (!isTRUE(file.exists(src_dans_exports))) {
      ok_copy <- tryCatch(file.copy(src_html, src_dans_exports, overwrite = TRUE), error = function(e) FALSE)
      if (isTRUE(ok_copy)) src_html <- src_dans_exports
    } else {
      src_html <- src_dans_exports
    }

    tags$iframe(
      src = paste0("/", rv$exports_prefix, "/", basename(src_html)),
      style = "width: 100%; height: 70vh; border: 1px solid #999;"
    )
  })

    tags$div(
      style = "text-align: center;",
      tags$img(
        src = paste0("/", rv$exports_prefix, "/", src_rel),
        style = "max-width: 100%; height: auto; border: 1px solid #999; display: inline-block;"
      )
    )
  })

  output$table_stats_classe <- renderTable({
    req(input$classe_viz, rv$res_stats_df)
    classe_norm <- normaliser_id_classe_ui(input$classe_viz)
    classe_stats <- if (is.na(classe_norm)) input$classe_viz else classe_norm

    extraire_stats_chd_classe(
      rv$res_stats_df,
      classe = classe_stats,
      n_max = 50,
      max_p = if (isTRUE(input$filtrer_affichage_pvalue)) input$max_p else 1,
      seuil_p_significativite = input$max_p,
      style = "iramuteq_clone"
    )
  }, rownames = FALSE, sanitize.text.function = function(x) x)

  output$plot_afc <- renderPlot({
    if (!is.null(rv$afc_erreur) && nzchar(rv$afc_erreur)) {
      plot.new()
      text(0.5, 0.5, "AFC indisponible (erreur).", cex = 1.1)
      return(invisible(NULL))
    }
    if (is.null(rv$afc_obj) || is.null(rv$afc_obj$ca)) {
      plot.new()
      text(0.5, 0.5, "AFC non disponible. Lance une analyse.", cex = 1.1)
      return(invisible(NULL))
    }

    activer_repel <- TRUE
    if (!is.null(input$afc_reduire_chevauchement)) activer_repel <- isTRUE(input$afc_reduire_chevauchement)

    taille_sel <- "frequency"
    if (!is.null(input$afc_taille_mots) && nzchar(as.character(input$afc_taille_mots))) {
      taille_sel <- as.character(input$afc_taille_mots)
    }
    if (!taille_sel %in% c("frequency", "chi2")) taille_sel <- "frequency"

    top_termes <- 120
    if (!is.null(input$afc_top_termes) && is.finite(input$afc_top_termes)) top_termes <- as.integer(input$afc_top_termes)

    tracer_afc_classes_termes(rv$afc_obj, axes = c(1, 2), top_termes = top_termes, taille_sel = taille_sel, activer_repel = activer_repel)
  })

  output$ui_table_afc_mots_par_classe <- renderUI({
    if (is.null(rv$afc_table_mots)) {
      output$table_afc_mots_message <- renderTable({
        data.frame(Message = "AFC mots : non disponible.", stringsAsFactors = FALSE)
      }, rownames = FALSE)
      return(tableOutput("table_afc_mots_message"))
    }

    df <- rv$afc_table_mots
    colonnes <- intersect(c("Terme", "Classe_max", "frequency", "chi2", "p_value", "Segment_texte"), names(df))
    df <- df[, colonnes, drop = FALSE]
    if ("p_value" %in% names(df)) {
      df$p_value <- ifelse(
        is.na(df$p_value),
        NA_character_,
        formatC(df$p_value, format = "f", digits = 6)
      )
    }

    classes <- unique(as.character(df$Classe_max))
    classes <- classes[!is.na(classes) & nzchar(classes)]
    classes <- sort(classes)

    if (length(classes) == 0) {
      output$table_afc_mots_message <- renderTable({
        data.frame(Message = "AFC mots : aucune classe disponible.", stringsAsFactors = FALSE)
      }, rownames = FALSE)
      return(tableOutput("table_afc_mots_message"))
    }

    ui_tables <- lapply(seq_along(classes), function(i) {
      cl <- classes[[i]]
      id <- paste0("table_afc_mots_", i)

      output[[id]] <- renderUI({
        sous_df <- df[df$Classe_max == cl, , drop = FALSE]
        colonnes <- intersect(c("Terme", "frequency", "chi2", "p_value", "Segment_texte"), names(sous_df))
        sous_df <- sous_df[, colonnes, drop = FALSE]

        if ("p_value" %in% names(sous_df)) {
          sous_df$p_value <- ifelse(
            is.na(sous_df$p_value),
            NA_character_,
            formatC(sous_df$p_value, format = "f", digits = 6)
          )
        }

        if ("chi2" %in% names(sous_df)) {
          sous_df <- sous_df[order(-sous_df$chi2), , drop = FALSE]
          sous_df$chi2 <- ifelse(
            is.na(sous_df$chi2),
            NA_character_,
            formatC(sous_df$chi2, format = "f", digits = 6)
          )
        }

        sous_df <- head(sous_df, 100)
        generer_table_html_afc_mots(sous_df)
      })

      tagList(
        tags$h5(cl),
        uiOutput(id)
      )
    })

    do.call(tagList, ui_tables)
  })

  output$plot_afc_vars <- renderPlot({
    if (!is.null(rv$afc_vars_erreur) && nzchar(rv$afc_vars_erreur)) {
      plot.new()
      text(0.5, 0.5, "AFC variables étoilées indisponible (erreur).", cex = 1.1)
      return(invisible(NULL))
    }
    if (is.null(rv$afc_vars_obj) || is.null(rv$afc_vars_obj$ca)) {
      plot.new()
      text(0.5, 0.5, "AFC variables étoilées non disponible. Lance une analyse.", cex = 1.1)
      return(invisible(NULL))
    }

    activer_repel <- TRUE
    if (!is.null(input$afc_reduire_chevauchement)) activer_repel <- isTRUE(input$afc_reduire_chevauchement)

    top_mod <- 120
    if (!is.null(input$afc_top_modalites) && is.finite(input$afc_top_modalites)) top_mod <- as.integer(input$afc_top_modalites)

    tracer_afc_variables_etoilees(rv$afc_vars_obj, axes = c(1, 2), top_modalites = top_mod, activer_repel = activer_repel)
  })

  output$table_afc_vars <- renderTable({
    if (is.null(rv$afc_table_vars)) {
      return(data.frame(Message = "AFC variables étoilées : non disponible.", stringsAsFactors = FALSE))
    }
    df <- rv$afc_table_vars
    colonnes <- intersect(c("Modalite", "Classe_max", "frequency", "chi2", "p_value"), names(df))
    df <- df[, colonnes, drop = FALSE]
    if ("p_value" %in% names(df)) {
      p_values <- df$p_value
      df$p_value <- ifelse(
        is.na(p_values),
        NA_character_,
        ifelse(
          p_values > 0.05,
          sprintf("<span style='color:#d97706;font-weight:600;'>%s</span>", formatC(p_values, format = "f", digits = 6)),
          formatC(p_values, format = "f", digits = 6)
        )
      )
    }
    if ("chi2" %in% names(df)) df <- df[order(-df$chi2), , drop = FALSE]
    if ("chi2" %in% names(df)) {
      df$chi2 <- ifelse(
        is.na(df$chi2),
        NA_character_,
        formatC(df$chi2, format = "f", digits = 6)
      )
    }
    head(df, 200)
  }, rownames = FALSE, sanitize.text.function = function(x) x)

  output$table_afc_eig <- renderTable({
    if (!is.null(rv$afc_erreur) && nzchar(rv$afc_erreur)) {
      return(data.frame(Message = "AFC indisponible (erreur).", stringsAsFactors = FALSE))
    }
    if (is.null(rv$afc_obj) || is.null(rv$afc_obj$ca)) {
      return(data.frame(Message = "AFC non disponible.", stringsAsFactors = FALSE))
    }
    eig <- rv$afc_obj$ca$eig
    if (is.null(eig)) return(data.frame(Message = "Valeurs propres indisponibles.", stringsAsFactors = FALSE))
    df <- as.data.frame(eig)
    df$Dim <- rownames(df)
    rownames(df) <- NULL
    df <- df[, c("Dim", names(df)[1], names(df)[2], names(df)[3]), drop = FALSE]
    names(df) <- c("Dim", "Valeur_propre", "Pourcentage_inertie", "Pourcentage_cumule")
    df
  }, rownames = FALSE)

  output$dl_segments <- downloadHandler(
    filename = function() "segments_par_classe.txt",
    content = function(file) {
      req(rv$segments_file)
      file.copy(rv$segments_file, file, overwrite = TRUE)
    }
  )

  output$dl_stats <- downloadHandler(
    filename = function() "stats_par_classe.csv",
    content = function(file) {
      req(rv$stats_file)
      file.copy(rv$stats_file, file, overwrite = TRUE)
    }
  )

  output$dl_html <- downloadHandler(
    filename = function() "segments_par_classe.html",
    content = function(file) {
      req(rv$html_file)
      file.copy(rv$html_file, file, overwrite = TRUE)
    }
  )

  output$dl_zip <- downloadHandler(
    filename = function() "exports_rainette.zip",
    content = function(file) {
      req(rv$zip_file)
      file.copy(rv$zip_file, file, overwrite = TRUE)
    }
  )

  output$dl_afc_zip <- downloadHandler(
    filename = function() "afc_exports.zip",
    content = function(file) {
      req(rv$afc_dir)
      zip_tmp <- tempfile(fileext = ".zip")
      ancien <- getwd()
      on.exit(setwd(ancien), add = TRUE)
      setwd(dirname(rv$afc_dir))
      if (file.exists(zip_tmp)) unlink(zip_tmp)
      utils::zip(zipfile = zip_tmp, files = basename(rv$afc_dir))
      file.copy(zip_tmp, file, overwrite = TRUE)
    }
  )

}

app <- shinyApp(ui = ui, server = server)
app
