# Rôle du fichier: server_events_lancer_iramuteq.R porte le pipeline d'analyse IRaMuTeQ-like.
# Ce script centralise une responsabilité métier/technique utilisée par l'application.
# Module server - événement principal `input$lancer`
# Ce fichier encapsule le pipeline principal lancé au clic sur "Lancer l'analyse"
# (préparation, CHD/AFC/NER, exports) pour alléger `app.R` à comportement constant.

register_events_lancer <- function(input, output, session, rv) {
    app_dir <- tryCatch(shiny::getShinyOption("appDir"), error = function(e) NULL)
    if (is.null(app_dir) || !nzchar(app_dir)) app_dir <- getwd()
    env_modules <- environment()

    charger_module_langue <- function() {
      candidats_langue <- unique(c(
        file.path(app_dir, "iramuteq-like", "nlp_lexique_iramuteq.R"),
        file.path(getwd(), "iramuteq-like", "nlp_lexique_iramuteq.R"),
        file.path("iramuteq-like", "nlp_lexique_iramuteq.R"),
        file.path(app_dir, "iramuteq-like", "nlp_lexique_iramuteq.R"),
        file.path(getwd(), "iramuteq-like", "nlp_lexique_iramuteq.R"),
        file.path("iramuteq-like", "nlp_lexique_iramuteq.R")
      ))

      dernier_chemin <- candidats_langue[[1]]
      derniere_raison <- "fonction verifier_coherence_dictionnaire_langue absente après source"

      for (chemin_langue in candidats_langue) {
        dernier_chemin <- chemin_langue
        if (!file.exists(chemin_langue)) next

        source_res <- tryCatch({
          source(chemin_langue, encoding = "UTF-8", local = env_modules)
          NULL
        }, error = function(e) e)

        if (inherits(source_res, "error")) {
          derniere_raison <- paste0("échec source: ", conditionMessage(source_res))
          next
        }

        if (exists("verifier_coherence_dictionnaire_langue", mode = "function", envir = env_modules, inherits = TRUE)) {
          return(list(ok = TRUE, chemin = chemin_langue, raison = ""))
        }

        derniere_raison <- "fonction verifier_coherence_dictionnaire_langue absente après source"
      }

      list(ok = FALSE, chemin = dernier_chemin, raison = derniere_raison)
    }

    if (!exists("appliquer_nettoyage_iramuteq", mode = "function", inherits = TRUE)) {
      chemin_nettoyage_iramuteq <- file.path(app_dir, "iramuteq-like", "nettoyage_iramuteq.R")
      if (file.exists(chemin_nettoyage_iramuteq)) {
        source(chemin_nettoyage_iramuteq, encoding = "UTF-8", local = TRUE)
      }
    }

    if (!exists("appliquer_nettoyage_rainette", mode = "function", inherits = TRUE)) {
      appliquer_nettoyage_rainette <- function(textes,
                                               activer_nettoyage = FALSE,
                                               forcer_minuscules = FALSE,
                                               supprimer_chiffres = FALSE,
                                               supprimer_apostrophes = FALSE) {
        ajouter_log(rv, "Avertissement: appliquer_nettoyage_rainette indisponible; nettoyage contourné pour préserver l'exécution.")
        if (is.null(textes)) return(character(0))
        x <- as.character(textes)
        if (isTRUE(forcer_minuscules)) x <- tolower(x)
        x
      }
    }

    if (!exists("appliquer_nettoyage_iramuteq", mode = "function", inherits = TRUE)) {
      appliquer_nettoyage_iramuteq <- appliquer_nettoyage_rainette
    }


    executer_textprepa_iramuteq <- function(ids, textes, input, rv) {
      candidats_script <- unique(c(
        file.path(app_dir, "iramuteq-like", "textprepa_iramuteq.py"),
        file.path(getwd(), "iramuteq-like", "textprepa_iramuteq.py"),
        file.path("iramuteq-like", "textprepa_iramuteq.py")
      ))
      script_path <- candidats_script[file.exists(candidats_script)][1]
      if (is.na(script_path) || !nzchar(script_path)) {
        stop("IRaMuTeQ-like: script textprepa_iramuteq.py introuvable.")
      }

      py_bin <- Sys.which("python3")
      if (!nzchar(py_bin)) py_bin <- Sys.which("python")
      if (!nzchar(py_bin)) {
        stop("IRaMuTeQ-like: Python introuvable (python3/python).")
      }

      in_tsv <- tempfile(pattern = "iramuteq_prepa_in_", fileext = ".tsv")
      out_tsv <- tempfile(pattern = "iramuteq_prepa_out_", fileext = ".tsv")

      df_in <- data.frame(
        doc_id = as.character(ids),
        text = as.character(textes),
        stringsAsFactors = FALSE
      )
      write.table(df_in, file = in_tsv, sep = "	", row.names = FALSE, col.names = TRUE, quote = TRUE, fileEncoding = "UTF-8")

      args <- c(
        script_path,
        "--input", in_tsv,
        "--output", out_tsv,
        "--nettoyage_caracteres", ifelse(isTRUE(input$nettoyage_caracteres), "1", "0"),
        "--forcer_minuscules_avant", ifelse(isTRUE(input$forcer_minuscules_avant), "1", "0"),
        "--supprimer_chiffres", ifelse(isTRUE(input$supprimer_chiffres), "1", "0"),
        "--supprimer_apostrophes", ifelse(isTRUE(input$supprimer_apostrophes), "1", "0")
      )

      res <- tryCatch(
        system2(py_bin, args = args, stdout = TRUE, stderr = TRUE),
        error = function(e) structure(conditionMessage(e), status = 1L)
      )
      status <- attr(res, "status")
      if (is.null(status)) status <- 0L
      if (!identical(as.integer(status), 0L) || !file.exists(out_tsv)) {
        out_msg <- if (length(res)) paste(res, collapse = " | ") else "(aucun message)"
        stop(paste0("IRaMuTeQ-like: échec textprepa_iramuteq.py (code ", status, ") : ", out_msg))
      }

      df_out <- read.delim(out_tsv, sep = "	", header = TRUE, stringsAsFactors = FALSE, quote = '"', encoding = "UTF-8")
      if (!all(c("doc_id", "text") %in% names(df_out))) {
        stop("IRaMuTeQ-like: sortie textprepa invalide (colonnes doc_id/text manquantes).")
      }

      ids_chr <- as.character(ids)
      idx <- match(ids_chr, as.character(df_out$doc_id))
      if (any(is.na(idx))) {
        stop("IRaMuTeQ-like: alignement doc_id invalide après textprepa.")
      }

      textes_prep <- as.character(df_out$text[idx])
      names(textes_prep) <- ids_chr
      ajouter_log(rv, "IRaMuTeQ-like: préparation texte exécutée via iramuteq-like/textprepa_iramuteq.py")
      textes_prep
    }

    formater_df_csv_6_decimales <- function(df) {
      if (is.null(df)) return(df)
      df_out <- df
      for (nm in names(df_out)) {
        col <- df_out[[nm]]
        if (is.numeric(col)) {
          df_out[[nm]] <- ifelse(
            is.na(col),
            NA_character_,
            formatC(col, format = "f", digits = 6)
          )
        }
      }
      df_out
    }

    ecrire_csv_6_decimales <- function(df, chemin, row.names = FALSE) {
      write.csv(formater_df_csv_6_decimales(df), chemin, row.names = row.names)
    }

    observeEvent(input$modele_chd, {
      if (identical(as.character(input$modele_chd), "iramuteq")) {
        updateRadioButtons(
          session,
          "source_dictionnaire",
          choices = c("Lexique (fr)" = "lexique_fr"),
          selected = "lexique_fr"
        )
        if (isTRUE(input$activer_ner)) {
          updateCheckboxInput(session, "activer_ner", value = FALSE)
          ajouter_log(rv, "Mode IRaMuTeQ-like : NER spaCy automatiquement désactivé (mode uniquement Lexique fr).")
        }
      } else {
        updateRadioButtons(
          session,
          "source_dictionnaire",
          choices = c("spaCy" = "spacy", "Lexique (fr)" = "lexique_fr"),
          selected = if (identical(as.character(input$source_dictionnaire), "lexique_fr")) "lexique_fr" else "spacy"
        )
      }
    }, ignoreInit = FALSE)

    output$ui_concordancier_iramuteq <- renderUI({
      req(rv$export_dir)

      if (!identical(rv$res_type, "iramuteq")) {
        return(tags$p("Concordancier IRaMuTeQ-like indisponible (mode Rainette actif)."))
      }

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

    output$ui_wordcloud_iramuteq <- renderUI({
      req(rv$export_dir, rv$exports_prefix)

      if (!identical(rv$res_type, "iramuteq")) {
        return(tags$p("Nuage de mots IRaMuTeQ-like indisponible (mode Rainette actif)."))
      }

      classe_sel <- as.character(input$classe_viz_iramuteq)
      if (length(classe_sel) != 1 || is.na(classe_sel) || !nzchar(classe_sel)) {
        return(tags$p("Sélectionne une classe pour afficher le nuage de mots."))
      }

      src_rel <- file.path("wordclouds", paste0("cluster_", classe_sel, "_wordcloud.png"))
      if (!file.exists(file.path(rv$export_dir, src_rel))) {
        return(tags$p("Aucun nuage de mots disponible pour cette classe."))
      }

      tags$div(
        style = "text-align: center;",
        tags$img(
          src = paste0("/", rv$exports_prefix, "/", src_rel),
          style = "max-width: 100%; height: auto; border: 1px solid #999; display: inline-block;"
        )
      )
    })

    normaliser_id_classe_local <- function(x) {
      x_chr <- as.character(x)
      x_chr <- trimws(x_chr)

      x_num <- suppressWarnings(as.numeric(x_chr))
      need_extract <- is.na(x_num) & !is.na(x_chr) & nzchar(x_chr)

      if (any(need_extract)) {
        extrait <- sub("^.*?(\\d+).*$", "\\1", x_chr[need_extract])
        extrait[!grepl("\\d", x_chr[need_extract])] <- NA_character_
        x_num[need_extract] <- suppressWarnings(as.numeric(extrait))
      }

      x_num
    }

    observeEvent(input$lancer, {
      rv$logs <- ""
      rv$statut <- "Vérification du fichier..."
      rv$progression <- 0

      rv$spacy_tokens_df <- NULL
      rv$lexique_fr_df <- NULL
      rv$textes_indexation <- NULL
      rv$ner_df <- NULL
      rv$ner_nb_segments <- NA_integer_
      rv$afc_obj <- NULL
      rv$afc_erreur <- NULL
      rv$afc_vars_obj <- NULL
      rv$afc_vars_erreur <- NULL

      rv$afc_dir <- NULL
      rv$afc_table_mots <- NULL
      rv$afc_table_vars <- NULL
      rv$afc_plot_classes <- NULL
      rv$afc_plot_termes <- NULL
      rv$afc_plot_vars <- NULL

      rv$segments_file <- NULL
      rv$stats_file <- NULL
      rv$html_file <- NULL
      rv$ner_file <- NULL
      rv$zip_file <- NULL

      rv$res <- NULL
      rv$res_chd <- NULL
      rv$dfm_chd <- NULL
      rv$res_type <- "simple"
      rv$max_n_groups <- NULL
      rv$max_n_groups_chd <- NULL
      rv$explor_assets <- NULL
      rv$stats_corpus_df <- NULL
      rv$stats_zipf_df <- NULL

      ajouter_log(rv, "Clic sur 'Lancer l'analyse' reçu.")

      modele_chd <- "iramuteq"
      mode_iramuteq <- TRUE
      source_dictionnaire <- "lexique_fr"
      updateRadioButtons(
        session,
        "source_dictionnaire",
        choices = c("Lexique (fr)" = "lexique_fr"),
        selected = "lexique_fr"
      )
      updateRadioButtons(
        session,
        "modele_chd",
        selected = "iramuteq"
      )

      if (is.null(input$fichier_corpus) || is.null(input$fichier_corpus$datapath) || !file.exists(input$fichier_corpus$datapath)) {
        rv$statut <- "Aucun fichier uploadé."
        rv$progression <- 0
        ajouter_log(rv, "Aucun fichier uploadé côté serveur. Sélectionne un .txt puis relance.")
        showNotification("Aucun fichier uploadé. Choisis un .txt.", type = "error", duration = 6)
        return(invisible(NULL))
      }

      if (isTRUE(input$activer_ner) && mode_iramuteq) {
        rv$statut <- "Configuration invalide : NER indisponible en mode IRaMuTeQ-like."
        rv$progression <- 0
        ajouter_log(rv, "Blocage de l'analyse : NER activé en mode IRaMuTeQ-like (mode uniquement Lexique fr).")
        showNotification(
          "Analyse bloquée : le mode IRaMuTeQ-like fonctionne uniquement avec Lexique (fr) et sans NER spaCy.",
          type = "error",
          duration = 8
        )
        return(invisible(NULL))
      }

      if (isTRUE(input$activer_ner) && !is.null(input$fichier_ner_json) && !is.null(input$fichier_ner_json$datapath) && file.exists(input$fichier_ner_json$datapath)) {
        rv$ner_file <- input$fichier_ner_json$datapath
        ajouter_log(rv, paste0("NER : dictionnaire JSON importé via l'UI : ", input$fichier_ner_json$name))
      }

        p <- Progress$new(session, min = 0, max = 1)
        on.exit(try(p$close(), silent = TRUE), add = TRUE)

        avancer <- function(valeur, detail) {
          valeur <- max(0, min(1, valeur))
          p$set(value = valeur, message = "Calculs CHD en cours", detail = detail)
          rv$progression <- round(valeur * 100)
        }

        tryCatch({

          avancer(0.02, "Préparation des répertoires")
          rv$statut <- "Préparation des répertoires..."

          rv$base_dir <- file.path(tempdir(), paste0("rainette_", session$token))
          rv$export_dir <- file.path(rv$base_dir, "exports")
          dir.create(rv$export_dir, showWarnings = FALSE, recursive = TRUE)
          ajouter_log(rv, paste0("export_dir = ", rv$export_dir))

          avancer(0.08, "Import du corpus")
          rv$statut <- "Import du corpus..."
          chemin_fichier <- input$fichier_corpus$datapath
          md5 <- md5_fichier(chemin_fichier)
          ajouter_log(rv, paste0("MD5 fichier = ", md5))

          corpus <- import_corpus_iramuteq(chemin_fichier)
          ajouter_log(rv, paste0("Nombre de documents importés : ", ndoc(corpus)))

          avancer(0.14, "Segmentation")
          rv$statut <- "Segmentation..."
          segment_size <- input$segment_size
          corpus <- split_segments(corpus, segment_size = segment_size)
          ajouter_log(rv, paste0("Nombre de segments après découpage : ", ndoc(corpus)))

          stats_corpus <- calculer_stats_corpus(
            chemin_fichier = chemin_fichier,
            corpus_segments = corpus,
            nom_corpus = input$fichier_corpus$name
          )
          if (is.null(stats_corpus)) {
            rv$stats_corpus_df <- NULL
            rv$stats_zipf_df <- NULL
          } else {
            rv$stats_corpus_df <- stats_corpus$table
            rv$stats_zipf_df <- stats_corpus$zipf
          }

          ids_orig <- as.character(docnames(corpus))
          ids_corpus <- ids_orig
          invalides <- is.na(ids_corpus) | !nzchar(trimws(ids_corpus))
          if (any(invalides)) {
            ids_corpus[invalides] <- paste0("doc_", which(invalides))
          }

          ids_uniques <- make.unique(ids_corpus, sep = "_dup")
          modif_ids <- any(ids_uniques != ids_orig)
          if (isTRUE(modif_ids)) {
            n_problemes <- sum(invalides) + sum(duplicated(ids_corpus))
            ajouter_log(rv, paste0("Docnames invalides/dupliqués détectés après segmentation : ", n_problemes, ". Renommage automatique via make.unique()."))
          }

          docnames(corpus) <- ids_uniques
          ids_corpus <- as.character(docnames(corpus))

          textes_orig <- as.character(corpus)

          avancer(0.18, "Préparation texte (nettoyage / minuscules)")
          rv$statut <- "Préparation texte..."

          if (mode_iramuteq) {
            textes_nettoyes <- appliquer_nettoyage_iramuteq(
              textes = textes_orig,
              activer_nettoyage = isTRUE(input$nettoyage_caracteres),
              forcer_minuscules = isTRUE(input$forcer_minuscules_avant),
              supprimer_chiffres = isTRUE(input$supprimer_chiffres),
              supprimer_apostrophes = isTRUE(input$supprimer_apostrophes)
            )

            textes_chd <- executer_textprepa_iramuteq(
              ids = ids_corpus,
              textes = textes_nettoyes,
              input = input,
              rv = rv
            )
          } else {
            textes_chd <- appliquer_nettoyage_rainette(
              textes = textes_orig,
              activer_nettoyage = isTRUE(input$nettoyage_caracteres),
              forcer_minuscules = isTRUE(input$forcer_minuscules_avant),
              supprimer_chiffres = isTRUE(input$supprimer_chiffres),
              supprimer_apostrophes = isTRUE(input$supprimer_apostrophes)
            )
            names(textes_chd) <- ids_corpus
          }

          if (!exists("verifier_coherence_dictionnaire_langue", mode = "function", inherits = TRUE)) {
            charge_langue <- charger_module_langue()
            if (!isTRUE(charge_langue$ok)) {
              stop(paste0("Module langue indisponible (", charge_langue$raison, ") : ", charge_langue$chemin))
            }
            ajouter_log(rv, paste0("Diagnostic langue: module chargé depuis ", charge_langue$chemin, "."))
          }

          source_dictionnaire <- "lexique_fr"

          verifier_coherence_dictionnaire_langue(
            textes_chd,
            if (identical(source_dictionnaire, "lexique_fr")) "fr" else as.character(input$spacy_langue),
            rv = rv
          )

          avancer(0.22, "Prétraitement + DFM")
          rv$statut <- "Prétraitement et DFM..."

          ajouter_log(
            rv,
            paste0(
              "Diagnostic pipeline: dictionnaire=", source_dictionnaire,
              " | langue UI=", as.character(input$spacy_langue),
              " | filtrage_morpho=", ifelse(isTRUE(input$filtrage_morpho), "1", "0"),
              " | retirer_stopwords=", ifelse(isTRUE(input$retirer_stopwords), "1", "0"),
              " | supprimer_ponctuation=", ifelse(isTRUE(input$supprimer_ponctuation), "1", "0"),
              " | supprimer_chiffres=", ifelse(isTRUE(input$supprimer_chiffres), "1", "0"),
              " | supprimer_apostrophes=", ifelse(isTRUE(input$supprimer_apostrophes), "1", "0"),
              " | nettoyage_caracteres=", ifelse(isTRUE(input$nettoyage_caracteres), "1", "0")
            )
          )

          sortie_pipeline <- executer_pipeline_iramuteq(
            input = input,
            rv = rv,
            textes_chd = textes_chd
          )

          tok <- sortie_pipeline$tok
          dfm_obj <- sortie_pipeline$dfm_obj
          langue_reference <- sortie_pipeline$langue_reference
          source_dictionnaire <- sortie_pipeline$source_dictionnaire

          if (anyDuplicated(docnames(dfm_obj)) > 0) {
            dups_dfm <- sum(duplicated(as.character(docnames(dfm_obj))))
            docnames(dfm_obj) <- make.unique(as.character(docnames(dfm_obj)), sep = "_dup")
            ajouter_log(rv, paste0("DFM : docnames dupliqués détectés (", dups_dfm, "). Renommage automatique."))
          }

          included_segments <- as.character(docnames(dfm_obj))
          included_segments <- included_segments[!is.na(included_segments) & nzchar(included_segments)]
          included_segments <- unique(included_segments)

          filtered_corpus <- corpus[included_segments]
          if (anyDuplicated(docnames(filtered_corpus)) > 0) {
            dups_corpus <- sum(duplicated(as.character(docnames(filtered_corpus))))
            docnames(filtered_corpus) <- make.unique(as.character(docnames(filtered_corpus)), sep = "_dup")
            ajouter_log(rv, paste0("Corpus filtré : docnames dupliqués détectés (", dups_corpus, "). Renommage automatique."))
          }

          tok <- tok[included_segments]
          if (anyDuplicated(docnames(tok)) > 0) {
            dups_tok <- sum(duplicated(as.character(docnames(tok))))
            docnames(tok) <- make.unique(as.character(docnames(tok)), sep = "_dup")
            ajouter_log(rv, paste0("Tokens : docnames dupliqués détectés (", dups_tok, "). Renommage automatique."))
          }

          dfm_obj <- assurer_docvars_dfm_minimal(dfm_obj, filtered_corpus)

          tmp <- supprimer_docs_vides_dfm(dfm_obj, filtered_corpus, tok, rv)
          dfm_obj <- tmp$dfm
          filtered_corpus <- tmp$corpus
          tok <- tmp$tok

          ajouter_log(rv, paste0("Après suppression segments vides : ", ndoc(dfm_obj), " docs ; ", nfeat(dfm_obj), " termes."))
          verifier_dfm_avant_rainette(dfm_obj, input)

          rv$textes_indexation <- vapply(as.list(tok), function(x) paste(x, collapse = " "), FUN.VALUE = character(1))
          names(rv$textes_indexation) <- docnames(dfm_obj)

          avancer(0.52, "Classification (rainette / rainette2)")
          rv$statut <- "Classification en cours..."

          modele_chd <- "iramuteq"

          type_classif <- as.character(input$type_classification)
          if (!type_classif %in% c("simple", "double")) type_classif <- "simple"

          groupes <- NULL
          res_final <- NULL

          if (identical(modele_chd, "iramuteq")) {

            rv$res_type <- "iramuteq"
            ajouter_log(rv, "Mode : classification IRaMuTeQ-like.")

            k_iramuteq <- suppressWarnings(as.integer(input$k_iramuteq))
            if (is.na(k_iramuteq) || k_iramuteq < 2L) k_iramuteq <- 10L

            mincl_mode_iramuteq <- as.character(input$iramuteq_mincl_mode)
            if (!mincl_mode_iramuteq %in% c("auto", "manuel")) mincl_mode_iramuteq <- "auto"

            mincl_iramuteq <- suppressWarnings(as.integer(input$iramuteq_mincl))
            if (is.na(mincl_iramuteq) || mincl_iramuteq < 1L) mincl_iramuteq <- 1L

            classif_mode_iramuteq <- as.character(input$iramuteq_classif_mode)
            if (!classif_mode_iramuteq %in% c("simple", "double")) classif_mode_iramuteq <- "simple"

            svd_method_iramuteq <- as.character(input$iramuteq_svd_method)
            if (!svd_method_iramuteq %in% c("irlba", "svdR")) svd_method_iramuteq <- "irlba"

            ajouter_log(
              rv,
              paste0(
                "Paramètres IRaMuTeQ-like : k=", k_iramuteq,
                " | mincl_mode=", mincl_mode_iramuteq,
                if (identical(mincl_mode_iramuteq, "manuel")) paste0(" | mincl=", mincl_iramuteq) else "",
                " | classif_mode=", classif_mode_iramuteq,
                " | svd_method=", svd_method_iramuteq,
                " | mode_patate=", ifelse(isTRUE(input$iramuteq_mode_patate), "1", "0")
              )
            )

            res_ira <- lancer_moteur_chd_iramuteq(
              dfm_obj = dfm_obj,
              k = k_iramuteq,
              mincl_mode = mincl_mode_iramuteq,
              mincl = mincl_iramuteq,
              classif_mode = classif_mode_iramuteq,
              svd_method = svd_method_iramuteq,
              mode_patate = isTRUE(input$iramuteq_mode_patate),
              binariser = TRUE
            )

            groupes <- as.integer(res_ira$classes)
            if (all(is.na(groupes)) || length(unique(groupes[groupes > 0])) < 2) {
              stop("IRaMuTeQ-like n'a pas pu produire au moins 2 classes exploitables.")
            }

            res_final <- res_ira
            rv$res_chd <- NULL
            rv$dfm_chd <- NULL
            rv$max_n_groups <- length(unique(groupes[groupes > 0]))
            rv$max_n_groups_chd <- rv$max_n_groups

          } else if (type_classif == "simple") {

            rv$res_type <- "simple"
            ajouter_log(rv, "Mode : classification simple (rainette).")

            k_effectif <- calculer_k_effectif(dfm_obj, input$k, input$min_split_members, rv)

            res <- rainette(
              dfm_obj,
              k = k_effectif,
              min_segment_size = input$min_segment_size,
              min_split_members = input$min_split_members,
              doc_id = "segment_source"
            )

            if (is.null(res) || is.null(res$group) || length(res$group) == 0) stop("Rainette n'a pas pu calculer de clusters. Diminue les filtrages, augmente segment_size, ou réduis k.")

            groupes <- res$group
            res_final <- res
            rv$res_chd <- res
            rv$dfm_chd <- dfm_obj
            rv$max_n_groups <- max(res$group, na.rm = TRUE)
            rv$max_n_groups_chd <- rv$max_n_groups

          } else {

            rv$res_type <- "double"
            ajouter_log(rv, "Mode : classification double (rainette2).")

            k_effectif <- calculer_k_effectif(dfm_obj, input$k, input$min_split_members, rv)

            res1 <- rainette(dfm_obj, k = k_effectif, min_segment_size = input$min_segment_size, min_split_members = input$min_split_members, doc_id = "segment_source")
            if (is.null(res1) || is.null(res1$group) || length(res1$group) == 0) stop("Classification 1 (rainette) impossible.")

            res2 <- rainette(dfm_obj, k = k_effectif, min_segment_size = input$min_segment_size2, min_split_members = input$min_split_members, doc_id = "segment_source")
            if (is.null(res2) || is.null(res2$group) || length(res2$group) == 0) stop("Classification 2 (rainette) impossible.")

            res_d <- rainette2(res1, res2, max_k = input$max_k_double)
            groupes <- cutree(res_d, k = k_effectif)

            res_final <- res_d
            rv$res_chd <- res1
            rv$dfm_chd <- dfm_obj
            rv$max_n_groups <- input$max_k_double
            rv$max_n_groups_chd <- max(res1$group, na.rm = TRUE)
          }

          docvars(filtered_corpus)$Classes <- groupes

          classes_calculees <- suppressWarnings(as.integer(docvars(filtered_corpus)$Classes))
          idx_ok <- !is.na(classes_calculees) & classes_calculees > 0

          nb_non_assignes <- sum(!idx_ok)
          if (nb_non_assignes > 0) {
            ajouter_log(
              rv,
              paste0(
                "Segments non assignés à une classe terminale (Classe 0 / NA) : ",
                nb_non_assignes,
                ". Exclusion des calculs CHD/AFC."
              )
            )
          }
          filtered_corpus_ok <- filtered_corpus[idx_ok]
          dfm_ok <- dfm_obj[idx_ok, ]
          tok_ok <- tok[idx_ok]

          if (ndoc(dfm_ok) < 2) stop("Après classification, il reste moins de 2 segments classés (hors NA).")
          if (nfeat(dfm_ok) < 2) stop("Après classification, le DFM classé est trop pauvre (moins de 2 termes).")

          rv$clusters <- sort(unique(docvars(filtered_corpus_ok)$Classes))
          rv$res <- res_final
          rv$dfm <- dfm_ok
          rv$filtered_corpus <- filtered_corpus_ok
          rv$res_stats_df <- NULL

          avancer(0.58, "NER (si activé)")
          rv$statut <- "NER (si activé)..."

          if (isTRUE(input$activer_ner)) {
            ajouter_log(rv, "NER ignoré : cette branche IRaMuTeQ-like est strictement sans spaCy.")
          }

          avancer(0.62, "Exports + stats")
          rv$statut <- "Exports et statistiques..."

          segments_vec <- as.character(filtered_corpus_ok)
          names(segments_vec) <- docnames(filtered_corpus_ok)
          segments_by_class <- split(segments_vec, docvars(filtered_corpus_ok)$Classes)

          segments_file <- file.path(rv$export_dir, "segments_par_classe.txt")
          writeLines(unlist(lapply(names(segments_by_class), function(cl) c(paste0("Classe ", cl, ":"), unname(segments_by_class[[cl]]), ""))), segments_file)

          if (identical(rv$res_type, "iramuteq")) {
            ajouter_log(rv, "Statistiques CHD : calcul IRaMuTeQ-like (contingence classe × terme).")
            res_stats_df <- construire_stats_classes_iramuteq(
              dfm_obj = dfm_ok,
              classes = docvars(filtered_corpus_ok)$Classes,
              max_p = 1
            ) %>%
              mutate(Classe = normaliser_id_classe_local(Classe)) %>%
              arrange(Classe, desc(chi2))
          } else {
            res_stats_list <- rainette_stats(
              dtm = dfm_ok,
              groups = docvars(filtered_corpus_ok)$Classes,
              measure = c("chi2", "lr", "frequency", "docprop"),
              n_terms = 9999,
              # Harmonisation avec le graphe CHD :
              # - pas de chi2 négatifs dans l'onglet Statistiques
              # - pas de coupe préalable sur p-value pour conserver le même
              #   vivier de termes entre les vues (la colonne p_value_filter
              #   reste disponible pour distinguer les termes significatifs).
              show_negative = FALSE,
              max_p = 1
            )

            labels_stats <- names(res_stats_list)
            labels_groupes <- as.character(sort(unique(docvars(filtered_corpus_ok)$Classes)))

            if (is.null(labels_stats) || length(labels_stats) != length(res_stats_list) || any(!nzchar(labels_stats))) {
              labels_stats <- labels_groupes
            }

            if (length(labels_stats) != length(res_stats_list)) {
              labels_stats <- as.character(seq_along(res_stats_list))
            }

            tailles_stats <- vapply(res_stats_list, nrow, integer(1))

            res_stats_df <- bind_rows(res_stats_list) %>%
              mutate(ClusterID = rep(labels_stats, times = tailles_stats)) %>%
              rename(Terme = feature, Classe = ClusterID) %>%
              mutate(
                p_value = p,
                Classe_brut = as.character(Classe),
                Classe = normaliser_id_classe_local(Classe),
                p_value_filter = ifelse(p <= input$max_p, paste0("≤ ", input$max_p), paste0("> ", input$max_p))
              ) %>%
              arrange(Classe, desc(chi2))
          }

          if (identical(source_dictionnaire, "lexique_fr") &&
              !is.null(rv$lexique_fr_df) &&
              is.data.frame(rv$lexique_fr_df) &&
              nrow(rv$lexique_fr_df) > 0 &&
              "Terme" %in% names(res_stats_df) &&
              exists("construire_type_lexique_fr", mode = "function", inherits = TRUE)) {
            res_stats_df$Type <- construire_type_lexique_fr(res_stats_df$Terme, rv$lexique_fr_df)
          }

          stats_file <- file.path(rv$export_dir, "stats_par_classe.csv")
          ecrire_csv_6_decimales(res_stats_df, stats_file, row.names = FALSE)

          rv$segments_file <- segments_file
          rv$stats_file <- stats_file
          rv$res_stats_df <- res_stats_df

          avancer(0.72, "AFC (classes × termes)")
          rv$statut <- "Calcul AFC classes × termes..."

          rv$afc_obj <- NULL
          rv$afc_erreur <- NULL
          rv$afc_vars_obj <- NULL
          rv$afc_vars_erreur <- NULL
          rv$afc_dir <- file.path(rv$export_dir, "afc")
          dir.create(rv$afc_dir, showWarnings = FALSE, recursive = TRUE)

          filtrer_affichage_pvalue <- isTRUE(input$filtrer_affichage_pvalue)

          termes_signif <- NULL
          if (isTRUE(filtrer_affichage_pvalue)) {
            termes_signif <- unique(subset(res_stats_df, p <= input$max_p)$Terme)
            termes_signif <- termes_signif[!is.na(termes_signif) & nzchar(termes_signif)]
            if (length(termes_signif) < 2) termes_signif <- NULL
          }

          tryCatch({
            groupes_docs <- docvars(filtered_corpus_ok)$Classes

            obj <- executer_afc_classes(
              dfm_obj = dfm_ok,
              groupes = groupes_docs,
              termes_cibles = termes_signif,
              max_termes = 400,
              seuil_p = if (isTRUE(filtrer_affichage_pvalue)) input$max_p else 1,
              rv = rv
            )

            if (!is.null(obj$termes_stats) && !is.null(rv$res_stats_df)) {
              df_m <- obj$termes_stats
              df_m$Classe_num <- suppressWarnings(as.numeric(gsub("^Classe\\s+", "", as.character(df_m$Classe_max))))
              rs <- rv$res_stats_df

              rs2 <- rs[, intersect(c("Terme", "Classe", "chi2", "p", "frequency", "docprop", "lr"), names(rs)), drop = FALSE]
              rs2$Classe <- as.numeric(rs2$Classe)

              m <- merge(
                df_m,
                rs2,
                by.x = c("Terme", "Classe_num"),
                by.y = c("Terme", "Classe"),
                all.x = TRUE,
                suffixes = c("_global", "_rainette")
              )

              if ("chi2" %in% names(m)) {
                df_m$chi2 <- ifelse(is.na(m$chi2), df_m$chi2, m$chi2)
              }
              if ("p" %in% names(m)) {
                df_m$p_value <- ifelse(is.na(m$p), df_m$p_value, m$p)
              }

              df_m$Classe_num <- NULL
              obj$termes_stats <- df_m
            }

            obj$termes_stats <- construire_segments_exemples_afc(
              termes_stats = obj$termes_stats,
              dfm_obj = dfm_ok,
              corpus_obj = filtered_corpus_ok
            )

            rv$afc_obj <- obj
            ajouter_log(rv, "AFC classes × termes : calcul terminé.")

          }, error = function(e) {
            rv$afc_erreur <- paste0("AFC classes × termes : ", e$message)
            ajouter_log(rv, rv$afc_erreur)
            showNotification(rv$afc_erreur, type = "error", duration = 8)
          })

          avancer(0.74, "AFC (variables étoilées)")
          rv$statut <- "Calcul AFC variables étoilées..."

          tryCatch({
            if (!is.null(docvars(filtered_corpus_ok)$Classes)) {
              objv <- executer_afc_variables_etoilees(
                corpus_aligne = filtered_corpus_ok,
                groupes = docvars(filtered_corpus_ok)$Classes,
                max_modalites = 400,
                seuil_p = if (isTRUE(filtrer_affichage_pvalue)) input$max_p else 1,
                rv = rv
              )
              rv$afc_vars_obj <- objv
              ajouter_log(rv, "AFC variables étoilées : calcul terminé.")
            }
          }, error = function(e) {
            rv$afc_vars_erreur <- paste0("AFC variables étoilées : ", e$message)
            ajouter_log(rv, rv$afc_vars_erreur)
          })

          if (!is.null(rv$afc_obj) && !is.null(rv$afc_obj$ca)) {

            afc_classes_png <- file.path(rv$afc_dir, "afc_classes.png")
            afc_termes_png <- file.path(rv$afc_dir, "afc_termes.png")

            activer_repel <- TRUE
            if (!is.null(input$afc_reduire_chevauchement)) activer_repel <- isTRUE(input$afc_reduire_chevauchement)

            taille_sel <- "frequency"
            if (!is.null(input$afc_taille_mots) && nzchar(as.character(input$afc_taille_mots))) {
              taille_sel <- as.character(input$afc_taille_mots)
            }
            if (!taille_sel %in% c("frequency", "chi2")) taille_sel <- "frequency"

            top_termes <- 120
            if (!is.null(input$afc_top_termes) && is.finite(input$afc_top_termes)) top_termes <- as.integer(input$afc_top_termes)

            png(afc_classes_png, width = 1800, height = 1400, res = 180)
            try(tracer_afc_classes_seules(rv$afc_obj, axes = c(1, 2), cex_labels = 1.05), silent = TRUE)
            dev.off()

            png(afc_termes_png, width = 2000, height = 1600, res = 180)
            try(tracer_afc_classes_termes(rv$afc_obj, axes = c(1, 2), top_termes = top_termes, taille_sel = taille_sel, activer_repel = activer_repel), silent = TRUE)
            dev.off()

            rv$afc_plot_classes <- afc_classes_png
            rv$afc_plot_termes <- afc_termes_png

            ecrire_csv_6_decimales(rv$afc_obj$table, file.path(rv$afc_dir, "table_classes_termes.csv"), row.names = TRUE)
            ecrire_csv_6_decimales(rv$afc_obj$rowcoord, file.path(rv$afc_dir, "coords_classes.csv"), row.names = TRUE)
            ecrire_csv_6_decimales(rv$afc_obj$colcoord, file.path(rv$afc_dir, "coords_termes.csv"), row.names = TRUE)
            ecrire_csv_6_decimales(rv$afc_obj$termes_stats, file.path(rv$afc_dir, "stats_termes.csv"), row.names = FALSE)

            if (!is.null(rv$afc_obj$ca$eig)) {
              ecrire_csv_6_decimales(as.data.frame(rv$afc_obj$ca$eig), file.path(rv$afc_dir, "valeurs_propres.csv"), row.names = TRUE)
            }

            rv$afc_table_mots <- rv$afc_obj$termes_stats
          }

          if (!is.null(rv$afc_vars_obj) && !is.null(rv$afc_vars_obj$ca)) {

            afc_vars_png <- file.path(rv$afc_dir, "afc_variables_etoilees.png")

            activer_repel2 <- TRUE
            if (!is.null(input$afc_reduire_chevauchement)) activer_repel2 <- isTRUE(input$afc_reduire_chevauchement)

            top_mod <- 120
            if (!is.null(input$afc_top_modalites) && is.finite(input$afc_top_modalites)) top_mod <- as.integer(input$afc_top_modalites)

            png(afc_vars_png, width = 2000, height = 1600, res = 180)
            try(tracer_afc_variables_etoilees(rv$afc_vars_obj, axes = c(1, 2), top_modalites = top_mod, activer_repel = activer_repel2), silent = TRUE)
            dev.off()

            rv$afc_plot_vars <- afc_vars_png

            ecrire_csv_6_decimales(rv$afc_vars_obj$table, file.path(rv$afc_dir, "table_classes_variables.csv"), row.names = TRUE)
            ecrire_csv_6_decimales(rv$afc_vars_obj$rowcoord, file.path(rv$afc_dir, "coords_classes_vars.csv"), row.names = TRUE)
            ecrire_csv_6_decimales(rv$afc_vars_obj$colcoord, file.path(rv$afc_dir, "coords_modalites.csv"), row.names = TRUE)
            ecrire_csv_6_decimales(rv$afc_vars_obj$modalites_stats, file.path(rv$afc_dir, "stats_modalites.csv"), row.names = FALSE)

            if (!is.null(rv$afc_vars_obj$ca$eig)) {
              ecrire_csv_6_decimales(as.data.frame(rv$afc_vars_obj$ca$eig), file.path(rv$afc_dir, "valeurs_propres_vars.csv"), row.names = TRUE)
            }

            rv$afc_table_vars <- rv$afc_vars_obj$modalites_stats
          }

          avancer(0.76, "Concordancier HTML")
          rv$statut <- "Concordancier..."

          html_file <- file.path(rv$export_dir, "segments_par_classe.html")
          textes_index_ok <- rv$textes_indexation[docnames(dfm_ok)]
          names(textes_index_ok) <- docnames(dfm_ok)

          wordcloud_dir <- file.path(rv$export_dir, "wordclouds")
          dir.create(wordcloud_dir, showWarnings = FALSE, recursive = TRUE)
          cooc_dir <- file.path(rv$export_dir, "cooccurrences")
          dir.create(cooc_dir, showWarnings = FALSE, recursive = TRUE)

          classes_uniques <- sort(unique(as.integer(docvars(filtered_corpus_ok)$Classes)))
          classes_uniques <- classes_uniques[is.finite(classes_uniques)]

          if (length(classes_uniques) > 0) {
            classes_choices <- as.character(classes_uniques)
            updateSelectInput(
              session,
              "classe_viz_iramuteq",
              choices = classes_choices,
              selected = classes_choices[[1]]
            )
          }

          if (!identical(rv$res_type, "iramuteq")) {
            for (cl in classes_uniques) {
            top_n_demande <- suppressWarnings(as.integer(input$top_n))
            if (!is.finite(top_n_demande) || is.na(top_n_demande)) top_n_demande <- 20L
            top_n_demande <- max(5L, top_n_demande)

            if (isTRUE(input$filtrer_affichage_pvalue)) {
              df_stats_cl <- subset(res_stats_df, Classe == cl & p <= input$max_p)
            } else {
              df_stats_cl <- subset(res_stats_df, Classe == cl)
            }
            if (nrow(df_stats_cl) > 0) {
              df_stats_cl <- df_stats_cl[order(-df_stats_cl$chi2), , drop = FALSE]
              df_stats_cl <- head(df_stats_cl, top_n_demande)

              wc_png <- file.path(wordcloud_dir, paste0("cluster_", cl, "_wordcloud.png"))
              try({
                png(wc_png, width = 800, height = 600)
                suppressWarnings(wordcloud(
                  words = df_stats_cl$Terme,
                  freq = df_stats_cl$chi2,
                  scale = c(10, 0.5),
                  min.freq = 0,
                  max.words = nrow(df_stats_cl),
                  colors = brewer.pal(8, "Dark2")
                ))
                dev.off()
              }, silent = TRUE)
            }

            }

            generer_cooccurrences_par_classe(
              tok_ok = tok_ok,
              filtered_corpus_ok = filtered_corpus_ok,
              classes_uniques = classes_uniques,
              cooc_dir = cooc_dir,
              top_n = input$top_n,
              top_feat = input$top_feat,
              window_cooc = input$window_cooc
            )
          } else {
            generer_wordclouds_iramuteq(
              res_stats_df = res_stats_df,
              classes_uniques = classes_uniques,
              wordcloud_dir = wordcloud_dir,
              top_n = input$top_n,
              filtrer_pvalue = isTRUE(input$filtrer_affichage_pvalue),
              max_p = input$max_p
            )
            ajouter_log(rv, "Mode IRaMuTeQ-like : nuages de mots générés via wordcloud_iramuteq.R (cooccurrences Explore rainette désactivées).")
          }

          explor_assets <- NULL
          ok_chd_png <- FALSE
          if (!identical(rv$res_type, "iramuteq")) {
            ok_chd_png <- generer_chd_explor_si_absente(rv)
          }

          chd_png_rel <- NULL
          if (isTRUE(ok_chd_png) && file.exists(file.path(rv$export_dir, "explor", "chd.png"))) {
            chd_png_rel <- file.path("explor", "chd.png")
          }
          chd_html_rel <- generer_chd_html_explor(rv, chd_png_rel)

          wc_files <- list.files(wordcloud_dir, pattern = "\\.png$", full.names = FALSE)
          if (length(wc_files) > 0) {
            wc_classes <- gsub("^cluster_([0-9]+)_wordcloud\\.png$", "\\1", wc_files)
            wordclouds_df <- data.frame(
              classe = wc_classes,
              src = file.path("wordclouds", wc_files),
              stringsAsFactors = FALSE
            )
            wordclouds_df <- wordclouds_df[order(suppressWarnings(as.integer(wordclouds_df$classe))), , drop = FALSE]
          } else {
            wordclouds_df <- data.frame(classe = character(0), src = character(0), stringsAsFactors = FALSE)
          }

          coocs_df <- construire_table_cooccurrences(cooc_dir)

          explor_assets <- list(
            chd = chd_png_rel,
            chd_html = chd_html_rel,
            wordclouds = wordclouds_df,
            coocs = coocs_df
          )
          rv$explor_assets <- explor_assets

          args_concordancier <- list(
            chemin_sortie = html_file,
            segments_by_class = segments_by_class,
            res_stats_df = res_stats_df,
            max_p = if (isTRUE(input$filtrer_affichage_pvalue)) input$max_p else 1,
            filtrer_pvalue = isTRUE(input$filtrer_affichage_pvalue),
            textes_indexation = textes_index_ok,
            spacy_tokens_df = rv$spacy_tokens_df,
            lexique_fr_df = rv$lexique_fr_df,
            source_dictionnaire = source_dictionnaire,
            avancer = avancer,
            rv = rv
          )

          # Priorité explicite au concordancier IRaMuTeQ-like lorsque le mode
          # IRaMuTeQ est sélectionné dans l'UI (même si rv$res_type est désynchronisé).
          mode_iramuteq_actif <- identical(as.character(input$modele_chd), "iramuteq") ||
            identical(rv$res_type, "iramuteq")

          fonction_concordancier <- if (isTRUE(mode_iramuteq_actif)) {
            generer_concordancier_iramuteq_html
          } else if (identical(source_dictionnaire, "lexique_fr")) {
            generer_concordancier_lexique_html
          } else {
            generer_concordancier_spacy_html
          }

          html_genere <- do.call(fonction_concordancier, args_concordancier)

          candidats_html <- unique(c(
            html_genere,
            html_file,
            file.path(rv$export_dir, "concordancier.html")
          ))
          candidats_html <- candidats_html[is.character(candidats_html) & !is.na(candidats_html) & nzchar(candidats_html)]
          html_existants <- candidats_html[file.exists(candidats_html)]

          if (length(html_existants) == 0) {
            html_fallback <- file.path(rv$export_dir, "concordancier.html")
            args_concordancier$chemin_sortie <- html_fallback
            ajouter_log(rv, "Concordancier HTML introuvable après la première génération. Nouvelle tentative vers exports/concordancier.html.")
            html_retry <- tryCatch(
              do.call(fonction_concordancier, args_concordancier),
              error = function(e) {
                ajouter_log(rv, paste0("Concordancier HTML : échec de la relance - ", e$message))
                NA_character_
              }
            )

            candidats_retry <- unique(c(html_retry, html_fallback, html_genere, html_file))
            candidats_retry <- candidats_retry[is.character(candidats_retry) & !is.na(candidats_retry) & nzchar(candidats_retry)]
            html_existants <- candidats_retry[file.exists(candidats_retry)]
          }

          if (length(html_existants) > 0) {
            rv$html_file <- html_existants[[1]]
            ajouter_log(rv, paste0("Concordancier HTML validé : ", rv$html_file))
          } else {
            rv$html_file <- html_file
            ajouter_log(rv, "Concordancier HTML introuvable après relance. Vérifier les logs de génération du concordancier.")
          }

          avancer(0.96, "ZIP")
          rv$statut <- "Création ZIP..."
          rv$zip_file <- file.path(rv$base_dir, "exports_rainette.zip")
          if (file.exists(rv$zip_file)) unlink(rv$zip_file)

          ancien_wd <- getwd()
          setwd(rv$base_dir)
          utils::zip(zipfile = rv$zip_file, files = "exports")
          setwd(ancien_wd)

          exports_prefix <- as.character(rv$exports_prefix)
          if (length(exports_prefix) > 1) exports_prefix <- exports_prefix[[1]]
          if (!length(exports_prefix) || is.na(exports_prefix) || !nzchar(exports_prefix)) {
            # Fallback robuste: évite une erreur "missing value where TRUE/FALSE needed"
            # lorsque le token de session est indisponible.
            exports_prefix <- paste0("exports_", format(Sys.time(), "%Y%m%d%H%M%S"))
            rv$exports_prefix <- exports_prefix
          }

          if (!(exports_prefix %in% names(shiny::resourcePaths()))) {
            shiny::addResourcePath(exports_prefix, rv$export_dir)
          }

          rv$statut <- "Analyse terminée."
          rv$progression <- 100
          ajouter_log(rv, "Analyse terminée.")
          showNotification("Analyse terminée.", type = "message", duration = 5)

        }, error = function(e) {
          rv$statut <- paste0("Erreur : ", e$message)
          ajouter_log(rv, paste0("ERREUR : ", e$message))
          showNotification(e$message, type = "error", duration = 8)
        })
    })
}
