# Rôle du fichier: pipeline_lexique_analysis.R porte une partie du pipeline d'analyse Rainette.
# Ce script centralise une responsabilité métier/technique utilisée par l'application.
# Il facilite la maintenance en explicitant le périmètre et les points d'intégration.
executer_pipeline_lexique <- function(input, rv, textes_chd) {
  filtrage_morpho <- isTRUE(input$filtrage_morpho)
  utiliser_lemmes_lexique <- isTRUE(input$lexique_utiliser_lemmes)
  langue_reference <- "fr"
  lexique_source_stopwords <- "quanteda"

  if (isTRUE(utiliser_lemmes_lexique) || isTRUE(filtrage_morpho)) {
    lexique_fr <- charger_lexique_fr("dictionnaires/lexique_fr.csv")
    ajouter_log(rv, paste0("Lexique (fr) chargé : ", nrow(lexique_fr), " entrées."))
    rv$lexique_fr_df <- lexique_fr
  } else {
    lexique_fr <- NULL
    rv$lexique_fr_df <- NULL
    ajouter_log(rv, "Lexique (fr) sans lemmatisation/filtrage : conservation des formes d'origine.")
  }

  rv$spacy_tokens_df <- NULL

  if (isTRUE(utiliser_lemmes_lexique) && !isTRUE(filtrage_morpho)) {
    ajouter_log(rv, "Lexique (fr) sans filtrage morphosyntaxique : lemmatisation directe forme->lemme.")

    textes_lexique <- lemmatiser_textes_lexique(
      textes = textes_chd,
      lexique = lexique_fr,
      rv = rv
    )

  } else if (isTRUE(filtrage_morpho)) {
    cgram_lexique_a_conserver <- toupper(trimws(as.character(input$pos_lexique_a_conserver)))
    cgram_lexique_a_conserver <- unique(cgram_lexique_a_conserver[nzchar(cgram_lexique_a_conserver)])
    if (is.null(cgram_lexique_a_conserver) || length(cgram_lexique_a_conserver) == 0) {
      cgram_lexique_a_conserver <- c("NOM", "ADJ", "VER")
    }

    ajouter_log(
      rv,
      paste0(
        "lexique_fr | filtrage morpho=1 (c_morpho: ",
        paste(cgram_lexique_a_conserver, collapse = ", "),
        ") | lemmes=", ifelse(utiliser_lemmes_lexique, "1", "0"),
        " | stopwords: quanteda (Lexique fr)"
      )
    )

    textes_lexique <- filtrer_textes_lexique_par_cgram(
      textes = textes_chd,
      lexique = lexique_fr,
      cgram_a_conserver = cgram_lexique_a_conserver,
      rv = rv
    )

    if (isTRUE(utiliser_lemmes_lexique)) {
      textes_lexique <- lemmatiser_textes_lexique(
        textes = textes_lexique,
        lexique = lexique_fr,
        rv = rv
      )
    }
  } else {
    ajouter_log(rv, "Lexique (fr) sans filtrage morphosyntaxique ni lemmatisation : pipeline standard.")
    textes_lexique <- textes_chd
  }

  tok_base <- tokens(
    textes_lexique,
    remove_punct = isTRUE(input$supprimer_ponctuation),
    remove_numbers = isTRUE(input$supprimer_chiffres)
  )

  res_dfm <- construire_dfm_avec_fallback_stopwords(
    tok_base = tok_base,
    min_docfreq = input$min_docfreq,
    retirer_stopwords = isTRUE(input$retirer_stopwords),
    langue_spacy = langue_reference,
    rv = rv,
    libelle = ifelse(utiliser_lemmes_lexique, "Lexique (fr)", "lexique_fr"),
    source_dictionnaire = "lexique_fr",
    lexique_source_stopwords = lexique_source_stopwords
  )

  list(
    tok = res_dfm$tok,
    dfm_obj = res_dfm$dfm,
    langue_reference = langue_reference,
    source_dictionnaire = "lexique_fr",
    lexique_source_stopwords = lexique_source_stopwords
  )
}
