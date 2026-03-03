# Rôle du fichier: pipeline_iramuteq_analysis.R porte le pipeline dédié au mode IRaMuTeQ-like.
# Ce flux est volontairement séparé de Rainette/spaCy pour éviter tout mélange de logique.

executer_pipeline_iramuteq <- function(input, rv, textes_chd) {
  ajouter_log(rv, "IRaMuTeQ-like : pipeline lexical dédié (lexique_fr uniquement).")

  tok_base <- tokens(
    textes_chd,
    remove_punct = isTRUE(input$supprimer_ponctuation),
    remove_numbers = isTRUE(input$supprimer_chiffres)
  )

  res_dfm <- construire_dfm_avec_fallback_stopwords(
    tok_base = tok_base,
    min_docfreq = input$min_docfreq,
    retirer_stopwords = isTRUE(input$retirer_stopwords),
    langue_spacy = "fr",
    rv = rv,
    libelle = "IRaMuTeQ-like",
    source_dictionnaire = "lexique_fr",
    lexique_source_stopwords = "quanteda"
  )

  rv$spacy_tokens_df <- NULL
  rv$lexique_fr_df <- NULL

  list(
    tok = res_dfm$tok,
    dfm_obj = res_dfm$dfm,
    langue_reference = "fr",
    source_dictionnaire = "lexique_fr"
  )
}
