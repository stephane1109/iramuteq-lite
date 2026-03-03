# Rôle du fichier: concordancier_ner.R porte une partie du pipeline d'analyse Rainette.
# Ce script centralise une responsabilité métier/technique utilisée par l'application.

construire_colonne_segment_ner <- function(df_entites, textes_par_doc) {
  if (is.null(df_entites) || nrow(df_entites) == 0) return(df_entites)

  textes_index <- as.character(textes_par_doc)
  names(textes_index) <- names(textes_par_doc)

  doc_ids <- as.character(df_entites$doc_id)
  segments <- unname(textes_index[doc_ids])
  segments[is.na(segments)] <- ""

  df_entites$segment_texte <- segments

  if (all(c("start_char", "end_char") %in% names(df_entites))) {
    df_entites <- df_entites[, setdiff(names(df_entites), c("start_char", "end_char")), drop = FALSE]
  }

  df_entites
}
