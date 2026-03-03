# Rôle du fichier: point d'entrée UI pour le tracé du dendrogramme IRaMuTeQ-like.

tracer_dendogramme_iramuteq_ui <- function(rv,
                                            top_n_terms = 4,
                                            orientation = "vertical",
                                            display_method = "compact") {
  if (is.null(rv$res) && is.null(rv$res_chd)) {
    plot.new()
    text(0.5, 0.5, "Dendrogramme CHD indisponible.", cex = 1.1)
    return(invisible(NULL))
  }

  chd_obj <- NULL
  if (!is.null(rv$res$chd)) {
    chd_obj <- rv$res$chd
  } else if (!is.null(rv$res_chd)) {
    chd_obj <- rv$res_chd
  } else if (exists("obtenir_objet_dendrogramme", mode = "function", inherits = TRUE)) {
    chd_obj <- obtenir_objet_dendrogramme(rv$res)
  }

  terminales <- if (!is.null(rv$res$terminales)) rv$res$terminales else NULL
  classes <- if (!is.null(rv$res$classes)) rv$res$classes else NULL
  if (is.null(classes) && !is.null(rv$filtered_corpus) && "Classes" %in% names(docvars(rv$filtered_corpus))) {
    classes <- docvars(rv$filtered_corpus)$Classes
  }

  if (is.null(chd_obj)) {
    plot.new()
    text(0.5, 0.5, "Dendrogramme CHD indisponible (objet CHD introuvable).", cex = 1.1)
    return(invisible(NULL))
  }

  tracer_dendrogramme_chd_iramuteq(
    chd_obj = chd_obj,
    terminales = terminales,
    classes = classes,
    res_stats_df = rv$res_stats_df,
    top_n_terms = top_n_terms,
    orientation = orientation,
    display_method = display_method
  )

  invisible(NULL)
}
