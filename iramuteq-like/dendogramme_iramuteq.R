# Rôle du fichier: point d'entrée UI pour le tracé du dendrogramme IRaMuTeQ-like.

.assurer_ale_depuis_depot <- function() {
  if (requireNamespace("ale", quietly = TRUE)) return(TRUE)
  if (!requireNamespace("remotes", quietly = TRUE)) return(FALSE)

  ok <- tryCatch({
    remotes::install_github("cran/ale", dependencies = FALSE, upgrade = "never", quiet = TRUE)
    requireNamespace("ale", quietly = TRUE)
  }, error = function(e) FALSE)

  isTRUE(ok)
}

.extraire_classes_dendrogramme <- function(rv) {
  if (!is.null(rv$res$classes)) return(rv$res$classes)

  if (!is.null(rv$filtered_corpus) &&
      exists("docvars", mode = "function", inherits = TRUE)) {
    dv <- tryCatch(docvars(rv$filtered_corpus), error = function(e) NULL)
    if (!is.null(dv) && "Classes" %in% names(dv)) return(dv$Classes)
  }

  NULL
}

tracer_dendogramme_iramuteq_ui <- function(rv,
                                            top_n_terms = 10,
                                            orientation = "vertical") {
  orientation <- match.arg(orientation, c("vertical", "horizontal"))

  ale_disponible <- .assurer_ale_depuis_depot()

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
  classes <- .extraire_classes_dendrogramme(rv)

  if (is.null(chd_obj)) {
    plot.new()
    text(0.5, 0.5, "Dendrogramme CHD indisponible (objet CHD introuvable).", cex = 1.1)
    return(invisible(NULL))
  }

  if (!exists("tracer_dendrogramme_chd_iramuteq", mode = "function", inherits = TRUE)) {
    plot.new()
    text(0.5, 0.5, "Dendrogramme CHD indisponible (moteur de tracé introuvable).", cex = 1.1)
    return(invisible(NULL))
  }

  tracer_dendrogramme_chd_iramuteq(
    chd_obj = chd_obj,
    terminales = terminales,
    classes = classes,
    res_stats_df = rv$res_stats_df,
    top_n_terms = top_n_terms,
    orientation = orientation
  )

  if (!isTRUE(ale_disponible)) {
    mtext("Package 'ale' indisponible: tracé maintenu via le moteur interne.", side = 1, line = -1, cex = 0.7)
  }

  invisible(NULL)
}
