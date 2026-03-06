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

.construire_hclust_pour_ale <- function(res_stats_df) {
  if (is.null(res_stats_df) || !is.data.frame(res_stats_df)) return(NULL)
  if (!all(c("Classe", "Terme") %in% names(res_stats_df))) return(NULL)

  df <- res_stats_df
  df$Classe <- suppressWarnings(as.integer(df$Classe))
  df$Terme <- as.character(df$Terme)
  df <- df[is.finite(df$Classe) & df$Classe > 0 & nzchar(df$Terme), , drop = FALSE]
  if (!nrow(df)) return(NULL)

  poids <- if ("frequency" %in% names(df)) suppressWarnings(as.numeric(df$frequency)) else rep(1, nrow(df))
  poids[!is.finite(poids) | is.na(poids) | poids < 0] <- 0
  df$poids <- poids

  mat <- stats::xtabs(poids ~ Classe + Terme, data = df)
  if (nrow(mat) < 2 || ncol(mat) < 2) return(NULL)

  vars <- apply(mat, 2, stats::sd)
  vars[!is.finite(vars)] <- 0
  mat <- mat[, vars > 0, drop = FALSE]
  if (nrow(mat) < 2 || ncol(mat) < 2) return(NULL)

  dist_obj <- stats::dist(mat, method = "euclidean")
  if (!inherits(dist_obj, "dist") || length(dist_obj) == 0) return(NULL)

  stats::hclust(dist_obj, method = "ward.D2")
}

.tracer_dendrogramme_avec_ale <- function(hc, orientation = c("vertical", "horizontal")) {
  orientation <- match.arg(orientation)
  ns_ale <- asNamespace("ale")

  # On privilégie les fonctions liées explicitement au dendrogramme si elles existent.
  candidats <- c(
    "plot_dendrogram", "draw_dendrogram", "dendrogram", "plot_hclust", "plot.ale"
  )
  dispo <- candidats[candidats %in% getNamespaceExports("ale")]

  dendro <- stats::as.dendrogram(hc)

  if (length(dispo)) {
    fun <- getExportedValue("ale", dispo[[1]])
    ok <- tryCatch({
      if (identical(orientation, "horizontal")) {
        fun(dendro, horiz = TRUE)
      } else {
        fun(dendro)
      }
      TRUE
    }, error = function(e) FALSE)
    if (isTRUE(ok)) return(TRUE)
  }

  # Repli: si ale expose une méthode plot pour ses objets, on tente via plot() sur dendrogramme.
  if (exists("plot", envir = ns_ale, inherits = TRUE)) {
    ok_plot <- tryCatch({
      if (identical(orientation, "horizontal")) {
        plot(dendro, horiz = TRUE, main = "Dendrogramme CHD (ale)")
      } else {
        plot(dendro, main = "Dendrogramme CHD (ale)")
      }
      TRUE
    }, error = function(e) FALSE)
    if (isTRUE(ok_plot)) return(TRUE)
  }

  FALSE
}

tracer_dendogramme_iramuteq_ui <- function(rv,
                                            top_n_terms = 10,
                                            orientation = "vertical") {
  orientation <- match.arg(orientation, c("vertical", "horizontal"))

  if (!requireNamespace("ale", quietly = TRUE)) {
    plot.new()
    text(0.5, 0.5, "Le package 'ale' est requis pour construire le dendrogramme.", cex = 1.0)
    return(invisible(NULL))
  }

  if (is.null(rv$res) && is.null(rv$res_chd)) {
    plot.new()
    text(0.5, 0.5, "Dendrogramme CHD indisponible.", cex = 1.1)
    return(invisible(NULL))
  }

  hc <- .construire_hclust_pour_ale(rv$res_stats_df)
  if (is.null(hc)) {
    plot.new()
    text(0.5, 0.5, "Données insuffisantes pour construire le dendrogramme avec ale.", cex = 1.0)
    return(invisible(NULL))
  }

  ok_ale <- .tracer_dendrogramme_avec_ale(hc, orientation = orientation)
  if (!isTRUE(ok_ale)) {
    plot.new()
    text(0.5, 0.5, "Impossible de tracer le dendrogramme via ale (API indisponible).", cex = 0.95)
  }

  if (!isTRUE(ale_disponible)) {
    mtext("Package 'ale' indisponible: tracé maintenu via le moteur interne.", side = 1, line = -1, cex = 0.7)
  }

  invisible(NULL)
}
