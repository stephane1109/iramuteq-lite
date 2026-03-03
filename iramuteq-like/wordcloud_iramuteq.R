# Rôle du fichier: générer des nuages de mots dédiés au mode IRaMuTeQ-like.

generer_wordclouds_iramuteq <- function(res_stats_df,
                                        classes_uniques,
                                        wordcloud_dir,
                                        top_n = 20L,
                                        filtrer_pvalue = FALSE,
                                        max_p = 1) {
  if (is.null(res_stats_df) || !is.data.frame(res_stats_df) || nrow(res_stats_df) == 0) {
    return(invisible(NULL))
  }

  dir.create(wordcloud_dir, showWarnings = FALSE, recursive = TRUE)

  top_n <- suppressWarnings(as.integer(top_n))
  if (!is.finite(top_n) || is.na(top_n)) top_n <- 20L
  top_n <- max(5L, top_n)

  classe_col <- if ("Classe" %in% names(res_stats_df)) "Classe" else NULL
  terme_col <- if ("Terme" %in% names(res_stats_df)) "Terme" else if ("forme" %in% names(res_stats_df)) "forme" else NULL
  chi2_col <- if ("chi2" %in% names(res_stats_df)) "chi2" else NULL
  p_col <- if ("p" %in% names(res_stats_df)) "p" else if ("p_value" %in% names(res_stats_df)) "p_value" else NULL

  if (is.null(classe_col) || is.null(terme_col) || is.null(chi2_col)) {
    return(invisible(NULL))
  }

  classes_num <- suppressWarnings(as.numeric(res_stats_df[[classe_col]]))

  for (cl in classes_uniques) {
    cl_num <- suppressWarnings(as.numeric(cl))
    if (!is.finite(cl_num) || is.na(cl_num)) next

    df_stats_cl <- res_stats_df[is.finite(classes_num) & !is.na(classes_num) & classes_num == cl_num, , drop = FALSE]
    if (nrow(df_stats_cl) == 0) next

    if (isTRUE(filtrer_pvalue) && !is.null(p_col) && is.finite(max_p) && !is.na(max_p)) {
      p_vals <- suppressWarnings(as.numeric(df_stats_cl[[p_col]]))
      df_stats_cl <- df_stats_cl[is.finite(p_vals) & !is.na(p_vals) & p_vals <= max_p, , drop = FALSE]
    }
    if (nrow(df_stats_cl) == 0) next

    chi2_vals <- suppressWarnings(as.numeric(df_stats_cl[[chi2_col]]))
    df_stats_cl <- df_stats_cl[is.finite(chi2_vals) & !is.na(chi2_vals), , drop = FALSE]
    if (nrow(df_stats_cl) == 0) next

    chi2_vals <- suppressWarnings(as.numeric(df_stats_cl[[chi2_col]]))
    df_stats_cl <- df_stats_cl[order(-chi2_vals), , drop = FALSE]
    df_stats_cl <- head(df_stats_cl, top_n)

    wc_png <- file.path(wordcloud_dir, paste0("cluster_", cl, "_wordcloud.png"))
    try({
      png(wc_png, width = 800, height = 600)
      chi2_vals <- suppressWarnings(as.numeric(df_stats_cl[[chi2_col]]))
      suppressWarnings(wordcloud::wordcloud(
        words = as.character(df_stats_cl[[terme_col]]),
        freq = pmax(chi2_vals, 0),
        scale = c(8, 0.8),
        min.freq = 0,
        random.order = FALSE,
        max.words = nrow(df_stats_cl),
        colors = RColorBrewer::brewer.pal(8, "Dark2")
      ))
      dev.off()
    }, silent = TRUE)
  }

  invisible(NULL)
}
