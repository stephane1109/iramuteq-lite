# Rôle du fichier: cooccurrences.R regroupe l'analyse des cooccurrences par classe.

generer_cooccurrences_par_classe <- function(tok_ok,
                                             filtered_corpus_ok,
                                             classes_uniques,
                                             cooc_dir,
                                             top_n,
                                             top_feat,
                                             window_cooc) {
  top_n_demande <- suppressWarnings(as.integer(top_n))
  if (!is.finite(top_n_demande) || is.na(top_n_demande)) top_n_demande <- 20L
  top_n_demande <- max(5L, top_n_demande)

  top_feat_demande <- suppressWarnings(as.integer(top_feat))
  if (!is.finite(top_feat_demande) || is.na(top_feat_demande)) top_feat_demande <- 20L
  top_feat_demande <- max(5L, top_feat_demande)

  window_effectif <- suppressWarnings(as.integer(window_cooc))
  if (!is.finite(window_effectif) || is.na(window_effectif)) window_effectif <- 5L
  window_effectif <- max(1L, window_effectif)

  for (cl in classes_uniques) {
    tok_cl <- tok_ok[docvars(filtered_corpus_ok)$Classes == cl]
    cooc_png <- file.path(cooc_dir, paste0("cluster_", cl, "_fcm_network.png"))

    try({
      if (length(tok_cl) > 0) {
        fcm_cl <- fcm(tok_cl, context = "window", window = window_effectif, tri = FALSE)
        term_freq <- sort(colSums(fcm_cl), decreasing = TRUE)

        # On borne aussi par top_n pour garder une cohérence entre nuage de mots et graphe de cooccurrences.
        top_feat_effectif <- min(top_feat_demande, top_n_demande)
        feat_sel <- names(term_freq)[seq_len(min(top_feat_effectif, length(term_freq)))]
        fcm_cl <- fcm_select(fcm_cl, feat_sel, selection = "keep")

        adj <- as.matrix(fcm_cl)
        g <- graph_from_adjacency_matrix(adj, mode = "undirected", weighted = TRUE, diag = FALSE)

        num_nodes <- length(V(g))
        palette_colors <- brewer.pal(min(8, num_nodes), "Set3")
        V(g)$color <- palette_colors[seq_along(V(g))]

        png(cooc_png, width = 1600, height = 1200)
        plot(
          g,
          layout = layout_with_fr(g),
          main = paste("Cooccurrences - Classe", cl),
          vertex.size = 16,
          vertex.color = V(g)$color,
          vertex.label = V(g)$name,
          vertex.label.cex = 1,
          edge.width = E(g)$weight / 2,
          edge.color = "gray80"
        )
        dev.off()
      }
    }, silent = TRUE)
  }
}

construire_table_cooccurrences <- function(cooc_dir) {
  cooc_files <- list.files(cooc_dir, pattern = "\\.png$", full.names = FALSE)
  if (length(cooc_files) > 0) {
    cooc_classes <- gsub("^cluster_([0-9]+)_fcm_network\\.png$", "\\1", cooc_files)
    coocs_df <- data.frame(
      classe = cooc_classes,
      src = file.path("cooccurrences", cooc_files),
      stringsAsFactors = FALSE
    )
    coocs_df <- coocs_df[order(suppressWarnings(as.integer(coocs_df$classe))), , drop = FALSE]
  } else {
    coocs_df <- data.frame(classe = character(0), src = character(0), stringsAsFactors = FALSE)
  }

  coocs_df
}
