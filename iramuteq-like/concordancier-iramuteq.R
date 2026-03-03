# Rôle du fichier: concordancier-iramuteq.R génère un concordancier HTML dédié au mode IRaMuTeQ-like.
# Le rendu suit le style Rainette (segments par classe + surlignage),
# avec une sélection des termes alignée sur les filtres statistiques IRaMuTeQ-like.

.generer_concordancier_iramuteq_termes <- function(res_stats_df, classe, max_p = 1, filtrer_pvalue = TRUE) {
  if (is.null(res_stats_df) || nrow(res_stats_df) == 0) return(character(0))
  if (!all(c("Classe", "Terme") %in% names(res_stats_df))) return(character(0))

  df <- res_stats_df
  cl <- suppressWarnings(as.numeric(classe))
  if (!is.na(cl)) {
    classes_num <- suppressWarnings(as.numeric(df$Classe))
    df <- df[!is.na(classes_num) & classes_num == cl, , drop = FALSE]
  }

  if (nrow(df) == 0) return(character(0))

  # Filtres IRaMuTeQ-like: p <= max_p et, par défaut, uniquement les chi2 positifs.
  if (isTRUE(filtrer_pvalue)) {
    if ("p" %in% names(df) && is.finite(max_p) && !is.na(max_p)) {
      p_vals <- suppressWarnings(as.numeric(df$p))
      df <- df[!is.na(p_vals) & p_vals <= max_p, , drop = FALSE]
    } else if ("p_value" %in% names(df) && is.finite(max_p) && !is.na(max_p)) {
      p_vals <- suppressWarnings(as.numeric(df$p_value))
      df <- df[!is.na(p_vals) & p_vals <= max_p, , drop = FALSE]
    }
  }

  if ("chi2" %in% names(df)) {
    chi2_vals <- suppressWarnings(as.numeric(df$chi2))
    df <- df[!is.na(chi2_vals) & chi2_vals > 0, , drop = FALSE]
    chi2_vals <- suppressWarnings(as.numeric(df$chi2))
    df <- df[order(-chi2_vals), , drop = FALSE]
  }

  termes <- unique(as.character(df$Terme))
  termes <- termes[!is.na(termes) & nzchar(trimws(termes))]
  termes
}

generer_concordancier_iramuteq_html <- function(
  chemin_sortie,
  segments_by_class,
  res_stats_df,
  max_p,
  filtrer_pvalue = TRUE,
  textes_indexation,
  avancer = NULL,
  rv = NULL,
  ...
) {
  if (!is.null(rv)) ajouter_log(rv, "Concordancier IRaMuTeQ-like : génération HTML (filtres IRaMuTeQ + surlignage Unicode).")

  con <- file(chemin_sortie, open = "wt", encoding = "UTF-8")
  on.exit(try(close(con), silent = TRUE), add = TRUE)

  writeLines("<html><head><meta charset='utf-8'/>", con)
  writeLines("<style>body{font-family:Arial,sans-serif;line-height:1.45;} span.highlight{background-color:yellow;} p.segment{margin:0 0 .45rem 0;} .classe-bloc{margin-bottom:1.25rem;padding-bottom:.8rem;border-bottom:1px solid #eee;}</style>", con)
  writeLines("</head><body>", con)
  writeLines("<h1>Concordancier IRaMuTeQ-like</h1>", con)
  writeLines("<h2>Segments par classe</h2>", con)
  writeLines(if (isTRUE(filtrer_pvalue)) "<h3>Filtrage: p ≤ seuil + χ² positif (puis fallback top χ²)</h3>" else "<h3>Filtrage: χ² positif (sans filtre p-value)</h3>", con)

  noms_classes <- names(segments_by_class)
  n_classes <- length(noms_classes)
  if (n_classes == 0) n_classes <- 1

  for (i in seq_along(noms_classes)) {
    cl <- noms_classes[[i]]
    if (!is.null(avancer)) avancer(0.75 + (i / n_classes) * 0.08, paste0("HTML IRaMuTeQ : classe ", cl))

    writeLines("<div class='classe-bloc'>", con)
    writeLines(paste0("<h2>Classe ", cl, "</h2>"), con)

    segments <- segments_by_class[[cl]]
    ids_cl <- names(segments)
    if (length(ids_cl) == 0) {
      writeLines("<p><em>Aucun segment.</em></p>", con)
      writeLines("</div>", con)
      next
    }

    textes_filtrage <- unname(segments)
    if (!is.null(textes_indexation) && length(textes_indexation) > 0) {
      tx <- textes_indexation[ids_cl]
      ok_tx <- !is.na(tx) & nzchar(tx)
      if (any(ok_tx)) textes_filtrage[ok_tx] <- tx[ok_tx]
    }

    termes_cl <- .generer_concordancier_iramuteq_termes(res_stats_df, cl, max_p = max_p, filtrer_pvalue = filtrer_pvalue)

    if (length(termes_cl) == 0 && !is.null(res_stats_df) && nrow(res_stats_df) > 0 && "Classe" %in% names(res_stats_df)) {
      df_cl <- res_stats_df[suppressWarnings(as.numeric(res_stats_df$Classe)) == suppressWarnings(as.numeric(cl)), , drop = FALSE]
      if (nrow(df_cl) > 0 && "chi2" %in% names(df_cl) && "Terme" %in% names(df_cl)) {
        chi2_vals <- suppressWarnings(as.numeric(df_cl$chi2))
        idx <- !is.na(chi2_vals) & !is.na(df_cl$Terme) & nzchar(as.character(df_cl$Terme))
        if (any(idx)) {
          df_cl <- df_cl[idx, , drop = FALSE]
          df_cl <- df_cl[order(-suppressWarnings(as.numeric(df_cl$chi2))), , drop = FALSE]
          termes_cl <- unique(head(as.character(df_cl$Terme), 20))
        }
      }
    }

    termes_cl <- expandir_variantes_termes(termes_cl)
    keep <- detecter_segments_contenant_termes_unicode(textes_filtrage, termes_cl)
    keep[is.na(keep)] <- FALSE

    segments_keep <- segments[keep]
    if (length(segments_keep) == 0 && length(segments) > 0) {
      segments_keep <- segments
      if (!is.null(rv)) {
        ajouter_log(rv, paste0(
          "Concordancier IRaMuTeQ-like : classe ", cl,
          " sans segment après filtrage, fallback sur tous les segments."
        ))
      }
    }

    writeLines(paste0("<p><em>Segments conservés : ", length(segments_keep), " / ", length(segments), "</em></p>"), con)

    if (length(segments_keep) == 0) {
      writeLines("<p><em>Aucun segment.</em></p>", con)
      writeLines("</div>", con)
      next
    }

    if (length(termes_cl) == 0) {
      for (seg in echapper_segments_en_preservant_surlignage(unname(segments_keep), "<span class='highlight'>", "</span>")) {
        writeLines(paste0("<p class='segment'>", seg, "</p>"), con)
      }
      writeLines("</div>", con)
      next
    }

    motifs <- preparer_motifs_surlignage_nfd(termes_cl, taille_lot = 80)
    segments_hl <- surligner_vecteur_html_unicode(
      unname(segments_keep),
      motifs,
      "<span class='highlight'>",
      "</span>",
      on_error = function(e, pat) {
        if (!is.null(rv)) {
          ajouter_log(rv, paste0("Concordancier IRaMuTeQ-like : erreur regex [", pat, "] - ", conditionMessage(e)))
        }
      }
    )

    has_hl <- any(grepl("<span class='highlight'>", segments_hl, fixed = TRUE))
    if (!has_hl) {
      textes_keep_idx <- textes_filtrage[keep]
      segments_hl_idx <- surligner_vecteur_html_unicode(
        unname(textes_keep_idx),
        motifs,
        "<span class='highlight'>",
        "</span>",
        on_error = function(e, pat) {
          if (!is.null(rv)) {
            ajouter_log(rv, paste0("Concordancier IRaMuTeQ-like : erreur regex index [", pat, "] - ", conditionMessage(e)))
          }
        }
      )
      if (any(grepl("<span class='highlight'>", segments_hl_idx, fixed = TRUE))) {
        segments_hl <- segments_hl_idx
      }
    }

    if (length(segments_hl) == 0 && length(segments_keep) > 0) {
      segments_hl <- unname(segments_keep)
    }

    for (seg in echapper_segments_en_preservant_surlignage(segments_hl, "<span class='highlight'>", "</span>")) {
      writeLines(paste0("<p class='segment'>", seg, "</p>"), con)
    }
    writeLines("</div>", con)
  }

  writeLines("</body></html>", con)
  close(con)
  if (!is.null(rv)) ajouter_log(rv, paste0("Concordancier IRaMuTeQ-like : HTML écrit dans : ", chemin_sortie))
  chemin_sortie
}
