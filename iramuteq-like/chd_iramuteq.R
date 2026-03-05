# Rôle du fichier: chd_iramuteq.R introduit une base "IRaMuTeQ-like" pour la CHD.
# Ce module prépare les entrées de CHD en respectant les options de nettoyage de l'application,
# expose des utilitaires pour le calcul de mincl (convention IRaMuTeQ texte),
# et fournit un calcul CHD réel en s'appuyant sur les scripts R historiques d'IRaMuTeQ.

# Valeur mincl automatique (mode texte IRaMuTeQ):
# mincl = round(n_uce / ind), avec ind = nbcl * 2 (double) sinon nbcl.
calculer_mincl_auto_iramuteq <- function(n_uce, nbcl, classif_mode = c("double", "simple")) {
  classif_mode <- match.arg(classif_mode)
  n_uce <- as.integer(n_uce)
  nbcl <- as.integer(nbcl)

  if (!is.finite(n_uce) || is.na(n_uce) || n_uce < 1) {
    stop("mincl auto IRaMuTeQ: n_uce invalide.")
  }
  if (!is.finite(nbcl) || is.na(nbcl) || nbcl < 1) {
    stop("mincl auto IRaMuTeQ: nbcl invalide.")
  }

  ind <- if (identical(classif_mode, "double")) nbcl * 2L else nbcl
  mincl <- round(n_uce / ind)
  as.integer(max(1L, mincl))
}

# Normalise une liste d'options de nettoyage selon les clés utilisées dans l'UI.
normaliser_options_nettoyage_iramuteq <- function(options_nettoyage = list()) {
  opts <- list(
    nettoyage_caracteres = isTRUE(options_nettoyage$nettoyage_caracteres),
    forcer_minuscules_avant = isTRUE(options_nettoyage$forcer_minuscules_avant),
    supprimer_chiffres = isTRUE(options_nettoyage$supprimer_chiffres),
    supprimer_apostrophes = isTRUE(options_nettoyage$supprimer_apostrophes),
    supprimer_ponctuation = isTRUE(options_nettoyage$supprimer_ponctuation),
    retirer_stopwords = isTRUE(options_nettoyage$retirer_stopwords)
  )
  opts
}

# Prépare textes/tokens/dfm en tenant compte des options de nettoyage existantes de l'application.
preparer_entrees_chd_iramuteq <- function(
  textes,
  langue = "fr",
  options_nettoyage = list(),
  appliquer_nettoyage_fun = NULL
) {
  if (!is.character(textes)) {
    textes <- as.character(textes)
  }
  textes[is.na(textes)] <- ""

  opts <- normaliser_options_nettoyage_iramuteq(options_nettoyage)

  if (is.null(appliquer_nettoyage_fun)) {
    if (exists("appliquer_nettoyage_et_minuscules", mode = "function", inherits = TRUE)) {
      appliquer_nettoyage_fun <- get("appliquer_nettoyage_et_minuscules", mode = "function", inherits = TRUE)
    } else {
    op <- par(no.readonly = TRUE)
    on.exit(par(op), add = TRUE)
    par(mar = c(3.0, 2.6, 3.4, 8.5))

      appliquer_nettoyage_fun <- function(textes,
                                          activer_nettoyage = FALSE,
                                          forcer_minuscules = FALSE,
                                          supprimer_chiffres = FALSE,
                                          supprimer_apostrophes = FALSE) {
        x <- as.character(textes)
        if (isTRUE(forcer_minuscules)) x <- tolower(x)
        x
      }
    }
  }

  textes_prep <- appliquer_nettoyage_fun(
    textes = textes,
    activer_nettoyage = opts$nettoyage_caracteres,
    forcer_minuscules = opts$forcer_minuscules_avant,
    supprimer_chiffres = opts$supprimer_chiffres,
    supprimer_apostrophes = opts$supprimer_apostrophes
  )

  if (!requireNamespace("quanteda", quietly = TRUE)) {
    stop("CHD IRaMuTeQ-like: package quanteda requis pour préparer les entrées.")
  }

  tok <- quanteda::tokens(
    textes_prep,
    remove_punct = opts$supprimer_ponctuation,
    remove_numbers = opts$supprimer_chiffres
  )

  if (opts$retirer_stopwords) {
    sw <- quanteda::stopwords(language = langue)
    tok <- quanteda::tokens_remove(tok, pattern = sw)
  }

  dfm_obj <- quanteda::dfm(tok)
  list(textes = textes_prep, tok = tok, dfm = dfm_obj, options = opts)
}

.trouver_fichier_insensible_casse <- function(dir_path, filename) {
  if (!dir.exists(dir_path)) return(NA_character_)
  files <- list.files(dir_path, full.names = TRUE)
  if (length(files) == 0) return(NA_character_)
  bn <- basename(files)
  idx <- which(tolower(bn) == tolower(filename))
  if (length(idx) == 0) return(NA_character_)
  files[idx[1]]
}

.trouver_rscripts_iramuteq <- function(base_dir = NULL) {
  scripts <- c("anacor.R", "CHD.R", "chdtxt.R")
  candidats_bruts <- unique(c(
    base_dir,
    "iramuteq-like",
    file.path(getwd(), "iramuteq-like")
  ))
  candidats <- candidats_bruts[
    !is.na(candidats_bruts) &
      nzchar(candidats_bruts) &
      dir.exists(candidats_bruts)
  ]

  for (cand in candidats) {
    paths <- vapply(scripts, function(sc) .trouver_fichier_insensible_casse(cand, sc), FUN.VALUE = character(1))
    if (all(!is.na(paths))) {
      return(unname(paths))
    }
  }

  stop(
    "CHD IRaMuTeQ-like: scripts R introuvables. Répertoires testés: ",
    if (length(candidats)) paste(candidats, collapse = ", ") else "(aucun répertoire existant)",
    ". Fichiers attendus: ",
    paste(scripts, collapse = ", "),
    "."
  )
}

.charger_scripts_iramuteq_chd <- function(base_dir = NULL) {
  paths <- .trouver_rscripts_iramuteq(base_dir)
  for (p in paths) {
    source(p, encoding = "UTF-8", local = .GlobalEnv)
  }
  invisible(paths)
}

.normaliser_n1_chd <- function(n1) {
  if (is.null(n1)) return(NULL)
  if (is.data.frame(n1)) n1 <- as.matrix(n1)
  if (is.vector(n1)) {
    n1 <- matrix(as.integer(n1), ncol = 1)
  }
  if (!is.matrix(n1)) return(NULL)
  if (nrow(n1) < 1 || ncol(n1) < 1) return(NULL)
  n1
}

# Calcul CHD IRaMuTeQ-like (algorithme historique via scripts R IRaMuTeQ).
calculer_chd_iramuteq <- function(
  dfm_obj,
  k = 3,
  mode_patate = FALSE,
  svd_method = c("irlba", "svdR"),
  libsvdc_path = NULL,
  binariser = FALSE,
  rscripts_dir = NULL
) {
  svd_method <- match.arg(svd_method)

  if (is.null(dfm_obj)) stop("CHD IRaMuTeQ-like: dfm_obj manquant.")
  if (!is.finite(k) || is.na(k) || as.integer(k) < 2) stop("CHD IRaMuTeQ-like: k doit être >= 2.")

  .charger_scripts_iramuteq_chd(rscripts_dir)

  mat <- as.matrix(dfm_obj)
  if (nrow(mat) < 2 || ncol(mat) < 2) {
    stop("CHD IRaMuTeQ-like: matrice trop pauvre (>=2 lignes et >=2 colonnes requises).")
  }

  if (isTRUE(binariser)) {
    mat <- ifelse(mat > 0, 1, 0)
  }

  rownames(mat) <- as.character(seq_len(nrow(mat)))

  nb_tours <- as.integer(k) - 1L
  if (nb_tours < 1) nb_tours <- 1L

  chd <- CHD(
    data.in = mat,
    x = nb_tours,
    mode.patate = isTRUE(mode_patate),
    svd.method = svd_method,
    libsvdc.path = libsvdc_path
  )

  n1 <- .normaliser_n1_chd(chd$n1)
  if (is.null(n1) || nrow(n1) != nrow(mat)) {
    stop("CHD IRaMuTeQ-like: sortie CHD invalide.")
  }

  chd$n1 <- n1

  chd
}

# Reconstitue des classes finales depuis la sortie CHD et le principe find.terminales.
reconstruire_classes_terminales_iramuteq <- function(
  chd_obj,
  mincl = 0,
  mincl_mode = c("auto", "manuel"),
  classif_mode = c("simple", "double"),
  nb_classes_cible = NULL,
  respecter_nb_classes = TRUE
) {
  mincl_mode <- match.arg(mincl_mode)
  classif_mode <- match.arg(classif_mode)

  n1 <- .normaliser_n1_chd(chd_obj$n1)
  list_mere <- chd_obj$list_mere
  list_fille <- chd_obj$list_fille

  if (is.null(n1) || is.null(list_mere) || is.null(list_fille)) {
    stop("CHD IRaMuTeQ-like: objet chd incomplet.")
  }

  nbcl <- length(unique(n1[, ncol(n1)]))
  nbcl <- max(2L, as.integer(nbcl))

  if (mincl_mode == "auto") {
    mincl_use <- calculer_mincl_auto_iramuteq(
      n_uce = nrow(n1),
      nbcl = nbcl,
      classif_mode = classif_mode
    )
  } else {
    op <- par(no.readonly = TRUE)
    on.exit(par(op), add = TRUE)
    par(mar = c(3.0, 2.6, 3.4, 8.5))

    mincl_use <- as.integer(mincl)
    if (!is.finite(mincl_use) || is.na(mincl_use) || mincl_use < 1) mincl_use <- 1L
  }

  terminales <- find.terminales(n1, list_mere, list_fille, mincl = mincl_use)
  if (is.character(terminales) && length(terminales) == 1 && terminales == "no clusters") {
    stop("CHD IRaMuTeQ-like: aucune classe terminale retenue.")
  }

  feuilles <- unique(as.integer(n1[, ncol(n1)]))

  if (isTRUE(respecter_nb_classes) && !is.null(nb_classes_cible) && is.finite(nb_classes_cible)) {
    nb_classes_cible <- as.integer(nb_classes_cible)
    if (nb_classes_cible >= 2 && length(feuilles) == nb_classes_cible && length(unique(terminales)) != nb_classes_cible) {
      terminales <- sort(feuilles)
      mincl_use <- 1L
    }
  }

  classes_finales <- rep(0L, nrow(n1))
  feuilles_docs <- suppressWarnings(as.integer(n1[, ncol(n1)]))

  # Reproduction fidèle de iramuteq_clone_v3/Rscripts/chdtxt.R::make.classes
  # (sans la partie manipulation du tree, non nécessaire au calcul des classes docs).
  cl_names <- seq_along(terminales)
  for (i in seq_along(terminales)) {
    cl <- suppressWarnings(as.integer(terminales[[i]]))
    if (!is.finite(cl)) next

    if (cl %in% feuilles) {
      classes_finales[which(feuilles_docs == cl)] <- cl_names[[i]]
    } else {
    op <- par(no.readonly = TRUE)
    on.exit(par(op), add = TRUE)
    par(mar = c(3.0, 2.6, 3.4, 8.5))

      filles <- suppressWarnings(as.integer(getfille(list_fille, cl, NULL)))
      tochange <- intersect(filles, feuilles)
      for (cl_fille in tochange) {
        classes_finales[which(feuilles_docs == cl_fille)] <- cl_names[[i]]
      }
    }
  }

  classes_finales[which(is.na(classes_finales))] <- 0L
  list(
    classes = classes_finales,
    terminales = as.integer(terminales),
    mincl = mincl_use
  )
}

# Calcule une table de statistiques par classe dans l'esprit des sorties IRaMuTeQ.
construire_stats_classes_iramuteq <- function(dfm_obj, classes, max_p = 1) {
  if (is.null(dfm_obj)) stop("Stats IRaMuTeQ-like: dfm_obj manquant.")
  if (is.null(classes)) stop("Stats IRaMuTeQ-like: classes manquantes.")

  mat <- as.matrix(dfm_obj)
  if (nrow(mat) != length(classes)) {
    stop("Stats IRaMuTeQ-like: longueur de classes incohérente avec le DFM.")
  }

  classes <- as.integer(classes)
  # Alignement IRaMuTeQ clone (BuildProf/chdfunct.R):
  # les UCE non classées (classe 0) sont exclues du comptage
  # des effectifs et des tableaux de contingence.
  ok_docs <- !is.na(classes) & classes > 0L
  mat <- mat[ok_docs, , drop = FALSE]
  classes <- classes[ok_docs]

  if (nrow(mat) < 2 || ncol(mat) < 1) return(data.frame())

  # Alignement avec l'approche IRaMuTeQ historique (BuildProf):
  # - contingence documentaire (présence/absence terme)
  # - chi2 signé (sur/sous-représentation)
  # - p-value issue de chisq.test(..., correct = FALSE)
  mat_bin <- ifelse(mat > 0, 1L, 0L)
  total_docs <- nrow(mat_bin)
  docs_par_terme <- colSums(mat_bin)
  occ_par_terme <- colSums(mat)

  calc_chi_sign <- function(a, b, c, d) {
    tb <- matrix(c(a, b, c, d), nrow = 2, byrow = TRUE)
    chi <- suppressWarnings(stats::chisq.test(tb, correct = FALSE))
    stat <- suppressWarnings(as.numeric(chi$statistic))
    pval <- suppressWarnings(as.numeric(chi$p.value))
    exp11 <- suppressWarnings(as.numeric(chi$expected[1, 1]))

    if (!is.finite(stat) || is.na(stat)) stat <- 0
    if (!is.finite(pval) || is.na(pval)) pval <- 1
    if (!is.finite(exp11) || is.na(exp11)) exp11 <- a

    signe <- if (a >= exp11) 1 else -1
    c(chi2 = stat * signe, p = pval)
  }

  classes_uniques <- sort(unique(classes))
  sorties <- vector("list", length(classes_uniques))

  for (i in seq_along(classes_uniques)) {
    cl <- classes_uniques[[i]]
    in_cl <- classes == cl

    docs_cl <- sum(in_cl)
    if (docs_cl < 1) next

    docs_terme_cl <- colSums(mat_bin[in_cl, , drop = FALSE])
    docs_terme_hors <- pmax(0, docs_par_terme - docs_terme_cl)

    n11 <- as.numeric(docs_terme_cl)
    n12 <- as.numeric(docs_terme_hors)
    n21 <- as.numeric(pmax(0, docs_cl - docs_terme_cl))
    n22 <- as.numeric(pmax(0, (total_docs - docs_cl) - docs_terme_hors))

    chi_p <- t(mapply(calc_chi_sign, n11, n12, n21, n22))

    freq_cl <- colSums(mat[in_cl, , drop = FALSE])
    docprop_cl <- if (docs_cl > 0) docs_terme_cl / docs_cl else rep(0, ncol(mat))
    lr <- mapply(function(a, b, c, d) {
      n <- a + b + c + d
      r1 <- a + b
      r2 <- c + d
      c1 <- a + c
      c2 <- b + d
      expected <- c(r1 * c1 / n, r1 * c2 / n, r2 * c1 / n, r2 * c2 / n)
      observed <- c(a, b, c, d)
      idx <- observed > 0 & expected > 0
      if (!any(idx)) return(0)
      2 * sum(observed[idx] * log(observed[idx] / expected[idx]))
    }, n11, n12, n21, n22)

    df <- data.frame(
      Terme = colnames(mat),
      chi2 = as.numeric(chi_p[, "chi2"]),
      lr = as.numeric(lr),
      frequency = as.numeric(freq_cl),
      docprop = as.numeric(docprop_cl),
      # Alignement IRaMuTeQ: les colonnes d'effectifs affichées en table
      # correspondent aux occurrences (et non au nombre de segments contenant le terme).
      # Ex.: "23/46" signifie 23 occurrences dans la classe sur 46 occurrences au total.
      eff_st = as.numeric(freq_cl),
      eff_total = as.numeric(occ_par_terme),
      pourcentage = as.numeric(ifelse(occ_par_terme > 0, 100 * freq_cl / occ_par_terme, 0)),
      # Colonnes documentaires conservées pour diagnostic (chi2 calculé sur présence/absence doc).
      eff_docs_st = as.numeric(docs_terme_cl),
      eff_docs_total = as.numeric(docs_par_terme),
      p = as.numeric(chi_p[, "p"]),
      Classe = as.integer(cl),
      stringsAsFactors = FALSE
    )

    df <- df[is.finite(df$chi2) & !is.na(df$chi2), , drop = FALSE]
    if (is.finite(max_p) && !is.na(max_p) && max_p < 1) {
      df <- df[df$p <= max_p, , drop = FALSE]
    }
    df <- df[order(-df$chi2, -df$frequency, -occ_par_terme[df$Terme]), , drop = FALSE]
    sorties[[i]] <- df
  }

  out <- dplyr::bind_rows(sorties)
  if (!nrow(out)) return(out)

  out$Classe_brut <- as.character(out$Classe)
  out$p_value <- out$p
  out$p_value_filter <- ifelse(out$p <= max_p, paste0("≤ ", max_p), paste0("> ", max_p))
  out
}

# Dendrogramme CHD basé sur la structure hiérarchique IRaMuTeQ (list_mere/list_fille).
tracer_dendrogramme_chd_iramuteq <- function(chd_obj,
                                              terminales = NULL,
                                              classes = NULL,
                                              res_stats_df = NULL,
                                              top_n_terms = 4,
                                              orientation = c("vertical", "horizontal")) {
  orientation <- match.arg(orientation)

  .tracer_dendrogramme_hclust <- function(res_stats_df, classes, top_n_terms, orientation) {
    if (is.null(res_stats_df) || !is.data.frame(res_stats_df)) return(FALSE)
    if (!all(c("Classe", "Terme") %in% names(res_stats_df))) return(FALSE)

    df <- res_stats_df
    df$Classe <- suppressWarnings(as.integer(df$Classe))
    df$Terme <- as.character(df$Terme)
    df <- df[is.finite(df$Classe) & df$Classe > 0 & nzchar(df$Terme), , drop = FALSE]
    if (!nrow(df)) return(FALSE)

    poids <- if ("frequency" %in% names(df)) suppressWarnings(as.numeric(df$frequency)) else rep(1, nrow(df))
    poids[!is.finite(poids) | is.na(poids) | poids < 0] <- 0
    df$poids <- poids

    mat <- stats::xtabs(poids ~ Classe + Terme, data = df)
    if (nrow(mat) < 2 || ncol(mat) < 2) return(FALSE)

    # Colonnes constantes retirées pour stabiliser dist/hclust.
    vars <- apply(mat, 2, stats::sd)
    vars[!is.finite(vars)] <- 0
    mat <- mat[, vars > 0, drop = FALSE]
    if (nrow(mat) < 2 || ncol(mat) < 2) return(FALSE)

    dist_obj <- stats::dist(mat, method = "euclidean")
    if (!inherits(dist_obj, "dist") || length(dist_obj) == 0) return(FALSE)
    hc <- stats::hclust(dist_obj, method = "ward.D2")

    classes_obs <- suppressWarnings(as.integer(classes))
    classes_obs <- classes_obs[is.finite(classes_obs) & classes_obs > 0]
    pct_par_classe <- if (length(classes_obs)) prop.table(table(classes_obs)) * 100 else NULL

    top_n_terms <- suppressWarnings(as.integer(top_n_terms))
    if (!is.finite(top_n_terms) || is.na(top_n_terms) || top_n_terms < 1L) top_n_terms <- 1L

    termes_chi2 <- vector("list", nrow(mat))
    if ("chi2" %in% names(df)) {
      df_chi <- df
      df_chi$chi2 <- suppressWarnings(as.numeric(df_chi$chi2))
      for (i in seq_len(nrow(mat))) {
        cl <- suppressWarnings(as.integer(rownames(mat)[[i]]))
        sous <- df_chi[df_chi$Classe == cl, , drop = FALSE]
        if (!nrow(sous)) {
          termes_chi2[[i]] <- character(0)
          next
        }
        sous <- sous[is.finite(sous$chi2), , drop = FALSE]
        sous <- sous[order(sous$chi2, decreasing = TRUE), , drop = FALSE]
        termes <- unique(as.character(sous$Terme))
        termes <- termes[nzchar(termes)]
        termes_chi2[[i]] <- utils::head(termes, top_n_terms)
      }
    }

    class_labels <- vapply(seq_len(nrow(mat)), function(i) {
      cl <- as.integer(rownames(mat)[[i]])
      pct <- if (!is.null(pct_par_classe)) suppressWarnings(as.numeric(pct_par_classe[[as.character(cl)]])) else NA_real_
      pct_txt <- if (is.finite(pct)) paste0(" (", format(round(pct, 1), nsmall = 1), " %)") else ""
      paste0("Classe ", cl, pct_txt)
    }, FUN.VALUE = character(1))

    termes_affiches <- lapply(seq_len(nrow(mat)), function(i) {
      termes <- termes_chi2[[i]]
      if (!length(termes)) {
        ligne <- mat[i, , drop = TRUE]
        termes <- names(sort(ligne, decreasing = TRUE))
        termes <- termes[is.finite(ligne[termes]) & ligne[termes] > 0]
      }
      utils::head(termes, top_n_terms)
    })

    hc$labels <- class_labels

    classes_hc <- suppressWarnings(as.integer(rownames(mat)))
    classes_hc[!is.finite(classes_hc)] <- seq_along(classes_hc)

    palette_classes <- grDevices::hcl.colors(length(unique(classes_hc)), palette = "Dark 3")
    cols_classes <- stats::setNames(palette_classes, as.character(sort(unique(classes_hc))))
    tip_cols <- unname(cols_classes[as.character(classes_hc)])
    tip_cols[!nzchar(tip_cols)] <- "#FFFFFF"

    op <- par(no.readonly = TRUE)
    on.exit(par(op), add = TRUE)
    par(bg = "white", fg = "black", col.axis = "black", col.lab = "black")

    tracer_nuage_ligne <- function(x, y, termes, col = "#1f1f1f") {
      termes <- as.character(termes)
      termes <- termes[nzchar(termes)]
      if (!length(termes)) return(invisible(NULL))
      n <- length(termes)
      offsets_x <- seq(-0.45, 0.45, length.out = n)
      offsets_y <- rep(c(0, -0.06, 0.06), length.out = n)
      tailles <- seq(0.9, 0.65, length.out = n)
      text(x + offsets_x, y + offsets_y, labels = termes, cex = tailles, col = col, xpd = TRUE)
      invisible(NULL)
    }

    if (identical(orientation, "horizontal")) {
      max_h <- suppressWarnings(max(hc$height, na.rm = TRUE))
      if (!is.finite(max_h) || max_h <= 0) max_h <- 1
      xlim_use <- c(0, max_h * 2.2)

      plot(
        hc,
        hang = -1,
        horiz = TRUE,
        labels = FALSE,
        cex = 0.75,
        main = "Dendrogramme CHD",
        xlab = "",
        sub = "",
        xlim = xlim_use
      )

      tip_y <- match(seq_along(hc$order), hc$order)
      usr <- par("usr")
      dx <- usr[[2]] - usr[[1]]
      x_text <- usr[[1]] + 0.02 * dx

      text(x = x_text, y = tip_y + 0.18, labels = class_labels[hc$order], pos = 4, offset = 0.2, cex = 0.75, col = tip_cols[hc$order], xpd = TRUE)
      for (i in seq_along(tip_y)) {
        tracer_nuage_ligne(x = x_text + 0.7, y = tip_y[[i]] - 0.18, termes = termes_affiches[[hc$order[[i]]]], col = tip_cols[hc$order[[i]]])
      }
      return(TRUE)
    }

    plot(
      hc,
      hang = -1,
      horiz = FALSE,
      labels = FALSE,
      cex = 0.75,
      main = "Dendrogramme CHD",
      xlab = "",
      sub = ""
    )

    usr <- par("usr")
    y_class <- usr[[3]] + 0.08 * (usr[[4]] - usr[[3]])
    y_words <- usr[[3]] + 0.02 * (usr[[4]] - usr[[3]])
    x_tips <- seq_along(hc$order)
    text(x = x_tips, y = y_class, labels = class_labels[hc$order], cex = 0.72, col = tip_cols[hc$order], xpd = TRUE)
    for (i in seq_along(x_tips)) {
      tracer_nuage_ligne(x = x_tips[[i]], y = y_words, termes = termes_affiches[[hc$order[[i]]]], col = tip_cols[hc$order[[i]]])
    }
    TRUE
  }

  n1 <- .normaliser_n1_chd(chd_obj$n1)
  list_fille <- chd_obj$list_fille
  has_chd_tree <- !is.null(n1) && is.list(list_fille) && length(list_fille) > 0

  # Priorité au vrai arbre CHD (list_mere/list_fille) pour rester fidèle au découpage des classes.
  # Le hclust sur la table Classes × Termes n'est qu'un repli en cas de structure CHD indisponible.
  if (!has_chd_tree) {
    if (.tracer_dendrogramme_hclust(res_stats_df = res_stats_df, classes = classes, top_n_terms = top_n_terms, orientation = orientation)) {
      return(invisible(NULL))
    }
    plot.new()
    text(0.5, 0.5, "Dendrogramme CHD indisponible.", cex = 1.1)
    return(invisible(NULL))
  }

  noms <- names(list_fille)
  if (is.null(noms) || any(!nzchar(noms))) noms <- as.character(seq_along(list_fille))
  map_filles <- stats::setNames(lapply(list_fille, function(x) as.integer(x)), noms)

  classes_utiles <- integer(0)
  if (!is.null(res_stats_df) && is.data.frame(res_stats_df) && "Classe" %in% names(res_stats_df)) {
    classes_stats <- suppressWarnings(as.integer(res_stats_df$Classe))
    classes_stats <- classes_stats[is.finite(classes_stats) & classes_stats > 0]
    classes_utiles <- sort(unique(classes_stats))
  }
  if (!length(classes_utiles) && !is.null(classes)) {
    classes_int <- suppressWarnings(as.integer(classes))
    classes_int <- classes_int[is.finite(classes_int) & classes_int > 0]
    classes_utiles <- sort(unique(classes_int))
  }

  terminales <- suppressWarnings(as.integer(terminales))
  terminales <- unique(terminales[is.finite(terminales)])
  if (length(classes_utiles) && length(terminales)) {
    keep <- classes_utiles >= 1L & classes_utiles <= length(terminales)
    classes_utiles <- classes_utiles[keep]
    terminales <- terminales[classes_utiles]
  }

  edges_df <- do.call(rbind, lapply(names(map_filles), function(mere_name) {
    mere <- suppressWarnings(as.integer(mere_name))
    filles <- suppressWarnings(as.integer(map_filles[[mere_name]]))
    filles <- filles[is.finite(filles)]
    if (!is.finite(mere) || !length(filles)) return(NULL)
    cbind(parent = rep.int(mere, length(filles)), child = filles)
  }))

  if (is.null(edges_df) || !nrow(edges_df)) {
    plot.new()
    text(0.5, 0.5, "Dendrogramme CHD indisponible (arbre vide).", cex = 1.1)
    return(invisible(NULL))
  }

  edges_df <- unique(as.data.frame(edges_df))
  all_nodes <- sort(unique(c(edges_df$parent, edges_df$child)))
  parent_nodes <- sort(unique(edges_df$parent))
  detected_tips <- setdiff(all_nodes, parent_nodes)

  if (!length(detected_tips)) {
    plot.new()
    text(0.5, 0.5, "Aucune classe terminale exploitable.", cex = 1.1)
    return(invisible(NULL))
  }

  tip_nodes <- detected_tips
  if (length(terminales)) {
    term_tips <- terminales[terminales %in% detected_tips]
    if (length(term_tips)) tip_nodes <- unique(term_tips)
  }

  class_by_tip <- stats::setNames(rep(NA_integer_, length(tip_nodes)), as.character(tip_nodes))
  if (length(terminales)) {
    class_ids <- if (length(classes_utiles) == length(terminales)) classes_utiles else seq_along(terminales)
    for (i in seq_along(terminales)) {
      node <- as.character(terminales[[i]])
      if (node %in% names(class_by_tip)) class_by_tip[[node]] <- class_ids[[i]]
    }
  }
  fallback <- which(!is.finite(class_by_tip))
  if (length(fallback)) class_by_tip[fallback] <- seq_along(fallback)

  pct_par_classe <- NULL
  if (!is.null(classes)) {
    classes <- suppressWarnings(as.integer(classes))
    classes <- classes[is.finite(classes) & classes > 0]
    if (length(classes)) pct_par_classe <- prop.table(table(classes)) * 100
  }

  top_n_terms <- suppressWarnings(as.integer(top_n_terms))
  if (!is.finite(top_n_terms) || is.na(top_n_terms) || top_n_terms < 1L) top_n_terms <- 1L
  termes_par_classe <- list()
  if (!is.null(res_stats_df) && is.data.frame(res_stats_df) && all(c("Classe", "Terme") %in% names(res_stats_df))) {
    df_terms <- res_stats_df
    df_terms$Classe <- suppressWarnings(as.integer(df_terms$Classe))
    df_terms <- df_terms[is.finite(df_terms$Classe) & nzchar(as.character(df_terms$Terme)), , drop = FALSE]
    for (cl in unique(class_by_tip)) {
      sous <- df_terms[df_terms$Classe == cl, , drop = FALSE]
      if (!nrow(sous)) next
      if ("chi2" %in% names(sous)) {
        chi <- suppressWarnings(as.numeric(sous$chi2))
        chi[!is.finite(chi)] <- -Inf
        sous <- sous[order(chi, decreasing = TRUE), , drop = FALSE]
      }
      termes <- unique(as.character(sous$Terme))
      termes <- termes[nzchar(termes)]
      if (length(termes)) termes_par_classe[[as.character(cl)]] <- paste(utils::head(termes, top_n_terms), collapse = ", ")
    }
  }

  tip_label <- vapply(names(class_by_tip), function(node) {
    cl <- class_by_tip[[node]]
    pct <- if (!is.null(pct_par_classe)) suppressWarnings(as.numeric(pct_par_classe[[as.character(cl)]])) else NA_real_
    pct_txt <- if (is.finite(pct)) paste0(" (", format(round(pct, 1), nsmall = 1), " %)") else ""
    termes_txt <- termes_par_classe[[as.character(cl)]]
    if (is.null(termes_txt) || !nzchar(termes_txt)) {
      paste0("Classe ", cl, pct_txt)
    } else {
      paste0("Classe ", cl, pct_txt, "\n", termes_txt)
    }
  }, FUN.VALUE = character(1))

  children_map <- split(edges_df$child, edges_df$parent)
  children_map <- lapply(children_map, function(x) as.integer(unique(x)))
  roots <- setdiff(parent_nodes, edges_df$child)
  if (!length(roots)) roots <- parent_nodes[1]

  assign_leaf_x <- function(node, state) {
    node_key <- as.character(node)
    kids <- children_map[[node_key]]
    kids <- kids[is.finite(kids)]

    if (!length(kids) || !(node %in% parent_nodes)) {
      if (!(node %in% tip_nodes)) return(state)
      state$x[[node_key]] <- state$next_x
      state$next_x <- state$next_x + 1
      return(state)
    }

    for (kid in kids) state <- assign_leaf_x(kid, state)
    kid_keys <- as.character(kids)
    kid_x <- unname(unlist(state$x[kid_keys]))
    kid_x <- kid_x[is.finite(kid_x)]
    if (length(kid_x)) state$x[[node_key]] <- mean(kid_x)
    state
  }

  state <- list(x = list(), next_x = 1)
  for (rt in roots) state <- assign_leaf_x(rt, state)
  x_pos <- state$x

  y_pos <- list()
  assign_depth <- function(node, depth = 0L) {
    node_key <- as.character(node)
    if (!is.null(y_pos[[node_key]]) && y_pos[[node_key]] <= depth) return(invisible(NULL))
    y_pos[[node_key]] <<- depth
    kids <- children_map[[node_key]]
    kids <- kids[is.finite(kids)]
    for (kid in kids) assign_depth(kid, depth + 1L)
    invisible(NULL)
  }
  for (rt in roots) assign_depth(rt, 0L)

  all_y <- unname(unlist(y_pos))
  max_depth <- if (length(all_y)) max(all_y, na.rm = TRUE) else 1
  if (!is.finite(max_depth) || max_depth <= 0) max_depth <- 1

  node_keys <- names(y_pos)
  x_vals <- vapply(node_keys, function(k) {
    val <- x_pos[[k]]
    if (!length(val)) return(NA_real_)
    as.numeric(val[[1]])
  }, numeric(1))
  y_vals <- vapply(node_keys, function(k) as.numeric(y_pos[[k]]), numeric(1))
  keep_nodes <- is.finite(x_vals) & is.finite(y_vals)
  node_keys <- node_keys[keep_nodes]
  x_vals <- x_vals[keep_nodes]
  y_vals <- y_vals[keep_nodes]

  if (!length(node_keys)) {
    plot.new()
    text(0.5, 0.5, "Dendrogramme CHD indisponible (coordonnées invalides).", cex = 1.1)
    return(invisible(NULL))
  }

  node_xy <- stats::setNames(vector("list", length(node_keys)), node_keys)
  for (i in seq_along(node_keys)) {
    node_xy[[node_keys[[i]]]] <- c(x = x_vals[[i]], y = y_vals[[i]])
  }

  op <- par(no.readonly = TRUE)
  on.exit(par(op), add = TRUE)

  if (identical(orientation, "vertical")) {
    par(mar = c(2, 1, 3, 1))
    plot(0, 0,
      type = "n",
      xlim = c(min(x_vals) - 0.5, max(x_vals) + 0.5),
      ylim = c(max_depth + 0.8, -0.5),
      axes = FALSE,
      xlab = "", ylab = "",
      main = "Dendrogramme CHD"
    )

    for (i in seq_len(nrow(edges_df))) {
      p_key <- as.character(edges_df$parent[[i]])
      c_key <- as.character(edges_df$child[[i]])
      p_xy <- node_xy[[p_key]]
      c_xy <- node_xy[[c_key]]
      if (is.null(p_xy) || is.null(c_xy)) next
      segments(p_xy[["x"]], p_xy[["y"]], c_xy[["x"]], c_xy[["y"]], xpd = TRUE)
    }

    for (tip in names(class_by_tip)) {
      xy <- node_xy[[tip]]
      if (is.null(xy)) next
      lignes <- strsplit(tip_label[[tip]], "\n", fixed = TRUE)[[1]]
      classe_txt <- lignes[[1]]
      termes_txt <- if (length(lignes) > 1) lignes[[2]] else ""
      text(xy[["x"]], xy[["y"]] + 0.2, labels = classe_txt, cex = 0.75, pos = 1, xpd = TRUE)
      if (nzchar(termes_txt)) {
        termes_vec <- trimws(strsplit(termes_txt, ",", fixed = TRUE)[[1]])
        termes_vec <- termes_vec[nzchar(termes_vec)]
        if (length(termes_vec)) {
          offsets_x <- seq(-0.45, 0.45, length.out = length(termes_vec))
          offsets_y <- rep(c(-0.1, -0.2, 0), length.out = length(termes_vec))
          tailles <- seq(0.85, 0.65, length.out = length(termes_vec))
          text(xy[["x"]] + offsets_x, xy[["y"]] + 0.02 + offsets_y, labels = termes_vec, cex = tailles, xpd = TRUE)
        }
      }
    }
  } else {
    par(mar = c(1, 2, 3, 2))
    plot(0, 0,
      type = "n",
      xlim = c(-0.5, max_depth + 0.8),
      ylim = c(min(x_vals) - 0.5, max(x_vals) + 0.5),
      axes = FALSE,
      xlab = "", ylab = "",
      main = "Dendrogramme CHD"
    )

    for (i in seq_len(nrow(edges_df))) {
      p_key <- as.character(edges_df$parent[[i]])
      c_key <- as.character(edges_df$child[[i]])
      p_xy <- node_xy[[p_key]]
      c_xy <- node_xy[[c_key]]
      if (is.null(p_xy) || is.null(c_xy)) next
      segments(p_xy[["y"]], p_xy[["x"]], c_xy[["y"]], c_xy[["x"]], xpd = TRUE)
    }

    for (tip in names(class_by_tip)) {
      xy <- node_xy[[tip]]
      if (is.null(xy)) next
      lignes <- strsplit(tip_label[[tip]], "\n", fixed = TRUE)[[1]]
      classe_txt <- lignes[[1]]
      termes_txt <- if (length(lignes) > 1) lignes[[2]] else ""
      text(xy[["y"]] + 0.15, xy[["x"]] + 0.1, labels = classe_txt, cex = 0.75, pos = 4, xpd = TRUE)
      if (nzchar(termes_txt)) {
        termes_vec <- trimws(strsplit(termes_txt, ",", fixed = TRUE)[[1]])
        termes_vec <- termes_vec[nzchar(termes_vec)]
        if (length(termes_vec)) {
          offsets_y <- seq(-0.35, 0.35, length.out = length(termes_vec))
          tailles <- seq(0.85, 0.65, length.out = length(termes_vec))
          text(xy[["y"]] + 0.55, xy[["x"]] + offsets_y, labels = termes_vec, cex = tailles, pos = 4, xpd = TRUE)
        }
      }
    }
  }

  invisible(NULL)
}
