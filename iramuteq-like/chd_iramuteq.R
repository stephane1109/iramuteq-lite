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
    candidats <- unique(c(
    base_dir,
    "iramuteq-like",
    "iramuteq-like/Rscripts",
    "iramuteq_clone_v3/Rscripts"
  ))
  candidats <- candidats[!is.na(candidats) & nzchar(candidats)]

  for (cand in candidats) {
    paths <- vapply(scripts, function(sc) .trouver_fichier_insensible_casse(cand, sc), FUN.VALUE = character(1))
    if (all(!is.na(paths))) {
      return(unname(paths))
    }
  }

  stop(
    "CHD IRaMuTeQ-like: scripts R introuvables. Répertoires testés: ",
    paste(candidats, collapse = ", "),
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
  binariser = TRUE,
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
                                              orientation = c("vertical", "horizontal"),
                                              display_method = c("standard", "compact", "iramuteq_blocks")) {
  orientation <- match.arg(orientation)
  display_method <- match.arg(display_method)

  if (is.null(chd_obj)) {
    plot.new()
    text(0.5, 0.5, "Dendrogramme CHD indisponible.", cex = 1.1)
    return(invisible(NULL))
  }

  n1 <- .normaliser_n1_chd(chd_obj$n1)
  if (is.null(chd_obj$list_fille) || is.null(n1)) {
    plot.new()
    text(0.5, 0.5, "Dendrogramme CHD indisponible.", cex = 1.1)
    return(invisible(NULL))
  }

  list_fille <- chd_obj$list_fille
  if (!is.list(list_fille) || length(list_fille) == 0) {
    plot.new()
    text(0.5, 0.5, "Dendrogramme CHD indisponible (list_fille vide).", cex = 1.1)
    return(invisible(NULL))
  }

  noms <- names(list_fille)
  if (is.null(noms) || any(!nzchar(noms))) noms <- as.character(seq_along(list_fille))
  map_filles <- stats::setNames(lapply(list_fille, function(x) as.integer(x)), noms)

  meres <- suppressWarnings(as.integer(names(map_filles)))
  meres <- meres[is.finite(meres)]
  enfants <- unique(as.integer(unlist(map_filles, use.names = FALSE)))
  enfants <- enfants[is.finite(enfants)]

  racines <- setdiff(meres, enfants)
  racine <- if (length(racines)) racines[[1]] else if (length(meres)) meres[[1]] else NA_integer_
  if (!is.finite(racine)) {
    feuilles_n1 <- suppressWarnings(as.integer(n1[, ncol(n1)]))
    feuilles_n1 <- feuilles_n1[is.finite(feuilles_n1)]
    if (length(feuilles_n1)) racine <- min(feuilles_n1, na.rm = TRUE)
  }

  if (!is.finite(racine)) {
    plot.new()
    text(0.5, 0.5, "Structure CHD invalide.", cex = 1.1)
    return(invisible(NULL))
  }

  get_filles <- function(node) {
    key <- as.character(node)
    x <- map_filles[[key]]
    x <- x[is.finite(x)]
    if (is.null(x)) integer(0) else as.integer(x)
  }

  terminales <- suppressWarnings(as.integer(terminales))
  terminales <- terminales[is.finite(terminales)]
  terminales <- unique(terminales)

  # Source de vérité pour le nombre de classes à afficher :
  # 1) résultats statistiques CHD (si disponibles), sinon
  # 2) vecteur des classes finales documentaires.
  # Comme dans iramuteq_clone_v3 (cutree(..., k = clnb)), on borne l'affichage
  # aux classes finales réellement exploitées.
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

  if (length(classes_utiles) && length(terminales)) {
    idx_valides <- classes_utiles[classes_utiles >= 1L & classes_utiles <= length(terminales)]
    terminales <- terminales[idx_valides]
    classes_utiles <- idx_valides
  }

  utiliser_terminales <- length(terminales) > 0

  leaves <- integer(0)
  visited <- integer(0)
  walk_leaves <- function(node) {
    if (node %in% visited) return(invisible(NULL))
    visited <<- c(visited, node)

    if (isTRUE(utiliser_terminales) && node %in% terminales) {
      leaves <<- c(leaves, node)
      return(invisible(NULL))
    }

    filles <- get_filles(node)
    if (!length(filles)) {
      if (!isTRUE(utiliser_terminales)) {
        leaves <<- c(leaves, node)
      }
      return(invisible(NULL))
    }
    for (f in filles) walk_leaves(f)
  }
  walk_leaves(racine)

  if (isTRUE(utiliser_terminales)) {
    terminales_atteintes <- intersect(unique(terminales), unique(visited))
    leaves <- unique(c(leaves, terminales_atteintes))
  }

  if (!length(leaves) && !length(classes_utiles)) {
    leaves <- sort(unique(suppressWarnings(as.integer(n1[, ncol(n1)]))))
    leaves <- leaves[is.finite(leaves)]
  }
  if (!length(leaves)) {
    plot.new()
    text(0.5, 0.5, "Aucune feuille exploitable pour le dendrogramme.", cex = 1.1)
    return(invisible(NULL))
  }

  leaves <- unique(leaves)
  y_map <- stats::setNames(seq_along(leaves), as.character(leaves))
  pos <- list()
  seen <- integer(0)

  layout_phylo <- function(node, depth = 0L) {
    if (node %in% seen) return(pos[[as.character(node)]])
    seen <<- c(seen, node)

    if (isTRUE(utiliser_terminales) && node %in% leaves) {
      y <- unname(y_map[[as.character(node)]])
      if (is.null(y) || !is.finite(y)) y <- max(unname(y_map)) + 1
      pos[[as.character(node)]] <<- c(x = depth, y = y)
      return(pos[[as.character(node)]])
    }

    filles <- get_filles(node)
    if (!length(filles)) {
      y <- unname(y_map[[as.character(node)]])
      if (is.null(y) || !is.finite(y)) y <- max(unname(y_map)) + 1
      pos[[as.character(node)]] <<- c(x = depth, y = y)
      return(pos[[as.character(node)]])
    }

    child_pos <- lapply(filles, function(f) layout_phylo(f, depth + 1L))
    ys <- vapply(child_pos, function(v) as.numeric(v[["y"]]), numeric(1))
    pos[[as.character(node)]] <<- c(x = depth, y = mean(ys))
    return(pos[[as.character(node)]])
  }

  layout_phylo(racine, 0L)

  if (!length(pos)) {
    plot.new()
    text(0.5, 0.5, "Dendrogramme CHD indisponible (positions vides).", cex = 1.1)
    return(invisible(NULL))
  }

  all_pos <- do.call(rbind, pos)
  if (is.null(dim(all_pos))) {
    all_pos <- matrix(all_pos, nrow = 1L, dimnames = list(names(pos)[1], names(all_pos)))
  }
  all_pos <- as.matrix(all_pos)
  if (is.null(colnames(all_pos)) || !all(c("x", "y") %in% colnames(all_pos))) {
    plot.new()
    text(0.5, 0.5, "Dendrogramme CHD indisponible (positions invalides).", cex = 1.1)
    return(invisible(NULL))
  }
  depth_max <- max(all_pos[, "x"], na.rm = TRUE)
  order_max <- max(all_pos[, "y"], na.rm = TRUE)

  node_ids <- suppressWarnings(as.integer(rownames(all_pos)))
  node_ids[!is.finite(node_ids)] <- NA_integer_
  tip_idx <- which(node_ids %in% leaves)

  tip_cols <- rep("#5B8FF9", nrow(all_pos))
  if (length(terminales)) tip_cols[which(node_ids %in% terminales)] <- "#d62728"

  tip_labels <- paste0("Classe ", rownames(all_pos)[tip_idx])
  tip_nodes_chr <- rownames(all_pos)[tip_idx]
  classe_par_noeud <- stats::setNames(rep(NA_integer_, length(tip_nodes_chr)), tip_nodes_chr)

  if (length(terminales)) {
    for (i in seq_along(terminales)) {
      node <- terminales[[i]]
      idx_node <- which(tip_nodes_chr == as.character(node))
      if (!length(idx_node)) next
      tip_labels[idx_node] <- paste0("Classe ", i)
      classe_par_noeud[as.character(node)] <- i
    }
  }

  if (!is.null(classes)) {
    classes <- suppressWarnings(as.integer(classes))
    classes <- classes[is.finite(classes) & classes > 0]
    if (length(classes) && length(terminales)) {
      pct_par_classe <- prop.table(table(classes)) * 100
      for (i in seq_along(terminales)) {
        node <- terminales[[i]]
        idx_node <- which(tip_nodes_chr == as.character(node))
        if (!length(idx_node)) next
        pct <- unname(pct_par_classe[as.character(i)])
        if (!is.finite(pct) || is.na(pct)) pct <- 0
        tip_labels[idx_node] <- paste0("Classe ", i, " (", format(round(pct, 1), nsmall = 1), " %)")
      }
    }
  }

  top_n_terms <- suppressWarnings(as.integer(top_n_terms))
  if (!is.finite(top_n_terms) || is.na(top_n_terms) || top_n_terms < 1L) top_n_terms <- 1L
  termes_par_classe <- list()
  if (!is.null(res_stats_df) && is.data.frame(res_stats_df) && nrow(res_stats_df) > 0 && all(c("Classe", "Terme") %in% names(res_stats_df))) {
    df_terms <- res_stats_df
    classes_num <- suppressWarnings(as.integer(df_terms$Classe))
    df_terms <- df_terms[is.finite(classes_num) & !is.na(df_terms$Terme) & nzchar(as.character(df_terms$Terme)), , drop = FALSE]
    df_terms$Classe <- suppressWarnings(as.integer(df_terms$Classe))

    for (i in seq_along(terminales)) {
      sous <- df_terms[df_terms$Classe == i, , drop = FALSE]
      if (!nrow(sous)) next
      if ("chi2" %in% names(sous)) {
        chi <- suppressWarnings(as.numeric(sous$chi2))
        chi[!is.finite(chi)] <- -Inf
        sous <- sous[order(chi, decreasing = TRUE), , drop = FALSE]
      }
      termes <- unique(as.character(sous$Terme))
      termes <- termes[nzchar(termes)]
      if (length(termes)) {
        termes_par_classe[[as.character(i)]] <- paste(utils::head(termes, top_n_terms), collapse = ", ")
      }
    }
  }

  if (identical(orientation, "vertical")) {
    if (identical(display_method, "iramuteq_blocks")) {
      op <- par(no.readonly = TRUE)
      on.exit(par(op), add = TRUE)
      par(mar = c(1.4, 1.4, 2.6, 1.4), xpd = NA)

      class_ids <- sort(unique(na.omit(as.integer(classe_par_noeud))))
      if (!length(class_ids) && length(terminales)) class_ids <- seq_along(terminales)
      if (!length(class_ids)) {
        plot.new()
        text(0.5, 0.5, "Aucune classe terminale disponible.", cex = 1.1)
        return(invisible(NULL))
      }

      cols_palette <- c("#ff3300", "#00ff00", "#1f3bff", "#ff00a8", "#00d4ff", "#ffaa00")
      class_cols <- stats::setNames(cols_palette[(seq_along(class_ids) - 1L) %% length(cols_palette) + 1L], as.character(class_ids))

      pct_par_classe <- NULL
      if (!is.null(classes)) {
        classes <- suppressWarnings(as.integer(classes))
        classes <- classes[is.finite(classes) & classes > 0]
        if (length(classes)) pct_par_classe <- prop.table(table(classes)) * 100
      }

      order_nodes <- tip_nodes_chr
      if (!length(order_nodes) && length(terminales)) order_nodes <- as.character(terminales)
      classes_ord <- suppressWarnings(as.integer(classe_par_noeud[order_nodes]))
      keep <- is.finite(classes_ord)
      order_nodes <- order_nodes[keep]
      classes_ord <- classes_ord[keep]
      if (!length(classes_ord)) classes_ord <- class_ids

      n <- length(classes_ord)
      x_pos <- seq(0.12, 0.88, length.out = n)

      plot.new()
      plot.window(xlim = c(0, 1), ylim = c(0, 1))

      y_top <- 0.90
      y_box_top <- 0.78
      y_box_bot <- 0.64
      box_w <- min(0.28, if (n > 1) 0.72 / (n - 1) else 0.3)

      if (n >= 2) {
        mid <- mean(x_pos)
        segments(mid, y_top, min(x_pos), y_box_top + 0.02, lwd = 2.5, col = "#111111")
        segments(mid, y_top, max(x_pos), y_box_top + 0.02, lwd = 2.5, col = "#111111")
      }

      for (i in seq_len(n)) {
        cl <- classes_ord[i]
        col_cl <- unname(class_cols[as.character(cl)])
        if (is.na(col_cl) || !nzchar(col_cl)) col_cl <- "#1f3bff"
        x <- x_pos[i]

        rect(x - box_w / 2, y_box_bot, x + box_w / 2, y_box_top, col = col_cl, border = NA)
        segments(x, y_box_top + 0.02, x, y_box_top, lwd = 2.5, col = "#111111")

        text(x, y_box_top + 0.05, labels = paste0("classe ", cl), col = col_cl, cex = 1.2, font = 2)
        pct <- if (!is.null(pct_par_classe)) unname(pct_par_classe[as.character(cl)]) else NA_real_
        pct_lbl <- if (is.finite(pct)) paste0(format(round(pct, 1), trim = TRUE), " %") else ""
        if (nzchar(pct_lbl)) text(x, y_box_bot + 0.015, labels = pct_lbl, cex = 0.9, col = "#111111")

        termes <- character(0)
        if (!is.null(termes_par_classe[[as.character(cl)]])) {
          termes <- strsplit(termes_par_classe[[as.character(cl)]], "\\s*,\\s*")[[1]]
          termes <- termes[nzchar(termes)]
        }

        if (length(termes)) {
          y0 <- 0.58
          step <- 0.055
          for (j in seq_len(min(length(termes), 9L))) {
            text(x - box_w / 2, y0 - (j - 1) * step, labels = termes[j], adj = c(0, 1), col = col_cl, cex = 1.6, font = 2)
          }
        }
      }

      title(main = "Dendrogramme CHD IRaMuTeQ-like (style IRaMuTeQ)")
      return(invisible(NULL))
    }

    op <- par(no.readonly = TRUE)
    on.exit(par(op), add = TRUE)
    par(mar = c(3.2, 2.8, 3.6, 1.5))

    all_pos_plot <- cbind(x = all_pos[, "y", drop = TRUE], y = depth_max - all_pos[, "x", drop = TRUE])
    all_pos_plot <- as.matrix(all_pos_plot)
    x_max <- max(all_pos_plot[, "x"], na.rm = TRUE)
    y_max <- max(all_pos_plot[, "y"], na.rm = TRUE)

    plot(
      NA,
      xlim = c(0.5, x_max + 0.5),
      ylim = c(if (length(termes_par_classe) && identical(display_method, "standard")) -1.6 else -0.8, y_max + 0.8),
      axes = FALSE,
      xlab = "",
      ylab = "",
      main = if (identical(display_method, "compact")) "Dendrogramme CHD IRaMuTeQ-like (compact)" else "Dendrogramme CHD IRaMuTeQ-like"
    )

    for (mere_name in names(map_filles)) {
      mere <- suppressWarnings(as.integer(mere_name))
      if (!is.finite(mere)) next
      p_m <- pos[[as.character(mere)]]
      if (is.null(p_m)) next
      p_m_plot <- c(x = p_m[["y"]], y = depth_max - p_m[["x"]])

      filles <- as.integer(map_filles[[mere_name]])
      filles <- filles[is.finite(filles)]
      if (!length(filles)) next

      x_child <- numeric(0)
      for (f in filles) {
        p_f <- pos[[as.character(f)]]
        if (!is.null(p_f)) x_child <- c(x_child, p_f[["y"]])
      }
      if (length(x_child) >= 2) {
        segments(min(x_child), p_m_plot[["y"]], max(x_child), p_m_plot[["y"]], col = "#2f4f4f", lwd = 1.6)
      }

      for (f in filles) {
        p_f <- pos[[as.character(f)]]
        if (is.null(p_f)) next
        p_f_plot <- c(x = p_f[["y"]], y = depth_max - p_f[["x"]])
        segments(p_f_plot[["x"]], p_m_plot[["y"]], p_f_plot[["x"]], p_f_plot[["y"]], col = "#2f4f4f", lwd = 1.6)
      }
    }

    if (length(tip_idx)) {
      points(all_pos_plot[tip_idx, "x", drop = TRUE], all_pos_plot[tip_idx, "y", drop = TRUE], pch = 19, col = tip_cols[tip_idx], cex = 0.95)
      text(x = all_pos_plot[tip_idx, "x", drop = TRUE], y = all_pos_plot[tip_idx, "y", drop = TRUE] - 0.18, labels = tip_labels, adj = c(0.5, 1), cex = 0.78)

      if (length(termes_par_classe) && identical(display_method, "standard")) {
        for (j in seq_along(tip_idx)) {
          node_id <- tip_nodes_chr[j]
          class_id <- suppressWarnings(as.integer(classe_par_noeud[[node_id]]))
          if (!is.finite(class_id)) next
          termes_lbl <- termes_par_classe[[as.character(class_id)]]
          if (is.null(termes_lbl) || !nzchar(termes_lbl)) next
          text(
            x = all_pos_plot[tip_idx[j], "x", drop = TRUE],
            y = all_pos_plot[tip_idx[j], "y", drop = TRUE] - 0.56,
            labels = termes_lbl,
            adj = c(0.5, 1),
            cex = 0.66,
            col = "#333333"
          )
        }
      }

      if (length(termes_par_classe) && identical(display_method, "compact")) {
        legend_lines <- unlist(lapply(sort(names(termes_par_classe)), function(cl_id) {
          paste0("Classe ", cl_id, " : ", termes_par_classe[[cl_id]])
        }), use.names = FALSE)

        if (length(legend_lines)) {
          texte_legend <- paste(legend_lines, collapse = "\n")
          mtext(texte_legend, side = 1, line = 0.4, adj = 0, cex = 0.62, col = "#333333")
        }
      }
    }
  } else {
    op <- par(no.readonly = TRUE)
    on.exit(par(op), add = TRUE)
    par(mar = c(3.0, 2.6, 3.4, 8.5))

    plot(
      NA,
      xlim = c(-0.2, depth_max + 1.4),
      ylim = c(order_max + 0.6, 0.4),
      axes = FALSE,
      xlab = "",
      ylab = "",
      main = if (identical(display_method, "compact")) "Dendrogramme CHD IRaMuTeQ-like (phylogramme compact)" else "Dendrogramme CHD IRaMuTeQ-like (phylogramme)"
    )

    for (mere_name in names(map_filles)) {
      mere <- suppressWarnings(as.integer(mere_name))
      if (!is.finite(mere)) next
      p_m <- pos[[as.character(mere)]]
      if (is.null(p_m)) next

      filles <- as.integer(map_filles[[mere_name]])
      filles <- filles[is.finite(filles)]
      if (!length(filles)) next

      y_child <- numeric(0)
      for (f in filles) {
        p_f <- pos[[as.character(f)]]
        if (!is.null(p_f)) y_child <- c(y_child, p_f[["y"]])
      }
      if (length(y_child) >= 2) {
        segments(p_m[["x"]], min(y_child), p_m[["x"]], max(y_child), col = "#2f4f4f", lwd = 1.6)
      }

      for (f in filles) {
        p_f <- pos[[as.character(f)]]
        if (is.null(p_f)) next
        segments(p_m[["x"]], p_f[["y"]], p_f[["x"]], p_f[["y"]], col = "#2f4f4f", lwd = 1.6)
      }
    }

    if (length(tip_idx)) {
      points(all_pos[tip_idx, "x", drop = TRUE], all_pos[tip_idx, "y", drop = TRUE], pch = 19, col = tip_cols[tip_idx], cex = 0.95)
      text(x = all_pos[tip_idx, "x", drop = TRUE] + 0.12, y = all_pos[tip_idx, "y", drop = TRUE], labels = tip_labels, adj = c(0, 0.5), cex = 0.78)

      if (length(termes_par_classe) && identical(display_method, "standard")) {
        for (j in seq_along(tip_idx)) {
          node_id <- tip_nodes_chr[j]
          class_id <- suppressWarnings(as.integer(classe_par_noeud[[node_id]]))
          if (!is.finite(class_id)) next
          termes_lbl <- termes_par_classe[[as.character(class_id)]]
          if (is.null(termes_lbl) || !nzchar(termes_lbl)) next
          text(
            x = all_pos[tip_idx[j], "x", drop = TRUE] + 0.12,
            y = all_pos[tip_idx[j], "y", drop = TRUE] + 0.28,
            labels = termes_lbl,
            adj = c(0, 0.5),
            cex = 0.66,
            col = "#333333"
          )
        }
      }

      if (length(termes_par_classe) && identical(display_method, "compact")) {
        legend_lines <- unlist(lapply(sort(names(termes_par_classe)), function(cl_id) {
          paste0("Classe ", cl_id, " : ", termes_par_classe[[cl_id]])
        }), use.names = FALSE)

        if (length(legend_lines)) {
          legend(
            "bottomright",
            legend = legend_lines,
            bty = "n",
            cex = 0.64,
            text.col = "#333333",
            inset = c(-0.02, -0.02),
            xpd = NA
          )
        }
      }
    }
  }

  invisible(NULL)
}
