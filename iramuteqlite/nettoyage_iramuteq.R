# Rôle du fichier: nettoyage_iramuteq.R isole la préparation texte du mode IRaMuTeQ-like.
# Cette logique est volontairement séparée d'un ancien moteur externe car les conventions de préparation
# ne sont pas identiques (script textprepa Python et dictionnaire lexique_fr imposé).

appliquer_nettoyage_iramuteq <- function(textes,
                                         activer_nettoyage = FALSE,
                                         forcer_minuscules = FALSE,
                                         supprimer_chiffres = FALSE,
                                         supprimer_apostrophes = FALSE,
                                         remplacer_tirets_espaces = FALSE) {
  x <- as.character(textes)
  if (length(x) == 0) return(character(0))

  x <- gsub("\u00A0", " ", x, fixed = TRUE)

  if (isTRUE(supprimer_chiffres)) {
    x <- gsub("[0-9]+", " ", x, perl = TRUE)
  }

  if (isTRUE(remplacer_tirets_espaces)) {
    x <- gsub("-", " ", x, fixed = TRUE)
  }

  if (isTRUE(supprimer_apostrophes)) {
    x <- gsub("(?i)\\b(?:[cdjlmnst]|qu)['’`´ʼʹ](?=\\p{L})", "", x, perl = TRUE)
  }

  if (isTRUE(activer_nettoyage)) {
    regex_autorises <- "a-zA-Z0-9àÀâÂäÄáÁåÅãéÉèÈêÊëËìÌîÎïÏíÍóÓòÒôÔöÖõÕøØùÙûÛüÜúÚçÇßœŒ’ñÑ\\.:,;!\\?'"
    regex_a_supprimer <- paste0("[^", regex_autorises, "]")
    x <- gsub(regex_a_supprimer, " ", x, perl = TRUE)
  }

  x <- gsub("\\s+", " ", x, perl = TRUE)
  x <- trimws(x)

  if (isTRUE(forcer_minuscules)) {
    x <- tolower(x)
  }

  x
}

# Supprime les documents vides d'un DFM (somme de ligne nulle) et aligne
# optionnellement les objets corpus/tokens selon le même masque.
supprimer_docs_vides_dfm <- function(dfm_obj,
                                     filtered_corpus = NULL,
                                     tok = NULL,
                                     logger = NULL) {
  if (is.null(dfm_obj)) {
    stop("supprimer_docs_vides_dfm: dfm_obj manquant.")
  }

  sommes_docs <- Matrix::rowSums(dfm_obj)
  idx_non_vides <- !is.na(sommes_docs) & (sommes_docs > 0)

  if (!any(idx_non_vides)) {
    stop("Le DFM ne contient aucun segment non vide après prétraitement.")
  }

  nb_vides <- sum(!idx_non_vides)
  if (nb_vides > 0 && is.function(logger)) {
    logger(paste0("Segments vides supprimés du DFM : ", nb_vides, "."))
  }

  list(
    dfm_obj = dfm_obj[idx_non_vides, ],
    filtered_corpus = if (is.null(filtered_corpus)) NULL else filtered_corpus[idx_non_vides],
    tok = if (is.null(tok)) NULL else tok[idx_non_vides],
    idx_non_vides = idx_non_vides,
    nb_vides = nb_vides
  )
}
