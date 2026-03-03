# Rôle du fichier: nettoyage_iramuteq.R isole la préparation texte du mode IRaMuTeQ-like.
# Cette logique est volontairement séparée de Rainette car les conventions de préparation
# ne sont pas identiques (script textprepa Python et dictionnaire lexique_fr imposé).

appliquer_nettoyage_iramuteq <- function(textes,
                                         activer_nettoyage = FALSE,
                                         forcer_minuscules = FALSE,
                                         supprimer_chiffres = FALSE,
                                         supprimer_apostrophes = FALSE) {
  x <- as.character(textes)
  if (length(x) == 0) return(character(0))

  x <- gsub("\u00A0", " ", x, fixed = TRUE)

  if (isTRUE(supprimer_chiffres)) {
    x <- gsub("[0-9]+", " ", x, perl = TRUE)
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
