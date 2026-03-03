# Rôle du fichier: nlp_lexique.R porte une partie du pipeline d'analyse Rainette.
# Ce script centralise une responsabilité métier/technique utilisée par l'application.
# Il facilite la maintenance en explicitant le périmètre et les points d'intégration.
# Module NLP - lemmatisation via lexique externe (dictionnaires/lexique_fr.csv)
# Ce module charge un lexique 3 colonnes au format canonique
# (c_mot, c_lemme, c_morpho).
# et applique une lemmatisation explicite sans fallback silencieux vers spaCy.

charger_lexique_fr <- function(chemin = "dictionnaires/lexique_fr.csv") {
  fichier <- tryCatch(normalizePath(chemin, mustWork = TRUE), error = function(e) NA_character_)
  if (is.na(fichier) || !file.exists(fichier)) {
    stop(
      paste0(
        "Lexique (fr) introuvable : fichier '", chemin,
        "' absent. Ajoute dictionnaires/lexique_fr.csv (format lexique_fr ou IRaMuTeQ)."
      )
    )
  }

  lire_tsv <- function() {
    read.delim(
      fichier,
      sep = "\t",
      header = TRUE,
      stringsAsFactors = FALSE,
      fileEncoding = "UTF-8",
      check.names = FALSE
    )
  }

  lire_csv <- function() {
    read.csv(
      fichier,
      sep = ",",
      header = TRUE,
      stringsAsFactors = FALSE,
      fileEncoding = "UTF-8",
      check.names = FALSE
    )
  }

  lire_csv_point_virgule <- function() {
    read.csv(
      fichier,
      sep = ";",
      header = TRUE,
      stringsAsFactors = FALSE,
      fileEncoding = "UTF-8",
      check.names = FALSE
    )
  }

  lexique <- tryCatch(lire_tsv(), error = function(e) NULL)
  if (is.null(lexique) || ncol(lexique) <= 1) {
    lexique <- tryCatch(lire_csv(), error = function(e) NULL)
  }
  if (is.null(lexique) || ncol(lexique) <= 1) {
    lexique <- tryCatch(lire_csv_point_virgule(), error = function(e) NULL)
  }

  if (is.null(lexique) || nrow(lexique) == 0) {
    stop("Lexique (fr) invalide : fichier vide ou illisible.")
  }

  names(lexique) <- trimws(sub("^\ufeff", "", names(lexique)))

  colonnes_attendues <- c("c_mot", "c_lemme", "c_morpho")
  manquantes <- setdiff(colonnes_attendues, names(lexique))
  if (length(manquantes) > 0) {
    stop(
      paste0(
        "Lexique (fr) mal configuré : colonnes manquantes [",
        paste(manquantes, collapse = ", "),
        "]. Format attendu : c_mot, c_lemme, c_morpho."
      )
    )
  }

  lexique$c_mot <- tolower(trimws(as.character(lexique$c_mot)))
  lexique$c_lemme <- tolower(trimws(as.character(lexique$c_lemme)))
  lexique$c_morpho <- toupper(trimws(as.character(lexique$c_morpho)))

  lexique <- lexique[
    nzchar(lexique$c_mot) &
      nzchar(lexique$c_lemme) &
      nzchar(lexique$c_morpho),
    ,
    drop = FALSE
  ]

  if (nrow(lexique) == 0) {
    stop("Lexique (fr) mal configuré : aucune entrée exploitable après nettoyage.")
  }

  lexique
}

construire_type_lexique_fr <- function(termes, lexique) {
  if (is.null(termes)) return(character(0))
  x <- tolower(trimws(as.character(termes)))
  x[is.na(x)] <- ""

  if (is.null(lexique) || !is.data.frame(lexique) || nrow(lexique) < 1) {
    return(rep("", length(x)))
  }

  cols_ok <- all(c("c_mot", "c_lemme", "c_morpho") %in% names(lexique))
  if (!cols_ok) {
    return(rep("", length(x)))
  }

  key_mot <- tolower(trimws(as.character(lexique$c_mot)))
  key_lemme <- tolower(trimws(as.character(lexique$c_lemme)))
  val_type <- tolower(trimws(as.character(lexique$c_morpho)))

  map_lemme <- tapply(val_type, key_lemme, function(v) unique(v)[1])
  map_mot <- tapply(val_type, key_mot, function(v) unique(v)[1])

  out <- unname(map_lemme[x])
  idx_na <- is.na(out) | !nzchar(out)
  if (any(idx_na)) {
    out[idx_na] <- unname(map_mot[x[idx_na]])
  }

  out[is.na(out)] <- ""
  out
}


lemmatiser_textes_lexique <- function(textes, lexique, rv = NULL) {
  tok <- quanteda::tokens(
    textes,
    remove_punct = FALSE,
    remove_numbers = FALSE
  )

  map_forme_lemme <- tapply(
    lexique$c_lemme,
    lexique$c_mot,
    function(x) unique(x)[1]
  )

  liste_tok <- as.list(tok)
  textes_lem <- vapply(liste_tok, function(v) {
    if (length(v) == 0) return("")
    v_low <- tolower(as.character(v))
    lem <- unname(map_forme_lemme[v_low])
    lem[is.na(lem) | !nzchar(lem)] <- v_low[is.na(lem) | !nzchar(lem)]
    paste(lem, collapse = " ")
  }, FUN.VALUE = character(1))

  if (!is.null(rv)) {
    ajouter_log(rv, "Lexique (fr) : lemmatisation forme->lemme appliquée sans spaCy.")
  }

  textes_lem
}



filtrer_textes_lexique_par_cgram <- function(textes, lexique, cgram_a_conserver, rv = NULL) {
  cgram_keep <- unique(toupper(trimws(as.character(cgram_a_conserver))))
  cgram_keep <- cgram_keep[nzchar(cgram_keep)]
  if (length(cgram_keep) == 0) return(textes)

  formes_keep <- unique(lexique$c_mot[lexique$c_morpho %in% cgram_keep])
  formes_keep <- formes_keep[nzchar(formes_keep)]

  tok <- quanteda::tokens(
    textes,
    remove_punct = FALSE,
    remove_numbers = FALSE
  )

  liste_tok <- as.list(tok)
  total_tokens <- 0L
  total_conserves <- 0L

  textes_filtres <- vapply(liste_tok, function(v) {
    if (length(v) == 0) return("")
    v_low <- tolower(trimws(as.character(v)))
    total_tokens <<- total_tokens + length(v_low)
    garder <- v_low %in% formes_keep
    total_conserves <<- total_conserves + sum(garder)
    paste(v_low[garder], collapse = " ")
  }, FUN.VALUE = character(1))

  if (!is.null(rv)) {
    ajouter_log(
      rv,
      paste0(
        "Lexique (fr) : filtrage c_morpho [", paste(cgram_keep, collapse = ", "),
        "] => ", total_conserves, "/", total_tokens, " token(s) conservé(s)."
      )
    )
  }

  names(textes_filtres) <- names(textes)
  textes_filtres
}
