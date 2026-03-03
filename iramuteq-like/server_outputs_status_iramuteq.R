# Rôle du fichier: server_outputs_status_iramuteq.R centralise les sorties de statut global.
# Ce module évite un crash de session si l'enregistrement des sorties n'est pas disponible.

register_outputs_status <- function(input, output, session, rv) {
  output$statut <- renderText({
    statut <- rv$statut
    if (is.null(statut) || !nzchar(statut)) {
      return("En attente.")
    }
    statut
  })

  output$logs <- renderText({
    logs <- rv$logs
    if (is.null(logs) || !nzchar(logs)) {
      return("Aucun événement pour le moment.")
    }
    logs
  })
}
