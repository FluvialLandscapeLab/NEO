#' @export
NEO_Phase = function(phaseName, model, order, dynamNames) {

  newPhase = NEO_Environment(phaseName, model$phases, "NEO_Context")

  newPhase$order = order
  newPhase$myDynams = as.list(model$dynams)[dynamNames]

  invisible(newPhase)
}
