NEO_Phase = function(phaseName, model, order, dynamList) {

  newPhase = NEO_Environment(phaseName, model$phases, "NEO_Context")

  newPhase$order = order
  newPhase$myDynams = dynamList

  invisible(newPhase)
}
