#' @export
NEO_Behavior = function(behaviorName, model, statamList, dynamList) {
  newBehavior = NEO_Environment(behaviorName, model$behaviors, "NEO_Behavior")

  newBehavior$myDynams = dynamList
  newBehavior$myStatams = statamList

  invisible(newBehavior)
}
