NEO_Behavior = function(behaviorName, model, statimList, dynamList) {
  newBehavior = NEO_Environment(behaviorName, model$behaviors, "NEO_Behavior")

  newBehavior$myDynams = dynamList
  newBehavior$myStatims = statimList

  invisible(newBehavior)
}
