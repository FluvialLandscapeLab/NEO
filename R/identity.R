#' @export
NEO_Identity = function(identityName, model, behaviorList) {
  newIdentity = NEO_Environment(identityName, model$identities, "NEO_Identity")

  newIdentity$myBehaviors = behaviorList

  invisible(newIdentity)
}
