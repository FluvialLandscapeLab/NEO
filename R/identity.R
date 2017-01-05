#' @export
NEO_Identity = function(identityName, model, behaviorNames) {
  
  # Check to be sure the requested behaviors exist.
  invalidBehaviors = !(behaviorNames %in% ls(model$behaviors))
  if(any(invalidBehaviors)) stop("The following behaviors were requested by NEO_Identity '", identityName, "' but do not exist: ", paste0(behaviorNames[invalidBehaviors], collapse = ", "))
  
  # Create the new identity
  newIdentity = NEO_Environment(identityName, "NEO_Identity", model$identities)

  newIdentity$myBehaviors = as.list(model$behaviors)[behaviorNames]

  invisible(newIdentity)
}
