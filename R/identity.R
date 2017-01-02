#' @export
NEO_Identity = function(identityName, model, behaviorNames) {
  invalidBehaviors = !(behaviorNames %in% ls(model$behaviors))
  if(any(invalidBehaviors)) stop("The following behaviors were requested by NEO_Identity '", identityName, "' but do not exist: ", paste0(behaviorNames[invalidBehaviors], collapse = ", "))
  
  newIdentity = NEO_Environment(identityName, model$identities, "NEO_Identity")

  newIdentity$myBehaviors = as.list(model$behaviors)[behaviorNames]

  invisible(newIdentity)
}
