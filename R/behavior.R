#' @export
NEO_Behavior = function(behaviorName, model, statamNames = character(0), dynamNames = character(0)) {
  # Check to be sure the statamNames and dynamNames exist
  invalidStatumNames = !(statamNames %in% ls(model$statams))
  invalidDynamNames = !(dynamNames %in% ls(model$dynams))
  allNames = c(statamNames, dynamNames)
  invalidNames = c(invalidStatumNames, invalidDynamNames)
  if(any(invalidNames)) stop("The following NEO_Statums or NEO_Dynams were rquested by NEO_Behavior '", behaviorName, "' but do not exist: ", paste0(allNames[invalidNames], collapse = ", "))

  # Create the behavior as a new NEO_Environment
  newBehavior = NEO_Environment(behaviorName, "NEO_Behavior", model$behaviors)

  # Add the lists of xams
  newBehavior$myDynams = as.list(model$dynams)[dynamNames]
  newBehavior$myStatams = as.list(model$statams)[statamNames]

  invisible(newBehavior)
}
