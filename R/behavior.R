#' @export
NEO_Behavior = function(behaviorName, model, statamNames = character(0), dynamNames = character(0)) {
  invalidStatumNames = !(statamNames %in% ls(model$statams))
  invalidDynamNames = !(dynamNames %in% ls(model$dynams))
  allNames = c(statamNames, dynamNames)
  invalidNames = c(invalidStatumNames, invalidDynamNames)
  if(any(invalidNames)) stop("The following NEO_Statums or NEO_Dynams were requireds by NEO_Behavior '", behaviorName, "' but do not exist: ", paste0(allNames[invalidNames], collapse = ", "))

  newBehavior = NEO_Environment(behaviorName, model$behaviors, "NEO_Behavior")

  newBehavior$myDynams = as.list(model$dynams)[dynamNames]
  newBehavior$myStatams = as.list(model$statams)[statamNames]

  invisible(newBehavior)
}
