#' @export
NEO_Phase = function(phaseName, model, order, dynamNames = NULL) {
  
  if(order != 0 & is.null(dynamNames)) stop("When calling NEO_Phase() function, dynamNames argument must not be NULL if order argument > 0")

  if(phaseName %in% ls(model$phases)) stop("Duplicate phase name.  Attempt to define NEO_Phase '", phaseName, "' more then once.")

  newPhase = NEO_Environment(phaseName, model$phases, "NEO_Phase")

  newPhase$order = order
  if(order == 0) {
    newPhase$myStatams = as.list(model$statams)
  } else {
    newPhase$myDynams = as.list(model$dynams)[dynamNames]
  }
  
  invisible(newPhase)
}
