#' @export
NEO_Phase = function(phaseName, model, order, dynamNames = NULL) {

  badOrderMessage = "'order' arguement to NEO_Phase() must be an positive interger of length() == 1."
  if(as.integer(order) != order || length(order) != 1) stop(badOrderMessage)  
  if(order < 0) stop(badOrderMessage)
  
  #Initialize phase (order == 0) has no dynams.  But no point in creating a calculate phase (order > 0) if there are no dynams associated with it.
  if(order != 0 & is.null(dynamNames)) stop("When calling NEO_Phase() function, dynamNames argument must not be NULL if order argument > 0")

  # do some basic error checking...
  # make a vector of the orders of phases that were already installed.
  installedOrders = sapply(as.list(model$phases), "[[", "order")
  if(order == 0) {
    # if installing initialization phase...
    # make sure there is only one initialization phase.
    if(any(installedOrders == 0)) stop("Attempt to define two initialization phases (where order = 0) in NEO_Model '", attr(model, "name"), "'.")
  } else {
    # otherwise...
    # make sure there are no duplicates order values
    if(order %in% installedOrders) stop("The 'order' arguement requested for NEO_Phase '", phaseName, "' has already been used by another installed NEO_Phase.")
    # make sure that the requested dynames aren't already installed in a different phase...
    alreadyInstalled = dynamNames %in% unlist(sapply(as.list(model$phases), "[[", "dynams"))
    if(any(alreadyInstalled)) stop("When creating NEO_Phase '", phaseName, ", the dynam(s) (", dynamNames[alreadyInstalled], ") were already assigned to a different order.")
  }

  newPhase = NEO_Environment(phaseName, "NEO_Phase", model$phases)

    # install all statams if we are defining the initializaton phase, otherwise install the requested dynams.
  if(order == 0) {
    newPhase$myStatams = as.list(model$statams)
  } else {
    newPhase$myDynams = as.list(model$dynams)[dynamNames]
  }
  newPhase$order = order

  invisible(newPhase)
}
