# NEO_Execute builds and executes the model
NEO_Execute = function(model) {
  
  # Interpret contexts to install holon reference lists in each xam, install
  # myDynam and myStatam lists in each identity, and determine dependencies and,
  # accordingly, reorder the myDynams and myStatams lists in each xam.
  NEO_BuildModel(model)
  
  # Make two lists -- one contains the initialize phase and the other contains
  # the other phases in the model
  allPhases = as.list(model$phases)
  phaseOrders = sapply(allPhases, function(phase) phase$order)
  initializePhase = allPhases[phaseOrders == 0]

  executePhases = allPhases[phaseOrders > 0]

  # reorder the calculation phase list so that it reflects the execution phase order
  executePhaseOrders = sapply(executePhases, function(phase) phase$order)
  executePhases = executePhases[match(sort(executePhaseOrders), executePhaseOrders)]
  
  if(length(initializePhase) != 1) stop("One (and only one) calculation phase must be defined as the initialization phase (with 'order' = 0); ", length(initializePhase), " calculation phases have order = 0.")

  # Initialize the model
  NEO_ExecutePhases(initializePhase, model)
  
  # Run the model
  NEO_ExecutePhases(executePhases, model)
  
  invisible(model)
}

# Call NEO_ExecutePhase() for each phase is a list of phases.
NEO_ExecutePhases = function(listOfNEO_Phases, model) {
  lapply(
    listOfNEO_Phases,
    NEO_ExecutePhase,
    model
  )
} 

# Call CalculateXam for each xam in a phase
NEO_ExecutePhase = function(phase, model) {
  if(phase$order == 0) {
    xamCollectionName = "myStatams"
  } else {
    xamCollectionName = "myDynams"
  }
  lapply(
    phase[[xamCollectionName]],
    NEO_CalculateXam,
    model = model
  )
}

# "Holy indirection, Batman!"  Call the "calculation" function of the calculate 
# object in the attributes of each xam, using argements delineated by the
# "calcHolonCollections" and "calcHolonsAttrs" attributes of the xam.
NEO_CalculateXam = function(xam, model) {
  NEO_HolonAttr(model, attr(xam, "targetAttr"), xam$holons$myHolons) =
    # do.call calls a function using function arguments stored in a list.
    do.call(
      attr(xam, "calculation")$calculation,
      # this mapply gets the values of the holonAttributes associated with a 
      # holon reference vector or list of vectors for each xam.  The holon 
      # references come from interpreting the contexts installed in the xam. The
      # names of the holon references are stored in the "calcHolonCollection" 
      # object of the xam, and the name of the holonAttribues are stored in the 
      # "calcHolonAttr" attribute of the xam.  The NEO_HolonAttr function can 
      # receive either a vector of holon references, or a list of vectors of 
      # holon references and returns holon attribute values in the same form. 
      # Calculation functions can expect either a vector of holon attributes or
      # a list of vectors for each function argument.
      mapply(
        function(calcHolonCollectionName, attributeName) {
          NEO_HolonAttr(model, attributeName, xam$holons[[calcHolonCollectionName]])
        },
        attr(xam, "calcHolonCollections"),
        attr(xam, "calcHolonAttrs"),
        SIMPLIFY = F
      )
    )
}