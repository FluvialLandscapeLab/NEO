NEO_Execute = function(model) {
  
  NEO_BuildModel(model)
  
  allPhases = as.list(model$phases)
  phaseOrders = sapply(allPhases, function(phase) phase$order)
  initializePhase = allPhases[phaseOrders == 0]

  executePhases = allPhases[phaseOrders > 0]
  executePhaseOrders = sapply(executePhases, function(phase) phase$order)
  executePhases = executePhases[match(sort(executePhaseOrders), executePhaseOrders)]
  
  if(length(initializePhase) != 1) stop("One (and only one) calculation phase must be defined as the initializaion phase (with 'order' = 0); ", length(initializePhase), " calculation phases have order = 0.")

  NEO_ExecutePhases(initializePhase, model)
  NEO_ExecutePhases(executePhases, model)
  
  invisible(model)
}


NEO_ExecutePhases = function(listOfNEO_Phases, model) {
  lapply(
    listOfNEO_Phases,
    NEO_ExecutePhase,
    model
  )
} 

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

NEO_CalculateXam = function(xam, model) {
  NEO_HolonAttr(model, attr(xam, "targetAttr"), xam$holons$myHolons) = 
    do.call(
      attr(xam, "calculation")$calculation,
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