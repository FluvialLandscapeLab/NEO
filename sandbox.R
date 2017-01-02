#To set dependencies, will have to be able to look up the list of dynams that
#calculate each value referenced.  Any dynam in the same calculation step will
#be a dependency.  This means that I may have to create an attribute that

#To set dependencies
# 1) for each referenced value...
# 2) in holon, look at behavior for the value
# 3) in behavior, find at dynam for the value
# 4) get unique list of dynams
# 5) eliminate any dynams that are not in the same calculation set.
# 6) remainder are dependencies.

library(igraph)

doit = function() {

  helton = NEO_Model("helton")

  NEO_Calculation(
    "accumulate",
    helton,
    function(storage, gains, losses) {
      storage + sapply(gains, sum) - sapply(losses, sum)
    }
  )

  NEO_Calculation(
    "echoValue",
    helton,
    function(value) {
      return(value)
    }
  )

  NEO_Calculation(
    "multiply",
    helton,
    function(operand1, operand2) {
      operand1 * operand2
    }
  )
  
  NEO_Calculation(
    "divide",
    helton,
    function(numerator, denominator) {
      numerator / denominator
    }
  )

  NEO_Context("fromEdgeList", helton, function() lapply(myHolons, function(.v) E(network)[to(.v)]))
  NEO_Context("toEdgeList", helton, function() lapply(myHolons, function(.v) E(network)[from(.v)]))
  NEO_Context("fromVertices", helton, function() V(network)[from(myHolons)])
  NEO_Context("toVertices", helton, function() V(network)[to(myHolons)])

  # WATER: VERTICES
  NEO_Dynam(
    "accumulateWater",
    helton,
    "accumulate",
    "water",
    storage = "myHolons.water",
    gains = "fromEdgeList.water",
    losses = "toEdgeList.water"
  )
  
  NEO_Statam(
    "produceWater",
    helton,
    "multiply",
    "water",
    operand1 = "myHolons.area",
    operand2 = "myHolons.waterYield"
  )

  # WATER: EDGES
  NEO_Dynam(
    "transferAllWater",
    helton,
    "echoValue",
    "water",
    value = "fromVertices.water"
  )
  
  # NO3 VERTICES
  NEO_Dynam(
    "accumulateNO3",
    helton,
    "accumulate",
    "NO3",
    storage = "myHolons.NO3",
    gains = "fromEdgeList.NO3",
    losses = "toEdgeList.NO3"
  )
  
  NEO_Dynam(
    "dissolveNO3",
    helton,
    "divide",
    "NO3Conc",
    numerator = "myHolons.NO3",
    denominator = "myHolons.water"
  )

  # NO3 EDGES
  NEO_Dynam(
    "advectNO3",
    helton,
    "multiply",
    "NO3",
    operand1 = "fromVertices.NO3Conc",
    operand2 = "myHolons.water"
  )
  
  
  NEO_Phase("trade", helton, 1, dynamNames = c("transferAllWater", "advectNO3"))
  NEO_Phase("store", helton, 2, dynamNames = c("accumulateWater", "accumulateNO3", "dissolveNO3"))

  NEO_Behavior("vertexWaterAccumulation", helton, dynamNames = c("accumulateWater"))
  NEO_Behavior("edgeWaterAccumulation", helton, dynamNames = c("transferAllWater"))
  NEO_Behavior("vertexWaterConstantYield", helton, statamNames = c("produceWater"))
  NEO_Behavior("edgeNO3Advection", helton, dynamNames = c("advectNO3"))
  NEO_Behavior("vertexNO3Accumulation", helton, dynamNames = c("accumulateNO3", "dissolveNO3"))

  NEO_Identity("StreamSegment.Accumulation", helton, behaviorNames = c("vertexWaterAccumulation", "vertexNO3Accumulation"))
  NEO_Identity("StreamXSec.TransferAll", helton, behaviorNames = c("edgeWaterAccumulation", "edgeNO3Advection"))
  NEO_Identity("HillSlope.ConstantYield", helton, behaviorNames = c("vertexWaterConstantYield"))

  helton$network = makeNetwork()

  NEO_Initialize(helton)

  invisible(helton)
}

NEO_XamDependencies = function(model, xamBinName) {
  
  xamCollection = paste0("my", toupper(substr(xamBinName, 1, 1)), substr(xamBinName, 2, nchar(xamBinName)))
  
  #get all of the dynams and statams
  xamList = as.list(model[[xamBinName]])
  
  #subset the xamList according to the "target" -- that is the variable
  #calculated by the dyanam.  This will be useful later.
  xamTargetAttr = sapply(xamList, attr, "targetAttr")
  xamsByTarget = 
    sapply(
      unique(xamTargetAttr), 
      function(tA) {
        xamList[xamTargetAttr == tA]
      },
      simplify = F
    )

  # get the holon collections and attributes that are passed to the xam calculation  
  calcHolonCollectionNamesByXam = 
    sapply(
      xamList, 
      function(xam) structure(attr(xam, "calcHolonCollections"), names = attr(xam, "calcHolonAttrs")),
      simplify = F
    )
  calcHolonAttrsNamesByXam = sapply(xamList, attr, "calcHolonAttrs", simplify = F)

  willy = function(behavior, dependencyAttr, xam, dynamsNamesByPhaseList, xamPhase) {
    #for each behavior, get the xams
    potentialDependencies = behavior[[xamCollection]]
    #determine which xams calculate the dependencyAttr of the holonCollection
    pDIdx = sapply(potentialDependencies, attr, "targetAttr") == dependencyAttr
    #determine which potential dependencies are in the same
    #calculation phase as the xam
    if (inherits(xam, "NEO_Dynam")) {
      pDIdx = pDIdx & (names(potentialDependencies) %in% dynamsNamesByPhaseList[[xamPhase]])
    }
    potentialDependencies[pDIdx]
  }
  
  
   
  TESTIT =  function(holonCollectionNamesByDependencyAttr, xam) {
    xamTargetAttr = attr(xam, "targetAttr")
    dynamsNamesByPhaseList = lapply(as.list(model$phases), function(phase) names(phase$myDynams))
    xamPhase = names(dynamsNamesByPhaseList)[sapply(dynamsNamesByPhaseList, function(dNames) attr(xam, "name") %in% dNames)]
    
    holonCollectionsByDependencyAttr = lapply(holonCollectionNamesByDependencyAttr, function(cHC) xam$holons[[cHC]])
    identityNamesByDependencyAttr = 
      mapply(
        function(hC, dependencyAttr) {
          # for an individual holonCollection, get a unique list of the associated identity names
          identityNames = unique(unlist(NEO_HolonAttr(hC, model, "identity")))
          identityList = 
            sapply(
              identityNames, 
              function(identityName) {
                # for each identityName, get the identity and the associated behaviors
                behaviors = get(identityName, envir = model$identities)$myBehaviors
                dependantXams = 
                  sapply(
                    behaviors,
                    willy,
                    dependencyAttr,
                    xam,
                    dynamsNamesByPhaseList,
                    xamPhase
                  )
              }, 
              simplify = F)
        },
        holonCollectionsByDependencyAttr, 
        names(holonCollectionsByDependencyAttr),
        SIMPLIFY = F
      )
   }
 
  #get the holon collections for each calculation attribute
  dependenciesByXamList= 
    mapply(
      TESTIT,
      calcHolonCollectionNamesByXam,
      xamList, 
      SIMPLIFY = F
    )
  

  return(model)
}

makeNetwork = function() {
  #Create the matrix and holons
  streamCells = sapply(1:10, function(.i) paste0("Str", LETTERS[.i]))
  hillCells = sapply(1:10, function(.i) paste0("HS_", LETTERS[.i]))
  each = c(1, rep(2, 4), 1)
  streamEdges = as.character(unlist(lapply(1:6, function(.i) rep(streamCells[.i], each = each[.i]))))
  hillEdges = unlist(Map(c, hillCells, streamCells))
  someedges = c(streamEdges, hillEdges, c("StrH", "StrD", "StrG", "StrH", "StrI", "StrJ", "StrJ", "StrC"))
  network = graph(edges = someedges, directed = T)

  StreamVs = V(network)[grepl("^Str", V(network)$name)]
  HillVs = V(network)[grepl("^HS_", V(network)$name)]

  #  StreamEs = Es[from(StreamVs)]
  #  HillEs = Es[from(HillVs)]

  V(network)[StreamVs]$identity = "StreamSegment.Accumulation"
  V(network)[StreamVs]$water = 0

  V(network)[HillVs]$identity = "HillSlope.ConstantYield"
  V(network)[HillVs]$waterYield = 1
  V(network)[HillVs]$area = 10

  E(network)$name = attr(E(network), "vnames")
  E(network)$identity = "StreamXSec.TransferAll"

  return(network)
}

# # gets objectes in the "names" list from a NEO_Bin object.
# NEO_EnvironmentList = function(names, NEO_Bin) {
#   envList =
#     lapply(
#       names,
#       function(nm) {
#         if(nm == "myHolons") {
#           target = NULL
#         } else {
#           target = NEO_Bin[[nm]]
#           if(is.null(target)) stop("Item '", nm, "', requested from model$", attr(NEO_Bin, "name"), ", does not exist.")
#         }
#         return(target)
#       }
#     )
#   names(envList) = lapply(envList, attr, which = "name")
#   return(envList)
# }

