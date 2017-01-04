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
    function(echo) {
      return(echo)
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

#### CONTEXTS ####

  NEO_Context(
    "fromEdgeList", 
    helton, 
    function() {
      Es = lapply(myHolons, function(.v) names(E(network)[to(.v)]))
      names(Es) = names(myHolons)
      class(Es) = "NEO_EdgeNamesList"
      return(Es)
    }
  )
  
  NEO_Context(
    "toEdgeList", 
    helton, 
    function() {
      Es = lapply(myHolons, function(.v) names(E(network)[from(.v)]))
      names(Es) = names(myHolons)
      class(Es) = "NEO_EdgeNamesList"
      return(Es)
    }
  )
  
  NEO_Context(
    "fromVertices", 
    helton, 
    function() {
      Vs = sapply(myHolons, function(.e) names(V(network)[from(.e)]))
      names(Vs) = names(myHolons)
      class(Vs) = "NEO_VertexNames"
      return(Vs)
    }
  )
      
  NEO_Context(
    "toVertices", 
    helton, 
    function() {
      Vs = sapply(myHolons, function(.e) names(V(network)[to(.e)]))
      names(Vs) = names(myHolons)
      class(Vs) = "NEO_VertexNames"
      return(Vs)
    }
  )
  
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
    echo = "fromVertices.water"
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
  
  NEO_Phase("initialize", helton, 0)
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

  NEO_Execute(helton)
  
  invisible(helton)
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
  V(network)[StreamVs]$NO3Conc = 0

  V(network)[HillVs]$identity = "HillSlope.ConstantYield"
  V(network)[HillVs]$waterYield = 1
  V(network)[HillVs]$area = 10
  V(network)[HillVs]$NO3Conc = 5

  E(network)$name = attr(E(network), "vnames")
  E(network)$identity = "StreamXSec.TransferAll"

  return(network)
}

