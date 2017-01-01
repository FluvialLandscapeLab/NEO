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

  NEO_Context("fromEdgeList", helton, function(holons) lapply(holons, function(.v) E(network)[from(.v)]))
  NEO_Context("toEdgeList", helton, function(holons) lapply(holons, function(.v) E(network)[to(.v)]))
  NEO_Context("fromVertices", helton, function(holons) V(network)[from(holons)])
  NEO_Context("toVertices", helton, function(holons) V(network)[to(holons)])

  NEO_Dynam(
    "accumulateWater",
    helton,
    "accumulate",
    "water",
    storage = "myHolons.water",
    gains = "fromEdgeList.water",
    losses = "toEdgeList.water"
  )

  NEO_Dynam(
    "transferAllWater",
    helton,
    "echoValue",
    "water",
    value = "fromVertices.water"
  )

  NEO_Statim(
    "produceWater",
    helton,
    "multiply",
    "water",
    operand1 = "myHolons.area",
    operand2 = "myHolons.waterYield"
  )

  NEO_Phase("trade", helton, 1, dynamList = list("transferAllWater"))
  NEO_Phase("store", helton, 2, dynamList = list("accumulateWater"))

  NEO_Behavior("waterAccumulationVertex", helton, statimList = list(), dynamList = list("accumulateWater"))
  NEO_Behavior("waterAccumulationEdge", helton, statimList = list(), dynamList = list("transferAllWater"))
  NEO_Behavior("waterConstantYield", helton, statimList = list("produceWater"), dynamList = list())

  NEO_Identity("StreamSegment.Accumulation", helton, behaviorList = list("waterAccumulationVertex"))
  NEO_Identity("StreamXSec.TransferAll", helton, behaviorList = list("waterAccumulationEdge"))
  NEO_Identity("HillSlope.ConstantYeild", helton, behaviorList = list("waterConstantYield"))

  helton$network = makeNetwork()

# BUILD THE MODEL!!!
  NEO_DynamMyHolons(helton)
  NEO_DynamContexts(helton)
  NEO_XamDependencies(helton)

  invisible(helton)
}

NEO_XamDependencies = function(model) {
  xamList = c(as.list(model$dynams), as.list(model$statims))
  calcArgsList = lapply(xamList, attr, which = "calcArguments")
  contextList = lapply(calcArgsList, function(x) structure(x[, 1], names = row.names(x)))
  attributeList = lapply(calcArgsList, function(x) structure(x[, 2], names = row.names(x)))

  NEO_IdentitiesFromContexts = function(contextName, xam) {


  }
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

# gets objectes in the "names" list from a NEO_Bin object.
NEO_EnvironmentList = function(names, NEO_Bin) {
  envList =
    lapply(
      names,
      function(nm) {
        if(nm == "myHolons") {
          target = NULL
        } else {
          target = NEO_Bin[[nm]]
          if(is.null(target)) stop("Item '", nm, "', requested from model$", attr(NEO_Bin, "name"), ", does not exist.")
        }
        return(target)
      }
    )
  names(envList) = lapply(envList, attr, which = "name")
  return(envList)
}

