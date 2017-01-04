#' @export
NEO_Model = function(modelName, units = NEO_DefaultUnits(), envir = globalenv()){
  newModel = structure(
    new.env(),
    class = c("NEO_Model", "NEO_Environment"),
    name = modelName
  )

  NEO_Environment("dynams", newModel, "NEO_Bin")
  NEO_Environment("statams", newModel, "NEO_Bin")
  NEO_Environment("contexts", newModel, "NEO_Bin")
  NEO_Environment("calculations", newModel, "NEO_Bin")
  NEO_Environment("behaviors", newModel, "NEO_Bin")
  NEO_Environment("identities", newModel, "NEO_Bin")
  NEO_Environment("phases", newModel, "NEO_Bin")

  if(!identical(sort(names(units)), sort(names(NEO_DefaultUnits())))) stop("Names of 'units' vector must be: ", print0("\"", names(NEO_DefaultUnits), "\"", collapse = ", "))

  newModel$units = units
  newModel$timeStep = NULL
  newModel$outputInterval = NULL
  assign(modelName, newModel, envir = envir)

  invisible(newModel)
}

#' @export
as.list.NEO_Bin = function(x) {
  binList = lapply(ls(x), get, envir = x, inherits = F)
  names(binList) = ls(x)
  return(binList)
}

#' @export
NEO_DefaultUnits = function() return(c(L = "m", t = "s", M = "kg", T = "degC", E = "kJ"))

#' @export
NEO_HolonAttr = function(model, attributeName, holons) {
  UseMethod("NEO_HolonAttr", holons)
}

#' @export
NEO_HolonAttr.igraph.es = function(model, attributeName, holons) {
  igraph::edge_attr(model$network, attributeName, holons)
}

#' @export
NEO_HolonAttr.igraph.vs = function(model, attributeName, holons) {
  igraph::vertex_attr(model$network, attributeName, holons)
}

#' @export
NEO_HolonAttr.NEO_EdgeNames = function(model, attributeName, holons) {
  igraph::edge_attr(model$network, attributeName, igraph::E(model$network)[holons])
}

#' @export
NEO_HolonAttr.NEO_VertexNames = function(model, attributeName, holons) {
  igraph::vertex_attr(model$network, attributeName, igraph::V(model$network)[holons])
}

#' @export
NEO_HolonAttr.NEO_EdgeNamesList = function(model, attributeName, holons) {
  lapply(holons, function(hln) igraph::edge_attr(model$network, attributeName, E(model$network)[match(hln, names(E(helton$network)))]))
}

#' @export
NEO_HolonAttr.NEO_VertexNamesList = function(model, attributeName, holons) {
  lapply(holons, function(hln) igraph::vertex_attr(model$network, attributeName, V(model$network)[match(hln, names(V(helton$network)))]))
}

#' @export
`NEO_HolonAttr<-` = function(model, attributeName, holons, value) {
  UseMethod("NEO_HolonAttr<-", holons)
}

#' @export
`NEO_HolonAttr<-.NEO_EdgeNames` = function(model, attributeName, holons, value) {
  model$network = igraph::set.edge.attribute(model$network, attributeName, E(model$network)[holons], value)
  return(model)
}

#' @export
`NEO_HolonAttr<-.NEO_VertexNames` = function(model, attributeName, holons, value) {
  model$network = igraph::set.vertex.attribute(model$network, attributeName, V(model$network)[holons], value)
  return(model)
}

#' @export
`NEO_HolonAttr<-.igraph.es` = function(model, attributeName, holons, value) {
  model$network = igraph::set.edge.attribute(model$network, attributeName, holons, value)
  return(model)
}

#' @export
`NEO_HolonAttr<-.igraph.vs` = function(model, attributeName, holons, value) {
  model$network = igraph::set.vertex.attribute(model$network, attributeName, holons, value)
  return(model)
}


# `NEO_HolonAttr<-NEO_EdgeNamesList` = function(model, attributeName, holons, value) {
#   for(hlns in holons) {
#     model$network = igraph::set.edge.attribute(model$network, attributeName, hlns, value)
#   }
#   return(model)
# }
# 
# `NEO_HolonAttr<-NEO_VertexNamesList` = function(model, attributeName, holons, value) {
#   for(hlns in holons) {
#     model$network = igraph::set.vertex.attribute(model$network, attributeName, hlns, value)
#   }
#   return(model)
# }
