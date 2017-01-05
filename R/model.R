#' @export
NEO_Model = function(modelName, units = NEO_DefaultUnits(), envir = globalenv()){

  if(!identical(sort(names(units)), sort(names(NEO_DefaultUnits())))) stop("Names of 'units' vector must be: ", print0("\"", names(NEO_DefaultUnits), "\"", collapse = ", "))
  
  # create a NEO_Model
  newModel = NEO_Environment(modelName, "NEO_Model")

  # install required NEO_Bin environments to contain model objects
  NEO_Environment("dynams", "NEO_Bin", newModel)
  NEO_Environment("statams", "NEO_Bin", newModel)
  NEO_Environment("contexts", "NEO_Bin", newModel)
  NEO_Environment("calculations", "NEO_Bin", newModel)
  NEO_Environment("behaviors", "NEO_Bin", newModel)
  NEO_Environment("identities", "NEO_Bin", newModel)
  NEO_Environment("phases", "NEO_Bin", newModel)

  #intall some global variables.
  newModel$units = units
  newModel$timeStep = NULL
  newModel$outputInterval = NULL
  assign(modelName, newModel, envir = envir)

  invisible(newModel)
}

#' Get contents of a NEO_Bin as a named list.
#' @export
as.list.NEO_Bin = function(x) {
  binList = lapply(ls(x), get, envir = x, inherits = F)
  names(binList) = ls(x)
  return(binList)
}

#' Return default units for NEO models.
#' @export
NEO_DefaultUnits = function() return(c(L = "m", t = "s", M = "kg", T = "degC", E = "kJ"))

##### NEO_HolonAttr #############################

#' Return attribute values of holons (graph vertices or edges) using a colleciton (vector or list of vectors) of vertex or edge indices
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
NEO_HolonAttr.igraph.es.list = function(model, attributeName, holons) {
  lapply(holons, igraph::edge_attr, graph = model$network, name = attributeName)
}

#' @export
NEO_HolonAttr.igraph.vs.list = function(model, attributeName, holons) {
  lapply(holons, igraph::vertex_attr, graph = model$network, name = attributeName)
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


