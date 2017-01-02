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
  names(binList) = sapply(binList, attr, which = "name")
  return(binList)
}

#' @export
NEO_DefaultUnits = function() return(c(L = "m", t = "s", M = "kg", T = "degC", E = "kJ"))

#' @export
NEO_HolonAttr = function(holons, model, attributeName) {
  UseMethod("NEO_HolonAttr", holons, model, attributeName)
}

#' @export
NEO_HolonAttr.igraph.es = function(holons, model, attributeName) {
  igraph::edge_attr(model$network, attributeName, holons)
}

#' @export
NEO_HolonAttr.igraph.vs = function(holons, model, attributeName) {
  igraph::vertex_attr(model$network, attributeName, holons)
}

#' @export
NEO_HolonAttr.igraph.es.list = function(holons, model, attributeName) {
  lapply(holons, igraph::edge_attr, graph = model$network, name = attributeName)
}

#' @export
NEO_HolonAttr.igraph.vs.list = function(holons, model) {
  lapply(holons, igraph::vertex_attr, graph = model$network, name = attributeName)
}
