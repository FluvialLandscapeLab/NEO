#' @export
NEO_Model = function(modelName, units = NEO_DefaultUnits(), envir = globalenv()){
  newModel = structure(
    new.env(),
    class = c("NEO_Model", "NEO_Environment"),
    name = modelName
  )

  NEO_Environment("dynams", newModel, "NEO_Bin")
  NEO_Environment("statims", newModel, "NEO_Bin")
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

as.list.NEO_Bin = function(x) {
  binList = lapply(ls(x), get, envir = x, inherits = F)
  names(binList) = sapply(binList, attr, which = "name")
  return(binList)
}

#' @export
NEO_DefaultUnits = function() return(c(L = "m", t = "s", M = "kg", T = "degC", E = "kJ"))
