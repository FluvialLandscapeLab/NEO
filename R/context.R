#' @export
NEO_Context = function(contextName, model, fun) {

#  requiredFormals = sapply(formals(fun), is.name)
#  if(!identical(requiredFormals, c(holons = T))) stop("A functions used to create a NEO_Context can only have one parameter which must be named 'holons' and which can't have a default value")

  newContext = NEO_Environment(contextName, "NEO_Context", model$contexts)

  newContext$context = fun

  invisible(newContext)
}
