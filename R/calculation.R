#' @export
NEO_Calculation = function(calculationName, model, fun) {

  newCalculation = NEO_Environment(calculationName, model$calculations, "NEO_Calculation")
  attr(newCalculation, "requiredArgs") = sapply(as.list(formals(fun)), is.name)

  newCalculation$calculation = fun

  invisible(newCalculation)
}
