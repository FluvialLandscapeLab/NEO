#' @export
NEO_Calculation = function(calculationName, model, fun) {

  # Create the calculation as a new NEO_Environment
  newCalculation = NEO_Environment(calculationName, "NEO_Calculation", model$calculations)
  
  # set appropriate attributes and variables.
  attr(newCalculation, "requiredArgs") = sapply(as.list(formals(fun)), is.name)

  newCalculation$calculation = fun

  invisible(newCalculation)
}
