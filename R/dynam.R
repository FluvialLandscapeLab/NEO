#' @export
NEO_Dynam = function(dynamName, model, calculationName, attributeName, ...) {

  invisible(NEO_Xam(dynamName, model, calculationName, attributeName, "dynam", ...))

}

NEO_Xam = function(xamName, model, calculationName, attributeName, xamType, ...) {

  # create the plural
  xamTypes = paste0(xamType, "s")

  # create capital first letter
  xamTypeCap = paste0(toupper(substr(xamType, 1, 1)), substr(xamType, 2, nchar(xamType)))

  # create xamName and check to see if it already exists
  if(xamName %in% ls(model[[xamTypes]])) stop("Attempt to define ", xamType, " '", xamName, "' more than once.")

  # be sure the requested calculations are in the model
  missingCalcs = !(calculationName %in% ls(model$calculations))
  if(any(missingCalcs)) stop("Calculations '", calculationName, "' requested by NEO_", xamTypeCap, " '", xamName, "' not found in model '", attr(model, "name"), "'.")

  calculation = helton$calculations[[calculationName]]

  # expand the dots, which are character representations of arguments for the
  # calculation in the form: 'contextName.attributeName'
  dots = list(...)

  # calcArgs is a named logical vector.  The names are the parameter names and
  # the values represent whether or not the parameter is required.
  calcArgs = attr(calculation, "requiredArgs")

  # determine if any of the parameter names doesn't match the calculation
  illegalArgs = !(names(dots) %in% names(calcArgs))
  if(any(illegalArgs)) stop("Calculation argument(s) named (", paste0("'", names(dots)[illegalArgs], "'", collapse = " and "), " were passed to NEO_", xamTypeCap, "() but are not used by NEO_Calculation '", calculationName, "'.")

  # make sure all required parameters are in calcArgs
  reqArgs = calcArgs[calcArgs]
  missingArgs = !(names(reqArgs) %in% names(dots))
  if(any(missingArgs)) stop("The following parameters are required by NEO_Calculation '", calculationName, "' but were not supplied to NEO_", xamTypeCap, "(): ", paste0(names(reqArgs)[missingArgs], collapse = ", "))

  # make a matrix with rows called "holons" and "holonAttr" which contain the
  # context names and attributes passed as arguments to the dynam calculation.
  # One column for each argument.
  splitDots =
    lapply(
      dots,
      function(x) {
        conAttr = unlist(strsplit(x, split = ".", fixed = TRUE))
        if(length(x) != 1 || length(conAttr) != 2 || !is.character(x)) stop("Calculation parameters passed to NEO_", xamTypeCap, "() must be defined as character(length = 1) vectors: 'contextName.attributeName'.")
        if(!(conAttr[1] %in% c(ls(model$contexts), "myHolons"))) stop("Calculation parameters passed to NEO_", xamTypeCap, "() requested a NEO_Context named '", conAttr[1], "', which does not exist in model '", attr(model, "name"), "'.")
        return(conAttr)
      }
    )

  calcHolonsCollections = sapply(splitDots, "[", 1)
  calcHolonAttrs = sapply(splitDots, "[", 2)
  
  # create the xam obejct
  newXam = NEO_Environment(xamName, paste0("NEO_", xamTypeCap), model[[xamTypes]])

  # set some attributes and variables.
  newXam$holons = NEO_Environment("holons", "NEO_Bin", newXam)
  attr(newXam, "targetAttr") = attributeName
  attr(newXam, "calcHolonCollections") = calcHolonsCollections
  attr(newXam, "calcHolonAttrs") = calcHolonAttrs
  attr(newXam, "calculation") = calculation

  return(newXam)
}

