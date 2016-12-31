NEO_GetDynam = function(dynamName, model) {
  return(model$dynams[[dynamName]])
}

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

  # make a matrix with rows called "context" and "attribute" which contain the
  # context names and attributes passed as arguments to the dynam calculation.
  # One column for each argument.
  splitDots =
    sapply(
      dots,
      function(x) {
        conAttr = unlist(strsplit(x, split = ".", fixed = TRUE))
        if(length(x) != 1 || length(conAttr) != 2 || !is.character(x)) stop("Calculation parameters passed to NEO_", xamTypeCap, "() must be defined as character(length = 1) vectors: 'contextName.attributeName'.")
        if(!(conAttr[1] %in% c(ls(model$contexts), "myHolons"))) stop("Calculation parameters passed to NEO_", xamTypeCap, "() requested a NEO_Context named '", conAttr[1], "', which does not exist in model '", attr(model, "name"), "'.")
        return(conAttr)
      }
    )

  dimnames(splitDots)[[1]] = c("context", "attribute")

  newXam = NEO_Environment(xamName, model[[xamTypes]], paste0("NEO_", xamTypeCap))

  # duplicateNames = names(dots) %in% ls(newXam)
  # if(any(duplicateNames)) {
  #   rm(xamName, envir = model[[xamTypes]])
  #   stop("The NEO_calculation '", calculationName, "' uses parameter names (", paste0(names(dots)[duplicateNames], collapse = ", "), ") that collide with default variable names used by dyanms.  Rewrite the calculation to use different parameter names.")
  # }

  newXam$myHolons = NULL
  attr(newXam, "attribute") = attributeName
  attr(newXam, "calcArguments") = splitDots
  attr(newXam, "calculation") = calculation

  return(newXam)
}

NEO_DynamMyHolons = function(model) {
  #make lists of all dynams, behaviors, and identities
  dynamList = as.list(model$dynams)
  behaviorList = as.list(model$behaviors)
  identityList = as.list(model$identities)

  #make a list of logical vectors (one for each dynam) that says if the dyanam
  #is referenced by the behavior
  dynamInBehavior =
    lapply(
      dynamList,
      function(d) {
        sapply(
          behaviorList,
          function(b, d) {
            attr(d, "name") %in% b$myDynams
          },
          d = d
        )
      }
    )

  #convert to a list of character vectors (one for each dynam) that contains
  #associated behavior names
  behaviorsByDynam = lapply(dynamInBehavior, function(x) names(x)[x])

  #make a list of logical vectors (one for each dynams) that says wether any
  #associated behaviors is referenced by each identity
  behaviorInIdentity =
    lapply(
      behaviorsByDynam,
      function(behaviors) {
        sapply(
          identityList,
          function(i, behaviors) {
            any(behaviors %in% i$myBehaviors)
          },
          behaviors = behaviors
        )
      }
    )

  #convert to a list of character vectors (one for each dynam) that contains
  #associated identity names.
  identitiesByDynam = lapply(behaviorInIdentity, function(x) names(x)[x])

  #for each dynam, get the collection of edges and vertices that have any of the
  #identities associated with the dynam
  edges = E(model$network)
  verts = V(model$network)
  edgesByDynam = lapply(identitiesByDynam, function(i) edges[edges$identity %in% i])
  vertsByDynam = lapply(identitiesByDynam, function(i) verts[verts$identity %in% i])

  #check to be sure one dynam isn't associated with both edges and vertices, and
  #if not, combine the edge and verts list.
  holonsByDynam =
    mapply(
      function (es, vs, name){
        if(length(es) != 0 && length(vs) != 0) stop("NEO_Dynam '", name, "' is associated with both edges and vertices.")
        if(length(es > 0)) {
          return(es)
        } else {
          return(vs)
        }
      },
      edgesByDynam,
      vertsByDynam,
      names(edgesByDynam)
    )

  # set the myHolons variable in each dynam
  mapply(function(d, hL) d$myHolons = hL, dynamList, holonsByDynam)

  invisible(model)
}

NEO_DynamContexts = function(model) {
  xamList = c(as.list(model$dynams), as.list(model$statims))

  contextNamesByXam = lapply(xamList, function(d) attr(d, which = "dependencies")["context",])
  contextNamesByXam = lapply(contextNamesByXam, function(nm) unique(nm[nm != "myHolons"]))
  contextsByXam = lapply(contextNamesByXam, NEO_EnvironmentList, NEO_Bin = model$contexts)

  mapply(
    function(xam, contexts) {
      lapply(
        contexts,
        function(con) {
          environment(con$context) = xam
          xam[[attr(con, "name")]] = con$context(xam$myHolons)
        }
      )
    },
    xamList,
    contextsByXam
  )

  invisible(model)
}

# NEO_CheckContextDependencyList = function(value, protected) {
#   if(!is.list(value) || is.null(names(value))) stop("Contexts or dependencies must be stored as named lists.")
#   isProtected = names(value) %in% protected
#   if(any(isProtected)) stop("The following names are protected; they can't be used as names in a context or dependency list: ", paste0(names(values)[isProtected], collapse = ", "))
#   return(T)
# }
#
# NEO_DynamDependencies = function(dynam) {
#   return(dynam$dependencies)
# }
#
# `NEO_DynamDependencies<-` = function(dynam, value) {
#   NEO_CheckContextDependencyList(value, attr(dynam, "protected"))
#   attr(dynam, "calcArguments") = value
#   dynam
# }
#
#
# NEO_DynamCalculation = function(dynam) {
#   return(dynam$calculation)
# }
#
# `NEO_DynamCalculation<-` = function(dynam, value) {
#   dynam = NEO_DynamFunction(dynam, "calculation", value, "NEO_CalcDynam")
#   dynam
# }
#
#
# NEO_DynamInitialization = function(dynam) {
#   return(dynam$initialization)
# }
#
# `NEO_DynamInitialization<-` = function(dynam, value) {
#   dynam = NEO_DynamFunction(dynam, "initialization", value, "NEO_InitDynam")
#   dynam
# }
#
# NEO_DynamFunction = function(dynam, funName, fun, calcClass) {
#   if(is.null(fun)) {
#     class(dynam) = class(dynam)[class(dynam) != calcClass]
#   } else {
#     if(!is.function(class)) stop("Dynam calculations must be a function.")
#     if(!inherits(dynam, calcClass)) {
#       class(dynam) = c(calcClass, class(dynam))
#     }
#   }
#   attr(dynam, funName) = fun
#   return(dynam)
# }
#
#
# NEO_DynamValue = function(dynam, valueName, value, overwrite = F) {
#   if(!overwrite){
#     if(valueName %in% ls(dynam)) stop("Variable '", valueName, "' is protected in dynam '", attr(dynam, "name"), "' and cannot be overwritten.")
#   }
#   dynam[[valueName]] = value
# }
#
# NEO_DynamAccumulate = function(my, from, to) {
#   newAttributeValues = my +
#     sapply(from, sum) -
#     sapply(to, sum)
#   return()
# }


