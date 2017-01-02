#' @export
NEO_Initialize = function(model) {
  # BUILD THE MODEL!!!
  NEO_XamContexts(model)
  NEO_XamDependencies(model, "statams")
  NEO_XamDependencies(model, "dynams")
}

NEO_XamContexts = function(model) {
  
  # create "myHolons" context
  NEO_XamMyHolons(model, "statams")
  NEO_XamMyHolons(model, "dynams")
  
  # gather list of dynams and statams
  xamList = c(as.list(model$dynams), as.list(model$statams))
  
  # extract the class
  contextNamesByXam = lapply(xamList, function(d) attr(d, which = "calcHolonCollections"))
  contextNamesByXam = lapply(contextNamesByXam, function(nm) unique(nm[nm != "myHolons"]))
  contextsByXam = lapply(contextNamesByXam, function(ns) as.list(model$contexts)[ns])
  
  mapply(
    function(xam, contexts) {
      lapply(
        contexts,
        function(con) {
          conName = attr(con, "name")
          if(conName %in% ls(xam$holons)) stop("Attempt to define context '", conName, "' in ", attr(xam, "class")[1], " '", attr(xam, "name"), "'.")
          environment(con$context) = xam$holons
          holonCollection = con$context()
          if(class(holonCollection) == "list") {
            holonClass = unique(sapply(holonCollection, class))
            if(length(holonClass) > 1) stop("Holon list '", conName,"' of ", class(xam), " '", attr(xam, "name"), "' has more than one type of holon." )
            class(holonCollection) = c(paste0(holonClass, ".list"))
          }
          xam$holons[[conName]] = holonCollection
        }
      )
    },
    xamList,
    contextsByXam
  )
  
  invisible(model)
}

NEO_XamMyHolons = function(model, xamBinName) {
  capXamType = paste0(toupper(substr(xamBinName, 1, 1)), substr(xamBinName, 2, nchar(xamBinName) - 1))
  xamCollection = paste0("my", capXamType, "s")
  #make lists of all dynams, behaviors, and identities
  xamList = as.list(model[[xamBinName]])
  behaviorList = as.list(model$behaviors)
  identityList = as.list(model$identities)
  
  #make a list of logical vectors (one for each dynam) that says if the xam
  #is referenced by the behavior
  xamInBehavior =
    lapply(
      xamList,
      function(xam) {
        sapply(
          behaviorList,
          function(b, x) {
            attr(x, "name") %in% names(b[[xamCollection]])
          },
          x = xam
        )
      }
    )
  
  #convert to a list of character vectors (one for each dynam) that contains
  #associated behavior names
  behaviorsByXam = lapply(xamInBehavior, function(x) names(x)[x])
  
  #make a list of logical vectors (one for each dynams) that says wether any
  #associated behaviors is referenced by each identity
  behaviorInIdentity =
    lapply(
      behaviorsByXam,
      function(behaviors) {
        sapply(
          identityList,
          function(i, behaviors) {
            any(behaviors %in% names(i$myBehaviors))
          },
          behaviors = behaviors
        )
      }
    )
  
  #convert to a list of character vectors (one for each dynam) that contains
  #associated identity names.
  identitiesByXam = lapply(behaviorInIdentity, function(x) names(x)[x])
  
  #for each dynam, get the collection of edges and vertices that have any of the
  #identities associated with the dynam
  edges = E(model$network)
  verts = V(model$network)
  edgesByXam = lapply(identitiesByXam, function(i) edges[edges$identity %in% i])
  vertsByXam = lapply(identitiesByXam, function(i) verts[verts$identity %in% i])
  
  #check to be sure one dynam isn't associated with both edges and vertices, and
  #if not, combine the edge and verts list.
  holonsByXam =
    mapply(
      function (es, vs, name){
        if(length(es) != 0 && length(vs) != 0) stop("NEO_", capXamType, "'", name, "' is associated with both edges and vertices.")
        if(length(es > 0)) {
          return(es)
        } else {
          return(vs)
        }
      },
      edgesByXam,
      vertsByXam,
      names(edgesByXam),
      SIMPLIFY = F
    )
  
  # set the myHolons variable in each dynam
  mapply(function(d, hL) d$holons$myHolons = hL, xamList, holonsByXam)
  
  invisible(model)
}

