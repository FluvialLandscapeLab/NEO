#' @export
NEO_Initialize = function(model) {
  # BUILD THE MODEL!!!
  NEO_XamContexts(model)
  NEO_XamDependencies(model)
}

NEO_XamContexts = function(model) {
  
  # create "myHolons" context
  NEO_XamMyHolons(model)
  
  # gather list of dynams and statams
  xamList = c(as.list(model$dynams), as.list(model$statams))
  
  # extract the class
  contextNamesByXam = lapply(xamList, function(d) attr(d, which = "calcArguments")[,"holons"])
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

NEO_XamMyHolons = function(model) {
  #make lists of all dynams, behaviors, and identities
  dynamList = c(as.list(model$dynams), as.list(model$statams))
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
  mapply(function(d, hL) d$holons$myHolons = hL, dynamList, holonsByDynam)
  
  invisible(model)
}

