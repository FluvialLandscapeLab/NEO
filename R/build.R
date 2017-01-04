#' @export
NEO_BuildModel = function(model) {
  
  ### CHECK TO BE SURE ALL DYNAMS ARE IN ONE AND ONLY ONE CALCULATION PHASE.
  
  NEO_XamContexts(model)
  for(binName in c("dynams", "statams")) {
    # investigate behaviors of each identity to create a flat list of the xams
    # associated with the identity.  Name of each xam in the list is attr(xam,
    # "targetAttr").
    NEO_IdentityMyXams(model, binName)
    # using the new xam list, calculation the dependencies for each xam in the
    # model.
    NEO_XamDependencies(model, binName)
  }
  
  invisible(T)
#  NEO_PhaseDynamOrder(model)
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
          if(conName %in% ls(xam$holons)) stop("Attempt to define context '", conName, "' in ", attr(xam, "class")[1], " '", attr(xam, "name"), "' more than once.")
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

NEO_IdentityMyXams = function(model, xamBinName) {
  capXamType = paste0(toupper(substr(xamBinName, 1, 1)), substr(xamBinName, 2, nchar(xamBinName) - 1))
  xamCollection = paste0("my", capXamType, "s")
  
  identities = as.list(model$identities)
  behaviorsByIdentity = 
    lapply(
      identities, 
      function(ident) {
        xams = unlist(
          lapply(
            ident$myBehaviors,
            function(behav) behav[[xamCollection]]
          ),
          recursive = F
        )
        names(xams) = sapply(xams, attr, "targetAttr")
        dupXams = duplicated(names(xams))
        if(any(dupXams)) stop("In NEO_Identity '", attr(ident, "name"), "', more than one NEO_", capXamType, " is defined to calculate to following holon attributes: ", paste0(names(dynams)[dupDynams], collapse = ", "))
        ident[[xamCollection]] = xams
      }
    )
  
}

NEO_XamDependencies = function(model, xamBinName) {
  
  xamCollectionName = paste0("my", toupper(substr(xamBinName, 1, 1)), substr(xamBinName, 2, nchar(xamBinName)))
  
  #get all of the dynams and statams
  xamList = as.list(model[[xamBinName]])
  
  #get a list of dynamNameVectors for each calculation phase in the model. This
  #is useful later.
  xamsNamesByPhaseList = lapply(as.list(model$phases), function(phase) names(phase[[xamCollectionName]]))
  #if a names list is null, there are no xams of type xamBinName associated with
  #the phase, so no need to calculate dependencies.  Remove the name from the
  #list.
  xamsNamesByPhaseList = xamsNamesByPhaseList[!sapply(xamsNamesByPhaseList, is.null)]
  
  # for each xam, make a list of other xams that represents dependencies
  dependenciesList = 
    sapply(
      xamList, 
      function(xam) {
        # Determine and store the name of the phase where xam is calculated.
        xam$myPhaseName = names(xamsNamesByPhaseList)[sapply(xamsNamesByPhaseList, function(dNames) attr(xam, "name") %in% dNames)]
        # for each xam, get the dependency collection names from 
        # "calcHolonCollections" attribute
        calcHolonCollectionNames = structure(attr(xam, "calcHolonCollections"), names = attr(xam, "calcHolonAttrs"))
        # using the names, grab the collections themselves from the xam
        calcHolonCollections = sapply(calcHolonCollectionNames, function(cHCN) xam$holons[[cHCN]], simplify = F)
        
        # determine the dependencies and save them in the xam.
        xam$myDependencies =
          unlist(
            mapply(
              function(cHC, dependencyAttr) {
                # for an individual holonCollection, get a unique vector of the associated identity names
                dependencyIdentityNames = unique(unlist(NEO_HolonAttr(model, "identity", cHC)))
                dependencies = 
                  unlist(
                    lapply(
                      dependencyIdentityNames, 
                      function(dIN) {
                        # for each identity name associated with a holonCollection, 
                        # get the identity's xam that calculates the calcHolonAttr 
                        # (which is stored in "dependencyAttr")
                        dependency = model$identities[[dIN]][[xamCollectionName]][[dependencyAttr]]
                        # if there are no dependencies, dependency = NULL. 
                        # Convert NULL to empty named list.
                        if(is.null(dependency)) dependency = structure(list(), names = character(0))
                        # If we are dealing with dynams rather than statams, remove
                        # any potential dependencies that are not in the same 
                        # calculation phase as the xam.  Remove any
                        # potentialDependency references to the xam
                        if (inherits(xam, "NEO_Dynam") & length(dependency) > 0) {
                          notDep = !attr(dependency, "name") %in% xamsNamesByPhaseList[[xam$myPhaseName]]
                          notDep = notDep | (identical(dependency, xam))
                          if(notDep) dependency = structure(list(), names = character(0))
                        }
                        return(dependency)
                      }
                    ),
                    recursive = F
                  )
              },
              calcHolonCollections,
              attr(xam, "calcHolonAttrs"),
              SIMPLIFY = F
            ),
            recursive = F
          )
        names(xam$myDependencies) = sapply(xam$myDependencies, attr, "name")
        return(xam$myDependencies)
      },
      simplify = F
    )
  
  phases = as.list(model$phases)[names(xamsNamesByPhaseList)]
  
  dependencyPairsList =
    lapply(
      phases,
      # for each calculation phase...
      function(phase){
        dependencyPairs =
          unlist(
            lapply(
              phase[[xamCollectionName]],
              # for each xam in the phase...
              function(xam) {
                unlist(
                  lapply(
                    xam$myDependencies,
                    # pair all dependency names with the xam name to make a dependency pair
                    function(dependency) {
                      c(attr(dependency, "name"), attr(xam, "name"))
                    }
                  ),
                  use.names = F
                )
              }
            ),
            use.names = F
          )
        # if there are no dependency pairs for the phase, we need do nothing. 
        # The order of the xam list associated with the phase doesn't matter. 
        # If, on the other hand, there are dependency pairs for the phase, we
        # order the xam list to account for dependencies
        if(!is.null(dependencyPairs)) phase[[xamCollectionName]] = NEO_CalculationOrder(phase, dependencyPairs, xamCollectionName)
      }
    )
  
  invisible(model)
}

NEO_CalculationOrder = function(phase, dependencyPairs, xamCollectionName) {
  # make a directed graph from the dependency pairs
  depGraph = igraph::graph(edges = dependencyPairs, directed = T)
  # check for circular dependencies
  if(!igraph::is.dag(depGraph)) stop("Circular dependency in dynam calculations in phase '", attr(phase, "name"), "'.")
  # use a topological sort to determine a calculation order that respects the
  # dependencies
  xamNameOrder = igraph::topo_sort(depGraph, mode = "out")$name
  # find any xams in the phase that are not involved in dependencies
  allXamNames = names(phase[[xamCollectionName]])
  unorderedNames = allXamNames[!(allXamNames %in% xamNameOrder)]
  # create a list of ordered xams with independent xams first, followed by the
  # topological sort order.
  return(phase[[xamCollectionName]][c(unorderedNames, xamNameOrder)])
}

