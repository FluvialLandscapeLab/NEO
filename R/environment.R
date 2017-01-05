#' @export
NEO_Environment = function(envirName, class, parent = NULL) {

  # create the NEO_Environment object
  newEnvir = structure(
    new.env(),
    name = envirName,
    class = c(class, "NEO_Environment")
  )

  if(!is.null(parent)) {
    parent.env(newEnvir) = parent
    
    # be sure the parent is a NEO_Environment
    if(!inherits(parent, "NEO_Environment")) stop("Argument 'parent' passed to NEO_Enviroment() must be NULL or a 'NEO_Environment' object.")
    # check to be sure we're not going to overwrite anything in the parent environment
    if(envirName %in% ls(parent)) stop("Attempt to redefine", class, " '", envirName, "' in NEO_Environment '", attr(parent, "name"))
    attr(newEnvir, "parentName") = attr(parent, "name")
    parent[[envirName]] = newEnvir
  }

  invisible(newEnvir)
}

#' @export
print.NEO_Environment = function(x) {

  # helper function for use below; take a list of variables, looks at each one. 
  # Any NEO_Environment is converted to its name, so the name prints rather then
  # default environment name.
  convert2Names = function(items) {
    items =
      lapply(
        items,
        function(i) {
          if(inherits(i, "NEO_Environment")) i = paste(attr(i, "name"), "(NEO_Environment)") 
          return(i)
        }
      )
  }

  # helper function to get the attributes of an item
  getAttrNames = function(item) {
    attrNms = names(attributes(item))
    return(attrNms[attrNms != "class"])
  }

  # print the name of the NEO.Environment
  cat(attr(x, "name"), " (", class(x)[1], ")", sep = "")
  # for each item in the NEO_Environment...
  lapply(
    ls(x),
    function(.x) {
      cat(paste0("\n ", .x))
      if(is.environment(x[[.x]])) {
        # if it's an environment, print the variables and attributes
        cat("\n  variables: ")
        cat(paste0(ls(x[[.x]]), collapse = ", "))
        cat("\n  attributes: ")
        cat(paste0(getAttrNames(x[[.x]]), collapse = ", "))
      } else if(is.list(x[[.x]])) {
        # if it's an environment, print anything is the list, but with environments converted to a name.
        cat(":", paste0(convert2Names(x[[.x]]), collapse = ", "))
      } else if(is.function(x[[.x]])) {
        # if it's a function, print the body, indented a few spaces.
        xFormals = formals(x[[.x]])
        xFormalsNames = names(xFormals)
        defaults = sapply(xFormals, function(f) ifelse(class(f) == "name", "", paste0(" = ", f)))
        sourceVector = capture.output(print(body(x[[.x]])))
        cat(paste0("\n  function(", paste0(xFormalsNames, defaults, collapse = ", "), ") ", sourceVector[1]))
        if(length(sourceVector) > 1) cat(paste0("\n  ", sourceVector[-1])) #cat the source code vector, indenting everything.
      } else {
        # otherwise, just cat it.
        cat(":", x[[.x]])
      }
    }
  )
  # follow up with attributes of the NEO_Environment
  cat("\nattributes:", paste0(getAttrNames(x), collapse = ", "))
}

