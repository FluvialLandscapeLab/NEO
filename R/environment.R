#' @export
NEO_Environment = function(envirName, parent, class) {

  if(envirName %in% ls(parent)) stop("Attempt to define", class, " '", envirName, "' more than once.")

  newEnvir = structure(
    new.env(parent = parent),
    name = envirName,
    parentName = attr(parent, "name"),
    class = c(class, "NEO_Environment")
  )

  parent[[envirName]] = newEnvir

  invisible(newEnvir)
}

#' @export
print.NEO_Environment = function(x) {

  convert2Names = function(items) {
    items =
      lapply(
        items,
        function(i) {
          if(inherits(i, "NEO_Environment")) i = attr(i, "name")
          return(i)
        }
      )
  }

  getAttrNames = function(item) {
    attrNms = names(attributes(item))
    return(attrNms[attrNms != "class"])
  }


  args(helton$calculations$accumulate$calculation)

  cat(attr(x, "name"))
  lapply(
    ls(x),
    function(.x) {
      cat(paste0("\n ", .x))
      if(is.environment(x[[.x]])) {
        cat("\n  variables: ")
        cat(paste0(ls(x[[.x]]), collapse = ", "))
        cat("\n  attributes: ")
        cat(paste0(getAttrNames(x[[.x]]), collapse = ", "))
      } else if(is.list(x[[.x]])) {
        cat(":", paste0(convert2Names(x[[.x]]), collapse = ", "))
      } else if(is.function(x[[.x]])) {
        xFormals = formals(x[[.x]])
        xFormalsNames = names(xFormals)
        defaults = sapply(xFormals, function(f) ifelse(class(f) == "name", "", paste0(" = ", f)))
        sourceVector = capture.output(print(body(x[[.x]])))
        cat(paste0("\n  function(", paste0(xFormalsNames, defaults, collapse = ", "), ") ", sourceVector[1]))
        if(length(sourceVector) > 1) cat(paste0("\n  ", sourceVector[-1])) #cat the source code vector, indenting everything.
      } else {
        cat(":", x[[.x]])
      }
    }
  )
  cat("\nattributes:", paste0(getAttrNames(x), collapse = ", "))
}

