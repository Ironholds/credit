#' function returning character vector of packages currently in use
#'
#' @param base should names of base packages be returned?
#'
#' @param loadedonly should loaded but not attached packeges be returned?
#'
#' @param attached should names of attached, non-bse packages be returned?
#'
#' @export
#'
packages_in_use <- function(base=TRUE, loadedonly=TRUE, attached=TRUE){
  tmp <- sessionInfo()
  packages <- character(0)
  if(base){
    packages <- c(packages, tmp$basePkgs)
  }
  if(loadedonly){
    packages <- c(packages, unlist(lapply(tmp$loadedOnly, `[[`, "Package")))
  }
  if(attached){
    packages <- c(packages, unlist(lapply(tmp$otherPkgs, `[[`, "Package")))
  }
  names(packages) <- NULL
  return(packages)
}


#' function returning x with modified class
#' @param x object for which class should be changed
#' @param y character vector of class names to which to switch
set_class <- function(x, y){
  class(x) <- y
  x
}

#' function to extract textVersion attribute from citation object
#' @param x citation object to get textVersion from
get_text_version <- function(x){
  class(x) <- "list"
  attr(x[[1]],"textVersion")
}

#' function to extract bibtype attribute from citation object
#' @param x citation object to get textVersion from
get_bibtype <- function(x){
  class(x) <- "list"
  attr(x[[1]],"bibtype")
}

#' function transforming citation object to bibtex string
#' @param x citation object for which to produce Bibtex string
to_bibtex_string <- function(x){
  paste0(toBibtex(x), collapse = "\n")
}


