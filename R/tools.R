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





