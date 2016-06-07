#' function that returns citations for a list of packages
#'
#' @param packages if NULL the packages_in_use() function is used genarate a
#' list of all packages currently in use and work with that; if a character
#' vector of package names is supplied all those package will be cited
#'
#' @param type the return type, only first element will be used: (raw) the raw
#' values returned by citation() are returned as a list; (bibtex) a character
#' vector of BibTeX entries is returned with each entry coresponding to one
#' element; (text) a character vector of un-marked-up text is returned with
#' each element coresponding to one citation; (markdown) a character vector of
#' preformatted markdown text; (html) a character vector of preformatted html
#' text
#'
#' @export
#'

citations <- function(
  packages=NULL,
  type=c("text", "bibtex", "raw", "html","markdown")
){
  if( is.null(packages) ){
    packages <- packages_in_use()
  }
  tmp <- suppressWarnings(lapply(packages, function(x){try(citation(x))} ))
  if(type[1] == "raw" ){
    return(tmp)
  }else if(type=="bibtex"){
    tmp <- lapply(tmp, toBibtex)
    tmp <- lapply(tmp, paste0, collapse="\n")
    return(unlist(tmp))
  }else if(type=="markdown"){

  }else if(type=="html"){

  }else{

  }
}
