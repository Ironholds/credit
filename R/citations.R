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

citations <- function(
  packages = NULL,
  type     = c("text", "bibtex", "raw", "html","markdown", "list"),
  file     = NULL
){
  # use only the first type
  type <- type[1]
  if( !is.null(file) ){
    if(type=="bibtex"){
      return <- function(x){
        writeLines(
          paste0(x, collapse = ", \n"),
          file
        )
      }
    }else{
      return <- function(x){
        writeLines(x, file)
      }
    }
  }
  # provide package names if none are provided
  if( is.null(packages) ){
    packages <- packages_in_use()
  }
  # doing duty to do
  cit <- suppressWarnings(lapply(packages, function(x){try(citation(x))} ))
  # generating output
  if(       type == "text"     ){
    cit <- unlist(lapply(cit, get_text_version))
    return(cit)
  }else if( type == "raw"      ){
    return(cit)
  }else if( type == "bibtex"   ){
    cit <- unlist(lapply(cit, to_bibtex_string))
    return(cit)
  }else if( type == "markdown" ){
    warning("ToBeDone")
    return("ToBeDone")
  }else if( type == "html"     ){
    warning("ToBeDone")
    return("ToBeDone")
  }else if( type == "list"     ){
    text_version <- lapply(cit, get_text_version)
    bibtype      <- lapply(cit, get_bibtype)
    bibtex       <- lapply(cit, to_bibtex_string)
    cit <- lapply(cit, unlist, recursive=FALSE)
    cit <- lapply(cit, set_class, "list")
    for(i in seq_along(cit)){
      cit[[i]]["bibtype"]     <- bibtype[[i]]
      cit[[i]]["textVersion"] <- text_version[[i]]
      cit[[i]]["bibtex"]      <- bibtex[[i]]
    }
    names(cit) <- packages
    return(cit)
  }
}
























