storage_file_path <- file.path(Sys.getenv("HOME"), ".tryR.package.vars.rds")
# storage_file_path <- tools::R_user_dir("tryR.package.vars.rds")
# Not work on Windows
#' .onLoad function
#'
#' This function is called automatically when the package is loaded.
.onLoad <- function(libname, pkgname) {
  # message("The libname is ", libname, ". The pkgname is ", pkgname)
  # message("This package is for YDL personal use...")
}


#' set The global Vars.
#' Current support is:
#'
#' hgnc_complete_genes_path
#' tmpFile
#'
#' @param varList a named vector with string key-values.
#'
#' @return no return
#' @export
#'
#' @examples
#' setGlobalVars(NULL); # Just for storage
#' setGlobalVars( list( tmpFile= 'path', hgnc_complete_genes_path = '/your/path/file.txt') )
setGlobalVars <- function(varList) {
  tryRGlobalVars <- getGlobalVars();

  if (rlang::is_empty(varList)) {
    saveRDS(tryRGlobalVars, file = storage_file_path);
  }

  var_names <- names(varList)
  if (length(var_names) != length(varList)) {
    rlang::abort(message = "Please read the help of this function.")
  }
  purrr::walk2(varList, var_names, .f = function(x,nm){
    tryRGlobalVars[[nm]] <- x;
  })
  saveRDS(tryRGlobalVars, file = storage_file_path);
}

#' Get the global values.
#' You may wonder Can we set the global values through the returned env. object?
#' No. (1) will not persist store in your desk.
#' (2) Temporary usage is enough setGlobalVars(NULL) to save.
#'
#' @return the env to store the values.
#' @export
#'
#' @examples
#' getGlobalVars()
getGlobalVars <- function() {
  # cat('storage_file_path is\t', storage_file_path)
  if( file.exists(storage_file_path) ){
    tryRGlobalVars <- readRDS(storage_file_path)
  }else {
    tryRGlobalVars <<- new.env()
    tryRGlobalVars[['hgnc_complete_genes_path']] <- NULL;
  }
  invisible(tryRGlobalVars)
}


get_hgnc_complete_geneTable_path <- function(){
  getGlobalVars()[['hgnc_complete_genes_path']]
}
