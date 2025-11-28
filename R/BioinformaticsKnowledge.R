#' Constant values of the blast software output format 6 default column names
#'
#' @return a char vector
#' @export
#'
#' @examples
#' colnames(df) <- const_blast_fmt6_defaultVarNames()
const_blast_fmt6_defaultVarNames <- function() {
  # Blast软件 fmt 6 默认输出的6个列名
  #
  ret <- c("qseqid", "sseqid", "pident", "length", "mismatch", "gapopen",
                               "qstart", "qend", "sstart", "send", "evalue", "score")
  return(ret)
}

#' Constant values of the hmmer software tblout.txt format file first 7 column names.
#'
#' @return a char vector
#' @export
#'
#' @examples
#' colnames(df) <- const_hmmer_tblout_full_VarNames()
const_hmmer_tblout_full_VarNames <- function() {
  # Blast软件 fmt 6 默认输出的6个列名
  #
  ret <- c("target name", "accession1", "query name", "accession2", "E-value", "score",
           "bias")
  return(ret)
}

#' Get human all gene names table.
#'
#' @return a tibble object
#' @export
#'
#' @examples
#' const_hgnc_complete_geneTable()
const_hgnc_complete_geneTable <- function() {
  path <- get_hgnc_complete_geneTable_path();
  if (rlang::is_empty(path)) {
    rlang::abort("setGlobalVars( list( 'hgnc_complete_genes_path' = '/your/path/file.txt') );\n Please set first.")
  }
  readxl::read_excel(path = path, sheet = 1,
                     col_types = "text")
}
