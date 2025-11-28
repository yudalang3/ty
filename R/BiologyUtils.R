


#' Get the gene information from partial gene name description from organism mouse.
#'
#' @param str partial description of the gene. E.g. transcript factor
#'
#' @return the df of the result genes
#' @export
#'
#' @examples
#' getGeneInfoFromPartialGeneNameDesc_mouse('Albumin')
getGeneInfoFromPartialGeneNameDesc_mouse <- function(str) {
  # 小鼠中的情况
  library(org.Mm.eg.db)
  ids <- toTable(org.Mm.egGENENAME)
  all_factors <- ids[grepl(str, ids$gene_name, ignore.case = T), ]
  col_to_look <- c("SYMBOL", "GENETYPE", "GENENAME")
  annotation_tf_fac <-
    select(
      org.Mm.eg.db,
      keys = all_factors$gene_id,
      keytype = "ENTREZID",
      columns = col_to_look
    )
  return(annotation_tf_fac)
}
