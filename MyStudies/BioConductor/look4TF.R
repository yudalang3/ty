# 载入org.Hs.eg.db包，该包提供了与人类基因组有关的注释信息
library(org.Hs.eg.db)
library(magrittr)

# 使用toTable函数获取org.Hs.egGENENAME表格，其中包含了基因ID和基因名的对应关系
ids <- toTable(org.Hs.egGENENAME)

all_factors <- ids[grepl('transcription factor', ids$gene_name),]

col_to_look <- c("SYMBOL","GENETYPE","GENENAME")
annotation_tf_fac <- select(org.Hs.eg.db, keys = all_factors$gene_id, keytype = "ENTREZID", columns = col_to_look)

result_tf_human <-
  merge.data.frame(all_factors, annotation_tf_fac , by.x = 'gene_id', by.y = 'ENTREZID')

head_of_allComn <- local({
  columns(org.Hs.eg.db)
  # [1] "ACCNUM"       "ALIAS"        "ENSEMBL"      "ENSEMBLPROT"
  # [5] "ENSEMBLTRANS" "ENTREZID"     "ENZYME"       "EVIDENCE"
  # [9] "EVIDENCEALL"  "GENENAME"     "GENETYPE"     "GO"
  # [13] "GOALL"        "IPI"          "MAP"          "OMIM"
  # [17] "ONTOLOGY"     "ONTOLOGYALL"  "PATH"         "PFAM"
  # [21] "PMID"         "PROSITE"      "REFSEQ"       "SYMBOL"
  # [25] "UCSCKG"       "UNIPROT"

  keytypes(org.Hs.eg.db)

  getTheHeaderOfEachKeytype <- function(x) {
    key <- keys( org.Hs.eg.db, keytype = x ) %>% head()
    list(key = key, key_name = x)
  }
  tmp <-
    purrr::map(keytypes(org.Hs.eg.db),
               getTheHeaderOfEachKeytype ,
               .progress = T)

  #更加优雅地写法
  inter_list <- lapply(tmp, function(x){
    name <- x$key_name;
    x$key_name <- NULL;
    return(setNames(x, name))
  })
  data.frame(inter_list) %>% View(.)
  #完美解决
})


