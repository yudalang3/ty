# 载入org.Hs.eg.db包，该包提供了与人类基因组有关的注释信息
library(org.Hs.eg.db)
library(magrittr)

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
  data.frame(inter_list)
  #完美解决
})

write.csv(x = head_of_allComn, file = 'C:\\Users\\yudal\\Documents\\temp\\head.csv')


# 想查看一下 肝细胞的标记基因
# 在肝细胞的标记基因中，以下是一些常用的例子：
#
# Albumin（白蛋白）：白蛋白是肝脏合成并分泌的主要蛋白质，也是血浆中最丰富的蛋白质之一。在肝细胞中，白蛋白基因（ALB）的表达非常高，因此白蛋白被广泛用作肝细胞的标记基因。
# Alb
# Cytochrome P450家族成员：肝细胞是解毒过程中的重要细胞类型。肝细胞中存在多种细胞色素P450家族成员，它们参与药物代谢和化学物质解毒。不同的细胞色素P450成员可以作为肝细胞的标记基因，例如CYP1A2、CYP2E1、CYP3A4等。
# Cyp1a2
# TAT（Tyrosine aminotransferase，酪氨酸氨基转移酶）：TAT是肝细胞中一个关键的酶，参与酪氨酸代谢的转氨过程。TAT在肝脏中高度表达，因此可以用作肝细胞的标记基因。
# Tat
# CPS1（Carbamoyl-phosphate synthase 1，碳酰磷酸合成酶1）：CPS1是尿素循环中的一种重要酶，在肝细胞中高表达。因此，CPS1也常用作肝细胞的标记基因。
# Cps1
# 这些标记基因在单细胞RNA测序等研究中被广泛用于识别和分析肝细胞，有助于了解肝脏功能和生理学过程，以及在肝脏疾病研究中的应用。值得注意的是，具体使用哪些标记基因可能会根据研究的具体目的和实验设计而有所不同。
# 使用toTable函数获取org.Hs.egGENENAME表格，其中包含了基因ID和基因名的对应关系
ids <- toTable(org.Hs.egGENENAME)

all_factors <- ids[grepl('Carbamoyl-phosphate synthase', ids$gene_name,ignore.case = T),]
all_factors <- ids[grepl('Albumin', ids$gene_name,ignore.case = T),]

col_to_look <- c("SYMBOL","GENETYPE","GENENAME")
annotation_tf_fac <- select(org.Hs.eg.db, keys = all_factors$gene_id, keytype = "ENTREZID", columns = col_to_look)

result_tf_human <-
  merge.data.frame(all_factors, annotation_tf_fac , by.x = 'gene_id', by.y = 'ENTREZID')


a <- getGeneInfoFromPartialGeneNameDesc_mouse('Carbamoyl-phosphate')
