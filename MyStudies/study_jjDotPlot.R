library(scRNAtoolVis)

httest <- system.file("extdata", "htdata.RDS", package = "scRNAtoolVis")
pbmc <- readRDS(httest)

# 这个操作符默认会 对pbmc@meta.data这个数据框进行操作
# add groups
pbmc$groups <- rep(c('stim','control'),each = 1319)
# add celltype
pbmc$celltype <- Seurat::Idents(pbmc)

# load markergene
data("top3pbmc.markers")

# check
head(top3pbmc.markers,3)
# # A tibble: 3 x 7
# # Groups:   cluster [1]
#     vvp_val avg_log2FC pct.1 pct.2 p_val_adj cluster     gene
#       <dbl>      <dbl> <dbl> <dbl>     <dbl> <fct>       <chr>
# 1 1.74e-109       1.07 0.897 0.593 2.39e-105 Naive CD4 T LDHB
# 2 1.17e- 83       1.33 0.435 0.108 1.60e- 79 Naive CD4 T CCR7
# 3 3.28e- 49       1.05 0.333 0.103 4.50e- 45 Naive CD4 T LEF1

# 这里需要有两列，列名分别为 gene 和 cluster
annot_markerGene <- head(top3pbmc.markers, n = 3)[, 1:10]
# 下面开始绘制了
jjDotPlot(object = pbmc,
          markerGene = annot_markerGene,
          anno = T,
          plot.margin = c(3,1,1,1))
