#1st get some example keys
k <- head(keys(org.Hs.eg.db,keytype="ACCNUM"))
# then call mapIds
mapIds(org.Hs.eg.db, keys=k, column="ENZYME", keytype="ACCNUM")


library(EnsDb.Hsapiens.v86)
edb <- EnsDb.Hsapiens.v86
edb

## Retrieve all gene IDs of all lincRNAs encoded on chromosome Y
linkY <- keys(edb,
              filter=list(GeneBiotypeFilter("lincRNA"), SeqNameFilter("Y")))

linkY <- keys(edb,
              filter=list(SeqNameFilter("X")))
length(linkY)

txs <-
  select(
    edb,
    keys = linkY,
    columns = c("GENENAME", "TXID", "TXSEQSTART", "TXBIOTYPE"),
    keytype = "GENEID"
  )

nrow(txs)


tmp <- homologene::human2mouse(txs$GENENAME)
tmp <- subset.data.frame(x = tmp , select = c("humanGene","mouseGene"),drop = F)
resultGeneInX <- merge.data.frame(x = txs, y = tmp , by.x = "GENENAME", by.y = "humanGene",all.x = T)

write.csv(x = resultGeneInX, file = "Homologous_genes_inX_chromo_both_human_and_mouse.csv")
