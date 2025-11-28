library(org.Hs.eg.db)
library(TxDb.Hsapiens.UCSC.hg38.knownGene)
library(BSgenome.Hsapiens.UCSC.hg38)

#BiocManager::install(c("TxDb.Hsapiens.UCSC.hg38.knownGene", "BSgenome.Hsapiens.UCSC.hg38"))
gene_symbols_of_interest <- c("TGFB1", "TGFB2", "TGFB3",)
gene_df_of_interest <- data.frame(gene_symbol = gene_symbols_of_interest)
gene_df_of_interest$entrezID = mapIds(org.Hs.eg.db, keys = gene_symbols_of_interest, keytype = 'SYMBOL' , column = 'ENTREZID')

### 3.基因序列BSgenome
## 3.1 BSgenome
hg38 <- BSgenome.Hsapiens.UCSC.hg38
hg38  #物种、提供者、版本、发布日期、发布标准名称、染色体名称
#hg38等同于Hsapiens
length(hg38) #染色体/序列数目
seqinfo(hg38) #序列信息  #Hsapiens@seqinfo
#seqinfo有4个槽(名称seqnames/长度seqlengths/环形is_circular/genome)
seqnames(hg38) #染色体/序列名称  #hg38@seqinfo@seqnames
seqlengths(hg38)  #各染色体的长度  #hg38@seqinfo@seqlengths


chr22 <- getSeq(hg38,"chr22") hg38$chr22

getSeq(hg38,"chr22",start=1,end=30)



txdb <- TxDb.Hsapiens.UCSC.hg38.knownGene
gene=genes(txdb)#提取23056个基因信息，数据以Grangs格式显示.
exon=exons(txdb) #提取exons信息，共289969个exon。
trans=transcripts(txdb) #提取转录本信息，共82960个转录本。



cds=cds(txdb)#获取cds区域信息，提取到237533个cds信息。


#我们来提取目标基因的一些序列
## 设置活动的染色体
### 注意：完整的染色体数量有 25
## chr1-22, 加上X, Y 和 M
ias <- isActiveSeq(txdb)
t <- vector(mode = "logical", length = length(ias));
t[1:25] <- T
names(t) <- names(ias)
isActiveSeq(txdb) <- t #只保留22号染色体
cds=cds(txdb)
cds = cdsBy(txdb, by="gene")


columns(org.Hs.eg.db)
keytypes(org.Hs.eg.db)
keys(org.Hs.eg.db)

intervals <- cds[gene_df_of_interest$entrezID]
getSeq(hg38,intervals[[3]])

extractTranscriptSeqs(hg38 , cds[4])


range1 <- GRanges("chr19", IRanges(start=41331052,end=41331210), strand="-")
range1

getSeq(hg38, range1)





cds <- cds(txdb, filter=list(SYMBOL="APC"))

x <- getSeq(hg38, cds[1])








###
BiocManager::install("TxDb.Hsapiens.UCSC.hg19.knownGene")
BiocManager::install("BSgenome.Hsapiens.UCSC.hg19")

library(TxDb.Hsapiens.UCSC.hg19.knownGene)
txdb <- TxDb.Hsapiens.UCSC.hg38.knownGene

eid <- select(org.Hs.eg.db, "BRCA1", "ENTREZID", "SYMBOL")[["ENTREZID"]]
eid <- select(org.Hs.eg.db, "TGFB2", "ENTREZID", "SYMBOL")[["ENTREZID"]]
txid <- select(txdb, eid, "TXNAME", "GENEID")[["TXNAME"]]
length(txid)
cds <- cdsBy(txdb, by="tx", use.names=TRUE)

brca1cds <- cds[names(cds) %in% txid]
length(brca1cds)
class(brca1cds)

cdswidth <- width(brca1cds)             # width of each exon
all((sum(cdswidth) %% 3) == 0)          # sum within cds, modulus 3


require(Gviz)
anno <- AnnotationTrack(brca1cds)
plotTracks(list(GenomeAxisTrack(), anno))


tx_seq <- extractTranscriptSeqs(hg19, brca1cds)
tx_seq

library(BSgenome.Hsapiens.UCSC.hg19)
genome <- BSgenome.Hsapiens.UCSC.hg19
genome <- BSgenome.Hsapiens.UCSC.hg38
tx_seq <- extractTranscriptSeqs(genome, brca1cds)
tx_seq
