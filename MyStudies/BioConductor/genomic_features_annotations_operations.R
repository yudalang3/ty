### 1.Bioconductor注释资源
library(BSgenome.Hsapiens.UCSC.hg38)  #人类的全基因组序列
library(TxDb.Hsapiens.UCSC.hg38.knownGene)  #人类基因组注释,位置信息(GenomicFeatures)

# GenomicFeatures:序列特征,如gene model,genes、exons、UTRs、transcripts

# gene model:基因模型,被定义为基因产物的描述,覆盖认为是基因的核酸区域


### 2.基因区间RangeData
## 2.1 IRanges 存储区间信息 [start,end]
#两套坐标基准：0-based [start,end)，1-based [start,end]
#IRanges() 指定start、end、width三者之二
IRanges(start=4,end=13)
IRanges(start=4,width=3)
x <- IRanges(start=c(10, 20, 50), end=c(30, 40, 60))
x #创建了一个IRanges对象
str(x) #IRanges类,6个slots
length(x) #长度,行数
names(x) <- paste0("gene",1:3) #指定每个区间的名称
names(x)
start(x)  #end(x)  width(x)
restrict(x,20,40) #截取部分区间(restrict限制,约束)
flank(x,width=7) #上游/左侧,start=F下游(Flank侧面)
x+4L #上、下游同时增加4bp
shift(x,shift=10)  #向右平移10bp
range(x) #总区间,从最小start到最大end
reduce(x) #将重叠区域缩减为1个,得到总覆盖的几个大区域
gaps(x) #几个大区域之间的间区gap
x1 <- x
end(x1) <- end(x1) + 4
x1[start(x1) < 50]
y <- x1[1:2]
c(x,y) #简单合并
intersect(x,y) #交集,xy共有的
union(x,y) #并集,x或y有的
setdiff(x,y) #补集,x中有y中没有的部分





## 2.2 GRanges 基因区间的扩展
#坐标三要素：染色体/序列名称，区间，正负链。
#GRanges为IRanges+染色体+正负链信息
#至少需要三要素：seqname(seq)、ranges(=IRanges)、strand(+/-/*)
#GRanges() 指定seq、ranges(=IRanges)、strand | seqlengths、metadata
gr1 <- GRanges(seq = c("chr1","chr2","chr1"),
               ranges = IRanges(start=c(10, 20, 50), end=c(30, 40, 60)),
               strand = "+")
gr1 #创建了一个GRanges对象
gr2 <- GRanges(seq = c("chr1","chr3"),
               ranges = y,
               strand = "-",
               gc = seq(10,70,length.out = 2),
               seqlengths = c(chr1=150,chr2=200,chr3=250))
gr2 #增加的信息"gc"将作为metadata column，seqlengths为seqinfo
str(gr2) #GRanges类,7个slots
#seqnames(又含有4个slots),ranges(又含有6个slots),strand,seqinfo,metadata...
names(gr2) #每行的名称
length(gr2) #行数
seqlengths(gr2) #序列长度
seqnames(gr2) #序列名称,Rle类
strand(gr2)
ranges(gr2) #IRanges对象 #gr2@ranges #start(gr2) end(gr2) width(gr2)
mcols(gr2) #metadata columns数据框
gr2$gc #gc向量
gr2[seqnames(gr2)=="chr1"] #序列名称为chr1的行
sort(c(gr1,gr2)) #按基因组顺序排序(先染色体;再位置;正链+,负链-,不分链*)
gr2[order(gr2$gc)]  #按某一列从小到大进行排序,decreasing=T从大到小

gr1
gr2
#下面的操作得到的都是GRanges对象
restrict(gr2,20,40) #截取部分区间
flank(gr2,width=10) #上游,-链是右侧,start=F下游
gr2+4L #上、下游同时增加4bp
shift(gr2,shift=10)  #正负链都向染色体下游平移(正向平移)
range(gr1) #总区间,从最小start到最大end,相同seqnames才能合并
reduce(gr2) #将重叠区域缩减为1个,得到总覆盖的几个大区域
gaps(gr2) #基因间区gap,+/-/*链上所有没有基因覆盖的区间
intersect(gr1,gr2) #交集,共有的,正负链是不同的区间
union(gr1,gr2) #并集
setdiff(gr1,gr2) #补集,gr1中有gr2中没有的部分



## 2.3 GRangesList
#本质是列表，取子集用[]，取子集中的元素用[[]]
GRangesList(gr1, gr2) #创建一个包含2个元素的GRangesList
gr_split <- split(gr2,seqnames(gr2))  #按seqnames拆分GRanges成GRangesList
gr_split #包含3个元素的GRangesList(CompressedGRangesList类)
length(gr_split) #元素数目
names(gr_split) #子集名称,即seqnames
gr_split[[1]] #取第一个子集
unsplit(gr_split,seqnames(gr2)) #拆分后补回来还和gr2一样,GRanges对象
unlist(gr_split) #拆分后合并在一起,GRanges对象,行名不同了
c(gr_split,gr_split) #简单合并
unique(gr_split)  #和gr_split相同

gr3 <- c(gr1,gr2); gr3 #GRanges
gr4 <- split(gr3,seqnames(gr3)); gr4 #GRangesList
#下面的操作得到的都是GRangesList对象
restrict(gr4,20,40) #对GRangesList每一个元素(GRanges)的每一行截取部分区间
flank(gr4,width=10) #对每一个元素的每一行取上游10bp,start=F下游,不能小于0
gr4+4L #上、下游同时增加4bp
shift(gr4,shift=-10)  #正负链都向染色体上游平移(反向平移)
range(gr4) #总区间,每一个元素从最小start到最大end,正负链是不同的区间
reduce(gr4) #将重叠区域缩减为1个,得到总覆盖的几个大区域
#不能使用gaps()
gr5 <- c(gr4[1],gr4[1:2]); gr5
names(gr5) <- c("chr3","chr2","chr1") #并没有什么用
#gr4,gr5必须是相同长度的,下面的比较不会看元素名称,而是按元素顺序进行比较
intersect(gr4,gr5) #交集,共有的
union(gr4,gr5) #并集
setdiff(gr4,gr5) #补集,gr4中有gr5中没有的部分



## 2.4 Rle
# Rle类存储以运行长度编码格式(run-length encoding format)存储的原子向量
# Rle对象用值value+长度length两个属性来表示原向量,value和length为原子向量
chr.str <- c(rep("Chr1",10),rep("Chr2",20),rep("Chr1",5))
chr.str
chr.rle <- Rle(chr.str)
chr.rle  #表示为Chr1重复10次，Chr2重复20次，Chr1重复5次。
identical(as.vector(chr.rle),chr.str)  #Rle对象向量化后和原向量是完全相同的


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


## 3.2 DNAString 获取序列
chr22 <- getSeq(hg38,"chr22")  #hg38$chr22  #chr22<-Hsapiens[["chr22"]]
chr22 #得到一个DNAString实例
length(chr22) #核苷酸数

str1 = getSeq(hg38,"chr22",start=1,end=30)  #str1<-hg38[["chr22"]][1:30]
str1 #等同于chr22[1:30]  #chr22[IRanges(start=1,width=30)]
#得到DNAStringSet的getSeq(hg38,GRanges(seq="chr22",ranges=IRanges(start=1,end=30), strand="*"))
str2 = DNAString("TACCCTAACCCTAACTAACCCTAA")
str2
reverse(str2) #反向序列,直接反过来
reverseComplement(str2) #反向互补
translate(str2) #翻译成氨基酸,得到AAString实例
alphabetFrequency(str2) #每个碱基出现次数


## 3.3 DNAStringSet
irs <- IRanges(start=35310335,end=35395968)
grs <- GRanges(seqnames = "chr6",ranges = irs, strand = "*")
grs == GRanges("chr6:35310335-35395968")
seq <- getSeq(Hsapiens,grs)
seq  #DNAStringSet类,有1个元素
seq[[1]][100:200]

dna <- DNAStringSet(c("AAACTG", "CCCTTCAAC", "TACGAA"))
dna  #直接创建一个DNAStringSet实例


### 4.TxDb基因组注释数据库(GenomicFeatures)
## 4.1 TxDb
txdb <- TxDb.Hsapiens.UCSC.hg38.knownGene
txdb
## 4.2 设置活动的染色体
ias <- isActiveSeq(txdb)
t <- c(rep(FALSE,21),TRUE,rep(FALSE,length(ias)-22))
names(t) <- names(ias)
isActiveSeq(txdb) <- t #只保留22号染色体


## 4.3 genes/transcripts/exons 提取基因/转录本/外显子
tx1 <- transcripts(txdb)
tx1  #提取所有转录本的坐标,得到GRanges对象

## 4.4 transcriptsBy/cdsBy/exonsBy 提取基因组特征
#cdsBy(x, by=c("tx"), use.names=T)
#transcriptsBy/exonsBy/cdsBy/intronsByTranscript/fiveUTRsByTranscript/threeUTRsByTranscript
#x为TxDb对象;by为"gene", "tx","exon", "cds"之一
#得到GRangesList
txs <- transcriptsBy(txdb,by="gene") #按照基因提取转录本
txs #得到GRangesList,每一个元素是一个基因模型GRanges
exs <- exonsBy(txdb,by="tx",use.name=T) #按照转录本提取外显子
exs #每一个元素是一个转录本,包含多个外显子
cds1 <- cdsBy(txdb,by="tx",use.names=T) #按照转录本提取CDS
cds1
cds2 <- cds1[strand(cds1)=="+"][1:10] #取GRangesList正链上的10个元素

#These functions return all the features of a given type
#grouped by another feature type in a GRangesList object.
#use.names=F default,the group names are the internal ids of the features used for grouping
#返回列表的元素名,为用于分组的特征的独有ID,确保是唯一的
#use.names=T,the names of the grouping features are used instead of their internal ids
#返回列表的元素名,为用于分组的特征的名称,不一定是唯一的,甚至可能是NA
#by="gene"不能用use.names=T,因为基因ID是外部ID,txdb中没有"gene_name"栏


## 4.5 extractTranscriptSeqs 获取序列
#extractTranscriptSeqs(x, transcripts/cds, ...)
#x为BSgenome对象（或DNAString对象）
#transcripts/cds为GRangesList对象
cds.seqs <- extractTranscriptSeqs(Hsapiens,cds2)
cds.seqs #DNAStringSet

## 4.6 导出为fasta格式
writeXStringSet(cds.seqs,filepath="cds.fasta",format="fasta")
