# 载入org.Hs.eg.db包，该包提供了与人类基因组有关的注释信息
library(org.Hs.eg.db)
library(magrittr)

# 使用toTable函数获取org.Hs.egGENENAME表格，其中包含了基因ID和基因名的对应关系
ids <- toTable(org.Hs.egGENENAME)

# 查看ids表格的前几行
head(ids)

# 从ids表格中筛选出包含关键词"kinase"的所有基因
all_kinase <- ids[grepl('kinase', ids$gene_name),]

# 从all_kinase表格中进一步筛选出包含关键词"tyrosine"的所有基因，即酪氨酸激酶
all_tyrosine_kinase <- all_kinase[grepl('tyrosine', all_kinase$gene_name),]

# 从all_tyrosine_kinase表格中筛选出非受体酪氨酸激酶，即名称中包含关键词"non"的基因
nkt <- all_tyrosine_kinase[grepl('non', all_tyrosine_kinase$gene_name),]

# 打印非受体酪氨酸激酶的基因信息
nkt

# 从all_tyrosine_kinase表格中筛选出受体酪氨酸激酶，即名称中不包含关键词"non"的基因
rkt <- all_tyrosine_kinase[!grepl('non', all_tyrosine_kinase$gene_name),]

# 打印受体酪氨酸激酶的基因信息
rkt



# 从ids表格中筛选出包含关键词"factor"的所有基因
all_factors <- ids[grepl('transcription factor', ids$gene_name),]

