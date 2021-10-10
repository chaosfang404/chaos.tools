library(data.table)
library(usethis)
load("data-raw/genome_info.rda")
use_data(hg19, hg38, overwrite = T)
