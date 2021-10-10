library(data.table)
library(usethis)
load("data-raw/genome.rda")
use_data(hg19, hg38, overwrite = T)
