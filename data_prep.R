#data load codebook
library(dplyr)

x <- read.csv2(
  "suf_view_short_202012151737.csv", 
  sep=',', 
  header = T,
  encoding = "UTF-8")
