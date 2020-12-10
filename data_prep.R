#data load codebook

x <- read.csv2(
  "https://raw.githubusercontent.com/mwkoomen/tree2_metadata/main/data/tree2_metadata_202012091031.csv", 
  sep=',', 
  header = T,
  encoding = "UTF-8")
