#data prep codebook

library(dplyr)

x <- read.csv2(
  "https://raw.githubusercontent.com/mwkoomen/tree2_metadata/main/data/tree2_metadata_202012091031.csv", 
  sep=',', 
  header = T,
  encoding = "UTF-8")

test <- as.data.frame(x %>% 
  filter(wave==0 & item_text_e != "n/a") %>%
  group_by(item_name, wave, item_text_e) %>%
  tally() %>%
  select(item_name, wave))

