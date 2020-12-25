#data load codebook
library(dplyr)

x <- read.csv2(
  "suf_view_short_202012151737.csv", 
  sep=',', 
  header = T,
  encoding = "UTF-8")

x <- read.csv2(
  "https://raw.githubusercontent.com/mwkoomen/tree2_metadata/main/data/test.csv",
  sep=',',
  header = T,
  encoding = "UTF-8")

test <- x %>% filter(item_id==1) %>% group_by(wave) %>% tally() %>% select(wave) %>% pull(wave)
if (0 %in% test$wave){
  if (1 %in% test$wave) {
    if (2 %in% test$wave) {
      cat("Variable in waves: 0,1,2")
    }
    else cat("Variable in waves: 0,1")
  }
  else if (2 %in% test$wave) {
    cat("Variable in wave 0")
  }
}


test3 <- as.array(1)
test3

test4 <- dplyr::pull(test, wave)
