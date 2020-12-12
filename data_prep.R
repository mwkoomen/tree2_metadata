#data load codebook

x <- read.csv2(
  "https://raw.githubusercontent.com/mwkoomen/tree2_metadata/main/data/tree2_metadata_202012091031.csv", 
  sep=',', 
  header = T,
  encoding = "UTF-8")

test <- x %>% 
  filter(wave %in% c(0,1,2) &
           item_text_e != "n/a" &
           item_text_e != "" &
           data_collection %in% c(1,2)) %>%
  group_by(item_name,
           wave,
           item_text_e,
           data_collection,
           data_collection_a,
           mode, 
           mode_a,
           module,
           subsample) %>%
  tally() %>%
  select(item_name,
         wave,
         item_text_e,
         data_collection,
         data_collection_a,
         mode,
         mode_a,
         module,
         subsample)

s <- "wave %in% c(0)"

test2 <- assign(s, filter(x,))