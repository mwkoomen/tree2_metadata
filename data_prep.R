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
  group_by(item_id, 
           item_version,
           item_name,
           wave,
           item_text_e,
           data_collection,
           data_collection_a,
           mode, 
           mode_a,
           module,
           subsample) %>%
  tally() %>%
  select(item_id,
         item_version,
         item_name,
         wave,
         item_text_e,
         data_collection,
         data_collection_a,
         mode,
         mode_a,
         module,
         subsample)

test2 <- x %>% 
  filter(item_id==test$item_id[1] & item_version==test$item_version[1]) %>%
  select(response_id, response_value, value_text_e, value_text_d, value_text_f) 
