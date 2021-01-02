#data load codebook
library(dplyr)
library(remotes)
library(NestedMenu)
install.packages("devtools")
library(devtools)
install_github("shinyTree/shinyTree")

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

tabdata <- x %>%
  filter(item_text_e != "n/a" &
           grid_text_e != "n/a" &  
           item_text_e != "") %>%
  group_by(variable_name,
           theme1,
           theme2,
           theme3,
           concept_text,
           concept_text_long,
           suf_name,
           item_id, 
           item_version,
           wave,
           item_text_e,
           item_text_d,
           item_text_f,
           item_text_i,
           grid_text_e,
           grid_text_d,
           grid_text_f,
           grid_text_i,
           data_collection,
           data_collection_a,
           mode,
           mode_a,
           module,
           subsample,
           variable_type) %>%
  tally() %>%
  select(item_id,
         item_version,
         variable_name,
         wave,
         item_text_e,
         item_text_d,
         item_text_f,
         item_text_i,
         grid_text_e,
         grid_text_d,
         grid_text_f,
         grid_text_i,
         data_collection,
         data_collection_a,
         mode,
         mode_a,
         module,
         subsample,
         variable_type,
         theme1,
         theme2,
         theme3,
         concept_text,
         concept_text_long,
         suf_name
  )

theme2 <- tabdata %>% filter(theme1 %in% 'test0') %>% group_by(theme2) %>% tally() %>% select(theme2)
theme1 <- as.data.frame(tabdata$theme1)
theme1 <- rbind(data.frame(theme1=tabdata$theme1), data.frame(theme1='--- Please select a theme ---')) 

themes <- rbind(data.frame(
  theme1='--- Please select a theme ---',
  theme2='--- Please select a theme ---',
  theme3='--- Please select a theme ---'),
  data.frame(
    theme1=tabdata$theme1,
    theme2=tabdata$theme2,
    theme3=tabdata$theme3
  )
) 

theme1 <- tabdata %>% group_by(theme1) %>% tally() %>% select(theme1)

themes <- tabdata %>% group_by(theme1, theme2, theme3, concept_text_long) %>% 
  tally() %>% 
  select(theme1, theme2, theme3, concept_text_long)
