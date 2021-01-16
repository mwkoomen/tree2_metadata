#data load codebook
library(dplyr)
library(remotes)
#library(NestedMenu)
#library(D3partitionR)
library(tibble)
library(stringr)
library(DBI)
library(utf8)

# con <- dbConnect(odbc::odbc(), "PostgreSQL35W", timeout = 10)
# x <- dbGetQuery(con, "select * from tree2_metadaten.suf_view_short;")
#install.packages("devtools")
#library(devtools)
#install_github("shinyTree/shinyTree")

x <- read.csv2(
  "C:/Users/treyz/OneDrive/Documents/tree2_metadata/data/suf_view_short_202101162241.csv", 
  sep=',', 
  header = T,
  encoding = "UTF-8")
# x <- x %>% arrange(suf_name, variable_name, response_value) %>%
#   mutate(response_value=as.character(response_value))



x2 <- read.csv2(
  "C:/Users/treyz/OneDrive/Documents/tree2_metadata/data/suf_themes.csv",
  sep=',', 
  header = T,
  encoding = "UTF-8")

x <- read.csv2(
  "https://raw.githubusercontent.com/mwkoomen/tree2_metadata/main/data/test.csv",
  sep=',',
  header = T,
  encoding = "UTF-8")

t1 <- x %>% dplyr::group_by(theme_l1)%>%tally()%>%select(theme_l1)
theme1 <- as.list(t1$theme_l1)
theme_list <- list()
for (l in theme1){
    z <- x %>% dplyr::filter(theme_l1 == l) %>%
       group_by(theme_l2) %>% tally() %>% select(theme_l2)
    d <- as.list(z$theme_l2)
    u <- list()
    for (r in d){
      m <- x %>% dplyr::filter(theme_l2 == r) %>%
        group_by(theme_l3) %>% tally() %>% select(theme_l3) 
      h <- as.list(m$theme_l3)
      i <- list()
      for (n in h){
        v <- x %>% dplyr::filter(theme_l3 == n) %>%
          group_by(item_id) %>% tally() %>% select(item_id)
        t <- as.list(v$item_id)
        i[[n]] <- t
      }
      u[[r]] <- i 
    }
    theme_list[[l]] <- u 
}
rm(u,v,z,l,n,r,m,t,d,h,i)

test <- x %>% filter(format == 2)

x$response_value <- toString(x$response_value)


x <- x %>% mutate(value_text_e=ifelse(response_value==-999999, response_unit, value_text_e)) %>%
  mutate(response_value=ifelse(response_value==-999999, '', response_value))
                  
                  
