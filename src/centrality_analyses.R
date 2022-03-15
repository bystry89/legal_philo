library(tidyverse)
source("labelClust.R")

nodes <- read.csv("cmp_nodes.csv") %>% labelClusts() 
works <- read_csv("works_full.csv")
authors <- read.csv("authors_full.csv")
authors_list <- authors %>% 
  select(id, name) %>% 
  distinct(id, .keep_all = T)
nodes %>% 
  left_join(authors, by = c("id" = "item")) %>% 
  group_by(id.y) %>% summarise(sumEig = sum(eigen)) %>%
  arrange(desc(sumEig)) %>% 
  left_join(authors_list, by =c("id.y"="id")) %>% 
  select(name, sumEig) %>% head(20)
  

nodes %>% left_join(works) %>% 
arrange(desc(eigen)) %>% 
  select(first_author, display_name, eigen) %>%
  head(20)
