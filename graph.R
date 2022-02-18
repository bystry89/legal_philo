library(readr)
library(tidyverse)
library(igraph)
authors_full <- read_csv("authors_full.csv")
inst_full <- read_csv("insts_full.csv")
View(authors_full)

pairs <- authors_full %>% 
  select(item, id) %>% 
  inner_join(select(authors_full, item, id), by="item") %>% 
  filter(id.x != id.y) %>% 
  count(id.x, id.y) %>% 
  filter(!duplicated(t(apply(., 1, sort)))) %>% 
  arrange(desc(n)) 
#%>% 
 # mutate(id=1:nrow(.))

pairs <- pairs[,c(4,1,2,3)]


nodes <- authors_full %>% select(id, name) %>% 
  filter(!duplicated(t(apply(., 1, sort)))) %>% 
  filter(id %in% pairs$id.x | id %in% pairs$id.y)


graph <- graph_from_data_frame(pairs,vertices=nodes,  directed = F)

plot(graph)

pairs %>% 
  rename(Source=id.x, Target=id.y, weight=n) %>% write.csv("coauth_edges.csv")
nodes %>%
  rename(Label=name) %>% 
   write.csv("coauth_nodes.csv")
