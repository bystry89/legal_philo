library(tidyverse)
library(igraph)
source("labelClust.R")

nodes <- read.csv("data/cits/cmp_nodes.csv") %>% 
  labelClusts() %>% 
  filter(!is.na(cluster_label))


cits <- read.csv("data/cits/cits.csv") %>% 
  filter(work %in% nodes$id, cit %in% nodes$id) %>% 
  distinct(work, cit, .keep_all = T)

cits <- cits %>% 
  left_join(select(nodes, id, cluster_label), by = c("work" = "id")) %>% 
  rename(Source = cluster_label) %>% 
  left_join(select(nodes, id, cluster_label), by = c("cit" = "id")) %>% 
  rename(Target= cluster_label)

in_cits <- cits %>% count(Source, Target, sort = T) %>% rename(weight = n) 

write.csv(in_cits, "data/cits/in_cits.csv", row.names = F)

G <- %>% 
  graph_from_data_frame(in_cits)




