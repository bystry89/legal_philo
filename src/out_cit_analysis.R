library(tidyverse)
library(igraph)
source("src/functions/labelClust.R")

nodes <- read.csv("data/cits/nodes.csv") %>% 
  labelClusts() %>% 
  filter(!is.na(cluster_label))

work <- read.csv("data/raw/works_full.csv")


cits <- read.csv("data/raw/cits.csv") %>% 
  filter(work %in% nodes$id) %>% 
  distinct(work, cit, .keep_all = T)

journals_class <- read.csv("data/cits/scopus_class_mod.csv")


by_area <- read.csv("data/cits/out_cits.csv") %>% 
  inner_join(journals_class, by="issn") %>%
  group_by(work) %>% 
  mutate(weight = 1/n()) %>% 
  ungroup() %>% select(-X.x, -X.1, -X.y) %>% 
  right_join(cits, by = c("work" = "cit")) %>% 
  rename(Source = work.y,
         Target = work) %>% select(-...1, -group) %>% filter(!is.na(area2)) %>% 
  left_join(nodes, by  = c("Source" = "id"))
 
  
by_area %>% 
  group_by(cluster_label, area2) %>% 
  summarise(sum=sum(weight)) %>% 
  #mutate(cluster_label = forcats::fct_reorder(cluster_label, sum(sum))) %>% 
  ggplot(aes(x=area2, y=sum, fill=cluster_label))+
  geom_bar(stat = "identity") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 40, size = 10, vjust = 1, hjust = 1), axis.title = element_blank())

  by_area %>% 
    group_by(cluster_label, area2) %>% 
    summarise(sum=sum(weight)) %>% 
    pivot_wider(names_from = cluster_label, values_from = sum) 
  
  
by_area %>% 
  filter(cluster_label == "General jurisprudence") %>% 
  dplyr::select(Source, area2, weight) %>% 
  pivot_wider(names_from = area2, values_from = weight, values_fn = sum) %>% 
  ungroup() %>%
  mutate(across(where(is.numeric))/rowSums(across(where(is.numeric)))) %>% 
  summarise(across(where(is.numeric), mean, na.rm=T))


clusters <- unique(by_area$cluster_label)
tbl <- list()
for (i in 1:length(clusters)) {
tbl <- by_area %>% 
  filter(cluster_label == clusters[i]) %>% 
  dplyr::select(Source, area2, weight) %>% 
  pivot_wider(names_from = area2, values_from = weight, values_fn = sum) %>% 
  ungroup() %>%
  mutate(across(where(is.numeric))/rowSums(across(where(is.numeric)))) %>% 
  summarise(across(where(is.numeric), mean, na.rm=T)) %>% bind_cols(data.frame(cluster=clusters[i])) %>% 
  bind_rows(tbl)
}
