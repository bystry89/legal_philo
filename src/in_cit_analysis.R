library(tidyverse)
library(igraph)

inrefs <- inrefs %>% 
  rename(cit = Source, work = Target) %>% 
  left_join(select(nodes, Id, cluster_label), by = c("work" = "Id")) %>% 
  rename(Source = cluster_label) %>% 
  left_join(select(nodes, Id, cluster_label), by = c("cit" = "Id")) %>% 
  rename(Target= cluster_label)

mean_cited <- inrefs %>% 
  group_by(work) %>% 
  summarise(out=sum(Source!=Target), n=n()) %>% 
  left_join(nodes, by = c("work" = "Id")) %>% 
  group_by(cluster_label) %>% 
  summarise(out = mean(out, na.rm=T), n = mean(n)) %>% 
  mutate(ratio = out/n) %>% 
  filter(!is.na(cluster_label)) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  arrange(desc(ratio))

mean_citing <- inrefs %>% 
  group_by(cit) %>% 
  summarise(out=sum(Source!=Target), n=n()) %>% 
  left_join(nodes, by = c("cit" = "Id")) %>% 
  group_by(cluster_label) %>% 
  summarise(out = mean(out, na.rm=T), n = mean(n)) %>% 
  mutate(ratio = out/n) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  filter(!is.na(cluster_label)) %>% 
  arrange(desc(ratio))


net_cits <- mean_cited %>% left_join(mean_citing, by = "cluster_label") %>% 
  mutate(net = out.x - out.y) %>% arrange(desc(net)) %>% 
  rename(references = out.y,
         cites = out.x) %>% 
  dplyr::select(cluster_label, references, cites, net)


# cits %>% filter(Target == "General jurisprudence", Source != "General jurisprudence") %>% 
#   group_by(work) 
