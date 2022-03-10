library(tidyverse)
source("labelClust.R")

insts <- read.csv("insts_full.csv")
nodes_labelled <- read_csv("cmp_nodes.csv") %>% labelClusts() %>% 
  left_join(read_csv("works_full.csv"))

nodes_labelled %>% 
  group_by(cluster_label) %>% 
  summarize(year = median(publication_year)) %>% 
  arrange(year)


nodes_labelled %>% 
  left_join(insts, by = c("id" = "item")) %>% 
  group_by(cluster_label) %>% 
  summarize(US = mean(country_code == "US", na.rm=T),
            non_US_English = mean(country_code %in% c("GB", "CA", "AU", "NZ", "IE"), na.rm=T),
            other = mean(!country_code %in% c("GB", "CA", "AU", "NZ", "IE", "US"), na.rm=T)) %>% 
  View()
  
