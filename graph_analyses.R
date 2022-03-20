library(tidyverse)
source("src/functions/labelClust.R")

insts <- read.csv("data/raw/insts_full.csv")
nodes_labelled <- read_csv("data/cits/nodes.csv") %>% labelClusts() %>% 
  left_join(read_csv("data/raw/works_full.csv"))


nodes_labelled %>% 
  group_by(cluster_label) %>% 
  summarize(year = median(publication_year)) %>% 
  filter(!is.na(cluster_label)) %>% 
  arrange(year)


nodes_labelled %>% 
  filter(!is.na(cluster_label)) %>% 
  left_join(insts, by = c("id" = "item")) %>% 
  group_by(cluster_label) %>% 
  summarize(US = mean(country_code == "US", na.rm=T),
            non_US_English = mean(country_code %in% c("GB", "CA", "AU", "NZ", "IE"), na.rm=T),
            other = mean(!country_code %in% c("GB", "CA", "AU", "NZ", "IE", "US"), na.rm=T)) %>% 
  mutate_at(vars(select(.,-cluster_label)), round, 2)
  View()

nodes_labelled %>% 
  group_by(cluster_label) %>% 
  summarise(mean_eigen = mean(eigen)) %>% 
  arrange(desc(mean_eigen))

nodes %>% 
  left_join(works) %>% 
  group_by(type) %>% 
  summarize(n = n(), mean_eigen = mean(eigen)) %>% 
  filter(n>20) %>% 
  arrange(desc(mean_eigen))

nodes %>% 
  left_join(works) %>% 
  filter(publication_year>1960) %>% 
  filter(type %in% c("book", "journal-article")) %>% 
  lm(eigen ~ type*publication_year, data = .) %>% summary()

nodes %>% 
  left_join(works) %>% 
  filter(publication_year>1960, cluster_label!="Argumentation") %>% 
  filter(type %in% c("book", "journal-article")) %>% 
  ggplot(aes(x=publication_year, y=eigen, color= type)) +
  geom_smooth(method = "lm")
