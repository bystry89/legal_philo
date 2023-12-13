library(tidyverse)

source("../src/functions/labelClust.R")
source("../src/functions/proNSF.R")

#nodes <- read.csv("../data/cits/nodes_w=6.csv") %>% 
#  labelClusts() %>% 
#  filter(!is.na(cluster_label))

refs_full <- read_csv("../data/cits/out_refs.csv") %>% 
  anti_join(nodes, by = c("Target" = "Id"))


#journals_class <- readxl::read_excel("../data/cits/WoS_NSF_Classification.xlsx") %>% 
#  clasNSF()

refs <- refs_full%>%
  filter(Source %in% nodes$Id) %>% 
  proNSF()


median_especiality_ref <- refs  %>% 
  count(Source, disc) %>% 
  pivot_wider(id_cols = Source, names_from = disc, values_from = n, values_fill = 0) %>% 
  left_join(select(nodes, Id, cluster_label), by = c("Source" = "Id")) %>% 
  group_by(cluster_label) %>% 
  summarise(across(-Source, median)) 

median_especiality_ref <-  nodes %>% count(cluster_label) %>% 
  right_join(median_especiality_ref) %>% 
  arrange(desc(n)) %>%
  select(cluster_label, Philosophy, Law, `Other Social Sciences`, `Political Science and Public Administration`, Criminology, Computers)

age <- refs_full%>%
  filter(Source %in% nodes$Id) %>% 
  left_join(select(nodes, Id, publication_year), by = c("Source" = "Id")) %>% 
  mutate(age = publication_year.y - publication_year.x) %>%  
  filter(age>-1, age<100) %>% 
  mutate(Community = if_else(cluster_label == "General jurisprudence", "General jurisprudence", "Others"))

T_ref <- t.test(age ~ Community, data = age)


T_ref_phil <- age %>% 
  proNSF() %>% 
  filter(disc == "Philosophy") %>% 
  t.test(age ~ Community, data=.)

T_ref_law <- age %>% 
  proNSF() %>% 
  filter(disc == "Law") %>% 
  t.test(age ~ Community, data=.)

