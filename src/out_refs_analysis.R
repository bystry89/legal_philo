#library(tidyverse)

#source(here::here("src/functions/labelClust.R"))
#source(here::here("src/functions/proNSF.R"))

#nodes <- read.csv(here::here("data/cits/nodes_w=6.csv")) %>% 
#  labelClusts() %>% 
#  filter(!is.na(cluster_label))

refs_full <- read_csv(here::here("data/cits/out_refs.csv")) %>% 
  anti_join(nodes, by = c("Target" = "Id"))

#PoS_nodes <- read_csv(here::here("data/cits/PoS_nodes.csv")) 

#journals_class <- readxl::read_excel(here::here("data/cits/WoS_NSF_Classification.xlsx")) %>% 
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

PoS_refs_full <- read_csv(here::here("data/cits/PoS_outrefs.csv")) %>% 
  left_join(read_csv(here::here("data/cits/PoS_outrefs_meta.csv")), by=c("referenced_works" = "id"))

PoS_refs <- PoS_refs_full%>% 
  proNSF(PoS = T)


PoS_median_especiality_ref <- PoS_refs  %>% 
  count(id, disc) %>% 
  pivot_wider(id_cols = id, names_from = disc, values_from = n, values_fill = 0) %>% 
  summarise(across(-id, median)) 

T_ref_PoS <- PoS_refs_full %>% 
  left_join(PoS_nodes, by = c("id" = "id")) %>%
  mutate(age = publication_year.y - publication_year.x,
         Community = "PoS") %>%
  filter(age>-1, age<100) %>% 
  bind_rows(filter(age, Community=="General jurisprudence")) %>% 
  t.test(age ~ Community, data = .)

