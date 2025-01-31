#library(tidyverse)
#library(igraph)
source(here::here("src/functions/proNSF.R"))
#source(here::here("src/functions/labelClust.R"))

#nodes <- read_csv("data/cits/nodes_w=6.csv") %>% 
#  labelClusts() %>% 
#  filter(!is.na(cluster_label))

#work <- read.csv("data/raw/works_full.csv")
PoS_nodes <- read_csv(here::here("data/cits/PoS_nodes.csv")) 

cits_full <- read_csv(here::here("data/raw/outcits.csv"))%>%
  filter(id %in% nodes$Id) %>% 
  distinct(id, id.1, .keep_all = T) %>% 
  anti_join(nodes, by = c("id.1" = "Id"))

#journals_class <- read.csv("data/cits/scopus_class_mod.csv")
journals_class <- readxl::read_excel(here::here("data/cits/WoS_NSF_Classification.xlsx")) %>% 
  clasNSF()

cits <-   cits_full %>%
  proNSF()

median_especiality <- cits %>% left_join(nodes, by=c("id" = "Id")) %>% 
  count(id, disc) %>% 
  pivot_wider(id_cols = id, names_from = disc, values_from = n, values_fill = 0) %>% 
  left_join(select(nodes, Id, cluster_label), by = c("id" = "Id")) %>% 
  group_by(cluster_label) %>% 
  summarise(across(-id, median))

col_ord <- cits %>% count(disc, sort =T) %>% .$disc
 median_especiality <-  nodes %>% count(cluster_label) %>% 
    right_join(median_especiality) %>% 
     arrange(desc(n)) %>% 
     select(cluster_label, n, col_ord)

median_especiality <-  nodes %>% count(cluster_label) %>% 
  right_join(median_especiality) %>% 
  arrange(desc(n)) %>% 
  select(cluster_label, col_ord)

lawphil <- median_especiality %>% 
  mutate(law_phil = Law/(Philosophy)) %>% 
  select(cluster_label,Law, Philosophy, law_phil) %>% 
  arrange(law_phil)

age <- cits_full %>% 
  left_join(nodes, by = c("id" = "Id")) %>% 
  mutate(age = publication_year.x - publication_year.y) %>% 
  filter(age>-1, age<100) %>% 
  mutate(Community = if_else(cluster_label == "General jurisprudence", "General jurisprudence", "Others"))

T_cit <- t.test(age ~ Community, data = age)

T_cit_phil <- age %>% 
  proNSF() %>% 
  filter(disc == "Philosophy") %>% 
  t.test(age ~ Community, data=.)

T_cit_law <- age %>% 
  proNSF() %>% 
  filter(disc == "Law") %>% 
  t.test(age ~ Community, data=.)

PoS_cits_full <- read_csv(here::here("data/cits/PoS_outcits.csv")) %>%
  left_join(read_csv(here::here("data/cits/PoS_outcits_meta.csv"))) 

PoS_cits <- PoS_cits_full%>%
  proNSF(PoS=TRUE)

PoS_median_especiality <- PoS_cits %>% 
  count(Target, disc) %>% 
  pivot_wider(id_cols = Target, names_from = disc, values_from = n, values_fill = 0) %>% 
  summarise(across(-Target, median))

T_cit_PoS <- PoS_cits_full %>% 
  left_join(PoS_nodes, by = c("Target" = "id")) %>%
  mutate(age = publication_year.x - publication_year.y,
         Community = "PoS") %>%
  filter(age>-1, age<100) %>% 
  bind_rows(filter(age, Community=="General jurisprudence")) %>% 
  t.test(age ~ Community, data = .)
  