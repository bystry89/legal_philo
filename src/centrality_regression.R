#library(tidyverse)
#source("src/functions/labelClust.R")

#nodes <- read.csv("data/cits/nodes_w=6.csv") %>% labelClusts() 
PoS_nodes <- read_csv(here::here("data/cits/PoS_nodes.csv"))
#works <- read.csv("data/raw/works_full.csv")

model <- nodes %>%
  left_join(select(works, -publication_year), by = c("Id" = "id")) %>% 
  mutate(`Type of publication` = if_else(type == "book", "Book", "Others")) %>% 
  filter(publication_year > 1950) %>% 
  mutate(Community = if_else(cluster_label == "General jurisprudence", "General jurisprudence", "Others")) %>% 
  lm(eigencentrality ~ publication_year*Community*`Type of publication`, data = .)

anova <- car::Anova(model) %>% 
  broom::tidy() %>% 
  rename(`p value` = `p.value`,
         `F statistic` = statistic)

inter1 <- interactions::interact_plot(model, publication_year, Community)

inter2 <- interactions::cat_plot(model, Community, `Type of publication`)  

LP_date_cor <- nodes %>%
  left_join(select(works, -publication_year), by = c("Id" = "id")) %>% 
  mutate(`Type of publication` = if_else(type == "book", "Book", "Others")) %>% 
  filter(publication_year > 1950) %>% 
  summarise(correlation_test = list(cor.test(publication_year, eigencentrality))) %>%
  pull(correlation_test)

GJ_date_cor <- nodes %>%
  left_join(select(works, -publication_year), by = c("Id" = "id")) %>% 
  mutate(`Type of publication` = if_else(type == "book", "Book", "Others")) %>% 
  filter(publication_year > 1950, cluster_label=="General jurisprudence") %>%
  summarise(correlation_test = list(cor.test(publication_year, eigencentrality))) %>%
  pull(correlation_test)

PoS_date_cor <- PoS_nodes %>% 
  filter(publication_year > 1950) %>% 
  summarise(correlation_test = list(cor.test(publication_year, eigen))) %>%
  pull(correlation_test)


PoS_type_size <- PoS_nodes %>% 
  filter(publication_year > 1950) %>%
  mutate(`Type of publication` = if_else(type == "book", "Book", "Others")) %>%  
  effectsize::cohens_d(eigen ~ `Type of publication`, data = .)

LP_type_size <- nodes  %>%
  left_join(select(works, -publication_year), by = c("Id" = "id"))%>% 
  filter(publication_year > 1950) %>%
  mutate(`Type of publication` = if_else(type == "book", "Book", "Others")) %>% 
  effectsize::cohens_d(eigencentrality ~ `Type of publication`, data = .)

GJ_type_size <- nodes  %>%
  left_join(select(works, -publication_year), by = c("Id" = "id"))%>% 
  filter(publication_year > 1950, cluster_label == "General jurisprudence") %>%
  mutate(`Type of publication` = if_else(type == "book", "Book", "Others")) %>% 
  effectsize::cohens_d(eigencentrality ~ `Type of publication`, data = .)

