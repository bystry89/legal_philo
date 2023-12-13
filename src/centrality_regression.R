#library(tidyverse)
#source("src/functions/labelClust.R")


#nodes <- read.csv("data/cits/nodes_w=6.csv") %>% labelClusts() 

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
