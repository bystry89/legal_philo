library(tidyverse)

works <- read_csv("data/raw/works_full.csv")

works$author <- gsub("\r\n", "", works$author)
works$author <- lapply(works$author, function(x) eval(parse(text = x)))

works$country <-   sapply(works$author, function (x) x['institution_country_code'])
works$country <-sapply(works$country, function (x) x[1]) 

country <- nodes %>% 
  filter(!is.na(cluster_label)) %>% 
  left_join(works, by = c("Id" = "id")) %>% 
  filter(!is.na(country))

country %>% 
  group_by(cluster_label) %>% 
  summarize(
    #US = mean(country == "US", na.rm=T),
    English = mean(country %in% c("US", "GB", "CA", "AU", "NZ", "IE"), na.rm=T),
    non_English = mean(!country %in% c("GB", "CA", "AU", "NZ", "IE", "US"), na.rm=T)) %>% 
  mutate_at(c("English", "non_English"), round, 2) %>% 
  arrange(non_English)

country %>% 
  mutate(Community =as.factor(if_else(cluster_label == "General jurisprudence", "General jurisprudence", "Others")),
         Anglo = as.factor(if_else(country %in% c("US", "GB", "CA", "AU", "NZ", "IE"), 'yes', 'no'))) %>% 
  select(Community, Anglo) %>% table()
