library(igraph)
library(tidyverse)
library(tidytext)

nodes <- read_csv("cmp_nodes.csv")
edges <- read_csv("edges_cmp.csv")
works <- read_csv("works_full.csv")


clust_words <- unnest_tokens(nodes, 'token', 'Label') %>% 
  mutate(token = gsub("\\'s", "", token)) %>% 
  anti_join(get_stopwords(), by = c("token" = "word"))
clust_words$token <- textstem::lemmatize_strings(clust_words$token)
clust_words <- clust_words %>% count(token, Modularity.Class, sort=T)

tot_words <- clust_words %>% 
  group_by(Modularity.Class) %>% 
  summarize(total = sum(n))

clust_words <- clust_words %>% left_join(tot_words)

clust_tf_idf <-  clust_words %>% 
  bind_tf_idf(token, Modularity.Class, n)

clust_tf_idf %>% 
  filter(Modularity.Class == 1) %>% 
  arrange(desc(tf_idf)) %>% head(25)


nodes %>% filter(Modularity.Class==6) %>% 
  left_join(works, by="id") %>% 
  arrange(desc(eigen)) %>% select(id, Label, publication_year, first_author) %>% head(10)

nodes %>% count(Modularity.Class, sort=T)

authors <- read_csv("authors_full.csv") %>% distinct(item, id, .keep_all = T) %>% select(-...1, -X.1, -X)

authors %>% filter(item %in% nodes$id) %>% left_join(works, by=c("item"="id"))  %>% View()

inst <- read_csv("insts_full.csv")

inst %>% filter(item %in% nodes$id) %>% count(country_code, sort=T) %>% print(n=Inf)
