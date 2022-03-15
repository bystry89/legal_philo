library(igraph)
library(tidyverse)
library(tidytext)
library(ggplot2)

nodes <- read_csv("cmp_nodes.csv")
edges <- read_csv("edges_cmp.csv")
works <- read_csv("works_full.csv")

abstract_tokens <- read.csv("tokenized.csv") %>% 
  anti_join(get_stopwords(), by = c("term" = "word")) %>% 
  left_join(nodes, by = c("doc" = "id")) %>% 
  select(term, n, Modularity.Class) %>% 
  rename(token = term) %>% filter(nchar(token)>2)

clust_words <- unnest_tokens(nodes, 'token', 'Label') %>% 
  mutate(token = gsub("\\'s", "", token)) %>% 
  mutate(token = gsub("\\`s", "", token)) %>% 
  mutate(token = gsub("[[:punct:]]", "", token)) %>% 
  mutate(token = gsub("[0-9]", "", token))   %>%   
  filter(nchar(token)>2) %>% 
  anti_join(get_stopwords(), by = c("token" = "word")) 

clust_words$token <- textstem::lemmatize_strings(clust_words$token)
clust_words <- clust_words %>% count(token, Modularity.Class, sort=T) %>% 
  bind_rows(abstract_tokens) %>% 
  group_by(token, Modularity.Class) %>% 
  summarise(n = sum(n)) %>% ungroup() %>% 
  filter(nchar(token)>2)

tot_words <- clust_words %>% 
  group_by(Modularity.Class) %>% 
  summarize(total = sum(n))

clust_words <- clust_words %>% left_join(tot_words)

clust_tf_idf <-  clust_words %>% 
  bind_tf_idf(token, Modularity.Class, n)

tf_idf <- function(x) {
clust_tf_idf %>% 
  filter(Modularity.Class == x) %>% 
  arrange(desc(tf_idf)) %>% 
    select(token, tf_idf)%>% 
    head(10) %>% 
    mutate(token= reorder(token, tf_idf, mean))%>% 
    ggplot(aes(x = token, y = tf_idf)) + geom_bar(stat = "identity") +
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 40, size = 13))
}

nodes %>% filter(Modularity.Class==0) %>% 
  left_join(works, by="id") %>% 
  arrange(desc(eigen)) %>% select(id, display_name, publication_year, first_author) %>% head(10)

nodes %>% count(Modularity.Class, sort=T) %>% print(n=Inf)

#authors <- read_csv("authors_full.csv") %>% distinct(item, id, .keep_all = T) %>% select(-...1, -X.1, -X)

#authors %>% filter(item %in% nodes$id) %>% left_join(works, by=c("item"="id"))  %>% View()

inst <- read_csv("insts_full.csv")

#inst %>% filter(item %in% nodes$id) %>% count(country_code, sort=T) %>% print(n=Inf)

make_subgraph <- function(x) {
  nodes <- nodes %>% 
    filter(Modularity.Class == x)
  edges <- edges %>% 
    filter(Source %in% nodes$id & Target %in% nodes$id)
  return(graph_from_data_frame(edges, vertices = nodes, directed = F))
}
subgraphCentrality <- function(x) {
  G <- make_subgraph(x)
  eigen <- eigen_centrality(G, weights = NA)
data.frame(Label=names(eigen$vector), centrality = as.numeric(eigen$vector)) %>% 
  arrange(desc(centrality)) %>% head(10) %>% left_join(works, by = c("Label" = "id")) %>% select(first_author, display_name, centrality)
}
