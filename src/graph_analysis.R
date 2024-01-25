library(igraph)
library(tidyverse)
library(tidytext)
library(ggplot2)

#nodes <- read_csv("data/cits/nodes_w=6.csv")
#edges <- read_csv("data/cits/edges.csv")
#works <- read_csv("data/raw/works_full.csv")

stop <- c("jan", "apr", "httpsdoiorg", "oct", "mar", "ssrn", "journal", "aug", "electronic",
          "jun", "sep", "nov", "feb", "may", "jul", "dec", "study", "hereinafter",
          'rev', get_stopwords()$word)

abstract_tokens <- 
  #read.csv("data/text/tokenized_w=6.csv") %>% 
  tokenized %>% 
  filter(!term %in% stop) %>% 
  #anti_join(c(get_stopwords(), stop), by = c("term" = "word")) %>% 
  left_join(nodes, by = c("doc" = "Id")) %>% 
  select(term, n, modularity_class) %>% 
  rename(token = term) %>% filter(nchar(token)>2)

clust_words <- unnest_tokens(nodes, 'token', 'Label') %>% 
  mutate(token = gsub("\\'s", "", token)) %>% 
  mutate(token = gsub("\\`s", "", token)) %>% 
  mutate(token = gsub("\\’s", "", token)) %>% 
  mutate(token = gsub("[[:punct:]]", "", token)) %>% 
  mutate(token = gsub("[0-9]", "", token))   %>%   
  filter(nchar(token)>2) %>% 
  anti_join(get_stopwords(), by = c("token" = "word")) 

clust_words$token <- textstem::lemmatize_strings(clust_words$token)
clust_words <- clust_words %>% count(token, modularity_class, sort=T) %>% 
  bind_rows(abstract_tokens) %>% 
  group_by(token, modularity_class) %>% 
  summarise(n = sum(n)) %>% ungroup() %>% 
  filter(nchar(token)>2)

tot_words <- clust_words %>% 
  group_by(modularity_class) %>% 
  summarize(total = sum(n))

clust_words <- clust_words %>% left_join(tot_words)

clust_tf_idf <-  clust_words %>% 
  bind_tf_idf(token, modularity_class, n)

tf_idf <- function(x) {
clust_tf_idf %>% 
  filter(modularity_class == x) %>% 
  arrange(desc(tf_idf)) %>% 
    select(token, tf_idf)%>% 
    head(10) %>% 
    mutate(token= reorder(token, tf_idf, mean))%>% 
    ggplot(aes(x = token, y = tf_idf)) + geom_bar(stat = "identity") +
    #coord_cartesian(ylim = c(0,0.05))+
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 40, size = 8, vjust = 1, hjust = 1), axis.title = element_blank())
}
