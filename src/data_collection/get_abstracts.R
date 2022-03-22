library(httr)
library(tidyverse)
library(tidytext)
library(textstem)
nodes <- read.csv("data/cits/nodes.csv")


tokenized <- list()
for (i in 1418:nrow(nodes)) {
url <- gsub("openalex", "api.openalex", nodes[i, "id"])

response <- GET(url)
result <- jsonlite::fromJSON(rawToChar(response$content))
abs <- result$abstract_inverted_index
if (is.null(abs)) next
temp <- data.frame(term=names(lengths(abs)), n=as.numeric(lengths(abs))) %>% 
  mutate(term = tolower(term)) %>% 
  mutate(term = gsub("\\'s", "", term)) %>% 
  mutate(term = gsub("\\`s", "", term)) %>% 
  mutate(term = gsub("\\’s", "", term)) %>% 
  mutate(term = gsub("[[:punct:]]", "", term)) %>% 
  mutate(term = gsub("[0-9]", "", term)) %>% 
  filter(!term %in% get_stopwords()) %>% 
  filter(nchar(term)>2) %>% 
  mutate(term = lemmatize_words(term)) %>% 
  group_by(term) %>% 
  summarise(n=sum(n)) %>% 
  mutate(doc=nodes[i,'id'])

tokenized <- bind_rows(tokenized, temp)
print(round(i/nrow(nodes)*100))
}

write.csv(tokenized, "data/text/tokenized.csv")
tokenized <- tokenized %>% 
  filter(term != "")

dtm <- cast_dtm(tokenized, "doc", "term", "n")

tm <- topicmodels::LDA(dtm, k=20)

topics <- tidy(tm, matrix = "beta")

top_terms <- topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()
