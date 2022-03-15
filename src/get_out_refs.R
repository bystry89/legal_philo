library(tidyverse)
library(httr)
library(rjson)
source("src/functions/labelClust.R")

nullToNA <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}


nodes <- read.csv("data/cits/cmp_nodes.csv") %>% 
  labelClusts() %>% 
  filter(!is.na(cluster_label))

out_refs <- read.csv("data/raw/refs.csv") %>% 
  filter(work %in% nodes$id, !refs %in% nodes$id) %>% 
  left_join(select(nodes, id, cluster_label), by = c("work" = "id")) %>% 
  rename(Target= cluster_label)

jours <- unique(out_refs$refs)
venues <- list()
for (i in 5045:length(jours)) {
  url <- paste0(gsub("openalex.org","api.openalex.org",jours[i]), '?username=piotr.bystranowski@uj.edu.pl')
  response <- GET(url)
  
  
  result <- fromJSON(rawToChar(response$content)) %>% nullToNA()
  
  hv <- result$host_venue %>% nullToNA()
  auth <- result$authorships[[1]]$author$id %>% nullToNA()
  
  venues <- bind_rows(venues, data.frame(work = jours[i], title = result$display_name, year = result$publication_date,
                                         first_author = auth, issn = hv$issn_l, name = hv$display_name))
  print(i)
  print(round(i/length(jours)*100, 2))
}

write.csv(venues, "data/cits/out_refs.csv")
