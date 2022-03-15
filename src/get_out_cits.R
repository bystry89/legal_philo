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

out_cits <- read.csv("data/raw/cits.csv") %>% 
  filter(work %in% nodes$id, !cit %in% nodes$id) %>% 
  left_join(select(nodes, id, cluster_label), by = c("work" = "id")) %>% 
  rename(Source = cluster_label)

jours <- unique(out_cits$cit)
venues <- list()



j=i
for (i in j:length(jours)) {
  url <- paste0(gsub("openalex.org","api.openalex.org",jours[i]), '?username=piotr.bystranowski@uj.edu.pl')

  result <- NULL
  attempt <- 0
  while (is.null(result) && attempt <= 3) {
    attempt <- attempt + 1
    try (
  response <- GET(url))
    try(
  result <- fromJSON(rawToChar(response$content)) %>% nullToNA()
    )
}


hv <- result$host_venue %>% nullToNA()
if (length(result$authorships) > 0) {
auth <- result$authorships[[1]]$author$id %>% nullToNA()
} else auth <- NA

venues <- bind_rows(venues, data.frame(work = jours[i], title = result$display_name, year = result$publication_date,
                                       first_author = auth, issn = hv$issn_l, name = hv$display_name))
print(i)
print(round(i/length(jours)*100, 2))
}
write.csv(venues, "data/cits/out_cits.csv")
