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

venues %>% filter(!is.na(name), name != "Social Science Research Network") %>% 
 filter(is.na(.$issn)) %>% count(name, sort=T)


issn <- venues %>% filter(!is.na(issn), name != "Social Science Research Network") %>% 
  count(issn, sort=T)

journals_class <- list()
for (i in 5744:nrow(issn)){
  url <- paste0("https://api.elsevier.com/content/serial/title?issn=", issn[i, "issn"], "&view=STANDARD&apiKey=bd2c8d4ca7a5db08585fbeedd3b66405&httpAccept=application/json")
  response <- GET(url)
  result <- fromJSON(rawToChar(response$content))$`serial-metadata-response`$entry[[1]]
  if (is.null(result)) {
    journals_class <- bind_rows(journals_class,
                                data.frame(
                                  issn = issn[i, "issn"],
                                  area = NA))
  next
  }
  if (is.null(result$`subject-area`))  next
  for (j in 1:length(result$`subject-area`)) {
    journals_class <- bind_rows(journals_class,
    data.frame(
    issn = issn[i, "issn"],
    tilte = result$`dc:title`,
    code = result$`subject-area`[[j]]$`@code` ,
    area = result$`subject-area`[[j]]$`$`))
  }
  print(i)
  
  }

write.csv(journals_class, "data/cits/scopus_class.csv")
journals_class <- read.csv("data/cits/scopus_class.csv")

legal_philo <- 
  journals_class %>% filter(issn %in%
c("1871-9791",
"1352-3252",
"0167-5249",
"0214-8676",
"1581-7652",
"1460-728X",
"0353-5738",
"0952-1917",
"0841-8209",
"1356-4765",
"0957-8536",
"0167-5249",
"1581-7652",
"2175-2168",
"0001-2343",
"2040-3313")) %>% distinct(issn, tilte)

journals_class <- journals_class %>% filter(!is.na(code)) %>% 
  anti_join(legal_philo) %>% 
  mutate(
    area2 = case_when(
      grepl("3308",code) ~ "Law",
      grepl("1211",code) ~ "Philosophy",
      grepl("33..",code) ~ "Social sciences",
      grepl("20..",code) ~ "Social sciences",
      grepl("14..",code) ~ "Social sciences",
      grepl("12..",code) ~ "Humanities",
    )
  ) %>% bind_rows(mutate(legal_philo, area2="Philosophy of law")) %>% 
  mutate(area2 = if_else(is.na(area2), "Other", area2))

write.csv(journals_class, "data/cits/scopus_class_mod.csv")

journals_class %>% 
  filter(!is.na(area)) %>% 
  group_by(issn) %>% 
  mutate(weighted = 1/n()) %>% 
  ungroup() %>% left_join(venues) %>% 
  group_by(area) %>% summarise(sum = sum(weighted)) %>% 
  arrange(desc(sum))
