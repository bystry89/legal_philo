library(tidyverse)
library(httr)

nullToNA <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}

works <- read.csv("works_oa+j.csv") %>% 
  distinct(id, .keep_all=T)
  
refs <- read.csv("refs.csv") %>% 
  distinct(work, refs) %>% 
  filter(work %in% works$id) 

cits <- read.csv("cits.csv") %>% 
  distinct(work, cit)%>% 
  filter(work %in% works$id) 

## STEP 1

outrefs <- refs %>% 
  filter(!refs %in% works$id) %>% 
  count(refs) %>% 
  filter(n > 7) %>%  
  arrange(desc(n))

  articles2 <- list()
  inst2 <- list()
  authors2 <- data.frame()
for (i in 1:nrow(outrefs)) {
  url <- paste("https://api.openalex.org/works/", gsub("https://openalex.org/", "", outrefs[i,"refs"]),'?username=piotr.bystranowski@uj.edu.pl',sep='')
  json <- GET(url)
  if (json$status_code == 404) {
    faulty <- c(faulty, i)    
    next
  }
  
  result <- rjson::fromJSON(rawToChar(json$content)) %>% nullToNA()
  result$host_venue <- nullToNA(result$host_venue)
  result$ids <- nullToNA(result$ids)
  df <- as.data.frame(result[c('id','display_name', 'publication_year', "type")])
  df <- bind_cols(df, data.frame(journal=result$host_venue$display_name, journal_id = result$host_venue$id, 
                                 doi = result$ids$doi))
  if (!is.null(result$cited_by_count)) df <- bind_cols(df, data.frame(citations =result$cited_by_count))
  if (length(result$authorships)>0) df$first_author <- result$authorships[[1]]$author$display_name
  articles2 <- bind_rows(df, articles2)
  if (length(result$authorships)==0) next
  for (j in result$authorships) {
    j <- nullToNA(j)
    auth <- data.frame(item = result$id,
                       position= j$author_position,
                       id = j$author$id,
                       name = j$author$display_name)
    authors2 <- bind_rows(authors2, auth)
    
    for (k in j$institutions) {
      inst2 <- bind_rows(inst2, data.frame(item = result$id, 
                                         nullToNA(k)))
    }
  }
  print(i)
}

  
works2 <- 
  #articles2 %>% left_join(outrefs, c('id' = 'refs')) 
  outrefs %>% select(refs) %>% inner_join(works_full, by=c('refs'='id')) %>% rename(id=refs) %>% distinct(id, .keep_all = T)

works2 <- readODS::read_ods("expand1.ods") %>% filter(excl == 1) %>% select(id) %>% 
  anti_join(works2, .)

refs2 <- list()
cits2 <- list()
for (i in 3470:nrow(works2)) {
  url <- paste(gsub("https://openalex.org/", "https://api.openalex.org/", works2[i,"id"]),'?username=piotr.bystranowski@uj.edu.pl',sep='')
  json <- GET(url)

  result <- rjson::fromJSON(rawToChar(json$content)) 
  if (!is.null(result$referenced_works)) if (length(result$referenced_works)>0) {
    refs2 <- bind_rows(refs2, data.frame(work = works2[i, 'id'], 
                                       refs = result$referenced_works))
  }
  if (!is.null(result$cited_by_count)) {
    works2[i, 'citations'] <- result$cited_by_count
    
    if (result$cited_by_count > 0) {   
      numb <- result$cited_by_count %/% 200
    if (result$cited_by_count %% 200 > 0) numb <- numb + 1
    for (m in 1:numb) {
      json <- GET(paste0(result$cited_by_api_url, "&per_page=200&page=", m))
      result_cit <- rjson::fromJSON(rawToChar(json$content))$results 
      if (length(result_cit) > 0) {
        for (j in 1:length(result_cit)) {
          cits2 <- bind_rows(cits2, data.frame(work = works2[i, 'id'], 
                                             cit = result_cit[[j]]$id))
        }
      }}
    }}
  print(i)
}



works <- works2 %>% mutate(group="expand1") %>%  bind_rows(works)
refs <- 
  #refs2 %>% mutate(group="expand1") %>% bind_rows(refs)
  read.csv("refs.csv") %>% filter(work %in% works$id)
cits <- 
  #cits2 %>% mutate(group="expand1") %>% bind_rows(cits)
  read.csv("cits.csv") %>% filter(work %in% works$id)


## STEP 3

##temp
#cits <- read_csv("cits.csv") %>% 
# filter((is.na(group) & work %in% works[is.na(works$group),"id"]) | (group == 'expand1' & work %in% works2$id))

outcits <- cits %>% 
  distinct(work, cit) %>% 
  filter(work %in% works$id) %>% 
  filter(!cit %in% works$id) %>% 
  count(cit) %>% 
  filter(n > 4) %>% 
  arrange(desc(n)) 

outcits2 <- outcits %>% anti_join(works_full, by = c("cit" = "id"))

articles3 <- list()
inst3 <- list()
authors3 <- data.frame()
for (i in 6409:nrow(outcits)) {
  url <- paste("https://api.openalex.org/works/", gsub("https://openalex.org/", "", outcits[i,"cit"]),'?username=piotr.bystranowski@uj.edu.pl',sep='')
  json <- GET(url)

  result <- rjson::fromJSON(rawToChar(json$content)) %>% nullToNA()
  result$host_venue <- nullToNA(result$host_venue)
  result$ids <- nullToNA(result$ids)
  df <- as.data.frame(result[c('id','display_name', 'publication_year', "type")])
  df <- bind_cols(df, data.frame(journal=result$host_venue$display_name, journal_id = result$host_venue$id, 
                                 doi = result$ids$doi))
  if (!is.null(result$cited_by_count)) df <- bind_cols(df, data.frame(citations =result$cited_by_count))
  if (length(result$authorships)>0) df$first_author <- result$authorships[[1]]$author$display_name
  articles3 <- bind_rows(df, articles3)
  if (length(result$authorships)==0) next
  for (j in result$authorships) {
    j <- nullToNA(j)
    auth <- data.frame(item = result$id,
                       position= j$author_position,
                       id = j$author$id,
                       name = j$author$display_name)
    authors3 <- bind_rows(authors3, auth)
    
    for (k in j$institutions) {
      inst3 <- bind_rows(inst3, data.frame(item = result$id, 
                                           nullToNA(k)))
    }
  }
  print(i)
}


works3 <- works_full %>% filter(id %in% outcits$cit)
works3 <- articles3 %>% 
  left_join(outcits, c('id' = 'cit')) 
works3 <- readODS::read_ods("expand1.ods") %>% filter(excl == 1) %>% select(id) %>% 
  anti_join(works3, .)



works3 <- readODS::read_ods("expand2.ods") %>% filter(excl == 1) %>% select(id) %>% 
anti_join(works3, .) %>% filter(citations > 1)

works3a <- works3 %>% anti_join(read.csv("works_full.csv"))


refs3a <- list()
cits3a <- list()
for (i in 1:nrow(works3a)) {
  url <- paste("https://api.openalex.org/works/", gsub("https://openalex.org/", "", works3[i,"id"]),'?username=piotr.bystranowski@uj.edu.pl',sep='')
  json <- GET(url)
  if (json$status_code == 404) {
    faulty <- c(faulty, i)    
    next
  }
  result <- rjson::fromJSON(rawToChar(json$content)) 
  if (!is.null(result$referenced_works)) if (length(result$referenced_works)>0) {
    refs3a <- bind_rows(refs3a, data.frame(work = works3[i, 'id'], 
                                         refs = result$referenced_works))
  }
  if (!is.null(result$cited_by_count)) {
    works3[i, 'citations'] <- result$cited_by_count
    
    if (result$cited_by_count > 0) {
      numb <- result$cited_by_count %/% 200
      if (result$cited_by_count %% 200 > 0) numb <- numb + 1
      for (m in 1:numb) {
        json <- GET(paste0(result$cited_by_api_url, "&per_page=200&page=", m))
        result_cit <- rjson::fromJSON(rawToChar(json$content))$results 
        if (length(result_cit) > 0) {
          for (j in 1:length(result_cit)) {
            cits3a <- bind_rows(cits3a, data.frame(work = works3[i, 'id'], 
                                               cit = result_cit[[j]]$id))
          }
        }}
    }}
  print(i)
}

works <- bind_rows(works, mutate(works3, group="extend2"))
refs <- bind_rows(refs, mutate(refs3, group="extend2"))
cits <- bind_rows(cits, mutate(cits3, group="extend2"))

for (i in 1:nrow(works)) {
  if (is.na(works[i,"citations"])) {
    url <- paste("https://api.openalex.org/works/", gsub("https://openalex.org/", "", works[i,"id"]),'?username=piotr.bystranowski@uj.edu.pl',sep='')
    json <- GET(url)
    
    result <- rjson::fromJSON(rawToChar(json$content)) %>% nullToNA()
    if (!is.null(result$cited_by_count)) works[i, "citations"] <- result$cited_by_count
    print(i)
  }
}

works <- works %>% filter(citations>1)

ref_filt <- refs %>% 
  filter(refs %in% works$id)

cit_filt <- cits%>% 
  filter(cit %in% works$id)

cits %>% distinct(work, cit, .keep_all = T) %>% write.csv("cits.csv")

works <- ref_filt %>% 
  count(work) %>% 
  rename(inrefs_N=n) %>% 
  right_join(select(works, -inrefs_N), c("work" = "id"))

works <- ref_filt %>% 
  count(refs) %>% 
  rename(incits_N=n) %>% 
  right_join(select(works, -incits_N), c("refs" = "work")) %>% 
  rename(id = refs)

works_filt <- works %>% filter(inrefs_N>0 | incits_N > 0)

write.csv(works, "works_full.csv")

read.csv("authors_oa+j.csv") %>% 
  bind_rows(mutate(authors2, group="extend1"), 
            mutate(authors3, group="extend2")) %>% 
  filter(item %in% works$id) %>% 
  write.csv("authors_full.csv")

read.csv("insts_oa+j.csv") %>% 
  bind_rows(mutate(inst2, group="extend1"), 
            mutate(inst3, group="extend2")) %>% 
  filter(item %in% works$id) %>% 
  write.csv("insts_full.csv")



####
# works_full <- read_csv("works_full.csv") %>% filter(id %in% works$id) %>% bind_rows(mutate(works3a, group="extend2")) %>% 
#   distinct(id, .keep_all = T) 
# 
# read_csv("cits.csv") %>% filter(work %in% works_full$id) %>% bind_rows(mutate(cits3a, group = "extend2")) %>% 
#   distinct(work, cit, .keep_all = T) %>% write_csv("cits.csv")
# 
# read_csv("refs.csv") %>% filter(work %in% works_full$id) %>% bind_rows(mutate(refs3a, group = "extend2")) %>% 
#   distinct(work, refs, .keep_all = T) %>% write_csv("refs.csv")
# 
# read.csv("authors_full.csv") %>% bind_rows(authors3) %>% distinct(item, id, .keep_all = T) %>% write_csv("authors_full.csv")
# read.csv("insts_full.csv") %>% bind_rows(inst3) %>% distinct(item, id, .keep_all = T) %>% write_csv("insts_full.csv")
