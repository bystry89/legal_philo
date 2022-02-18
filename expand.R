library(tidyverse)

nullToNA <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}

works <- read.csv("works.csv") %>% 
  group_by(id) %>% 
  filter(X == max(X)) %>% 
  ungroup()
  
refs <- read.csv("refs.csv") %>% 
  group_by(work, refs) %>% 
  filter(X == max(X)) %>% 
  ungroup()

cits <- read.csv("cits.csv") %>% 
  group_by(work, cit) %>% 
  filter(X == max(X)) %>% 
  ungroup()

## STEP 1
#ref_filt <- refs %>% 
#  filter(refs %in% works$id)

#works <- ref_filt %>% 
#  count(work) %>% 
#  rename(inrefs_N=n) %>% 
#  right_join(works, c("work" = "id"))

#works <- ref_filt %>% 
#  count(refs) %>% 
#  rename(incits_N=n) %>% 
#  right_join(works, c("cit" = "work")) %>% 
#  rename(id = refs)

works <- works %>% 
  mutate(inrefs_N = ifelse(is.na(inrefs_N), 0, inrefs_N),
         incits_N = ifelse(is.na(incits_N), 0, incits_N))

works_filt <- works %>% filter(inrefs_N>0 | incits_N > 0)


## STEP 2
outrefs <- refs %>% 
  #filter(work %in% works_filt$id, 
  #       !refs %in% works_filt$id) %>% 
  filter(!refs %in% works$work) %>% 
  count(refs) %>% 
  filter(n > 3) %>% 
  arrange(desc(n))

  articles2 <- list()
  inst2 <- list()
  authors2 <- data.frame()
for (i in 1439:nrow(outrefs)) {
  url <- paste("https://api.openalex.org/works/:", outrefs[i,"refs"],'?username=piotr.bystranowski@uj.edu.pl',sep='')
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

  
works2 <- articles2 %>% left_join(outrefs, c('id' = 'refs')) 

refs2 <- list()
cits2 <- list()
for (i in 1394:nrow(works2)) {
  url <- paste("https://api.openalex.org/works:", works2[i,"id"],'?username=piotr.bystranowski@uj.edu.pl',sep='')
  json <- GET(url)
  if (json$status_code == 404) {
    faulty <- c(faulty, i)    
    next
  }
  result <- rjson::fromJSON(rawToChar(json$content)) 
  if (!is.null(result$referenced_works)) if (length(result$referenced_works)>0) {
    refs2 <- bind_rows(refs2, data.frame(work = works2[i, 'id'], 
                                       refs = result$referenced_works))
  }
  if (!is.null(result$cited_by_count)) {
    works2[i, 'citations'] <- result$cited_by_count
    
    if (result$cited_by_count > 0) {
      json <- GET(result$cited_by_api_url)
      result_cit <- rjson::fromJSON(rawToChar(json$content))$results 
      if (length(result_cit) > 0) {
        for (j in 1:length(result_cit)) {
          cits2 <- bind_rows(cits2, data.frame(work = works2[i, 'id'], 
                                             cit = result_cit[[j]]$id))
        }}
    }}
  print(i)
}

works <- bind_rows(works, works2)
refs <- bind_rows(refs, refs2)
cits <- bind_rows(cits, cits2)

ref_filt <- refs %>% 
  filter(refs %in% works$id)

cit_filt <- cits%>% 
  filter(cit %in% works$id)

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


## STEP 3
refs %>% 
  filter(work %in% works_filt$id, 
         !refs %in% works_filt$id) %>% 
  count(refs) %>% 
  filter(n > 3) %>% 
  arrange(desc(n)) %>% View()

outcits <- cits %>% 
  filter(work %in% works_filt$id, 
         !cit %in% works_filt$id) %>% 
  count(cit) %>% 
  filter(n > 4) %>% 
  arrange(desc(n)) 

articles3 <- list()
inst3 <- list()
authors3 <- data.frame()
for (i in 1:nrow(outcits)) {
  url <- paste("https://api.openalex.org/works/:", outcits[i,"cit"],'?username=piotr.bystranowski@uj.edu.pl',sep='')
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


works3 <- articles3 %>% left_join(outcits, c('id' = 'cit')) 

refs3 <- list()
cits3 <- list()
for (i in 734:nrow(works3)) {
  url <- paste("https://api.openalex.org/works:", works3[i,"id"],'?username=piotr.bystranowski@uj.edu.pl',sep='')
  json <- GET(url)
  if (json$status_code == 404) {
    faulty <- c(faulty, i)    
    next
  }
  result <- rjson::fromJSON(rawToChar(json$content)) 
  if (!is.null(result$referenced_works)) if (length(result$referenced_works)>0) {
    refs2 <- bind_rows(refs2, data.frame(work = works2[i, 'id'], 
                                         refs = result$referenced_works))
  }
  if (!is.null(result$cited_by_count)) {
    works3[i, 'citations'] <- result$cited_by_count
    
    if (result$cited_by_count > 0) {
      json <- GET(result$cited_by_api_url)
      result_cit <- rjson::fromJSON(rawToChar(json$content))$results 
      if (length(result_cit) > 0) {
        for (j in 1:length(result_cit)) {
          cits3 <- bind_rows(cits3, data.frame(work = works2[i, 'id'], 
                                               cit = result_cit[[j]]$id))
        }}
    }}
  print(i)
}

works <- bind_rows(works, works3)
refs <- bind_rows(refs, refs3)
cits <- bind_rows(cits, cits3)

ref_filt <- refs %>% 
  filter(refs %in% works$id)

cit_filt <- cits%>% 
  filter(cit %in% works$id)

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

works_filt %>% write.csv("works_full.csv")

read.csv("authors.csv") %>% bind_rows(authors2, authors3) %>% 
  write.csv("authors_full.csv")

read.csv("insts.csv") %>% bind_rows(inst2, inst3) %>% 
  write.csv("insts_full.csv")
