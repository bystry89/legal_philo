library(tidyverse)
library(openalexR)


source("src/functions/recodeDupl.R")

bind_rows_workaround <- function(df1, df2){
  df1 <- df1 %>% mutate(across(where(~ !is.character(.)), as.character))
  df2 <- df2 %>% mutate(across(where(~ !is.character(.)), as.character))
  bind_rows(df1, df2)
}

works <- read_csv("data/raw/works_j.csv") %>% 
  distinct(id, .keep_all = T)

works_cit <- works %>% 
  filter(cited_by_count>0)

## STEP 1 -- add citing articles
cits <- list()
pacman::p_load(progress)
pb <- progress_bar$new(total=nrow(works_cit), width=60, clear=F,
                       format = " Downloading [:bar] :percent ETA= :eta")
for (i in 1:nrow(works_cit)) {
  skip_to_next <- FALSE
  tryCatch(
  cits <-bind_rows(cits, bind_cols(data.frame(id=works_cit[i, "id"], 
                                              oa_fetch(entity = "work", cites=works_cit[i, "id"])))),
  error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }   
  
  pb$tick()
}

cits <- cits %>% 
  rename(Source = id,
         id = id.1)

cits %>%
  mutate(across(everything(), as.character)) %>% 
  write_csv("data/raw/cits.csv")
cits <- read_csv("data/raw/cits.csv")

cits <- cits %>% distinct(Source, id, .keep_all=T)

cits_filt <- cits %>% 
  count(id, sort=T) %>% 
  filter(n>4) %>% 
  left_join(distinct(cits, id, .keep_all=T)) 

cits_filt %>%
  mutate(across(everything(), as.character)) %>% 
  write_csv("data/raw/cits_filt.csv")


###STEP 2 -- THE PROBLEM OF BOOK CHAPTERS

works2 <- cits_filt %>% 
  filter(type%in% c("book-chapter", "paratext"))  %>% group_by(publication_date,so) %>% ungroup() %>% 
  slice(1) %>%
  bind_rows_workaround(filter(cits_filt, !type %in% c("book-chapter", "paratext"))) %>% 
  bind_rows_workaround(works) %>% 
  distinct(id, .keep_all = T) %>% 
  filter(!is.na(referenced_works))

dubl_referenced <- works2 %>% 
  count(referenced_works) %>% 
  filter(n>1 & nchar(referenced_works)>70)

dublets <-  dubl_referenced%>% 
  left_join(distinct(works2, referenced_works, .keep_all=T), by="referenced_works") %>% .$id

works2 <- works2 %>% 
  filter(id %in% dublets) %>% 
  bind_rows(filter(works2, !referenced_works %in% dubl_referenced$referenced_works)) 

refs <- list()


for (i in 1:nrow(works2)) {
  if (is.na(works2$referenced_works[i])) next
  if (grepl("^c",  works2$referenced_works[i])) {
    ref <- works2$referenced_works[i]%>% gsub("\r\n", "", .) %>%  parse(text = .) %>% eval()
  } else ref <- works2$referenced_works[i]
  refs <- bind_rows(refs, data.frame(Target=ref, Source=works$id[i]))
  print(i)
}

# 1049 items cited at least 6 times
outrefs <- refs %>% 
  filter(!Target %in% works2$id) %>% 
  count(Target) %>% 
  filter(n > 5) %>%  
  arrange(desc(n))

classics <- oa_fetch(entity = "work", identifier = outrefs$Target)

classics %>% left_join(outrefs, by = c("id" = "Target")) %>% 
  mutate(prop = n/cited_by_count, 
         first_author = stringr::str_match(author, "au_display_name = \"(.+?)\"")[,2],) %>% 
  select(display_name, first_author, publication_year, cited_by_count, n, prop) %>% View()

classics2 <- classics %>% left_join(outrefs, by = c("id" = "Target")) %>% 
  mutate(prop = n/cited_by_count) %>% 
  filter(prop > 0.015)

classics2 %>%
  mutate(across(everything(), as.character)) %>% 
  write_csv("data/raw/classics.csv")
classics2 <- read_csv("data/raw/classics.csv")

