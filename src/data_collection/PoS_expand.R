library(tidyverse)
library(openalexR)

works <- read_csv("data/raw/PoS_works_j.csv") %>% 
  distinct(id, .keep_all = T)

works_cit <- works %>% 
  filter(cited_by_count>0)

sum(read_csv("data/raw/works_j.csv")$cited_by_count)/sum(works$cited_by_count)
nrow(read_csv("data/raw/works_j.csv"))/nrow(works)


## STEP 1 -- add citing articles

# Split work IDs into batches
batch_size <- 50  # Adjust based on API limits and performance
id_batches <- split(works_cit$id, ceiling(seq_along(works_cit$id) / batch_size))

# Progress bar
pacman::p_load(progress)
pb <- progress_bar$new(total = length(id_batches), width = 60, clear = FALSE,
                       format = " Downloading [:bar] :percent ETA= :eta")


# Fetch citations in batches
cits <- list()
for (batch in 1:length(id_batches)) {
  tryCatch(
    {
      # Fetch data for the current batch
      result <- oa_fetch(entity = "work", cites = id_batches[[batch]], verbose = T)
      cits <- bind_rows(cits, result)
    },
    error = function(e) {
      message("Error in batch, skipping...")
    }
  )
  pb$tick()
}



cits %>%
  mutate(across(everything(), as.character)) %>% 
  write_csv("data/raw/PoS_cits.csv")
cits <- read_csv("data/raw/PoS_cits.csv")

cits_u <- cits %>% 
  distinct(id, referenced_works) %>% 
  mutate(referenced_works = str_remove_all(referenced_works, "[\r\n]"),  # Remove line breaks
         referenced_works = str_remove_all(referenced_works, "c\\(|\\)|\\\""),  # Remove 'c(', ')', and '"'
         referenced_works = str_split(referenced_works, ", ")) %>% 
  unnest(referenced_works) %>% 
  filter(referenced_works %in% works_cit$id) %>% 
  mutate(Target = referenced_works) %>% 
  left_join(distinct(cits, id, .keep_all=T), by="id")

cits_u %>%
  mutate(across(everything(), as.character)) %>% 
  write_csv("data/raw/PoS_cits_matched.csv")

#step 3
refs <- works %>% 
  mutate(referenced_works = str_remove_all(referenced_works, "[\r\n]"),  # Remove line breaks
         referenced_works = str_remove_all(referenced_works, "c\\(|\\)|\\\""),  # Remove 'c(', ')', and '"'
         referenced_works = str_split(referenced_works, ", ")) %>% 
  unnest(referenced_works) %>% 
  count(referenced_works, sort=T) %>% 
  filter(!is.na(referenced_works), referenced_works!="https://openalex.org/W4285719527", n>5, !referenced_works%in%works$id) %>% 
  rename(Target = referenced_works)

classics <- oa_fetch(entity = "work", identifier = refs$Target)

classics %>% left_join(refs, by = c("id" = "Target")) %>% 
  mutate(prop = n/cited_by_count, 
         first_author = stringr::str_match(author, "au_display_name = \"(.+?)\"")[,2],) %>% 
  select(display_name, first_author, publication_year, cited_by_count, n, prop) %>% View()

classics2 <- classics %>% left_join(refs, by = c("id" = "Target")) %>% 
  mutate(prop = n/cited_by_count) %>% 
  filter(prop > 0.015)

classics2 %>%
  mutate(across(everything(), as.character)) %>% 
  write_csv("data/raw/PoS_classics.csv")


