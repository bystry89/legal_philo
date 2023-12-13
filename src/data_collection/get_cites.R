library(tidyverse)
library(openalexR)
library(httr)

source("src/functions/recodeDupl.R")

bind_rows_workaround <- function(df1, df2){
  df1 <- df1 %>% mutate(across(where(~ !is.character(.)), as.character))
  df2 <- df2 %>% mutate(across(where(~ !is.character(.)), as.character))
  bind_rows(df1, df2)
}

works <- read_csv("data/raw/works_j.csv") %>% 
  filter(cited_by_count>0) %>% 
  bind_rows_workaround(read_csv("data/raw/cits_filt.csv")) %>% 
  bind_rows_workaround(read_csv("data/raw/classics.csv")) %>% 
  #filter(cited_by_count>1) %>% 
  distinct(id, .keep_all = T)



works$id2 <- dupl_titles(works$id)


cits <- list()
pacman::p_load(progress)
pb <- progress_bar$new(total=nrow(works), width=60, clear=F,
                       format = " Downloading [:bar] :percent ETA= :eta")
for (i in 1:nrow(works)) {
  skip_to_next <- FALSE
  tryCatch(
    cits <-bind_rows(cits, bind_cols(data.frame(id=works[i, "id"], 
                                                oa_fetch(entity = "work", cites=works[i, "id"])))),
    error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }   
  
  pb$tick()
}

cits %>%
  mutate(across(everything(), as.character)) %>% 
  write_csv("data/raw/outcits.csv")


cits_filt  <- read_csv("data/raw/outcits.csv")

works$author <- gsub("\r\n", "", works$author)
works$author <- lapply(works$author, function(x) eval(parse(text = x)))
works$first_author <- apply(works, 1, function(x) x['author'][[1]]["au_display_name"][[1]][1])

works %>% 
  mutate(across(everything(), as.character)) %>% 
  write_csv("data/raw/works_full.csv")

###### THE PROBLEM OF BOOKS CHAPTERS
cits <- cits_filt %>% 
  filter(type%in% c("book-chapter", "paratext"))  %>% group_by(publication_date,so) %>% ungroup() %>% 
  slice(1) %>%bind_rows_workaround(filter(cits_filt, !type %in% c("book-chapter", "paratext"))) %>% 
  filter(!id.1 %in% c("https://openalex.org/W4211239759", "https://openalex.org/W2946588354", 
                      "https://openalex.org/W2946581515", "https://openalex.org/W3202094968"))

dubl_referenced <- cits %>% distinct(id.1, .keep_all=T)%>% 
  filter(nchar(referenced_works)>60) %>% count(referenced_works, sort=T) %>% 
  filter(n>1 & nchar(referenced_works)>70)

  dublets <- dubl_referenced %>% left_join(distinct(cits, id.1, .keep_all=T), by="referenced_works") %>% .$id

  cits <- cits %>% 
    filter(id.1 %in% dublets) %>% 
    bind_rows(filter(cits, !referenced_works %in% dubl_referenced$referenced_works)) 
  
  cits$id2 <- dupl_titles(cits$id)
  ###
library(biblionetwork)
library(igraph)
cocit <- biblio_cocitation(cits, source="id.1", ref="id2", weight_threshold = 6)

g <- graph_from_data_frame(cocit, directed=F)
g <- igraph::induced_subgraph(g,V(g)[(degree(g)>1)])
components <- clusters(g, mode="weak")
biggest_cluster_id <- which.max(components$csize)

# ids
vert_ids <- V(g)[components$membership == biggest_cluster_id]

# subgraph
g1 <- igraph::induced_subgraph(g, vert_ids)


nodes <- data.frame(id=names(V(g1))) %>% left_join(works) %>% 
  select(id, publication_year, first_author, display_name) %>%  
  mutate(Label = paste0(stringr::str_extract(first_author, "[A-Z][a-z]+$"), ", ", publication_year))

write.csv(nodes, "data/cits/nodes.csv", row.names = F)


as_data_frame(g1) %>% select(Source, Target, weight) %>% 
  write.csv("data/cits/edges.csv", row.names = F)




####
data.frame(id=names(eigen_centrality(g1)$vector), eigen=as.numeric(eigen_centrality(g1)$vector)) %>% 
  left_join(works) %>% View()

bet <- betweenness(g1)
bet <- data.frame(id=names(bet), between=as.numeric(bet)) %>% 
  left_join(works)
  #(distinct(works, id2, .keep_all=T), by = c("id" = "id2")) 

louv <- cluster_louvain(g1, resolution = 1)
comm <- data.frame(id=names(membership(louv)), comm=as.numeric(membership(louv))) %>% 
  left_join(bet) 

comm %>% 
  filter(comm == 1) %>% 
  arrange(desc(between)) %>% View()

comm %>% count(comm, sort=T)

resid <- data.frame(id=names(V(g1))) %>% mutate(id = as.character(id)) %>% 
  anti_join(works_full)

resid <- oa_fetch(entity = "works", identifier = resid$id)

write_csv(resid, "data/raw/resid.csv")
resid <- read_csv("data/raw/resid.csv")

resid$first_author <- apply(resid, 1, function(x) x['author'][[1]]["au_display_name"][[1]][1])


nodes <-resid %>% 
  #mutate(publication_date=as.character(publication_date),
   #      volume=as.character(volume),
#         is_oa=as.character(is_oa),
#         cited_by_count=as.character(cited_by_count),
#         publication_year=as.character(publication_year),
#         is_paratext=as.character(is_paratext),
#         is_retracted=as.character(is_retracted)
#) %>% 
          select(-author, -counts_by_year, -ids, -referenced_works, -related_works, -concepts) %>% 
  bind_rows(works_full) %>% 
  distinct(id, .keep_all = T) 

nodes <-  data.frame(id=names(membership(louv))) %>% 
   left_join(nodes)

# nodes <- nodes %>% 
 #  select(display_name, publication_year, 
  #        first_author, display_name,
  #        id) %>% 
  # mutate(Label = paste0(stringr::str_extract(display_name, "[A-Z][a-z]+$"), ", ", publication_year))
 
 #nodes <- nodes %>% 


  works <- cits_filt
refs <- c()
for (i in 1:nrow(works)) {
  if (is.na(works$referenced_works[i])) next
  if (grepl("^c",  works$referenced_works[i])) {
    ref <- works$referenced_works[i] %>% parse(text = .) %>% eval()
  } else ref <- works$referenced_works[i]
  refs <- append(refs, ref, after =length(refs))
  print(i)
}

refs_expand2 <- data.frame(id = unlist(refs)) %>% 
  filter(!id %in% c(refs_filt$id, 
                    refs_expand$id, 
                    cits_filt$id, 
                    works$id))

refs_expand2 <- refs_expand2 %>% 
  count(id, sort=T) %>% 
  filter(n>2)

refs_expand_os <- oa_fetch(identifier = sapply(refs_expand2$id, stringr::str_extract, c(pattern = "W.+")), verbose = T) 

refs_expand2 %>% 
  left_join(refs_expand_os) %>% 
  mutate(first_author = stringr::str_match(author, "au_display_name = \"(.+?)\"")[,2],
         cited_by_count = as.integer(cited_by_count),
                                     ratio = n/cited_by_count) %>% 
  select(id, display_name,first_author, display_name, n, cited_by_count, ratio, publication_year, so) %>% 
  View()
