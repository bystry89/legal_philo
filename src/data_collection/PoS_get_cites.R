library(tidyverse)
library(openalexR)


cits <- read_csv("data/raw/PoS_cits_matched.csv")

cits_filt <- cits %>% 
  count(id) %>% 
  filter(n>18) %>% 
  left_join(distinct(cits, id,cited_by_count), by="id")

write_csv(cits_filt, "data/raw/PoS_cits_filt.csv")
cits_filt <- read_csv("data/raw/PoS_cits_filt.csv")

classics <- read_csv("data/raw/PoS_classics.csv") %>% 
  filter(prop>.054, n>17)

# Split work IDs into batches
batch_size <- 50  # Adjust based on API limits and performance
ids <- unique(cits_filt$id)
#ids <- unique(classics$id)
id_batches <- split(ids, ceiling(seq_along(ids) / batch_size))
ids <- unique(c(classics$id, cits_filt$id, read_csv("data/raw/PoS_works_j.csv")$id))

# Progress bar
pacman::p_load(progress)
pb <- progress_bar$new(total = length(id_batches), width = 60, clear = FALSE,
                       format = " Downloading [:bar] :percent ETA= :eta")

outcits <- list()
# Fetch citations in batches
for (batch in 1:length(id_batches)) {
  tryCatch(
    {
      # Fetch data for the current batch
      result <- oa_fetch(entity = "work", cites = id_batches[[batch]], verbose = TRUE, abstract = FALSE, options = list(select = c("id", "referenced_works")))
      
      # Filter `referenced_works` to retain only IDs in the `ids` vector
      result <- result %>%
        mutate(
               referenced_works = lapply(referenced_works, function(x) x[x %in% ids]))  %>% # Keep only IDs in `ids`
        unnest(referenced_works)
      
      # Combine the cleaned results
      outcits <- bind_rows(outcits, result)
    },
    error = function(e) {
      message("Error in batch, skipping...")
    }
  )
  pb$tick()
}

#write_csv(outcits, "data/raw/PoS_cits_outcits.csv")

###############
cits <- cits %>% 
  select(id, Target) 

classics_outcits <- read_csv("data/raw/PoS_classics_outcits.csv") %>% 
  rename(Target = referenced_works)

cits_outcits <- read_csv("data/raw/PoS_cits_outcits.csv") %>% 
  rename(Target = referenced_works)

cits <- bind_rows(cits, classics_outcits, cits_outcits) %>% 
  distinct(id, Target)

  ###
library(biblionetwork)
library(igraph)
cocit <- biblio_cocitation(cits, source="id", ref="Target", weight_threshold = 6)

g <- graph_from_data_frame(cocit, directed=F)
g <- igraph::induced_subgraph(g,V(g)[(degree(g)>1)])
components <- clusters(g, mode="weak")
biggest_cluster_id <- which.max(components$csize)

# ids
vert_ids <- V(g)[components$membership == biggest_cluster_id]

# subgraph
g1 <- igraph::induced_subgraph(g, vert_ids)

works <- read_csv("data/raw/PoS_works_j.csv") %>% 
  bind_rows(read_csv("data/raw/PoS_classics.csv")) %>%
  bind_rows(read_csv("data/raw/PoS_cits.csv")) %>% 
  distinct(id, .keep_all = T)

nodes <- data.frame(id=names(V(g1))) %>% left_join(works)

#write_csv(nodes, "data/cits/PoS_nodes.csv")

nodes <- read_csv("data/cits/PoS_nodes.csv")

as_data_frame(g1) %>% select(Source, Target, weight) %>% 
  write_csv("data/cits/PoS_edges.csv")


#####

outcits <- cits %>% 
  filter(Target %in% nodes$id) 

#write_csv(outcits, "data/cits/PoS_outcits.csv")

outcits <- read_csv("data/cits/PoS_outcits.csv")

# Split work IDs into batches
batch_size <- 50  # Adjust based on API limits and performance
ids <-unique(outcits$id)
#ids <- unique(classics$id)
id_batches <- split(ids, ceiling(seq_along(ids) / batch_size))

# Progress bar
pacman::p_load(progress)
pb <- progress_bar$new(total = length(id_batches), width = 60, clear = FALSE,
                       format = " Downloading [:bar] :percent ETA= :eta")

outcits_meta <- list()
# Fetch citations in batches
for (batch in 1:length(id_batches)) {
  tryCatch(
    {
      # Fetch data for the current batch
      result <- oa_fetch(entity = "work", identifier = id_batches[[batch]], verbose = TRUE, abstract = FALSE)
      
      # Filter `referenced_works` to retain only IDs in the `ids` vector

      
      # Combine the cleaned results
      outcits_meta <- bind_rows(outcits_meta, result)
    },
    error = function(e) {
      message("Error in batch, skipping...")
    }
  )
  pb$tick()
}
outcits_meta <- outcits %>% distinct(id) %>% oa_fetch(entity = "works", identifier = .$id, abstract=F, verbose=T)

write_csv(outcits_meta, "data/cits/PoS_outcits_meta.csv")

