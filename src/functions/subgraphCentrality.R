library(tidyverse)
library(igraph)

make_subgraph <- function(x) {
  nodes <- nodes %>% 
    filter(modularity_class == x)
  edges <- edges %>% 
    filter(Source %in% nodes$Id & Target %in% nodes$Id)
  return(graph_from_data_frame(edges, vertices = nodes, directed = F))
}
subgraphCentrality <- function(x) {
  G <- make_subgraph(x)
  eigen <- eigen_centrality(G, weights = NA)
  data.frame(Label=names(eigen$vector), centrality = round(as.numeric(eigen$vector),2)) %>% 
    arrange(desc(centrality)) %>% head(10) %>% 
    left_join(works, by = c("Label" = "id")) %>% 
    select(first_author, display_name, centrality)
}