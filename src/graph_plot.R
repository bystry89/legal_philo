library(igraph)
library(tidyverse)
library(tidytext)
library(ForceAtlas2)

nodes <- read_csv("cmp_nodes.csv")
edges <- read_csv("edges_cmp.csv")


G <- graph_from_data_frame(edges, vertices=nodes, directed=F)
layout <- layout.forceatlas2(G, iterations=300, plotstep=100)
names(G)

plot.igraph(G, layout=layout, label=NA, color=nodes$Modularity.Class, label.font=0)
