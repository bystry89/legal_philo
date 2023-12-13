library(tidyverse)
library(igraph)
source("labelClust.R")

nodes <- read.csv("data/cits/nodes_w=6.csv") %>% labelClusts() %>% 
  filter(!is.na(cluster_label))

edges <- read.csv("data/cits/edges.csv")

