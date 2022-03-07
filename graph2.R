library(tidyverse)
library(igraph)
library(biblionetwork)
library(tidytext)
works <- read.csv("works_full.csv") %>% 
  distinct(id, .keep_all=T)


refs <- read.csv("refs.csv") %>% 
  distinct(work, refs)

cits <- read.csv("cits.csv") %>% 
  distinct(work, cit)

CITS <- cits %>% 
  count(cit, sort=T) %>% 
  filter(n >1) %>% 
  select(cit) %>% 
  left_join(cits) %>% 
  select(work, cit)


cocits <- list()
for (i in 1:length(unique(CITS$cit))) {
  cocits <- CITS %>% 
    filter(cit == unique(CITS$cit)[i]) %>% 
    select(work) %>% 
    expand_grid(work2=work) %>% 
    filter(work != work2) %>% 
    bind_rows(cocits)
  print(i)
  print(round(i/length(unique(CITS$cit)),4)*100)
}

write_csv(cocits, "cocits.csv")
cocits <- read_csv("cocits.csv")

cocits <- cocits %>% filter(work %in% works$id & work2 %in% works$id)
dupl <- c("https://openalex.org/W310899332", "https://openalex.org/W201347684",
          "https://openalex.org/W1498909244",
          #to delete
          "https://openalex.org/W2189821821",
          "https://openalex.org/W398143332")
cocits <- cocits %>% filter(!(work %in% dupl|work2 %in% dupl))

cocits2 <- cocits %>% 
  rename(work=work2, work2=work) %>% 
  group_by(work3= pmin(work, work2), 
           work4= pmax(work, work2)) %>% 
  summarize(n=n()/2) %>% 
  arrange(desc(n)) %>% 
  filter(n>4)%>% 
  rename(Source = work3, Target = work4, weight = n)

nodes <- c(cocits2$Source, cocits2$Target) %>% 
  unique() %>% data.frame(id = .) %>% 
  left_join(works) %>% 
  select(id, display_name) %>% 
  rename(Label = display_name) 

G <- igraph::graph_from_data_frame(cocits2, vertices=nodes, directed=F)
cmp <- components(G)
cmp$csize
nodes_cmp <- data.frame(id=1:length(cmp$csize), size=cmp$csize) %>% filter(size>50) %>% 
  left_join(data.frame(id=names(cmp$membership), mbp=cmp$membership), by=c("id"="mbp")) %>% 
  rename(cmp=id, id=id.y)%>% left_join(works) %>% select(id, display_name) %>% 
  rename(Label = display_name)


edges_cmp <- cits%>% filter(work %in% nodes_cmp$id) %>% 
  biblio_cocitation(ref = "work", source = "cit", weight_threshold = 5) %>% 
  select(Source, Target, weight) %>% arrange((weight)) 
edges_cmp%>% write_csv("edges_cmp.csv")

#cocits3 <- cocits2 %>% filter(Source %in% nodes_cmp$id, Target %in% nodes_cmp$id)   
#write.csv(cocits3, "cocits3.csv", row.names = F)
G2 <- graph_from_data_frame(
  edges_cmp,
  #cocits3, 
  nodes_cmp, directed=F)

leid <- cluster_leiden(G2, objective_function = "modularity",resolution_parameter = 3, n_iterations = 400)
sort(table(leid$membership))
sum(table(leid$membership)[table(leid$membership)>30])/sum(table(leid$membership))

eigen <- eigen_centrality(G2)$vector

cmp_nodes <- data.frame(id=leid$names, `Modularity Class`=leid$membership) %>% 
  left_join(nodes_cmp) %>% left_join(data.frame(id=names(eigen), eigen=as.numeric(eigen)))
write.csv(cmp_nodes, "cmp_nodes.csv", row.names = F)

