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


## to move up later##
dupl <- c("https://openalex.org/W310899332", "https://openalex.org/W201347684",
          "https://openalex.org/W1498909244",
          "https://openalex.org/W1024763542",
          "https://openalex.org/W2150146195",
          "https://openalex.org/W2025894382",
          "https://openalex.org/W2008305937",
          "https://openalex.org/W3125999045",
          "https://openalex.org/W348020130",
          "https://openalex.org/W1999659239",
          "https://openalex.org/W259309357",
          "https://openalex.org/W1535271902",
          "https://openalex.org/W1992000778",
          "https://openalex.org/W310899332",
          "https://openalex.org/W2026785431",
          "https://openalex.org/W1988986431",
          "https://openalex.org/W2779693884",
          "https://openalex.org/W63493041",
          "https://openalex.org/W1977212245",
          "https://openalex.org/W2078936242",
          "https://openalex.org/W3142178200",
          "https://openalex.org/W2038705124",
          "https://openalex.org/W2153259910",
          "https://openalex.org/W1590996600",
          "https://openalex.org/W2086581722",
          "https://openalex.org/W2480782842",
          "https://openalex.org/W1544005079",
          "https://openalex.org/W2314437616",
          "https://openalex.org/W2032987097",
          "https://openalex.org/W1593499730", 
          "https://openalex.org/W2165448399",
          #to delete
          "https://openalex.org/W2189821821",
          "https://openalex.org/W398143332",
          "https://openalex.org/W2047504320",
          "https://openalex.org/W2794033063",
          "https://openalex.org/W1980367553")

works <- works %>% filter(!id %in% dupl)

cits <- cits %>% mutate(work = str_replace_all(work, c(
  "https://openalex.org/W2150146195" = "https://openalex.org/W3121484141",
  "https://openalex.org/W2025894382" = "https://openalex.org/W3126016604",
  "https://openalex.org/W2008305937" = "https://openalex.org/W3125377858",
  "https://openalex.org/W3125999045" = "https://openalex.org/W2145855346",
  "https://openalex.org/W348020130" = "https://openalex.org/W2131360862",
  "https://openalex.org/W1999659239" = "https://openalex.org/W3122443843",
  "https://openalex.org/W259309357" = "https://openalex.org/W3126004017",
  "https://openalex.org/W1535271902" = "https://openalex.org/W3130376029",
  "https://openalex.org/W1992000778" = "https://openalex.org/W2034278834",
  "https://openalex.org/W310899332" = "https://openalex.org/W2053924168",
  "https://openalex.org/W2026785431"= "https://openalex.org/W3126055011",
  "https://openalex.org/W1988986431" = "https://openalex.org/W3121657281",
  "https://openalex.org/W2779693884"= "https://openalex.org/W2921522229",
  "https://openalex.org/W63493041" = "https://openalex.org/W2587822774",
  "https://openalex.org/W1977212245" = "https://openalex.org/W3126001794",
  "https://openalex.org/W2078936242" = "https://openalex.org/W3126104191",
  "https://openalex.org/W3142178200" = "https://openalex.org/W2122165510",
  "https://openalex.org/W2038705124" = "https://openalex.org/W3124514122",
  "https://openalex.org/W2153259910" = "https://openalex.org/W1983411204",
  "https://openalex.org/W1590996600" = "https://openalex.org/W1590996600",
  "https://openalex.org/W2086581722" = "https://openalex.org/W2798329923",
  "https://openalex.org/W2480782842" = "https://openalex.org/W596202368",
  "https://openalex.org/W1544005079" = "https://openalex.org/W1489372415",
  "https://openalex.org/W2314437616" = "https://openalex.org/W2078058648",
  "https://openalex.org/W2032987097" = "https://openalex.org/W2034276459",
  "https://openalex.org/W1593499730" = "https://openalex.org/W1981364594",
  "https://openalex.org/W2165448399" = "https://openalex.org/W3122374186")
))%>% mutate(cit = str_replace_all(cit, c(
  "https://openalex.org/W2150146195" = "https://openalex.org/W3121484141",
  "https://openalex.org/W2025894382" = "https://openalex.org/W3126016604",
  "https://openalex.org/W2008305937" = "https://openalex.org/W3125377858",
  "https://openalex.org/W3125999045" = "https://openalex.org/W2145855346",
  "https://openalex.org/W348020130" = "https://openalex.org/W2131360862",
  "https://openalex.org/W1999659239" = "https://openalex.org/W3122443843",
  "https://openalex.org/W259309357" = "https://openalex.org/W3126004017",
  "https://openalex.org/W1535271902" = "https://openalex.org/W3130376029",
  "https://openalex.org/W1992000778" = "https://openalex.org/W2034278834",
  "https://openalex.org/W310899332" = "https://openalex.org/W2053924168",
  "https://openalex.org/W2026785431"= "https://openalex.org/W3126055011",
  "https://openalex.org/W1988986431" = "https://openalex.org/W3121657281",
  "https://openalex.org/W2779693884"= "https://openalex.org/W2921522229",
  "https://openalex.org/W63493041" = "https://openalex.org/W2587822774",
  "https://openalex.org/W1977212245" = "https://openalex.org/W3126001794",
  "https://openalex.org/W2078936242" = "https://openalex.org/W3126104191",
  "https://openalex.org/W3142178200" = "https://openalex.org/W2122165510",
  "https://openalex.org/W2038705124" = "https://openalex.org/W3124514122",
  "https://openalex.org/W2153259910" = "https://openalex.org/W1983411204",
  "https://openalex.org/W1590996600" = "https://openalex.org/W1590996600",
  "https://openalex.org/W2086581722" = "https://openalex.org/W2798329923",
  "https://openalex.org/W2480782842" = "https://openalex.org/W596202368",
  "https://openalex.org/W1544005079" = "https://openalex.org/W1489372415",
  "https://openalex.org/W2314437616" = "https://openalex.org/W2078058648",
  "https://openalex.org/W2032987097" = "https://openalex.org/W2034276459",
  "https://openalex.org/W1593499730" = "https://openalex.org/W1981364594",
  "https://openalex.org/W2165448399" = "https://openalex.org/W3122374186")
))

cits <- cits %>% filter(!work %in% dupl)

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

cocits <- cocits %>% filter(!(work %in% dupl|work2 %in% dupl))

cocits2 <- cocits %>% 
  rename(work=work2, work2=work) %>% 
  group_by(work3= pmin(work, work2), 
           work4= pmax(work, work2)) %>% 
  summarize(n=n()/2) %>% 
  arrange(desc(n)) %>% 
  filter(n>4)%>% 
  rename(Source = work3, Target = work4, weight = n)

nodes <- data.frame(work=c(cocits2$Source, cocits2$Target)) %>% count(work) %>% 
  filter(n>1) %>% .$work %>% data.frame(id = .) %>% 
  left_join(works) %>% 
  select(id, display_name) %>% 
  rename(Label = display_name) 

G <- igraph::graph_from_data_frame(filter(cocits2, Source %in% nodes$id, Target %in% nodes$id), vertices=nodes, directed=F)
cmp <- components(G)
cmp$csize
nodes_cmp <- data.frame(id=1:length(cmp$csize), size=cmp$csize) %>% filter(size>50) %>% 
  left_join(data.frame(id=names(cmp$membership), mbp=cmp$membership, degree=as.numeric(degree(G))), by=c("id"="mbp")) %>% 
  rename(cmp=id, id=id.y)%>% left_join(works) %>% select(id, display_name) %>% 
  rename(Label = display_name) 


edges_cmp <- cits%>% filter(work %in% nodes_cmp$id) %>% 
  biblio_cocitation(ref = "work", source = "cit", weight_threshold = 4) %>% 
  select(Source, Target, weight) %>% arrange((weight)) 
edges_cmp%>% write_csv("edges_cmp.csv")

#cocits3 <- cocits2 %>% filter(Source %in% nodes_cmp$id, Target %in% nodes_cmp$id)   
#write.csv(cocits3, "cocits3.csv", row.names = F)
G2 <- graph_from_data_frame(
  edges_cmp,
  #cocits3, 
  nodes_cmp, directed=F)
nodes_cmp <- read_csv("inferred.csv") %>% select(Id, stat_inf_class, modularity_class) %>% rename(id=Id) %>% 
  left_join(nodes_cmp)


nodes_cmp %>% count(modularity_class, sort=T)
#leid <- 
  #cluster_louvain(G2)
#  cluster_leiden(G2, objective_function = "modularity",resolution_parameter = 6, n_iterations = 400, weights = NA)
#sort(table(leid$membership))
#sum(table(leid$membership)[table(leid$membership)>70])/sum(table(leid$membership))

eigen <- eigen_centrality(G2, weights = NA)$vector

cmp_nodes <- 
  #data.frame(id=leid$names, `Modularity Class`=leid$membership) %>% 
  nodes_cmp %>% left_join(data.frame(id=names(eigen), eigen=as.numeric(eigen))) %>% rename(Modularity.Class = modularity_class)

write.csv(cmp_nodes, "cmp_nodes.csv", row.names = F)

source("labelClust.R")
nodes_labelled <- labelClusts(cmp_nodes)

