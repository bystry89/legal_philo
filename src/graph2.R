library(tidyverse)
library(igraph)
library(biblionetwork)
library(tidytext)
library(openalexR)

source("src/functions/recodeDupl.R")


  
  works <- mutate(read_csv("data/raw/works_classics.csv"), across(everything(),as.character))  %>% 
    bind_rows(mutate(read_csv("data/raw/cits.csv"), across(everything(),as.character))) %>% 
    bind_rows(mutate(read_csv("data/raw/works_j.csv"), across(everything(),as.character))) %>% 
    mutate(first_author = stringr::str_match(author, "au_display_name = \"(.+?)\"")[,2])

  
  
works <- works  %>% 
    filter(is.na(ratio) | ratio > 0.008) %>% 
    #filter(! id %in% badratio) %>% 
    distinct(id, .keep_all = T)
  
  

works$cited_by_count <- as.integer(works$cited_by_count)
pacman::p_load(progress)



pb <- progress_bar$new(total=nrow(works), width=60, clear=F,
                       format = " Downloading [:bar] :percent ETA= :eta")
  cits2 <- list()
for (i in 1:nrow(works)) {
  fetch <- oa_fetch(entity = "work", cites=works[i, "id"])
  if (!is.null(fetch)) {
  cits2 <- fetch %>% 
  mutate(Target = works[i,]$id) %>% 
  rename(Source = id) %>% bind_rows(cits2)
  }
  pb$tick()
}
  
#cits <- cits %>% filter(Target %in% works$id)

write_csv(cits, "data/raw/out_cits.csv")
cits <- read_csv("data/raw/out_cits.csv")

## to move up later##
dupl <- c("https://openalex.org/W310899332", 
          "https://openalex.org/W201347684", 
          "https://openalex.org/W3143175199",
          "https://openalex.org/W2027539254",
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
          "https://openalex.org/W3216089140",
          #to delete
          "https://openalex.org/W2189821821",
          "https://openalex.org/W398143332",
          "https://openalex.org/W2047504320",
          "https://openalex.org/W2794033063",
          "https://openalex.org/W1980367553")

works <- works %>% filter(!id %in% dupl)

cits <- cits %>% mutate(Target = str_replace_all(Target, c(
  "https://openalex.org/W3143175199" = "https://openalex.org/W3143175199",
  "https://openalex.org/W2027539254" = "https://openalex.org/W2083201648",
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
))%>% mutate(Source = str_replace_all(Source, c(
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


works <- works %>% mutate(id = dupl_titles(id)) %>% distinct(id, .keep_all = T)
works <- filterDupl(works, id)


cits <- cits %>% select(Target, Source) %>% 
  mutate(Target = dupl_titles(Target), Source = dupl_titles(Source)) %>% 
  distinct(Target, Source)

CITS <- cits %>% 
  count(Source, sort=T) %>% 
  filter(n >1) %>% 
  select(Source) %>% 
  left_join(cits) %>% 
  select(Target, Source)

pairs <- function(i, CITS) {CITS %>% 
  filter(Source == i) %>% 
  .$Target %>% 
  combn(., 2) %>% 
  t()}

cocits <- pbapply::pblapply(unique(CITS$Source), pairs, CITS) %>% do.call(rbind, .)


for (i in 1:length(unique(CITS$Source))) {
  cocits <- CITS %>% 
    filter(Source == unique(CITS$Source)[i]) %>% 
    select(Target) %>% 
    expand_grid(Target2=Target) %>% 
    filter(Target != Target2) %>% 
    bind_rows(cocits)
  pb$tick()
}
cocits <- data.frame(cocits) %>% rename(Target=X1, Target2=X2)
  write_csv(cocits, "data/cits/cocits2.csv")
cocits <- read_csv("data/cits/cocits2.csv")

cocits <- cocits %>% filter(Target %in% works$id & Target2 %in% works$id)


#cocits <- cocits %>% filter(!(Target %in% dupl|Target2 %in% dupl))

cocits2 <- cocits %>% 
  rename(Target=Target2, Target2=Target) %>% 
  group_by(Target3= pmin(Target, Target2), 
           Target4= pmax(Target, Target2)) %>% 
  summarize(n=n()/2) %>% 
  arrange(desc(n)) %>% 
  filter(#n>4)%>% 
    n>1) %>% 
  rename(Source = Target3, Target = Target4, weight = n)

nodes <- data.frame(work=c(cocits2$Source, cocits2$Target)) %>% count(work) %>% 
  filter(n>1) %>% .$work %>% data.frame(id = .) %>% 
  left_join(works) %>% 
  select(id, display_name) 

library(igraph)
G <- igraph::graph_from_data_frame(filter(cocits2, Source %in% nodes$id, Target %in% nodes$id), vertices=nodes, directed=F)
cmp <- components(G)
cmp$csize
nodes<- data.frame(id=1:length(cmp$csize), size=cmp$csize) %>% filter(size>50) %>% 
  left_join(data.frame(id=names(cmp$membership), mbp=cmp$membership, degree=as.numeric(degree(G))), by=c("id"="mbp")) %>% 
  rename(cmp=id, id=id.y)%>% left_join(works)
#%>% select(id, display_name) 


edges <- cits%>% filter(Target %in% nodes$id) %>% 
  biblio_cocitation(ref = "Target", source = "Source", weight_threshold = 2) %>% 
  select(Source, Target, weight) %>% arrange((weight)) 
edges%>% write_csv("data/cits/edges2.csv")


G2 <- graph_from_data_frame(
  edges,
  nodes, directed=F)
nodes <- read_csv("data/cits/inferred.csv") %>% select(Id, 
                                                       stat_inf_class,
                                                       modularity_class) %>% rename(id=Id) %>% 
  left_join(nodes)

leid <-cluster_louvain(G2)

#  cluster_leiden(G2, objective_function = "modularity",resolution_parameter = 6, n_iterations = 400, weights = NA)

#sort(table(leid$membership))

#sum(table(leid$membership)[table(leid$membership)>70])/sum(table(leid$membership))
nodes %>% count(modularity_class, sort=T)

eigen <- eigen_centrality(G2, weights = NA)$vector
betweenness <- betweenness(G2)
closeness <- closeness(G2)

nodes <- 
  nodes %>% 
  left_join(data.frame(id=names(eigen), eigen=as.numeric(eigen), betweenness=betweenness, closeness=closeness)) %>%
  rename(Modularity.Class = modularity_class) %>% 
  rename(Label = display_name)

source("src/functions/labelClust.R")
nodes_labelled <- labelClusts(nodes)

clusts <- nodes_labelled %>% 
  filter(!is.na(cluster_label)) %>% 
  .$Modularity.Class %>% unique()

source("src/functions/subgraphCentrality.R")

eigenMax <- list()
for (i in 1:length(clusts)) {
  eigenMax <- bind_rows(eigenMax, subgraphCentrality(clusts[i]))
}

nodes <- works %>% 
  select(display_name, publication_year, 
         first_author, 
         id) %>% 
 # right_join(eigenMax) %>% 
  mutate(Label = paste0(stringr::str_extract(first_author, "[A-Z][a-z]+$"), ", ", publication_year)) %>% 
  #select(-display_name, -first_author, -publication_year) %>% 
  right_join(nodes, by='id')
  #mutate(Label = if_else(is.na(Label), "", Label)
  #)

nodes <- nodes %>% 
  select(id, display_name, so, cited_by_count, publication_year, first_author, ratio) %>%  
  mutate(Label = paste0(stringr::str_extract(first_author, "[A-Z][a-z]+$"), ", ", publication_year))

write.csv(nodes, "data/cits/nodes.csv", row.names = F)

#remove Frankfurt, Rawls .008