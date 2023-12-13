library(tidyverse)
library(openalexR)

nodes <- read.csv("data/cits/nodes_w=6.csv") %>% 
  labelClusts() %>% 
  filter(!is.na(cluster_label))

works<- read_csv("data/raw/works_full.csv") %>% 
  filter(id %in% nodes$Id)

refs <- list()


for (i in 1:nrow(works)) {
  if (is.na(works$referenced_works[i])) next
  if (grepl("^c",  works$referenced_works[i])) {
    ref <- works$referenced_works[i]%>% gsub("\r\n", "", .) %>%  parse(text = .) %>% eval()
  } else ref <- works$referenced_works[i]
  refs <- bind_rows(refs, data.frame(Target=ref, Source=works$id[i]))
  print(i)
}


refs_out <- oa_fetch(entity = "works", identifier = refs$Target, verbose = T)

refs <- refs %>% 
  left_join(distinct(refs_out, id, .keep_all=T), by = c("Target" = "id")) %>% 
  left_join(select(nodes, Id, cluster_label), by=c("Source" = "Id"))

write_csv(refs, "data/cits/out_refs.csv")
