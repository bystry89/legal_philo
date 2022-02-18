library(tidyverse)

works <- read.csv("works.csv")

refs <- list()
cits <- list()


for (i in 978:nrow(works)) {

  url <- paste("https://api.openalex.org/works:", works[i,"id"],'?username=piotr.bystranowski@uj.edu.pl',sep='')
  json <- GET(url)
  if (json$status_code == 404) {
    faulty <- c(faulty, i)    
    next
  }
  result <- rjson::fromJSON(rawToChar(json$content)) 
  if (!is.null(result$referenced_works)) if (length(result$referenced_works)>0) {
  refs <- bind_rows(refs, data.frame(work = works[i, 'id'], 
                                     refs = result$referenced_works))
  }
  if (!is.null(result$cited_by_count)) {
  works[i, 'citations'] <- result$cited_by_count
  
  if (result$cited_by_count > 0) {
    json <- GET(result$cited_by_api_url)
    result_cit <- rjson::fromJSON(rawToChar(json$content))$results 
    if (length(result_cit) > 0) {
    for (j in 1:length(result_cit)) {
      cits <- bind_rows(cits, data.frame(work = works[i, 'id'], 
                                         cit = result_cit[[j]]$id))
    }}
  }}
  print(i)
}

bind_rows(cits, read.csv("cits.csv")) %>% 
  write.csv("cits.csv")
bind_rows(refs, read.csv("refs.csv")) %>% 
  write.csv("refs.csv")

