library(tidyverse)
library(httr)

works <- read.csv("works_oa+j.csv")

refs <- list()
cits <- list()


for (i in 13194:nrow(works)) {

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
    numb <- result$cited_by_count %/% 200
    if (result$cited_by_count %% 200 > 0) numb <- numb + 1
    for (m in 1:numb) {
      json <- GET(paste0(result$cited_by_api_url, "&per_page=200&page=", m))
      result_cit <- rjson::fromJSON(rawToChar(json$content))$results 
      if (length(result_cit) > 0) {
        for (j in 1:length(result_cit)) {
        cits <- bind_rows(cits, data.frame(work = works[i, 'id'], 
                                         cit = result_cit[[j]]$id))
        }
    }}
  }}
  print(i)
}


  write.csv(cits, "cits.csv")

  write.csv(refs, "refs.csv")

