library(bib2df)

library(tidyverse)
library(httr)
library(data.table)


nullToNA <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}
depth <- function(this) ifelse(is.list(this), 1L + max(sapply(this, depth)), 0L)

bind_at_any_depth <- function(l) {
  if (depth(l) == 2) {
    return(bind_rows(l))
  } else {
    l <- at_depth(l, depth(l) - 2, bind_rows)
    bind_at_any_depth(l)
  }

}

bib <- bib2df("philosophy-of-law.bib")

doi <- bib %>% filter(!is.na(DOI))


articles <- data.frame()
authors <- data.frame()
inst<- data.frame()
faulty <- c()
n <- 0
for (i in doi$DOI[1749:nrow(doi)]) {
  n = n + 1
  print(n)

articles <- list()
authors <- data.frame()
faulty <- c()
n <- 0
for (i in doi$DOI[100:103]) {

  url <- paste("https://api.openalex.org/works/doi:", i,'?username=piotr.bystranowski@uj.edu.pl',sep='')
  json <- GET(url)
  if (json$status_code == 404) {
    faulty <- c(faulty, i)    
    next
  }

  result <- rjson::fromJSON(rawToChar(json$content)) %>% nullToNA()
  result$host_venue <- nullToNA(result$host_venue)
  df <- as.data.frame(result[c('id','display_name', 'publication_year')])
  df <- bind_cols(df, data.frame(journal=result$host_venue$display_name, journal_id = result$host_venue$id, 
                                 first_author = result$authorships[[1]]$author$display_name,
                                 doi = result$ids$doi))
  articles <- bind_rows(df, articles)
  if (length(result$authorships)==0) {next}
  for (j in result$authorships) {
    j <- nullToNA(j)

  result <- rjson::fromJSON(rawToChar(json$content))
  df <- as.data.frame(result[c('id','doi', 'title', 'publication_year')])
  df <- bind_cols(df, data.frame(journal=result$host_venue$display_name, journal_id = result$host_venue$id, 
                                 first_author = result$authorships[[1]]$author$display_name))
  articles <- bind_rows(result, articles)
  for (j in result$authorships) {

    auth <- data.frame(item = result$id,
                       position= j$author_position,
                       id = j$author$id,
                       name = j$author$display_name)
    authors <- bind_rows(authors, auth)

    for (k in j$institutions) {
      inst <- bind_rows(inst, data.frame(item = result$id, 
                                         nullToNA(k)))
      
      
    }
  
  }
  

    }


  }
  
  n = n + 1
  n
    }
