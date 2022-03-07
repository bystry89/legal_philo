library(bib2df)
library(tidyverse)
library(httr)


source("escapeLatexSpecials.R")

nullToNA <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}

bib <- bib2df("philosophy-of-law.bib")

doi <- bib %>% filter(!is.na(DOI))



articles <- list()
inst <- list()
authors <- data.frame()
faulty <- c()
n <- 0
for (i in doi$DOI[6964:nrow(doi)]) {
  n = n + 1
  print(n)
  url <- paste("https://api.openalex.org/works/doi:", i,'?username=piotr.bystranowski@uj.edu.pl',sep='')
  json <- GET(url)
  if (json$status_code == 404) {
    faulty <- c(faulty, i)    
    next
  }

  result <- rjson::fromJSON(rawToChar(json$content)) %>% nullToNA()
  result$host_venue <- nullToNA(result$host_venue)
  df <- as.data.frame(result[c('id','display_name', 'publication_year', "type")])
  df <- bind_cols(df, data.frame(journal=result$host_venue$display_name, journal_id = result$host_venue$id, 
                                 doi = result$ids$doi))
  if (length(result$authorships)>0) df$first_author <- result$authorships[[1]]$author$display_name
  if (!is.null(result$cited_by_count)) df$citations <- result$cited_by_count
  articles <- bind_rows(df, articles)
  if (length(result$authorships)==0) next
  for (j in result$authorships) {
    j <- nullToNA(j)
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

#write_csv(articles, "articles.csv")
#write_csv(authors, "authors.csv")
#write_csv(inst, "inst.csv")
#write_csv(faulty, "faulty_doi.csv")



#### STEP2
#faulty <- read.csv("faulty_doi.csv")

faulty <- data.frame(faulty)


resid <- bib %>% filter(is.na(DOI)) %>% 
  bind_rows(filter(bib, DOI %in% faulty$faulty)) 

for (i in 1:nrow(resid)){
resid[i,"title"] <- escapeLatexSpecials(resid[i,"TITLE"])
}

resid$title <- gsub("[\\:\\,\\.\\}\\&\\\\\\;]", "", resid$title)
resid$title <- gsub("[\\/\\-\\?]", " ", resid$title)
resid$title <- gsub("\\'", '\\’', resid$title)
resid$title <- stringr::str_extract(resid$title, "^[\\w\\’\\-\\\"\\(\\) ]+(\\s|\\z)")
resid$title <- gsub(" ", '%20', resid$title)

articles2 <- list()
authors2 <- data.frame()
inst2 <- list()
faulty2 <- c()
for (i in 3010:nrow(resid)) {
  url <- paste('https://api.openalex.org/works?filter=display_name.search:"', 
               resid[i,"title"],'"',sep='')
  json <- GET(url)
  if (json$status_code == 404) {
    faulty2 <- c(faulty2, i)    
    next
  }
  results <- rjson::fromJSON(rawToChar(json$content)) %>% nullToNA()
  if (results$meta$count != 1) {
    resid[i, "count"] = results$meta$count
    next
  }
  result <- results$results
  result[[1]] <- nullToNA(result[[1]])
  result[[1]]$host_venue <- nullToNA(result[[1]]$host_venue)
  df <- as.data.frame(result[[1]][c('id','display_name', 'publication_year', "type")])
  df <- bind_cols(df, data.frame(journal=result[[1]]$host_venue$display_name, 
                                 journal_id = result[[1]]$host_venue$id, 
                                 first_author = result[[1]]$authorships[[1]]$author$display_name))
  if (!is.null(result$cited_by_count)) df$citations <- result$cited_by_count
  articles2 <- bind_rows(df, articles2)
  if (length(result[[1]]$authorships)==0) next
  for (j in result[[1]]$authorships) {
    j <- nullToNA(j)
    j[[1]] <- nullToNA(j[[1]])
    auth <- data.frame(item = result[[1]]$id,
                       position= j$author_position,
                       id = j$author$id,
                       name = j$author$display_name)
    authors2 <- bind_rows(authors2, auth)
    
    for (k in j$institutions) {
      inst2 <- bind_rows(inst2, data.frame(item = result[[1]]$id, 
                                         nullToNA(k)))
    }
  }
  print(i)
}

write_csv(articles2, "articles2.csv")
write_csv(authors2, "authors2.csv")
write_csv(inst2, "inst2.csv")

### step 2a
resid_a <- resid %>% filter(count==0) %>% select(-count)

resid_a$title <- gsub("\\’", "\\'", resid_a$title)

articles2a <- list()
authors2a <- data.frame()
inst2a <- list()
for (i in 1:nrow(resid_a)) {
  url <- paste('https://api.openalex.org/works?filter=display_name.search:"', 
               resid_a[i,"title"],'"',sep='')
  json <- GET(url)
  if (json$status_code == 404) {
    faulty2a <- c(faulty2a, i)    
    next
  }
  results <- rjson::fromJSON(rawToChar(json$content)) %>% nullToNA()
  if (results$meta$count != 1) {
    resid_a[i, "count"] = results$meta$count
    next
  }
  result <- results$results
  result[[1]] <- nullToNA(result[[1]])
  result[[1]]$host_venue <- nullToNA(result[[1]]$host_venue)
  df <- as.data.frame(result[[1]][c('id','display_name', 'publication_year', "type")])
  df <- bind_cols(df, data.frame(journal=result[[1]]$host_venue$display_name, 
                                 journal_id = result[[1]]$host_venue$id, 
                                 first_author = result[[1]]$authorships[[1]]$author$display_name))
  if (!is.null(result$cited_by_count)) df$citations <- result$cited_by_count
  articles2a <- bind_rows(df, articles2a)
  if (length(result[[1]]$authorships)==0) next
  for (j in result[[1]]$authorships) {
    j <- nullToNA(j)
    j[[1]] <- nullToNA(j[[1]])
    auth <- data.frame(item = result[[1]]$id,
                       position= j$author_position,
                       id = j$author$id,
                       name = j$author$display_name)
    authors2a <- bind_rows(authors2a, auth)
    
    for (k in j$institutions) {
      inst2a <- bind_rows(inst2a, data.frame(item = result[[1]]$id, 
                                           nullToNA(k)))
    }
  }
  print(i)
}

### step 3
resid_b <- resid %>% filter(count>1) %>% 
  bind_rows(filter(resid_a, count>1)) %>% select(-count)
resid_num <- resid_b %>% filter(!is.na(as.numeric(YEAR)))
articles3 <- list()
authors3 <- data.frame()
inst3 <- list()

for (i in 1278:nrow(resid_num)) {
  url <- paste('https://api.openalex.org/works?filter=display_name.search:"', 
               resid_num[i,"title"], 
               '",publication_year:',
               resid_num[i,"YEAR"],sep='')
  json <- GET(url)
  if (json$status_code == 404) {
    next
  }
  results <- rjson::fromJSON(rawToChar(json$content)) %>% nullToNA()
  if (results$meta$count != 1) {
    resid_num[i, "count"] = results$meta$count
    next
  }
  result <- results$results
  result[[1]] <- nullToNA(result[[1]])
  result[[1]]$host_venue <- nullToNA(result[[1]]$host_venue)
  df <- as.data.frame(result[[1]][c('id','display_name', 'publication_year', "type")])
  df <- bind_cols(df, data.frame(journal=result[[1]]$host_venue$display_name, 
                                 journal_id = result[[1]]$host_venue$id))
  if (length(result$authorships)>0) df$first_author <- result$authorships[[1]]$author$display_name
  articles3 <- bind_rows(df, articles3)
  if (length(result[[1]]$authorships)==0) next
  for (j in result[[1]]$authorships) {
    j <- nullToNA(j)
    auth <- data.frame(item = result[[1]]$id,
                       position= j$author_position,
                       id = j$author$id,
                       name = j$author$display_name)
    authors3 <- bind_rows(authors3, auth)
    
    for (k in j$institutions) {
      inst3 <- bind_rows(inst3, data.frame(item = result[[1]]$id, 
                                           nullToNA(k)))
    }
  }
  print(i)
}


bind_rows(articles,
          articles2,
          articles2a,
          articles3) %>% write.csv("works_oa.csv")

bind_rows(authors,
          authors2,
          authors2a,
          authors3) %>% write.csv("authors_oa.csv")

bind_rows(inst,
          inst2,
          inst2a,
          inst3) %>% write.csv("insts_oa.csv")
