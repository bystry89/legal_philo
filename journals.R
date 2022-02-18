library(tidyverse)
library(httr)


nullToNA <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}



journals <- c(#Ratio Juris
"V183135890",
#CLP
"V56047584",
#Law & Philosopohy
"V153292947",
#Legal Theory
"V139242520",
#Canadian J of LJ
"V2764647375",
#The American J of J
"V2735326605",
#Jurisprudence
"V2737598823")

articles <- list()
inst <- list()
authors <- data.frame()

for (i in journals) {
  url <- paste("https://api.openalex.org/works?filter=host_venue.id:", i,sep='')
  json <- GET(url)
  results <- rjson::fromJSON(rawToChar(json$content)) %>% nullToNA()
  numb <- results$meta$count %/% 200
  for (m in 1:(numb+1)) {
    arts <- list()
    auths <- list()
    ins <- list()
    url <- paste("https://api.openalex.org/works?filter=host_venue.id:", i, "&per_page=200&page=", m, sep='')
    json <- GET(url)
    results <- rjson::fromJSON(rawToChar(json$content))$results %>% nullToNA()
    for (result in results) {
      result$host_venue <- nullToNA(result$host_venue)
      result$ids <- nullToNA(result$ids)
      df <- as.data.frame(result[c('id','display_name', 'publication_year', "type")])
      df <- bind_cols(df, data.frame(journal=result$host_venue$display_name, journal_id = result$host_venue$id, 
                                     doi = result$ids$doi))
      if (length(result$authorships)>0) df$first_author <- result$authorships[[1]]$author$display_name
      arts <- bind_rows(df, arts)
      if (length(result$authorships)==0) next
      for (j in result$authorships) {
        j <- nullToNA(j)
        auth <- data.frame(item = result$id,
                           position= j$author_position,
                           id = j$author$id,
                           name = j$author$display_name)
        auths <- bind_rows(auths, auth)
        
        for (k in j$institutions) {
          ins <- bind_rows(ins, data.frame(item = result$id, 
                                             nullToNA(k)))
        }
      }
    }
    articles <- bind_rows(articles, arts)
    authors <- bind_rows(authors, auths)
    inst <- bind_rows(inst, ins)
    print(paste("journal: ", i, ", round: ", m, "/", numb+1, sep=""))
  }
}

#filter
articles <- articles %>% anti_join(read.csv("works.csv"), by='id')
authors <- authors %>% anti_join(read.csv("works.csv"), by=c('item'='id'))
inst <- inst %>% anti_join(read.csv("works.csv"), by=c('item'='id'))

bind_rows(articles, read.csv("works.csv")) %>% 
  write.csv("works.csv")

bind_rows(authors, read.csv("authors.csv")) %>% 
  write.csv("authors.csv")

bind_rows(inst, read.csv("insts.csv")) %>% 
  write.csv("insts.csv")          
