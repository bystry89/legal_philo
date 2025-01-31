library(tidyverse)
library(httr)
library(openalexR)


nullToNA <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}



journals <- c(#Philosophy of Science
"s147691735",
#The British Journal for the Philosophy of Science
"s107049385",
#Synthese
"s255146",
#Studies in History and Philosophy of Science
"s115229607",
"s21220828",
"s77764115",
"s115229607",
#Erkenntnis
"s63151042",
# International Studies in the Philosophy of Science
"s148888956",
#Journal for General Philosophy of Science
"s144298576",
#Foundations of Science
"s137799894",
#Hyle
"s951423404",
#History and Philosophy of the Life Sciences
"s120468566",
#European Journal for Philosophy of Science
"s2764495318",
#Biology & Philosophy
"s75346102",
#Theoretical Medicine and Bioethics
"s132915290",
#Journal of Medicine and Philosophy
"s166373307",
"s2734402716",
#Medicine Health Care and Philosophy
"s121541055")

articles <- 
  oa_fetch(entity = 'works', primary_location.source.id = journals, verbose = T)





articles <- apply(articles,2,as.character) 
articles <- data.frame(articles)

write_csv(articles, "data/raw/PoS_works_j.csv")


