library(tidyverse)
library(httr)
library(openalexR)


nullToNA <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}



journals <- c(#Ratio Juris
"S183135890",
#CLP
"S56047584",
#Law & Philosopohy
"S153292947",
#Legal Theory
"S139242520",
#Canadian J of LJ
"S2764647375",
#The American J of J
"S2735326605",
#Jurisprudence
"S2737598823",
#revus
"S4210187661",
#ARSP
"S3978835",
#Rechstheorie
"S99587756",
#DOXA -- nao tem
"S4210168614",
#Isonomía - Revista de teoría y filosofía del derecho
"S4210208673",
#Archiwum Filozofii Prawa i Filozofii Społecznej
"S4210214304",
#Analisi e Diritto
"S4306502022",
#Problema. Anuario de Filosofía y Teoría del Derecho
"S4210226858",
#Revista Brasileira de Filosofia do Direito
"S4210221729",
#Schriften zur Rechtstheorie
"S4210205342",
#Rivista internazionale di filosofia del diritto
"S4306528662",
#Archives de philosophie du droit
"S4210226380",
#american journal again
"S4210218839",
#jurisprudence again
"S4210200275",
#arsp again
"S4306503144",
"S4306502848",
"S4306502847",
"S4306502846")

articles <- 
  oa_fetch(entity = 'works', primary_location.source.id = journals, verbose = T)





articles <- apply(articles,2,as.character) 
articles <- data.frame(articles)

#works <- bind_rows(articles, data.frame(apply(works,2,as.character) ))
write.csv(articles, "data/raw/works_j.csv")




