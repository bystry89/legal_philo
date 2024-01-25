library(tidyverse)
library(gridExtra)
library(grid)
library(xtable)

centrTable <- function(x) {
  x <- x %>%
    mutate(display_name= sapply(display_name, 
                                function(x) {x |> 
                                    strwrap(width = 45) |> 
                                    paste(collapse = "\n")})) %>% 
    rename("First author" = "first_author",
           "Title" = "display_name") %>% 
    tableGrob(theme = ttheme_minimal(base_size=6, padding = unit(c(4, 2), "mm")))
}

#tokenized <- read.csv("data/text/tokenized_w=6.csv")
source("../src/graph_analysis.R")

tablePlot <- function(x) {
  p <- tf_idf(x) 
  
  t <- subgraphCentrality(x) %>% centrTable()
  grid.arrange(p, t, ncol=2, heights=unit(0.5, "npc"), widths=c(1/3,2/3))
}

