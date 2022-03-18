library(tidyverse)

labelClusts <- function(x, community = x$Modularity.Class) {
  mutate(x, cluster_label = 
dplyr::case_when(
  community == 0 ~ "Argumentation",
  community == 12 ~ "General jurisprudence",
  community == 11 ~ "Punishment and responsibility",
  community == 9 ~ "Justice and fairness",
  community == 2 ~ "Global justice",
  community == 7 ~ "Private law / Legal realism and descendants",
  community == 5 ~ "Constitutionalism / Legal pluralism",
  community == 19 ~ "Criminal law",
  community == 6 ~ "Law and political theory",
  community == 8 ~ "Natural law and non-positivism",
  community == 1 ~ "International (criminal) law",
  community == 13 ~ "Expert systems",
  community == 20 ~ "New theories / Reinterpretations",
  community == 21 ~ "Promising / Reasonableness",
  community == 23 ~ "Classical natural law",
  community == 3 ~ "Legal epistemology",
  community == 18 ~ "Corporate personhood",
  community == 17 ~ "Hate speech", 
  community == 4 ~ "Kant / Kelsen"
))
}
