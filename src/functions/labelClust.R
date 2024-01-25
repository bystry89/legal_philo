library(tidyverse)

labelClusts <- function(x, community = x$modularity_class) {
  mutate(x, cluster_label = 
dplyr::case_when(
  community == 5 ~ "General jurisprudence",
  community == 4 ~ "Law and political theory",
  community == 15 ~ "Punishment",
  community == 2 ~ "Responsibility",
  community == 0 ~ "Judicial review and constitutional rights",
  community == 19 ~ "Torts / causation",
  community == 9 ~ "Theory of rights / Contract law",
  community == 20 ~ "Justifications and excuses",
  community == 8 ~ "Self-defence / War / Killing",
  community == 3 ~ "Legal reasoning",
  community == 12 ~ "Sovereignty / Pluralism",
  community == 10 ~ "Evidence and proof",
  community == 17 ~ "Natural law",
  community == 7 ~ "Non-positivism (Alexy & Radbruch)",
  community == 13 ~ "Hate speech",
  community == 16 ~ "Risk and prevention in criminal law",
  community == 18 ~ "Consent"
))
}
