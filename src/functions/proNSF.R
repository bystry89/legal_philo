library(tidyverse)

clasNSF <- function(x) {
  x %>% 
    mutate(ESpecialite = case_when(
      revue == "SYNTHESE" ~ "Philosophy",
      revue == "ETHICS" ~ "Philosophy",
      revue == "SOCIAL PHILOSOPHY & POLICY" ~ "Philosophy",
      revue == "JOURNAL OF POLITICAL PHILOSOPHY" ~ "Philosophy",
      revue == "PHILOSOPHICAL PSYCHOLOGY" ~ "Philosophy",
      revue == "PHILOSOPHY & SOCIAL CRITICISM" ~ "Philosophy",
      revue == "PHILOSOPHY OF THE SOCIAL SCIENCES" ~ "Philosophy",
      revue == "PHILOSOPHY & PUBLIC AFFAIRS" ~ "Philosophy",
      revue == "PHILOSOPHY OF THE SOCIAL SCIENCES" ~ "Philosophy",
      .default = ESpecialite
    ))
}

proNSF <- function(x) {
  x %>% 
  mutate(so = toupper(so),
         so = case_when(
           so == "NOÛS" ~ "NOUS",
           so == "INTERNATIONAL JOURNAL FOR THE SEMIOTICS OF LAW" ~ "INTERNATIONAL JOURNAL FOR THE SEMIOTICS OF LAW-REVUE INTERNATIONALE DE SEMIOTIQUE JURIDIQUE",
           so == "INQUIRY: AN INTERDISCIPLINARY JOURNAL OF PHILOSOPHY" ~ "INQUIRY-AN INTERDISCIPLINARY JOURNAL OF PHILOSOPHY",
           so == "INQUIRY" ~ "INQUIRY-AN INTERDISCIPLINARY JOURNAL OF PHILOSOPHY",
           so == "THE PHILOSOPHICAL QUARTERLY" ~ "PHILOSOPHICAL QUARTERLY",
           so == "THE JOURNAL OF POLITICS" ~ "JOURNAL OF POLITICS",
           so == "THE MONIST" ~ "MONIST",
           so == "THE REVIEW OF POLITICS" ~"REVIEW OF POLITICS",
           so == "EPISTEME" ~"EPISTEME-A JOURNAL OF INDIVIDUAL AND SOCIAL EPISTEMOLOGY",
           so == "THE PHILOSOPHICAL REVIEW" ~ "PHILOSOPHICAL REVIEW",
           so == "HYPATIA: A JOURNAL OF FEMINIST PHILOSOPHY" ~ "HYPATIA-A JOURNAL OF FEMINIST PHILOSOPHY",
           .default = so)) %>% 
    inner_join(journals_class, by = c("so" = "revue")) %>% 
    # anti_join(nodes, by = c("Target" = "Id"))%>% 
    mutate(disc = case_when(
      ESpecialite == "Law" ~ "Law",
      ESpecialite == "Philosophy" ~ "Philosophy",
      ESpecialite == "Political Science and Public Administration" ~ "Political Science and Public Administration",
      ESpecialite == "Economics" ~ "Economics",
      ESpecialite == "Computers" ~ "Computers",
      ESpecialite == "Management" ~ "Management",
      ESpecialite == "Criminology" ~ "Criminology",
      EDiscipline == "Health" ~ "Health",
      EDiscipline == "Psychology" ~ "Psychology",
      EDiscipline == "Social Sciences" ~ "Other Social Sciences",
      EDiscipline == "Professional Fields" ~ "Other Professional Fields",
      EDiscipline %in% c("Humanities", "Arts") ~ "Other Humanities",
      EGrande_Discipline == "Natural Sciences and Engineering" ~ "Other NSE"
    ))
}