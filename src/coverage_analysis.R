library(tidyverse)

cov <- read_csv("../data/raw/works_j.csv")  %>% 
  mutate(anglo = if_else(so %in% c("The Canadian Journal of Law and Jurisprudence", "The American journal of jurisprudence", 
                                   "The American Journal of Jurisprudence", "Legal Theory", 
                                   "Law and Philosophy", "Jurisprudence", "Criminal Law and Philosophy"), "anglo", "not-anglo")) %>% 
  mutate(refs = if_else(is.na(referenced_works), "no", "yes")) %>% select(anglo, refs)

cov_prop <- cov %>% 
  count(anglo, refs) %>% 
  group_by(anglo) %>% 
  mutate(prop=n/sum(n))

chi <- cov%>% table() %>% chisq.test()


