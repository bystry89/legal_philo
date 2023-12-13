dupl_titles <- function(df) {
  recode(df,
                       #hart the concept
                       "https://openalex.org/W2035902442" = "https://openalex.org/W4233237171",
                       #Dworkin A matter
                       "https://openalex.org/W2073010516" = "https://openalex.org/W4242431379",
                       #owens a simple theory
                       "https://openalex.org/W4245966138" = "https://openalex.org/W1966835138",
                       #rawls a theory
                       "https://openalex.org/W4207012491" = "https://openalex.org/W4213439731",
                       "https://openalex.org/W4234578417" = "https://openalex.org/W4213439731",
                       #kelsen general theory
                       "https://openalex.org/W4248565099" = "https://openalex.org/W2122055745",
                       #mccormick
                       "https://openalex.org/W4230311549" = "https://openalex.org/W2166115596",
                       #williams moral luck
                       "https://openalex.org/W820207027" = "https://openalex.org/W4247225418",
                       #horton political obligation
                       "https://openalex.org/W4229544509" = "https://openalex.org/W4247770540",
                       #hart positivism
                       "https://openalex.org/W4246825995" = "https://openalex.org/W2989863638",
                       "https://openalex.org/W128243409" = "https://openalex.org/W2989863638",
                       # gardner the gist
                       "https://openalex.org/W3125277678" = "https://openalex.org/W2094143258",
                       # rawls the law
                       "https://openalex.org/W1981364594" = "https://openalex.org/W4231114141",
                       "https://openalex.org/W4235527692" = "https://openalex.org/W4231114141",
                       #raz the morality
                       "https://openalex.org/W4249036649" = "https://openalex.org/W2041607960",
                      #raz authority
                      "https://openalex.org/W2027539254" = "https://openalex.org/W4237574036",
         #austin plea
                      "https://openalex.org/W2311923057" = "https://openalex.org/W2342553802",
         #austin the province
         "https://openalex.org/W2483186065" = "https://openalex.org/W2311923057",
         #belamy
         "https://openalex.org/W4243162678" = "https://openalex.org/W1846881622",
         #coleman principle
         "https://openalex.org/W2262659060" = "https://openalex.org/W1527018558",
         # mccormick institutions
         "https://openalex.org/W4231007001" = "https://openalex.org/W2495969500",
         # mccormick rhetoric
         "https://openalex.org/W2256823583" = "https://openalex.org/W4247705233",
         # mccormick institutional theory
         "https://openalex.org/W2158632923" = "https://openalex.org/W181618895",
         # dworkin justice for hedgehogs
         "https://openalex.org/W4254058021" = "https://openalex.org/W1582197671",
         # Dworkin the model of rules
         "https://openalex.org/W1507985951" = "https://openalex.org/W4240732435",
         # hart punishment and responsibility
         "https://openalex.org/W1544005079" = "https://openalex.org/W1489372415",
         "https://openalex.org/W4214845982" = "https://openalex.org/W1489372415",
         # hart causation
         "https://openalex.org/W1579308153" = "https://openalex.org/W4237034108",
         # hirsh proportionality
         "https://openalex.org/W2017374803" = "https://openalex.org/W2064238556",
         # hochfeld
         "https://openalex.org/W4250371488" = "https://openalex.org/W1557385649",
         "https://openalex.org/W3143775862" = "https://openalex.org/W1557385649",
         # kelsen pure
         "https://openalex.org/W2795597305" = "https://openalex.org/W1485501857",
         "https://openalex.org/W4237608523" = "https://openalex.org/W1485501857",
         # kennedy form and substance
         "https://openalex.org/W2797638389" = "https://openalex.org/W4255738592",
         # kramer in defense of legal positism
         "https://openalex.org/W2500462578" = "https://openalex.org/W1568392343",
         # leiter naturalizing
         "https://openalex.org/W596798482" = "https://openalex.org/W4247328545",
         # murphy natural law
         "https://openalex.org/W1501768235" = "https://openalex.org/W3216089140",
         # holmes path
         "https://openalex.org/W2796592586" = "https://openalex.org/W4235023594",
         # raz the idea of public reason
         "https://openalex.org/W1469406" = "https://openalex.org/W4232037086",
         # scanlon moral dimensions
         "https://openalex.org/W1508831339" = "https://openalex.org/W4237918656",
         # schauer playing by the rules
         "https://openalex.org/W1997926766" = "https://openalex.org/W4245950606",
         # tamanaha on the rule of law
         "https://openalex.org/W4211037492" = "https://openalex.org/W591898085",
         # waldron law and disagreement
         "https://openalex.org/W4252495358" = "https://openalex.org/W2060498353")
}


filterDupl <- function(df, variable) {df %>% filter(!variable %in% c("https://openalex.org/W2035902442",
"https://openalex.org/W2073010516",
"https://openalex.org/W4245966138",
"https://openalex.org/W4207012491",
"https://openalex.org/W4234578417",
"https://openalex.org/W4248565099",
"https://openalex.org/W4230311549",
"https://openalex.org/W820207027",
"https://openalex.org/W4229544509",
"https://openalex.org/W4246825995",
"https://openalex.org/W3125277678",
"https://openalex.org/W1981364594",
"https://openalex.org/W4235527692",
"https://openalex.org/W4249036649"))}