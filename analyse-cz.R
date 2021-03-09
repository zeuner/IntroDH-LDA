library(tidyverse)

summary <- read.csv("czechia-summary.csv")

stopwords <- 
    read.csv("stopwords.csv")$Czech %>%
    paste(collapse=",") %>%
    str_replace_all(" ", "") %>%
    str_split(",") %>%
    unlist %>%
    .[sapply(., function(x) {x != ""})] %>%
    tolower


sapply (1:50 , function (x) {
    summary$top_terms[x] %>%
    str_split(", ") %>%
    unlist %>%
    setdiff(stopwords) %>%
    paste(collapse=", ")}) -> summary$top_terms_clean

for (i in 1:50) {
    summary$difference[i] <- 
        100 - length(unlist(str_split(summary$top_terms_clean[i], ",")))
    }

write.csv(summary, "czechia_summary_cleaned.csv")
