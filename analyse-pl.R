library(tidyverse)
library(hunspell)

summary <- read.csv("poland-summary.csv")

stopwords <- 
    read.csv("stopwords.csv")$Polish %>%
    paste(collapse=",") %>%
    str_replace_all(" ", "") %>%
    str_split(",") %>%
    unlist %>%
    .[sapply(., function(x) {x != ""})] %>%
    sapply(function(x) {
        word <- hunspell_stem(x, dict="pl_PL") %>%
            unlist %>% first
        if (length(word) == 0 || is.na(word) ) {
            c(x)}
        else c(word) }) %>%
    tolower


sapply (1:50 , function (x) {
    summary$top_terms[x] %>%
    str_split(", ") %>%
    unlist %>%
    sapply(function(x) {
        word <- hunspell_stem(x, dict="pl_PL") %>%
            unlist %>% first
        if (length(word) == 0 || is.na(word) ) {
            c(x)}
        else c(word) }) %>%
    setdiff(stopwords) %>%
    paste(collapse=", ")}) -> summary$top_terms_clean

for (i in 1:50) {
    summary$difference[i] <- 
        100 - length(unlist(str_split(summary$top_terms_clean[i], ",")))
    }

write.csv(summary, "poland_summary_cleaned.csv")
