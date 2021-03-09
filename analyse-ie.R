library(tidyverse)
library(hunspell)

summary <- read.csv("ireland-summary.csv")

stopwords <- 
    read.csv("stopwords.csv")$English %>%
    paste(collapse=",") %>%
    str_replace_all(" ", "") %>%
    str_split(",") %>%
    unlist %>%
    .[sapply(., function(x) {x != ""})] %>%
    sapply(function(x) {
        word <- hunspell_stem(x, dict="en_GB") %>%
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
        word <- hunspell_stem(x, dict="en_GB") %>%
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

write.csv(summary, "ireland_summary_cleaned.csv")
