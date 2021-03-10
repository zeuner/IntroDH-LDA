library(tidyverse)
library(hunspell)

summary <- read.csv("austria-summary.csv")

top_terms_list <- lapply (1:50 , function (x) {
    summary$top_terms[x] %>%
    str_split(", ") %>%
    unlist %>%
    sapply(function(x) {
        word <- hunspell_stem(x, dict="de_AT") %>%
            unlist %>% first
        if (length(word) == 0 || is.na(word) ) {
            c(x)}
        else c(word) })}) 

intersections <- NULL

for (i in 1:49) {
    for (j in (i+1):50) {
        intersections <- c(intersections, intersect(top_terms_list[[i]], 
                                                    top_terms_list[[j]]))
    }
}

intersections <- unique(intersections)

for (i in 1:50) {
    topic_spec_terms <- setdiff(top_terms_list[[i]], intersections)
    summary$top_terms_clean[i] <- paste(topic_spec_terms, collapse=", ")
    }

for (i in 1:50) {
    summary$difference[i] <- 
        100 - length(unlist(str_split(summary$top_terms_clean[i], ",")))
    }

write.csv(summary, "austria_summary_cleaned.csv")
