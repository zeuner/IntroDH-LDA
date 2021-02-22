## Description: Script for fetching Dutch parliamentary protocols
## Author: Isidor Zeuner
## dh2021@quidecco.de
## For license see: https://mit-license.org
## -------------------------------------------------------------------

library(tidyverse)
library(rlist)
library(jsonlite)
source("scrape.R")

## -------------------------------------------------------------------
## Loading the parliamentary archive

url_first_page <- "https://www.eerstekamer.nl/debat_gemist_2"

## find all pages
## we'll read them again later for the content, which should be fine
##  when caching is used
all_top_level_pages <- function (first) {
    service <- url_service(first)
    lines <- read_lines_html_caching(first)
    next_candidates <- lines[
        grep("<span>verder</span>", lines) - 1
    ]
    next_url <- next_candidates[
        grep("<a href=\"[^\"]*\"[^<>]*>", next_candidates)
    ] %>% head(n = 1) %>% str_replace(
        pattern = ".*<a href=\"([^\"]*)\"[^<>]*>.*",
        replacement = "\\1"
    ) %>% str_replace(
        pattern = "^(/)",
        replacement = paste0(service, "\\1")
    )
    if (0 == length(next_url)) {
        return (c(first))
    }
    list.prepend(all_top_level_pages(next_url), first)
}

all_top_level_pages(url_first_page)
