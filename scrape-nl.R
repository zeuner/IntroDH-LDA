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

top_level_pages <- all_top_level_pages(url_first_page)

top_level_sessions <- function (page) {
    service <- url_service(page)
    lines <- read_lines_html_caching(page)
    sessions_start <- grep("<ul class=\"ladder\">", lines)
    session_starts <- Filter(
        function (start) {
            sessions_start < start
        },
        grep("<li[^<>]*>", lines)
    )
    titles <- grep("<span class=\"strong\">.*</span>", lines)
    report_links <- grep("<a href=\"[^\"]*verslag[^\"]*\"[^<>]*>", lines)
    sessions <- map(
        session_starts,
        function (session_start) {
            title_line <- Filter(
                function (line) {
                    session_start < line
                },
                titles
            ) %>% head(n = 1)
            title <- lines[
                title_line
            ] %>% str_replace(
                pattern = ".*<span class=\"strong\">(.*)</span>.*",
                replacement = "\\1"
            ) %>% str_replace(
                pattern = " - (.*) (.*) (.*) (.*) (.*) uur",
                replacement = "%\\1%\\2%\\3%\\4%\\5"
            ) %>% str_replace(
                pattern = "^(.*) (.*) (.*) (.*) (.*) uur",
                replacement = "unspecified%\\1%\\2%\\3%\\4%\\5"
            )
            report_link_line <- Filter(
                function (line) {
                    session_start < line
                },
                report_links
            ) %>% head(n = 1)
            report_link <- lines[
                report_link_line
            ] %>% str_replace(
                pattern = ".*<a href=\"([^\"]*verslag[^\"]*)\"[^<>]*>.*",
                replacement = "\\1"
            ) %>% str_replace(
                pattern = "^(/)",
                replacement = paste0(service, "\\1")
            )
            c(title, report_link)
        }
    )
    Filter(
        function (session) {
            1 < length(session)
        },
        sessions
    )
}

map(
    top_level_pages,
    top_level_sessions
) %>% unlist(
    recursive = FALSE
)
