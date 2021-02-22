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
                replacement = "_\\1_\\2_\\3_\\4_\\5%"
            ) %>% str_replace(
                pattern = " - (.*) (.*) (.*) (.*), (plenaire vergadering|Verenigde Vergadering)$",
                replacement = "_\\1_\\2_\\3_\\4_NA_\\5"
            ) %>% str_replace(
                pattern = "^(.*) (.*) (.*) (.*) (.*) uur",
                replacement = "NA_\\1_\\2_\\3_\\4_\\5%"
            ) %>% str_replace(
                pattern = "%$",
                replacement = "_NA"
            ) %>% str_replace(
                pattern = "%, ",
                replacement = "_"
            ) %>% str_replace(
                pattern = "^(.*) (.*) (.*) (.*), (plenaire vergadering|Verenigde Vergadering)$",
                replacement = "NA_\\1_\\2_\\3_\\4_NA_\\5"
            ) %>% str_replace(
                pattern = "_(Maan|Dins|Woens|Vrij|Zater)dag_",
                replacement = "_%_"
            ) %>% str_replace(
                pattern = "_%_([0-9])_",
                replacement = "_0\\1_"
            ) %>% str_replace(
                pattern = "_%_",
                replacement = "_"
            ) %>% str_replace(
                pattern = "_januari_",
                replacement = "_01_"
            ) %>% str_replace(
                pattern = "_februari_",
                replacement = "_02_"
            ) %>% str_replace(
                pattern = "_maart_",
                replacement = "_03_"
            ) %>% str_replace(
                pattern = "_april_",
                replacement = "_04_"
            ) %>% str_replace(
                pattern = "_mei_",
                replacement = "_05_"
            ) %>% str_replace(
                pattern = "_juni_",
                replacement = "_06_"
            ) %>% str_replace(
                pattern = "_juli_",
                replacement = "_07_"
            ) %>% str_replace(
                pattern = "_september_",
                replacement = "_09_"
            ) %>% str_replace(
                pattern = "_oktober_",
                replacement = "_10_"
            ) %>% str_replace(
                pattern = "_november_",
                replacement = "_11_"
            ) %>% str_replace(
                pattern = "_december_",
                replacement = "_12_"
            ) %>% str_replace(
                pattern = "^([0-9]*)e vergadering_",
                replacement = "\\1_"
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
## for now, exclude PDF-based stenograms since they can be split into
## different debate items less easily
## TODO: implement PDF scraping
            (
                1 < length(session)
            ) && (
                0 == length(grep("[.]pdf", session[[2]]))
            )
        },
        sessions
    )
}

sessions <- map(
    top_level_pages,
    top_level_sessions
) %>% unlist(
    recursive = FALSE
)

session_debates <- function (session) {
    lines <- read_lines_html_caching(session[2])
    lines[
        grep("<h2><a id=\".*\"></a>", lines)
    ] %>% str_replace(
        pattern = "<h2><a id=\".*\"></a>",
        replacement = ""
    )
}
