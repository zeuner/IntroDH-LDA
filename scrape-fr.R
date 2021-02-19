## Description: Script for fetching French parliamentary protocols
## Author: Isidor Zeuner
## dh2021@quidecco.de
## For license see: https://mit-license.org
## -------------------------------------------------------------------

library(tidyverse)
library(jsonlite)
source("scrape.R")

## avoid runtime system locale configuration to get in the way
Sys.setlocale('LC_ALL','C')

## -------------------------------------------------------------------
## Loading the parliamentary archive

url_top_level <- "https://www.assemblee-nationale.fr/14/debats/"

url_top_level_service <- url_service(
    url_top_level
)

archive_top_level <- read_lines_html_caching(url_top_level)

session_categories <- archive_top_level[
    grep(
        "liens-liste",
        archive_top_level
    ) %>% (
        function (tag_start) { tag_start + 1 }
    )
] %>% str_replace(
    pattern = ".*href=\"([^\"]*)\".*",
    replacement = paste0(url_top_level_service, "\\1")
)

category_meetings <- function (category_url) {
    directory <- url_directory(category_url)
    category_data <- read_lines_html_caching(category_url)
    dates <- grep("class=\"date\"", category_data)
    meetings <- grep("h[1] class=\"seance\"", category_data)
    meetings %>% map(
        .f = function (meeting_line) {
            applicable_date <- dates %>% Filter(
                f = function (date_line) { date_line < meeting_line }
            ) %>% max
            date_text <- category_data[applicable_date] %>% str_replace(
                pattern = "<sup>([^<>]*)</sup>",
                replacement = "\\1"
            ) %>% str_replace(
                pattern = ".*>([^<>]+)<.*",
                replacement = "\\1"
            ) %>% str_replace(
                pattern = ".* (.*) (.*) (.*)",
                replacement = "\\1|\\2|\\3"
            ) %>% str_replace(
                pattern = "^1er\\|",
                replacement = "1|"
            ) %>% str_replace(
                pattern = "(^[0-9]\\|)",
                replacement = "0\\0"
            ) %>% str_replace(
                pattern = "\\|janvier\\|",
                replacement = "|01|"
            ) %>% str_replace(
                pattern = "\\|f.vrier\\|",
                replacement = "|02|"
            ) %>% str_replace(
                pattern = "\\|mars\\|",
                replacement = "|03|"
            ) %>% str_replace(
                pattern = "\\|avril\\|",
                replacement = "|04|"
            ) %>% str_replace(
                pattern = "\\|mai\\|",
                replacement = "|05|"
            ) %>% str_replace(
                pattern = "\\|juin\\|",
                replacement = "|06|"
            ) %>% str_replace(
                pattern = "\\|juillet\\|",
                replacement = "|07|"
            ) %>% str_replace(
                pattern = "\\|septembre\\|",
                replacement = "|09|"
            ) %>% str_replace(
                pattern = "\\|octobre\\|",
                replacement = "|10|"
            ) %>% str_replace(
                pattern = "\\|novembre\\|",
                replacement = "|11|"
            ) %>% str_replace(
                pattern = "\\|d.cembre\\|",
                replacement = "|12|"
            ) %>% str_replace(
                pattern = "(.*)\\|(.*)\\|(.*)",
                replacement = "\\3-\\2-\\1"
            )
            meeting_data <- category_data[meeting_line] %>% str_replace(
                pattern = ".*href=\"([^\"]*)\"[^<>]*>([0-9]*)<sup>.*</sup>.*</a>.*",
                replacement = paste0("\\2|", directory, "\\1")
            ) %>% str_replace(
                pattern = ".*href=\"([^\"]*)\"[^<>]*>[^<>]*(unique).*",
                replacement = paste0("\\2|", directory, "\\1")
            ) %>% strsplit(
                "\\|"
            )
            c(
                date_text,
                meeting_data
            ) %>% unlist
        }
    )
}

map(session_categories, category_meetings)
