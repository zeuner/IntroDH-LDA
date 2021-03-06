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

meetings <- map(
    session_categories,
    category_meetings
) %>% do.call(what = c)

## returns a list of debates from the meeting
## for each debate, this will be a vector, consisting of:
## - date (YYYY-MM-DD format)
## - meeting number within the day
## - item number within the meeting
## - text of the debate
meeting_debates <- function (meeting_record) {
    fragments <-read_lines_html_caching(
        meeting_record[3]
    ) %>% str_replace_all(
        pattern = "%",
        replacement = "%0"
    ) %>% str_replace_all(
        pattern = "(</[^<>]*>)",
        replacement = "\\1%1"
    ) %>% strsplit(
        split = "%1"
    ) %>% unlist(
        recursive = FALSE
    ) %>% str_replace_all(
        pattern = "%0",
        replacement = "%"
    )
    item_starts <- grep("<h5 class=\"numencad\">.*</h5>", fragments)
    items <- item_starts %>% map(
        function (item_start) {
            item_end <- Filter(
                function (subsequent) {
                    item_start < subsequent
                },
                item_starts
            ) %>% min
            if (!is.finite(item_end)) {
                scripts <- grep("<script[^<>]*>", fragments)
                item_end <- Filter(
                    function (subsequent) {
                        item_start < subsequent
                    },
                    scripts
                ) %>% min
            }
            ## in case of bad HTML, e.g. in
            ## https://www.assemblee-nationale.fr/14/cri/2012-2013/20130245.asp
            if (!is.finite(item_end)) {
                item_end <- length(fragments)
            }
            item_number <- fragments[
                item_start
            ] %>% str_replace(
                pattern = ".*<h5 class=\"numencad\">(.*)</h5>.*",
                replacement = "\\1"
            )
            item_start <- item_start + 1
            item_end <- item_end - 1
            text <- fragments[
                item_start : item_end
            ] %>% str_replace_all(
                "(<p[^<>]*>|<br />)",
                "\n"
            ) %>% str_replace_all(
                "<[^<>]*>",
                ""
            ) %>% paste0(
                collapse = "\n"
            ) %>% str_replace_all(
                "\n\n\n*",
                "\n\n"
            )
            c(meeting_record[1], meeting_record[2], item_number, text)
        }
    )
    items
}

debates <- map(meetings, meeting_debates) %>% unlist(
    recursive = FALSE
)

frame <- data.frame(
    date = unlist(map(debates,function(row){row[1]})),
    meeting = unlist(map(debates,function(row){row[2]})),
    item = unlist(map(debates,function(row){row[3]})),
    text = unlist(map(debates,function(row){row[4]}))
)

## need French locale to properly export the debate texts
Sys.setlocale('LC_ALL','fr_FR.UTF-8')

exported <- data.frame(
    id = paste(
        frame$date,
        frame$meeting,
        frame$item
    ) %>% str_replace_all(
        pattern = "[^0-9a-z]",
        replacement = "_"
    ),
    data = frame$text
)

json <- toJSON(exported)

write(json, file="france.json")
