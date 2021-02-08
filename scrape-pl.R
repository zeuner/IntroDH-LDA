## Description: Script for fetching Polish parliamentary protocols
## Author: Isidor Zeuner
## dh2021@quidecco.de
## For license see: https://mit-license.org
## -------------------------------------------------------------------

library(tidyverse)
source("scrape.R")

## -------------------------------------------------------------------
## Loading the parliamentary archive

url_top_level <- "http://www.sejm.gov.pl/Sejm9.nsf/page.xsp/archiwum"

current_legislatory_period_top_level <- "http://www.sejm.gov.pl/Sejm9.nsf/wypowiedzi.xsp"

url_service <- function(url) {
    str_replace(
        url,
        pattern="^([^/:]*://[^/]*)(/.*|$)",
        replacement="\\1"
    )
}

url_top_level_service <- url_service(
    url_top_level
)

archive_top_level <- read_lines_html_caching(url_top_level)

speeches_url <- function(legislatory_period_url) {
    legislatory_period_service <- url_service(
        legislatory_period_url
    )
    lines <- read_lines_html_caching(
        legislatory_period_url
    ) %>% iconv(
        "latin1",
        "utf-8"
    ) %>% map(
        function(line) {
            strsplit(line, "<a ")
        }
    ) %>% unlist(
        recursive = FALSE
    ) %>% do.call(
        what = c
    )
## TODO: implement a way to handle the legislatory period from 1989 to 1991.
##    Here, there are only scanned steganographic protocols available,
##    which make it harder to distinguish the different debates from
##    one meeting.
    lines <- lines[
        grep(">[^<>]*Wypowiedzi[^<>]*<", lines)
    ]
    lines %>% map(
        partial(
            str_replace,
            pattern=".*href=\"([^\"]*)\".*",
            replacement="\\1"
        )
    ) %>% map(
        partial(
            str_replace,
            pattern=".*HREF=\"([^\"]*)\".*",
            replacement="\\1"
        )
    ) %>% map(
        partial(
            str_replace,
            pattern="^(/)",
            replacement= paste(
                legislatory_period_service,
                "\\1",
                sep = ""
            )
        )
    )
}

legislatory_period_top_levels <- archive_top_level[
    grep(">Prace Sejmu<", archive_top_level)
] %>% map(
    partial(
        str_replace,
        pattern=".*href=\"([^\"]*)\".*",
        replacement="\\1"
    )
) %>% map(
    partial(
        str_replace,
        pattern="^(/)",
        replacement= paste(
            url_top_level_service,
            "\\1",
            sep = ""
        )
    )
) %>% map(
    speeches_url
) %>% do.call(
    what = c
) %>% append(
    current_legislatory_period_top_level
)

orka_next_level <- function (
    current_level,
    expand_pattern,
    excluded_pattern = NA
) {
    service <- url_service(
        current_level
    )
    lines <- read_lines_html_caching(
        current_level
    )
    lines <- lines[
        grep(expand_pattern, lines)
    ]
    if (!is.na(excluded_pattern)) {
        lines <- lines[
            grep(excluded_pattern, lines, invert = TRUE)
        ]
    }
    lines %>% map(
        partial(
            str_replace,
            pattern=".*href=\"([^\"]*)\".*",
            replacement="\\1"
        )
    ) %>% map(
        partial(
            str_replace_all,
            pattern="&amp;",
            replacement="&"
        )
    ) %>% map(
        partial(
            str_replace,
            pattern="^(/)",
            replacement= paste(
                service,
                "\\1",
                sep = ""
            )
        )
    ) %>% map(
        partial(
            str_replace,
            pattern="#.*",
            replacement=""
        )
    )
}

orka_bottom_level <- function (top_level) {
    service <- url_service(
        top_level
    )
    lines <- read_lines_html_caching(
        top_level
    )
    lines <- lines[
        grep("<frame.*name=\"Prawa\".*>", lines)
    ]
    no_item <- "<NOBR></NOBR></td><td>((<P>|)Oświadczenia[.](</P>|)|)</td>"
    lines %>% map(
        partial(
            str_replace,
            pattern=".*src=\"([^\"]*)\".*",
            replacement="\\1"
        )
    ) %>% map(
        partial(
            str_replace,
            pattern="^(/)",
            replacement= paste(
                service,
                "\\1",
                sep = ""
            )
        )
    ) %>% map(
        partial(
            orka_next_level,
            expand_pattern = "href=.*Expand=[0-9]*#"
        )
    ) %>% do.call(
        what = c
    ) %>% map(
        partial(
            orka_next_level,
            expand_pattern = "href=.*Expand=[0-9]*[.][0-9]*#"
        )
    ) %>% do.call(
        what = c
    ) %>% map(
        partial(
            orka_next_level,
            expand_pattern = "href=.*OpenDocument",
            excluded_pattern = no_item
        )
    ) %>% do.call(
        what = c
    )
}

orka_speech_item <- function(url) {
     lines <- read_lines_html_caching(
         url
     )
     lines <- lines[
         grep(">.* punkt porządku dziennego", lines)
     ]
     lines <- lines %>% map(
         partial(
             str_replace,
             pattern=".*>(.*) punkt porządku dziennego.*",
             replacement="\\1"
         )
     )
     return (lines[[1]])
}

www_archive_pages <- function (top_level) {
    lines <- read_lines_html_caching(
        top_level
    ) %>% map(
        function(line) {
            strsplit(line, "<a ")
        }
    ) %>% unlist(
        recursive = FALSE
    ) %>% do.call(
        what = c
    )
    views <- lines[
        grep("view:", lines)
    ]
    pages <- lines[
        grep("data-page=", lines)
    ] %>% map(
        partial(
            str_replace,
            pattern=".*href=\"([^\"]*)\".*",
            replacement="\\1"
        )
    )
    urls <- pages[
        !duplicated(pages)
    ] %>% map(
        partial(
            str_replace,
            pattern = "^",
            replacement = top_level
        )
    )
    if (0 == length(urls)) {
        if (0 == length(views)) {
            return (urls)
        } else {
            return (list(top_level))
        }
    } else {
        return (urls)
    }
}

www_archive_date_url <- function (archive_url) {
    archive_path <- str_replace(
        archive_url,
        pattern = "/[^/]*$",
        replacement = "/"
    )
    lines <- read_lines_html_caching(
        archive_url
    ) %>% map(
        function(line) {
            strsplit(line, "<a ")
        }
    ) %>% unlist(
        recursive = FALSE
    ) %>% do.call(
        what = c
    )
    lines[
        grep("posiedzenie[.]xsp", lines)
    ] %>% map(
        partial(
            str_replace,
            pattern = ".*href=\"([^\"]*)\".*",
            replacement = "\\1"
        )
    ) %>% map(
        partial(
            str_replace_all,
            pattern="&amp;",
            replacement="&"
        )
    ) %>% map(
        partial(
            str_replace,
            pattern  ="^([^/]*)$",
            replacement = paste0(archive_path, "\\1")
        )
    )
}

www_archive_speech_url <- function (date_url) {
    date_path <- str_replace(
        date_url,
        pattern = "/[^/]*$",
        replacement = "/"
    )
    lines <- read_lines_html_caching(
        date_url
    ) %>% map(
        function(line) {
            strsplit(line, "<a ")
        }
    ) %>% unlist(
        recursive = FALSE
    ) %>% do.call(
        what = c
    )
    lines <- lines[
        grep("wypowiedz[.]xsp", lines)
    ]
    lines <- lines[
        grep("</a>[^<>]*</td><td></td><td>", lines, invert = TRUE)
    ]
    lines %>% map(
        partial(
            str_replace,
            pattern = ".*href=\"([^\"]*)\".*",
            replacement = "\\1"
        )
    ) %>% map(
        partial(
            str_replace_all,
            pattern="&amp;",
            replacement="&"
        )
    ) %>% map(
        partial(
            str_replace,
            pattern  ="^([^/]*)$",
            replacement = paste0(date_path, "\\1")
        )
    )
}

www_archive_bottom_level <- function (top_level) {
    www_archive_pages(
        top_level
    ) %>% map(
        www_archive_date_url
    ) %>% do.call(
        what = c
    ) %>% map(
        www_archive_speech_url
    ) %>% do.call(
        what = c
    )
}
