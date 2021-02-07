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

archive_top_level <- read_lines_retrying(url_top_level)

speeches_url <- function(legislatory_period_url) {
    legislatory_period_service <- url_service(
        legislatory_period_url
    )
    lines <- read_lines_retrying(
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
    ) %>% (
        function(x) {
            do.call(c, x)
        }
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
) %>% (
    function(x) {
        do.call(c, x)
    }
) %>% append(
    current_legislatory_period_top_level
)

orka_next_level <- function (current_level, expand_pattern) {
    service <- url_service(
        current_level
    )
    lines <- read_lines_retrying(
        current_level
    )
    lines <- lines[
        grep(expand_pattern, lines)
    ]
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
    lines <- read_lines_retrying(
        top_level
    )
    lines <- lines[
        grep("<frame.*name=\"Prawa\".*>", lines)
    ]
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
    ) %>% (
        function(x) {
            do.call(c, x)
        }
    ) %>% map(
        partial(
            orka_next_level,
            expand_pattern = "href=.*Expand=[0-9]*[.][0-9]*#"
        )
    ) %>% (
        function(x) {
            do.call(c, x)
        }
    ) %>% map(
        partial(
            orka_next_level,
            expand_pattern = "href=.*OpenDocument"
        )
    ) %>% (
        function(x) {
            do.call(c, x)
        }
    )
}
