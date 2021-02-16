## Description: Script for fetching Polish parliamentary protocols
## Author: Isidor Zeuner
## dh2021@quidecco.de
## For license see: https://mit-license.org
## -------------------------------------------------------------------

library(tidyverse)
library(jsonlite)
source("scrape.R")

## -------------------------------------------------------------------
## Loading the parliamentary archive

url_top_level <- "http://www.sejm.gov.pl/Sejm9.nsf/page.xsp/archiwum"

current_legislatory_period_top_level <- "http://www.sejm.gov.pl/Sejm9.nsf/wypowiedzi.xsp"

url_service <- function(url) {
    str_replace(
        url,
        pattern = "^([^/:]*://[^/]*)(/.*|$)",
        replacement = "\\1"
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
    lines %>% str_replace(
        pattern = ".*href=\"([^\"]*)\".*",
        replacement = "\\1"
    ) %>% str_replace(
        pattern = ".*HREF=\"([^\"]*)\".*",
        replacement = "\\1"
    ) %>% str_replace(
        pattern = "^(/)",
        replacement= paste0(
            legislatory_period_service,
            "\\1"
        )
    )
}

legislatory_period_top_levels <- archive_top_level[
    grep(">Prace Sejmu<", archive_top_level)
] %>% str_replace(
    pattern = ".*href=\"([^\"]*)\".*",
    replacement = "\\1"
) %>% str_replace(
    pattern = "^(/)",
    replacement = paste0(
        url_top_level_service,
        "\\1"
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
            pattern = ".*href=\"([^\"]*)\".*",
            replacement = "\\1"
        )
    ) %>% map(
        partial(
            str_replace_all,
            pattern = "&amp;",
            replacement = "&"
        )
    ) %>% map(
        partial(
            str_replace,
            pattern = "^(/)",
            replacement= paste0(
                service,
                "\\1"
            )
        )
    ) %>% map(
        partial(
            str_replace,
            pattern = "#.*",
            replacement = ""
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
    no_item <- "<NOBR></NOBR></td><td>(<P>|)(Oświadczenia[.]|)(</P>|)</td>"
    lines %>% str_replace(
        pattern = ".*src=\"([^\"]*)\".*",
        replacement = "\\1"
    ) %>% str_replace(
        pattern = "^(/)",
        replacement= paste0(
            service,
            "\\1"
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

orka_speech_data <- function(url) {
    lines <- read_lines_html_caching(
        url
    ) %>% str_replace(
        pattern = "<B><FONT SIZE=\"[+]1\">([^<>]*)</B>([^<>]*)</FONT>(</B>|)",
        replacement = "<B><FONT SIZE=\"+1\">\\1\\2</FONT></B>"
    )
    item_regular <- lines[
        grep(">.* punkt porządku dziennego", lines)
    ]
    item_regular <- item_regular %>% map(
        partial(
            str_replace,
            pattern = ".*>(.*) punkt porządku dziennego.*",
            replacement = "\\1"
        )
    )
    item_special <- lines[
        grep(">.*\\(punkty .* porządku dziennego\\)", lines)
    ]
    item_special <- item_special %>% map(
        partial(
            str_replace,
            pattern = ".*>.*\\(punkty (.*) porządku dziennego\\).*",
            replacement = "\\1"
        )
    ) %>% map(
        partial(
            str_replace_all,
            pattern = "[.]",
            replacement = ""
        )
    )
    if (0 == length(item_regular)) {
        item <- item_special
    } else {
        item <- item_regular
    }
    meeting <- ">.* kadencja, (.* posiedzenie, .* dzień| *Zgromadzenie Narodowe)"
    date <- lines[
        grep(
            paste0(meeting, " \\(.*[.-].*[.-].*\\)"),
            lines
        )
    ]
    date <- date %>% map(
        partial(
            str_replace,
            pattern = paste0(".*", meeting, " \\((.*)[.-](.*)[.-](.*)\\).*"),
            replacement = "\\2|\\3|\\4"
        )
    ) %>% map(
        partial(
            strsplit,
            split = "\\|"
        )
    )
    start <- grep(
        "(<P><B><FONT SIZE=\"[+]1\">.*:</FONT></B></P>|<P class=\"mowca\">.*:</P>)",
        lines
    )
    post_end <- grep(
        "^(Przebieg posiedzenia|Przejście do dokumentu głównego)$",
        lines
    ) %>% tail(n = 1)
    end <- grep("^<BR>$", lines)
    end <- Filter(
        function (x) {
            x < post_end
        },
        end
    ) %>% max
    text <- lines[
        start : (end - 1)
    ] %>% str_replace(
        pattern = "<P( [^<>]*|)>",
        replacement = "\n"
    ) %>% str_replace_all(
        pattern = "<[^<>]*>",
        replacement = ""
    ) %>% paste0(
        collapse = "\n"
    )
    return (c(
        item[[1]],
        date[[1]][[1]][3],
        date[[1]][[1]][2],
        date[[1]][[1]][1],
        text
    ))
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
            pattern = ".*href=\"([^\"]*)\".*",
            replacement = "\\1"
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
    ] %>% str_replace(
        pattern = ".*href=\"([^\"]*)\".*",
        replacement = "\\1"
    ) %>% str_replace_all(
        pattern = "&amp;",
        replacement = "&"
    ) %>% str_replace(
        pattern  ="^([^/]*)$",
        replacement = paste0(archive_path, "\\1")
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
            pattern = "&amp;",
            replacement = "&"
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

www_archive_speech_data <- function(url) {
    lines <- read_lines_html_caching(
        url
    )
    item <- lines[
        grep(">.*[^.]([.]|) punkt porządku dziennego: *<", lines)
    ]
    item <- item %>% map(
        partial(
            str_replace,
            pattern = ".*>(.*[^.])([.]|) punkt porządku dziennego: *<.*",
            replacement = "\\1"
        )
    ) %>% map(
        partial(
            str_replace_all,
            pattern = "[.]",
            replacement = ""
        )
    )
    meeting <- ">(Posiedzenie nr .*|Zgromadzenie Narodowe) w dniu"
    date <- lines[
        grep(paste0(meeting, " .*-.*-[0-9]*( \\(.*[.] dzień obrad\\)|)<"), lines)
    ]
    date <- date %>% map(
        partial(
            str_replace,
            pattern = paste0(
                ".*",
                meeting,
                " (.*)-(.*)-([0-9]*)( \\(.*[.] dzień obrad\\)|)<.*"
            ),
            replacement = "\\2|\\3|\\4"
        )
    ) %>% map(
        partial(
            strsplit,
            split = "\\|"
        )
    )
    end_pattern <- "<a [^<>]*href=[^<>]*>.*Przebieg posiedzenia.*</a>"
    one_line_stenogram <- lines[
        grep(
            paste0("<div class=\"stenogram\">.*", end_pattern),
            lines
        )
    ]
    if (0 < length(one_line_stenogram)) {
        lines_text <- strsplit(
            one_line_stenogram,
            "</(h2|P)>"
        )[[1]]
    } else {
        lines_text <- lines
    }
    start <- grep("<h2( class=\"mowca\"|)>.*:", lines_text) %>% tail(n = 1)
    end <- grep(end_pattern, lines_text)
    text <- lines_text[
        start : (end - 1)
    ] %>% str_replace(
        pattern = "<(h2|[pP])( [^<>]*|)>",
        replacement = "\n"
    ) %>% str_replace_all(
        pattern = "<[^<>]*>",
        replacement = ""
    ) %>% paste0(
        collapse = "\n"
    )
    return (c(
        item[[1]],
        date[[1]][[1]][3],
        date[[1]][[1]][2],
        date[[1]][[1]][1],
        text
    ))
}

archive_bottom_level <- function (top_level) {
    if (0 == length(grep("orka", top_level))) {
        return (www_archive_bottom_level(top_level))
    } else {
        return (orka_bottom_level(top_level))
    }
}

## extracts a vector from a speech url, consisting of:
## - number of the item discussed
## - year (YYYY format)
## - month (MM format)
## - day (DD format)
## - text of the speech
archive_speech_data <- function(url) {
    if (0 == length(grep("orka", url))) {
        return (www_archive_speech_data(url))
    } else {
        return (orka_speech_data(url))
    }
}

archive_speech_data_frame <- function(urls) {
    data <- map(urls, archive_speech_data)
    frame <- data.frame(
        item = unlist(map(data,function(row){row[1]})),
        year = unlist(map(data,function(row){row[2]})),
        month = unlist(map(data,function(row){row[3]})),
        day = unlist(map(data,function(row){row[4]})),
        text = unlist(map(data,function(row){row[5]}))
    )
    return (frame)
}

archive_debate_data_frame <- function(urls) {
    frame <- urls %>% archive_speech_data_frame %>% group_by(
        item,
        year,
        month,
        day
    ) %>% summarize(
        debate_text = paste(text, collapse = "\n")
    )
    return (frame)
}

data <- legislatory_period_top_levels %>% map(
    archive_bottom_level
) %>% do.call(
    what = c
) %>% archive_debate_data_frame

exported <- data.frame(
    id = paste(
        data$year,
        data$month,
        data$day,
        data$item
    ) %>% str_replace_all(
        pattern = "[^0-9]",
        replacement = "_"
    ),
    data = data$debate_text
)

json <- toJSON(exported)

write(json, file="poland.json")
