## Description: Prepare a table of different country names in
##  different languages
## Author: Isidor Zeuner
## dh2021@quidecco.de
## For license see: https://mit-license.org
## -------------------------------------------------------------------

library(tidyverse)
library(urltools)
library(jsonlite)
source("scrape.R")

countries_url <- "https://en.wikipedia.org/wiki/Category:Countries_by_continent"

countries_service <- url_service(countries_url)

split_html_tags <- function (lines) {
    lines %>% str_replace_all(
        pattern = "%",
        replacement = "%0"
    ) %>% str_replace_all(
        pattern = "(<[^<>]*>)",
        replacement = "%1\\1%1"
    ) %>% strsplit(
        split = "%1"
    ) %>% unlist(
        recursive = FALSE
    ) %>% str_replace_all(
        pattern = "%0",
        replacement = "%"
    )
}

countries_lines <- read_lines_html_caching(countries_url) %>% split_html_tags

required_categories <- c(
    "Africa",
    "Asia",
    "Europe",
    "North_America",
    "Oceania",
    "South_America"
)

category_lines <- grep("aria-labelledby=", countries_lines)

required_category_lines <- Filter(
    function (line) {
        (
            categories <- countries_lines[[
                line
            ]] %>% str_replace(
                pattern = ".*aria-labelledby=\"([^\"]*)\".*",
                replacement = "\\1"
            )
        ) %in% required_categories
    },
    category_lines
)

filter_category <- function (x, category_line) {
    x %>% Filter(
        f = function (link_line) {
            (
                Filter(
                    function (category_line) {
                        category_line < link_line
                    },
                    category_lines
                ) %>% max
            ) == category_line
        }
    )
}

category_link_lines <- function (category_line) {
    navigation_group_lines <- grep("class=\"navbox-group\"", countries_lines)
    sovereign_states_line <- grep(
        "^Sovereign states$",
        countries_lines
    ) %>% filter_category(
        category_line = category_line
    )
    navigation_group_begin <- navigation_group_lines %>% Filter(
        f = function (line) {
            line < sovereign_states_line
        }
    ) %>% max
    navigation_group_end <- navigation_group_lines %>% Filter(
        f = function (line) {
            sovereign_states_line < line
        }
    ) %>% min
    grep("href=", countries_lines) %>% filter_category(
        category_line = category_line
    ) %>% Filter(
        f = function (line) {
            navigation_group_begin < line && line < navigation_group_end
        }
    )
}

country_urls_en <- countries_lines[
    required_category_lines %>% map(
        category_link_lines
    ) %>% unlist(
        recursive = FALSE
    )
] %>% str_replace(
    pattern = ".*href=\"([^\"]*)\".*",
    replacement = "\\1"
) %>% str_replace(
    pattern = "^/",
    replacement = paste0(countries_service, "/")
) %>% Filter(
    f = function (url) {
        0 == length(grep("List.*of.*states", url))
    }
)

country_names_in_languages <- function (country_url, languages) {
    lines <- read_lines_html_caching(country_url) %>% split_html_tags
## add missing interlanguage link to the Polish Wikipedia
    if ("https://en.wikipedia.org/wiki/The_Netherlands" == country_url) {
        lines[[
            length(lines) + 1
        ]] = paste0(
            "lang=\"",
            "pl",
            "\".*class=\"interlanguage-link-target\"",
            " href=\"",
            "https://pl.wikipedia.org/wiki/Holandia",
            "\" "
        )
    }
    lines[
        paste0(
            "lang=\"",
            languages,
            "\".*class=\"interlanguage-link-target\""
        ) %>% map(
            partial(
                grep,
                x = lines
            )
        ) %>% unlist(
            recursive = FALSE
        )
    ] %>% str_replace(
        pattern = ".*href=\"([^\"]*)\".*",
        replacement = "\\1"
    ) %>% str_replace(
        pattern = ".*/",
        replacement = ""
    ) %>% map(
        url_decode
    ) %>% str_replace_all(
        pattern = "_",
        replacement = " "
    ) %>% unlist
}

required_languages <- c("cs", "de", "es", "fr", "hu", "it", "nl", "pl")

country_names <- country_urls_en %>% map(
    partial(
        country_names_in_languages,
        languages = required_languages
    )
)

country_names_in_language <- function (language) {
    map(
        country_names,
        function (country) {
            country[[which(required_languages == language)]]
        }
    ) %>% unlist
}

exported <- data.frame(
    name_en = map(
        country_urls_en,
        function (url) {
            url %>% str_replace(
                pattern = ".*/",
                replacement = ""
            ) %>% str_replace_all(
                pattern = "_",
                replacement = " "
            ) %>% url_decode
        }
    ) %>% str_replace(
        pattern = " \\(country\\)$",
        replacement = ""
    ) %>% unlist,
    name_cs = country_names_in_language("cs"),
    name_de = country_names_in_language("de"),
    name_es = country_names_in_language("es") %>% str_replace(
        pattern = " \\(país\\)$",
        replacement = ""
    ),
    name_fr = country_names_in_language("fr") %>% str_replace(
        pattern = " \\((pays|pays constitutif)\\)$",
        replacement = ""
    ),
    name_hu = country_names_in_language("hu") %>% str_replace(
        pattern = " \\((ország|állam)\\)$",
        replacement = ""
    ),
    name_it = country_names_in_language("it") %>% str_replace(
        pattern = " \\(stato\\)$",
        replacement = ""
    ),
    name_nl = country_names_in_language("nl") %>% str_replace(
        pattern = " \\(land\\)$",
        replacement = ""
    ),
    name_pl = country_names_in_language("pl")
)

json <- toJSON(exported)

write(json, file="country-names.json")
