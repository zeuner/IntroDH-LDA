## Description: Create cartograms for visualizing country-topic relations
## Author: Isidor Zeuner
## dh2021@quidecco.de
## For license see: https://mit-license.org
## -------------------------------------------------------------------

library(jsonlite)
library(tidyverse)
library(maptools)
library(textmineR)
library(getcartr)

colours <- colorRampPalette(c("navy", "deepskyblue"))(33)

worldmap <- maps::map("world", fill = TRUE, plot = FALSE)

map_names_transform <- function (names) {
    outer <- sapply(strsplit(names, ":"), "[", 1L) %>% str_replace(
## no outer sovereignty
        pattern = "^Aruba$",
        replacement = "Netherlands"
    ) %>% str_replace(
## no outer sovereignty
        pattern = "^Bonaire$",
        replacement = "Netherlands"
    ) %>% str_replace(
## no outer sovereignty
        pattern = "^Curacao$",
        replacement = "Netherlands"
    ) %>% str_replace(
## no outer sovereignty
        pattern = "^Saba$",
        replacement = "Netherlands"
    ) %>% str_replace(
## no outer sovereignty
        pattern = "^Sint Eustatius$",
        replacement = "Netherlands"
    ) %>% str_replace(
## no outer sovereignty
        pattern = "^Sint Maarten$",
        replacement = "Netherlands"
    ) %>% str_replace(
## no outer sovereignty
        pattern = "^Anguilla$",
        replacement = "United Kingdom"
    ) %>% str_replace(
## no outer sovereignty
        pattern = "^Bermuda$",
        replacement = "United Kingdom"
    ) %>% str_replace(
## no outer sovereignty
        pattern = "^Cayman Islands$",
        replacement = "United Kingdom"
    ) %>% str_replace(
## no outer sovereignty
        pattern = "^Falkland Islands$",
        replacement = "United Kingdom"
    ) %>% str_replace(
## no outer sovereignty
        pattern = "^Guernsey$",
        replacement = "United Kingdom"
    ) %>% str_replace(
## no outer sovereignty
        pattern = "^Isle of Man$",
        replacement = "United Kingdom"
    ) %>% str_replace(
## no outer sovereignty
        pattern = "^Jersey$",
        replacement = "United Kingdom"
    ) %>% str_replace(
## no outer sovereignty
        pattern = "^Montserrat$",
        replacement = "United Kingdom"
    ) %>% str_replace(
## no outer sovereignty
        pattern = "^Pitcairn Islands$",
        replacement = "United Kingdom"
    ) %>% str_replace(
## no outer sovereignty
        pattern = "^Ascension Island$",
        replacement = "United Kingdom"
    ) %>% str_replace(
## no outer sovereignty
        pattern = "^Saint Helena$",
        replacement = "United Kingdom"
    ) %>% str_replace(
## no outer sovereignty
        pattern = "^South Georgia$",
        replacement = "United Kingdom"
    ) %>% str_replace(
## no outer sovereignty
        pattern = "^South Sandwich Islands$",
        replacement = "United Kingdom"
    ) %>% str_replace(
## no outer sovereignty
        pattern = "^Turks and Caicos Islands$",
        replacement = "United Kingdom"
    ) %>% str_replace(
## no outer sovereignty
        pattern = "^Virgin Islands, British$",
        replacement = "United Kingdom"
    ) %>% str_replace(
## no outer sovereignty
        pattern = "^Virgin Islands, US$",
        replacement = "United States"
    ) %>% str_replace(
## no outer sovereignty
        pattern = "^American Samoa$",
        replacement = "United States"
    ) %>% str_replace(
## no outer sovereignty
        pattern = "^Guam$",
        replacement = "United States"
    ) %>% str_replace(
## no outer sovereignty
        pattern = "^Northern Mariana Islands$",
        replacement = "United States"
    ) %>% str_replace(
## no outer sovereignty
        pattern = "^Puerto Rico$",
        replacement = "United States"
    ) %>% str_replace(
## no outer sovereignty
        pattern = "^Saint Pierre and Miquelon$",
        replacement = "France"
    ) %>% str_replace(
## no outer sovereignty
        pattern = "^French Southern and Antarctic Lands$",
        replacement = "France"
    ) %>% str_replace(
## no outer sovereignty
        pattern = "^New Caledonia$",
        replacement = "France"
    ) %>% str_replace(
## no outer sovereignty
        pattern = "^Saint Barthelemy$",
        replacement = "France"
    ) %>% str_replace(
## no outer sovereignty
        pattern = "^Reunion$",
        replacement = "France"
    ) %>% str_replace(
## no outer sovereignty
        pattern = "^French Guiana$",
        replacement = "France"
    ) %>% str_replace(
## no outer sovereignty
        pattern = "^French Polynesia$",
        replacement = "France"
    ) %>% str_replace(
## no outer sovereignty
        pattern = "^Mayotte$",
        replacement = "France"
    ) %>% str_replace(
## no outer sovereignty
        pattern = "^Martinique$",
        replacement = "France"
    ) %>% str_replace(
## no outer sovereignty
        pattern = "^Guadeloupe$",
        replacement = "France"
    ) %>% str_replace(
## no outer sovereignty
        pattern = "^Saint Martin$",
        replacement = "France"
    ) %>% str_replace(
## no outer sovereignty
        pattern = "^Wallis and Futuna$",
        replacement = "France"
    ) %>% str_replace(
## no outer sovereignty
        pattern = "^Azores$",
        replacement = "Portugal"
    ) %>% str_replace(
## no outer sovereignty
        pattern = "^Madeira Islands$",
        replacement = "Portugal"
    ) %>% str_replace(
## no outer sovereignty
        pattern = "^Canary Islands$",
        replacement = "Spain"
    ) %>% str_replace(
## no outer sovereignty
        pattern = "^Faroe Islands$",
        replacement = "Denmark"
    ) %>% str_replace(
## no outer sovereignty
        pattern = "^Greenland$",
        replacement = "Denmark"
    ) %>% str_replace(
## no outer sovereignty
        pattern = "^Niue$",
        replacement = "New Zealand"
    ) %>% str_replace(
## no outer sovereignty
        pattern = "^Cook Islands$",
        replacement = "New Zealand"
    ) %>% str_replace(
## no outer sovereignty
        pattern = "^Heard Island$",
        replacement = "Australia"
    ) %>% str_replace(
## no outer sovereignty
        pattern = "^Norfolk Island$",
        replacement = "Australia"
    ) %>% str_replace(
## no outer sovereignty
        pattern = "^Christmas Island$",
        replacement = "Australia"
    ) %>% str_replace(
## no outer sovereignty
        pattern = "^Cocos Islands$",
        replacement = "Australia"
    ) %>% str_replace(
## no outer sovereignty
        pattern = "^Chagos Archipelago$",
        replacement = "Mauritius"
    ) %>% str_replace(
## no outer sovereignty
        pattern = "^(Antigua|Barbuda)$",
        replacement = "Antigua and Barbuda"
    ) %>% str_replace(
## no outer sovereignty
        pattern = "^(Saint Kitts|Nevis)$",
        replacement = "Saint Kitts and Nevis"
    ) %>% str_replace(
## no outer sovereignty
        pattern = "^(Saint Vincent|Grenadines?)$",
        replacement = "Saint Vincent and Grenadines"
    ) %>% str_replace(
## no outer sovereignty
        pattern = "^(Trinidad|Tobago)$",
        replacement = "Trinidad and Tobago"
    ) %>% str_replace(
## abbreviation
        pattern = "^UK$",
        replacement = "United Kingdom"
    ) %>% str_replace(
## abbreviation
        pattern = "^USA$",
        replacement = "United States"
    ) %>% str_replace(
## abbreviation
        pattern = "^Vatican$",
        replacement = "Vatican City"
    ) %>% str_replace(
## does the map distinguish the (Britith) Northern Ireland?
        pattern = "^Ireland$",
        replacement = "Republic of Ireland"
    ) %>% str_replace(
## does the map distinguish other territories called Macedonia?
        pattern = "^Macedonia$",
        replacement = "North Macedonia"
    ) %>% str_replace(
## does the map distinguish other territories called Micronesia?
        pattern = "^Micronesia$",
        replacement = "Federated States of Micronesia"
    ) %>% str_replace(
        pattern = "^Sao Tome and Principe$",
        replacement = "São Tomé and Príncipe"
    ) %>% str_replace(
        pattern = "^Swaziland$",
        replacement = "Eswatini"
    ) %>% str_replace(
        pattern = "^Timor-Leste$",
        replacement = "East Timor"
    ) %>% str_replace(
        pattern = " the ",
        replacement = " "
    )
    outer
}

data_names_transform <- function (names) {
    names %>% str_replace(
        pattern = "^The ",
        replacement = ""
    ) %>% str_replace(
        pattern = " the ",
        replacement = " "
    )
}

from_map <- worldmap$names %>% map_names_transform %>% unique

from_data <- fromJSON(
    read_file("country-names.json")
)$name_en %>% data_names_transform

map_ignored <- c(
## complex territorial situation
    "Antarctica",
    "Palestine",
## disputed territorial situation
    "Siachen Glacier",
    "Western Sahara",
    "Kosovo",
    "Taiwan"
)

data_ignored <- c(
## no corresponding region found on the map
    "Tuvalu"
)

map(
    from_map,
    function (country) {
        if (!(country %in% from_data || country %in% map_ignored)) {
            stop(paste0(country, " missing in from_data"))
        }
    }
)

map(
    from_data,
    function (country) {
        if (!(country %in% from_map || country %in% data_ignored)) {
            stop(paste0(country, " missing in from_map"))
        }
    }
)

write_topic_cartogram <- function (topic, country_topics, country) {
    world_polygons <- map2SpatialPolygons(
        worldmap,
        IDs = worldmap$names %>% map_names_transform
    )
    pid <- sapply(slot(world_polygons, "polygons"), function(x) slot(x, "ID")) 
    world_polygons.df <- data.frame(
        ID = 1 : length(world_polygons),
        row.names = pid
    )
    world_polygons <- SpatialPolygonsDataFrame(
        world_polygons,
        world_polygons.df
    )
    country_fractions <- country_topics[, topic] / rowMeans(country_topics)
    data <- data.frame(
        country = names(country_fractions),
        fraction = country_fractions
    )
    matched <- match(
        row.names(world_polygons@data),
        data_names_transform(data[, "country"])
    )
    world_polygons@data <- data.frame(
        world_polygons@data,
        data$fraction[matched]
    )
    world_polygons.carto <- quick.carto(
        world_polygons,
        world_polygons$data.fraction.matched.
    )
    values <- seq(
        from = min(na.omit(world_polygons$data.fraction.matched.)),
        to = max(na.omit(world_polygons$data.fraction.matched.)),
        length.out = 33
    )
    country_colours <- map(
        world_polygons$data.fraction.matched.,
        function (value) {
            found <- which(values < value) %>% max
            if (is.infinite(found)) {
                return ("black")
            }
            colours[found]
        }
    ) %>% unlist
    png(paste0(country, "-", topic, ".png"), width = 1024, height = 1024)
    plot(
        world_polygons.carto,
        col = country_colours,
        main = paste0(
            "Cartogram of the topic ",
            topic,
            " in ",
            tools::toTitleCase(country),
            "'s parliament"
        )
    )
    dev.off()
}


write_country_topics_cartograms <- function (country, topics = NA) {
    country_topics <- readRDS(paste0(country, "-country-topics.rds"))
    processed_topics <- attributes(country_topics)$Dimnames[[2]]
    if (!is.na(topics)) {
        processed_topics <- intersect(processed_topics, topics)
    }
    map(
        processed_topics,
        partial(
            write_topic_cartogram,
            country_topics = country_topics,
            country = country
        )
    )
}

map(
    c(
        "austria",
        "belgium",
        "czechia",
        "france",
        "hungary",
        "ireland",
        "italy",
        "netherlands",
        "poland",
        "spain"
    ),
    write_country_topics_cartograms
)
