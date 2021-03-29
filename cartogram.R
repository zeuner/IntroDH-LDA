## Description: Create cartograms for visualizing country-topic relations
## Author: Isidor Zeuner
## dh2021@quidecco.de
## For license see: https://mit-license.org
## -------------------------------------------------------------------

library(textmineR)
library(getcartr)

source("spatial.R")

colours <- colorRampPalette(c("navy", "deepskyblue"))(33)

write_topic_cartogram <- function (topic, country_topics, country) {
    world_polygons <- map2spatial_polygons_data_frame(
        worldmap
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
