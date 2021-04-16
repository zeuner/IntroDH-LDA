## Description: Create cartograms for visualizing country-topic relations
## Author: Isidor Zeuner
## dh2021@quidecco.de
## For license see: https://mit-license.org
## -------------------------------------------------------------------

library(textmineR)
library(getcartr)
library(UScensus2000)

source("spatial.R")

colours <- colorRampPalette(c("navy", "deepskyblue"))(33)

colours_top <- colorRampPalette(c("deepskyblue", "magenta", "yellow"))(10)

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
        topic_fraction = data$fraction[matched],
        topic_fraction_area = data$fraction[matched] * areaPoly(world_polygons)
    )
    world_polygons.carto <- quick.carto(
        world_polygons,
        world_polygons$topic_fraction_area
    )
    values <- seq(
        from = min(na.omit(world_polygons$topic_fraction)),
        to = max(na.omit(world_polygons$topic_fraction)),
        length.out = 33
    )
    country_colours <- map(
        world_polygons$topic_fraction,
        function (value) {
            found <- which(values < value) %>% max
            if (is.infinite(found)) {
                return ("black")
            }
            colours[found]
        }
    ) %>% unlist
    top_countries <- order(-world_polygons$topic_fraction)
    country_colours[top_countries[length(colours_top) : 1]] <- colours_top
    png(paste0(country, "-", topic, ".png"), width = 1024, height = 576)
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
    pos <- legend(
        "bottomleft",
        NULL,
        row.names(world_polygons@data)[top_countries[length(colours_top) : 1]],
        col = colours_top,
        cex = 1.2,
        lwd = 5,
        lty = 1,
        plot = FALSE
    )
    legend(
        x = pos$rect$left,
        y = pos$rect$top,
        row.names(world_polygons@data)[top_countries[length(colours_top) : 1]],
        col = colours_top,
        cex = 1.2,
        lwd = 5,
        lty = 1
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
