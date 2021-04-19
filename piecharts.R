## Description: Create pie charts for topics
## Author: Isidor Zeuner
## dh2021@quidecco.de
## For license see: https://mit-license.org
## -------------------------------------------------------------------

library(textmineR)
library(Rfast)
library(marmap)

source("spatial.R")

polygon_data_frame <- map2spatial_polygons_data_frame(worldmap)

plot_country_pie_map <- function (country, topics = NA, countries = NA) {
    png(paste0(country, "-pies.png"), width = 4096, height = 4096)
    plot(polygon_data_frame)
    text(
        x = 0,
        y = 90,
        labels = paste0(
            "Topic world map of ",
            tools::toTitleCase(country),
            "'s parliament"
        ),
        adj = c(0.5, 0.5),
        cex = 8
    )
    centroids <- getSpPPolygonsLabptSlots(polygon_data_frame)
    country_topics <- readRDS(paste0(country, "-country-topics.rds"))
    processed_topics <- attributes(country_topics)$Dimnames[[2]]
    if (is.numeric(topics)) {
        country_topic_fractions <- country_topics / rowSums(country_topics)
        top_topics <- -colMaxs(
            as.matrix(country_topic_fractions),
            value = TRUE
        ) %>% order
        topics <- attributes(country_topics)$Dimnames[[2]][
            top_topics[1 : topics]
        ]
    }
    if (!is.na(topics)) {
        processed_topics <- intersect(processed_topics, topics)
    }
    colours <- colorRampPalette(
        c("red", "green", "blue")
    )(
        length(processed_topics)
    )
    piedata <- data.frame(x = centroids[, 1], y = centroids[, 2])
    slices <- sapply(
        polygon_data_frame@data$ID,
        function (id) {
            region <- attributes(polygon_data_frame[id, ]@data)$row.names
            country <- attributes(country_topics)$Dimnames[[1]] %>% Filter(
                f = function (country) {
                    data_names_transform(country) == region
                }
            )
            if (!is.na(countries)) {
                country <- country %>% Filter(
                    f = function (country) {
                        country %in% countries
                    }
                )
            }
            country <- country %>% head(n = 1)
            absolute <- country_topics[country, processed_topics]
            percentages <- absolute / sum(absolute) * 100
            if (!is.vector(percentages)) {
                names <- attributes(percentages)$Dimnames[[2]]
                percentages <- rep(NA, length(names))
                names(percentages) <- names
            }
            percentages
        }
    )
    for (topic_id in 1 : length(processed_topics)) {
        topic <- processed_topics[[topic_id]]
        piedata[topic] <- slices[topic, ]
        piedata[paste0("colour_", topic)] <- colours[topic_id]
    }
    piedata <- piedata[!is.na(piedata[processed_topics[[1]]]), ]
    space.pies(
        piedata$x,
        piedata$y,
        pie.slices = piedata[processed_topics],
        pie.col = piedata[paste0("colour_", processed_topics)],
        pie.radius = 3
    )
    legend(
        -120,
        -90,
        processed_topics,
        col = colours,
        cex = 5,
        lwd = 25,
        lty = 1,
        ncol = 8
    )
    dev.off()
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
    partial(
        plot_country_pie_map,
        topics = 10
    )
)
