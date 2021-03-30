## Description: Plot most relevant terms from topics according to the measure
##  defined by Sievert and Shirley (2014)
## Author: Isidor Zeuner
## dh2021@quidecco.de
## For license see: https://mit-license.org
## -------------------------------------------------------------------

library(textmineR)
library(tidyverse)

lambda <- 0.6

plot_relevant_terms <- function (country) {
    model <- readRDS(paste0(country, "-lda.Rds"))
    phi <- model$phi
    relevance <- exp(
        lambda * log(phi) + (1 - lambda) * log(phi / colSums(phi))
    )
    for (topic in dimnames(relevance)[[1]]) {
        png(paste0(country, "-", topic, "-relevance.png"))
        par(mai = c(0.7, 2, 0.3, 0.5))
        barplot(
            relevance[topic,] %>% sort %>% tail(n = 15),
            horiz = TRUE,
            las = 2
        )
        dev.off()
    }
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
    plot_relevant_terms
)
