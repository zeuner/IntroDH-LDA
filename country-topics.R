## Description: Relate country references to LDA topics
## Author: Isidor Zeuner
## dh2021@quidecco.de
## For license see: https://mit-license.org
## -------------------------------------------------------------------

library(textmineR)
library(tidyverse)

source("matrix-piecewise-mult.R")

compute_country_topic_matrix <- function (country) {
    refs <- as(readRDS(paste0(country, "-country-ref.rds")), "CsparseMatrix")
    model <- readRDS(paste0(country, "-lda.Rds"))
    dtm <- readRDS(paste0(country, "-dtm.rds"))
    generate_projection <- function (r) {
        projection <- predict(model, dtm[r,, drop = FALSE], method = "dot")
        Matrix(data = projection, dimnames = dimnames(projection))
    }
    country_topics <- matrix_mult_generated(
        t(refs),
        generate_projection,
        dimnames(dtm)[[1]],
        1000
    )
    saveRDS(country_topics, file = paste0(country, "-country-topics.rds"))
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
    compute_country_topic_matrix
)
