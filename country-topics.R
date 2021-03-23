library(textmineR)
library(tidyverse)

source("matrix-piecewise-mult.R")

refs <- as(readRDS("austria-country-ref.rds"), "CsparseMatrix")
model <- readRDS("austria-lda.Rds")
dtm <- readRDS("austria-dtm.rds")
projection <- predict(model, dtm, method = "dot")
country_topics <- matrix_mult(
    t(refs),
    Matrix(data = projection, dimnames = dimnames(projection)),
    1000
)
model$top_terms
map(
    attributes(country_topics)$Dimnames[[2]],
    function (topic) {
        result <- sort(
            country_topics[, topic] - rowMeans(country_topics),
            decreasing = TRUE
        ) %>% head
        result$topic <- topic
        result
    }
)
map(
    attributes(country_topics)$Dimnames[[1]],
    function (country) {
        result <- sort(
            country_topics[country,] - colMeans(country_topics),
            decreasing = TRUE
        ) %>% head
        result$country <- country
        result
    }
)

refs <- as(readRDS("belgium-country-ref.rds"), "CsparseMatrix")
model <- readRDS("belgium-lda.Rds")
dtm <- readRDS("belgium-dtm.rds")
projection <- predict(model, dtm, method = "dot")
country_topics <- matrix_mult(
    t(refs),
    Matrix(data = projection, dimnames = dimnames(projection)),
    1000
)
model$top_terms
map(
    attributes(country_topics)$Dimnames[[2]],
    function (topic) {
        result <- sort(
            country_topics[, topic] - rowMeans(country_topics),
            decreasing = TRUE
        ) %>% head
        result$topic <- topic
        result
    }
)
map(
    attributes(country_topics)$Dimnames[[1]],
    function (country) {
        result <- sort(
            country_topics[country,] - colMeans(country_topics),
            decreasing = TRUE
        ) %>% head
        result$country <- country
        result
    }
)

refs <- as(readRDS("czechia-country-ref.rds"), "CsparseMatrix")
model <- readRDS("czechia-lda.Rds")
dtm <- readRDS("czechia-dtm.rds")
projection <- predict(model, dtm, method = "dot")
country_topics <- matrix_mult(
    t(refs),
    Matrix(data = projection, dimnames = dimnames(projection)),
    1000
)
model$top_terms
map(
    attributes(country_topics)$Dimnames[[2]],
    function (topic) {
        result <- sort(
            country_topics[, topic] - rowMeans(country_topics),
            decreasing = TRUE
        ) %>% head
        result$topic <- topic
        result
    }
)
map(
    attributes(country_topics)$Dimnames[[1]],
    function (country) {
        result <- sort(
            country_topics[country,] - colMeans(country_topics),
            decreasing = TRUE
        ) %>% head
        result$country <- country
        result
    }
)

refs <- as(readRDS("france-country-ref.rds"), "CsparseMatrix")
model <- readRDS("france-lda.Rds")
dtm <- readRDS("france-dtm.rds")
projection <- predict(model, dtm, method = "dot")
country_topics <- matrix_mult(
    t(refs),
    Matrix(data = projection, dimnames = dimnames(projection)),
    1000
)
model$top_terms
map(
    attributes(country_topics)$Dimnames[[2]],
    function (topic) {
        result <- sort(
            country_topics[, topic] - rowMeans(country_topics),
            decreasing = TRUE
        ) %>% head
        result$topic <- topic
        result
    }
)
map(
    attributes(country_topics)$Dimnames[[1]],
    function (country) {
        result <- sort(
            country_topics[country,] - colMeans(country_topics),
            decreasing = TRUE
        ) %>% head
        result$country <- country
        result
    }
)

refs <- as(readRDS("hungary-country-ref.rds"), "CsparseMatrix")
model <- readRDS("hungary-lda.Rds")
dtm <- readRDS("hungary-dtm.rds")
projection <- predict(model, dtm, method = "dot")
country_topics <- matrix_mult(
    t(refs),
    Matrix(data = projection, dimnames = dimnames(projection)),
    1000
)
model$top_terms
map(
    attributes(country_topics)$Dimnames[[2]],
    function (topic) {
        result <- sort(
            country_topics[, topic] - rowMeans(country_topics),
            decreasing = TRUE
        ) %>% head
        result$topic <- topic
        result
    }
)
map(
    attributes(country_topics)$Dimnames[[1]],
    function (country) {
        result <- sort(
            country_topics[country,] - colMeans(country_topics),
            decreasing = TRUE
        ) %>% head
        result$country <- country
        result
    }
)

refs <- as(readRDS("ireland-country-ref.rds"), "CsparseMatrix")
model <- readRDS("ireland-lda.Rds")
dtm <- readRDS("ireland-dtm.rds")
projection <- predict(model, dtm, method = "dot")
country_topics <- matrix_mult(
    t(refs),
    Matrix(data = projection, dimnames = dimnames(projection)),
    1000
)
model$top_terms
map(
    attributes(country_topics)$Dimnames[[2]],
    function (topic) {
        result <- sort(
            country_topics[, topic] - rowMeans(country_topics),
            decreasing = TRUE
        ) %>% head
        result$topic <- topic
        result
    }
)
map(
    attributes(country_topics)$Dimnames[[1]],
    function (country) {
        result <- sort(
            country_topics[country,] - colMeans(country_topics),
            decreasing = TRUE
        ) %>% head
        result$country <- country
        result
    }
)

refs <- as(readRDS("italy-country-ref.rds"), "CsparseMatrix")
model <- readRDS("italy-lda.Rds")
dtm <- readRDS("italy-dtm.rds")
projection <- predict(model, dtm, method = "dot")
country_topics <- matrix_mult(
    t(refs),
    Matrix(data = projection, dimnames = dimnames(projection)),
    1000
)
model$top_terms
map(
    attributes(country_topics)$Dimnames[[2]],
    function (topic) {
        result <- sort(
            country_topics[, topic] - rowMeans(country_topics),
            decreasing = TRUE
        ) %>% head
        result$topic <- topic
        result
    }
)
map(
    attributes(country_topics)$Dimnames[[1]],
    function (country) {
        result <- sort(
            country_topics[country,] - colMeans(country_topics),
            decreasing = TRUE
        ) %>% head
        result$country <- country
        result
    }
)

refs <- as(readRDS("netherlands-country-ref.rds"), "CsparseMatrix")
model <- readRDS("netherlands-lda.Rds")
dtm <- readRDS("netherlands-dtm.rds")
projection <- predict(model, dtm, method = "dot")
country_topics <- matrix_mult(
    t(refs),
    Matrix(data = projection, dimnames = dimnames(projection)),
    1000
)
model$top_terms
map(
    attributes(country_topics)$Dimnames[[2]],
    function (topic) {
        result <- sort(
            country_topics[, topic] - rowMeans(country_topics),
            decreasing = TRUE
        ) %>% head
        result$topic <- topic
        result
    }
)
map(
    attributes(country_topics)$Dimnames[[1]],
    function (country) {
        result <- sort(
            country_topics[country,] - colMeans(country_topics),
            decreasing = TRUE
        ) %>% head
        result$country <- country
        result
    }
)

refs <- as(readRDS("poland-country-ref.rds"), "CsparseMatrix")
model <- readRDS("poland-lda.Rds")
dtm <- readRDS("poland-dtm.rds")
projection <- predict(model, dtm, method = "dot")
country_topics <- matrix_mult(
    t(refs),
    Matrix(data = projection, dimnames = dimnames(projection)),
    1000
)
model$top_terms
map(
    attributes(country_topics)$Dimnames[[2]],
    function (topic) {
        result <- sort(
            country_topics[, topic] - rowMeans(country_topics),
            decreasing = TRUE
        ) %>% head
        result$topic <- topic
        result
    }
)
map(
    attributes(country_topics)$Dimnames[[1]],
    function (country) {
        result <- sort(
            country_topics[country,] - colMeans(country_topics),
            decreasing = TRUE
        ) %>% head
        result$country <- country
        result
    }
)

refs <- as(readRDS("spain-country-ref.rds"), "CsparseMatrix")
model <- readRDS("spain-lda.Rds")
dtm <- readRDS("spain-dtm.rds")
projection <- predict(model, dtm, method = "dot")
country_topics <- matrix_mult(
    t(refs),
    Matrix(data = projection, dimnames = dimnames(projection)),
    1000
)
model$top_terms
map(
    attributes(country_topics)$Dimnames[[2]],
    function (topic) {
        result <- sort(
            country_topics[, topic] - rowMeans(country_topics),
            decreasing = TRUE
        ) %>% head
        result$topic <- topic
        result
    }
)
map(
    attributes(country_topics)$Dimnames[[1]],
    function (country) {
        result <- sort(
            country_topics[country,] - colMeans(country_topics),
            decreasing = TRUE
        ) %>% head
        result$country <- country
        result
    }
)
