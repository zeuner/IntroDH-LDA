## Description: Example script for investigating country-topic correlations
## Author: Isidor Zeuner
## dh2021@quidecco.de
## For license see: https://mit-license.org
## -------------------------------------------------------------------

library(textmineR)
library(tidyverse)

refs <- as(readRDS("austria-country-ref.rds"), "CsparseMatrix")
model <- readRDS("austria-lda.Rds")
dtm <- readRDS("austria-dtm.rds")
country_topics <- t(refs) %*% predict(model, dtm, method = "dot")
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
country_topics <- t(refs) %*% predict(model, dtm, method = "dot")
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
country_topics <- t(refs) %*% predict(model, dtm, method = "dot")
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
country_topics <- t(refs) %*% predict(model, dtm, method = "dot")
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
country_topics <- t(refs) %*% predict(model, dtm, method = "dot")
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
country_topics <- t(refs) %*% predict(model, dtm, method = "dot")
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
country_topics <- t(refs) %*% predict(model, dtm, method = "dot")
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
country_topics <- t(refs) %*% predict(model, dtm, method = "dot")
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
country_topics <- t(refs) %*% predict(model, dtm, method = "dot")
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
country_topics <- t(refs) %*% predict(model, dtm, method = "dot")
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
