## Description: Show an overview of country references related to LDA topics
## Author: Isidor Zeuner
## dh2021@quidecco.de
## For license see: https://mit-license.org
## -------------------------------------------------------------------

library(tidyverse)
library(textmineR)

model <- readRDS("austria-lda.Rds")

country_topics <- readRDS("austria-country-topics.rds")

model$top_terms

map(
    attributes(country_topics)$Dimnames[[2]],
    function (topic) {
        result <- sort(
            country_topics[, topic] / rowMeans(country_topics),
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
            country_topics[country,] / colMeans(country_topics),
            decreasing = TRUE
        ) %>% head
        result$country <- country
        result
    }
)

model <- readRDS("belgium-lda.Rds")

country_topics <- readRDS("belgium-country-topics.rds")

model$top_terms

map(
    attributes(country_topics)$Dimnames[[2]],
    function (topic) {
        result <- sort(
            country_topics[, topic] / rowMeans(country_topics),
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
            country_topics[country,] / colMeans(country_topics),
            decreasing = TRUE
        ) %>% head
        result$country <- country
        result
    }
)

model <- readRDS("czechia-lda.Rds")

country_topics <- readRDS("czechia-country-topics.rds")

model$top_terms

map(
    attributes(country_topics)$Dimnames[[2]],
    function (topic) {
        result <- sort(
            country_topics[, topic] / rowMeans(country_topics),
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
            country_topics[country,] / colMeans(country_topics),
            decreasing = TRUE
        ) %>% head
        result$country <- country
        result
    }
)

model <- readRDS("france-lda.Rds")

country_topics <- readRDS("france-country-topics.rds")

model$top_terms

map(
    attributes(country_topics)$Dimnames[[2]],
    function (topic) {
        result <- sort(
            country_topics[, topic] / rowMeans(country_topics),
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
            country_topics[country,] / colMeans(country_topics),
            decreasing = TRUE
        ) %>% head
        result$country <- country
        result
    }
)

model <- readRDS("hungary-lda.Rds")

country_topics <- readRDS("hungary-country-topics.rds")

model$top_terms

map(
    attributes(country_topics)$Dimnames[[2]],
    function (topic) {
        result <- sort(
            country_topics[, topic] / rowMeans(country_topics),
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
            country_topics[country,] / colMeans(country_topics),
            decreasing = TRUE
        ) %>% head
        result$country <- country
        result
    }
)

model <- readRDS("ireland-lda.Rds")

country_topics <- readRDS("ireland-country-topics.rds")

model$top_terms

map(
    attributes(country_topics)$Dimnames[[2]],
    function (topic) {
        result <- sort(
            country_topics[, topic] / rowMeans(country_topics),
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
            country_topics[country,] / colMeans(country_topics),
            decreasing = TRUE
        ) %>% head
        result$country <- country
        result
    }
)

model <- readRDS("italy-lda.Rds")

country_topics <- readRDS("italy-country-topics.rds")

model$top_terms

map(
    attributes(country_topics)$Dimnames[[2]],
    function (topic) {
        result <- sort(
            country_topics[, topic] / rowMeans(country_topics),
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
            country_topics[country,] / colMeans(country_topics),
            decreasing = TRUE
        ) %>% head
        result$country <- country
        result
    }
)

model <- readRDS("netherlands-lda.Rds")

country_topics <- readRDS("netherlands-country-topics.rds")

model$top_terms

map(
    attributes(country_topics)$Dimnames[[2]],
    function (topic) {
        result <- sort(
            country_topics[, topic] / rowMeans(country_topics),
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
            country_topics[country,] / colMeans(country_topics),
            decreasing = TRUE
        ) %>% head
        result$country <- country
        result
    }
)

model <- readRDS("poland-lda.Rds")

country_topics <- readRDS("poland-country-topics.rds")

model$top_terms

map(
    attributes(country_topics)$Dimnames[[2]],
    function (topic) {
        result <- sort(
            country_topics[, topic] / rowMeans(country_topics),
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
            country_topics[country,] / colMeans(country_topics),
            decreasing = TRUE
        ) %>% head
        result$country <- country
        result
    }
)

model <- readRDS("spain-lda.Rds")

country_topics <- readRDS("spain-country-topics.rds")

model$top_terms

map(
    attributes(country_topics)$Dimnames[[2]],
    function (topic) {
        result <- sort(
            country_topics[, topic] / rowMeans(country_topics),
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
            country_topics[country,] / colMeans(country_topics),
            decreasing = TRUE
        ) %>% head
        result$country <- country
        result
    }
)
