## Description: Compute country references from document term matrices
## Author: Isidor Zeuner
## dh2021@quidecco.de
## For license see: https://mit-license.org
## -------------------------------------------------------------------

library(jsonlite)
source("lda-generic.R")

country_names <- fromJSON(read_file("country-names.json"))

compute_country_references <- function (country) {
    dtm <- readRDS(paste0(country, "-dtm.rds"))
    stemmer <- country_stemmer(country)
## return the original word if the stemmer doesn't recognize it
    stem_preserving <- function (x) {
        stemmer(x) %>% append(values = x, after = 0) %>% tail(n = 1)
    }
    settings <- country_settings[[country]]
    country_references <- Matrix(
        nrow = length(attributes(dtm)$Dimnames[[1]]),
        ncol = 0,
        dimnames = list(
            attributes(dtm)$Dimnames[[1]],
            list()
        ),
        sparse = TRUE
    )
    columns_stemmed <- attributes(
        dtm
    )$Dimnames[[2]] %>% map(
        stem_preserving
    ) %>% unlist
    for (country_id in 1:length(country_names$name_en)) {
        country_name_dtm <- CreateDtm(
            doc_vec = country_names[[
                paste0("name_", settings$language)
            ]][[country_id]],
            ngram_window = c(1, 1),
            stopword_vec = c(
                country_stopwords(
                    country
                )
            ),
            lower = TRUE,
            remove_punctuation = TRUE,
            remove_numbers = TRUE,
            verbose = FALSE
        )
        country_words <- attributes(country_name_dtm)$Dimnames[[2]] %>% map(
            stem_preserving
        ) %>% unlist
        word_stem_documents <- function (stem) {
            columns <- which(columns_stemmed == stem)
            if (0 == length(columns)) {
## `pmax` can't compute a maximum over nothing
                zero <- vector()
                zero[attributes(dtm)$Dimnames[[1]]] <- 0
                return (zero)
            }
            columns %>% map(
                function (column) {
                    dtm[, column]
                }
            ) %>% do.call(what = pmax)
        }
        word_stems_documents <- function (stems) {
            documents <- stems %>% map(word_stem_documents)
            documents %>% do.call(what = pmin)
        }
        docs <- word_stems_documents(country_words)
        matches <- Matrix(
            data = docs,
            nrow = length(docs),
            ncol = 1,
            dimnames = list(
                attributes(dtm)$Dimnames[[1]],
                list(country_names$name_en[[country_id]])
            ),
            sparse = TRUE
        )
        country_references <- cbind(
            country_references,
            matches
        )
    }
    saveRDS(country_references, file = paste0(country, "-country-ref.rds"))
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
    compute_country_references
)
