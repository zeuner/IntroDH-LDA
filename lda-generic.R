## Description: Extract topics from JSON-supplied debates
## Author: Isidor Zeuner
## dh2021@quidecco.de
## For license see: https://mit-license.org
## -------------------------------------------------------------------

library(readr)
library(jsonlite)
library(hunspell)
library(SnowballC)
library(textmineR)
library(hash)
library(tidyverse)

cpus_lda <- parallel::detectCores()

country_settings <- hash()

country_settings[["hungary"]] <- list(
    locale = "hu_HU",
    language = "hu"
)

analyze_country <- function (country_name) {
    settings <- country_settings[[country_name]]
    dtm_cache_file <- paste0(country_name, "-dtm.rds")
    if (file.exists(dtm_cache_file)) {
        dtm <- readRDS(dtm_cache_file)
    } else {
        frame <- fromJSON(read_file(paste0(country_name, ".json")))
        stemming_function <- function (x) {
            hunspell_stem(
                x,
                settings$locale
            ) %>% map(
                partial(tail, n = 1)
            ) %>% unlist
        }
        dtm <- CreateDtm(
            doc_vec = frame$data,
            doc_names = frame$id,
            ngram_window = c(1, 1),
            stopword_vec = c(stopwords::stopwords(settings$language)),
            lower = TRUE,
            remove_punctuation = TRUE,
            remove_numbers = TRUE,
            verbose = FALSE,
            stem_lemma_function = stemming_function,
            cpus = cpus_lda
        )
        saveRDS(dtm, file = dtm_cache_file)
    }
## arbitrary seeding for reproducible output
    set.seed(12833)
    model <- FitLdaModel(
        dtm = dtm,
        k = 20,
        iterations = 500,
        burnin = 180,
        alpha = 0.1,
        beta = 0.05,
        optimize_alpha = TRUE,
        calc_likelihood = TRUE,
        calc_coherence = TRUE,
        calc_r2 = TRUE,
        cpus = cpus_lda
    )
    return (model)
}
