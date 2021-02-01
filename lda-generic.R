## Description: Extract topics from JSON-supplied debates
## Author: Isidor Zeuner
## iz32mega@studserv.uni-leipzig.de
## For license see: https://mit-license.org
## -------------------------------------------------------------------

library(readr)
library(jsonlite)
library(SnowballC)
library(textmineR)

frame <- fromJSON(read_file("hungary.json"))

dtm <- CreateDtm(
    doc_vec = frame$data,
    doc_names = frame$id,
    ngram_window = c(1, 1),
    stopword_vec = c(stopwords::stopwords("hu")),
    lower = TRUE,
    remove_punctuation = TRUE,
    remove_numbers = TRUE,
    verbose = FALSE,
    stem_lemma_function = function(x) SnowballC::wordStem(x, "hungarian"),
    cpus = 2
)

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
    cpus = 2
)

## sample output of top terms for each topic
model$top_terms <- GetTopTerms(phi = model$phi, M = 5)

head(t(model$top_terms))
