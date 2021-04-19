## Description: Interactive visualization using the LDAvis library
## Author: Isidor Zeuner
## dh2021@quidecco.de
## For license see: https://mit-license.org
## -------------------------------------------------------------------

library(textmineR)
library(LDAvis)
library(jsonlite)

## interactively visualize topics for one country.
##  For example, run `serve_interactive_visualization("austria")` from
##  an interactive R session
serve_interactive_visualization <- function (country) {
    model <- readRDS(paste0(country, "-lda.Rds"))
    dtm <- readRDS(paste0(country, "-dtm.rds"))
    document_lengths <- rowSums(dtm)
    term_frequencies <- TermDocFreq(dtm = dtm)
    json <- createJSON(
        phi = model$phi,
        theta = model$theta,
        doc.length = document_lengths,
        vocab = term_frequencies$term,
        term.frequency = term_frequencies$term_freq
    )
    serVis(json)
}
