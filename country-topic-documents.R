## Description: Helper function to find relevant documents for a country-topic
##  relation
## Author: Isidor Zeuner
## dh2021@quidecco.de
## For license see: https://mit-license.org
## -------------------------------------------------------------------

library(textmineR)

country_topic_documents <- function (parliament, country, topic) {
    country_references <- as(
        readRDS(paste0(parliament, "-country-ref.rds")),
        "CsparseMatrix"
    )
    model <- readRDS(paste0(parliament, "-lda.Rds"))
    dtm <- readRDS(paste0(parliament, "-dtm.rds"))
    document_topics <- predict(model, dtm, method = "dot")
    sort(-country_references[, country] * document_topics[, topic])[1 : 20]
}
