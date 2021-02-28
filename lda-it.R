## Description: Extract topics from JSON-supplied debates
## Author: Isidor Zeuner
## dh2021@quidecco.de
## For license see: https://mit-license.org
## -------------------------------------------------------------------

library(textmineR)
source("lda-generic.R")

model <- analyze_country("italy")

## sample output of top terms for each topic
model$top_terms <- GetTopTerms(phi = model$phi, M = 10)

head(t(model$top_terms))
