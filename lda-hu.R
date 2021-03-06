## Description: Extract topics from JSON-supplied debates
## Author: Isidor Zeuner
## dh2021@quidecco.de
## For license see: https://mit-license.org
## -------------------------------------------------------------------

library(textmineR)
source("lda-generic.R")

model <- analyze_country("hungary")$model

write_rds(model , file="hungary-lda.Rds")

write_csv(model$summary, "hungary-summary.csv")
