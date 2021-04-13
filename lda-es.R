## Description: Extract topics from JSON-supplied debates
## Author: Isidor Zeuner
## dh2021@quidecco.de
## For license see: https://mit-license.org
## -------------------------------------------------------------------

library(textmineR)
source("lda-generic.R")

model_file <- analyze_country("spain")$model_file

model <- readRDS(model_file)

write_rds(model , file="spain-lda.Rds")

write_csv(model$summary, "spain-summary.csv")
