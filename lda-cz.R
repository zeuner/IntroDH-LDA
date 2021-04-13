## Description: Extract topics from JSON-supplied debates
## Author: Isidor Zeuner
## dh2021@quidecco.de
## For license see: https://mit-license.org
## -------------------------------------------------------------------

library(textmineR)
source("lda-generic.R")

model_file <- analyze_country("czechia")$model_file

model <- readRDS(model_file)

write_rds(model , file="czechia-lda.Rds")

write_csv(model$summary, "czechia-summary.csv")
