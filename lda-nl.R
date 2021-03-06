## Description: Extract topics from JSON-supplied debates
## Author: Isidor Zeuner
## dh2021@quidecco.de
## For license see: https://mit-license.org
## -------------------------------------------------------------------

library(textmineR)
source("lda-generic.R")

model <- analyze_country("netherlands")$model

write_rds(model , file="netherlands-lda.Rds")

write_csv(model$summary, "netherlands-summary.csv")
