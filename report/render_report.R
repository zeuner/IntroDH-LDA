library(rmarkdown)
library(bookdown)
render("report.Rmd", pdf_document2(toc = FALSE))
