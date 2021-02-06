## -------------------------------------------------------------------
## Downloading and tidying Czech parliamentary protocols from 2013-
## 2017
## -------------------------------------------------------------------

library(tidyverse)
library(tidytext)
source("scrape.R")

## -------------------------------------------------------------------
## Loading the website

parliament_archive <-
    read_lines_retrying("https://public.psp.cz/eknih/2013ps/tesnopis/index.htm")

## -------------------------------------------------------------------
## Extracting the links for the pdf files

Sys.setlocale("LC_ALL", "C")

links <- grep("tz[0-9]{3}\\.pdf",parliament_archive, value = TRUE) %>%
    sapply(FUN=function(x) {
        str_replace(x, ".*\"(tz[0-9]{3}\\.pdf)\".*", "\\1")}) %>%
    sapply(FUN=function(x) {
        paste(paste("https://public.psp.cz/eknih/2013ps/tesnopis/", x, sep=""),
              paste("cz-", x, sep=""), sep="\t")}) %>%
    str_split("\t")


## -------------------------------------------------------------------
## Downloading pdf files into pdfs directory

download_results <- 
    lapply(1:length(links),
           function(x) {
               return_value <- download_pdfs(x,links)
               print(return_value)
               return(return_value)})
                                                    
writeLines(unlist(download_results),"./results/download_results-cz.txt")                                  
