## -------------------------------------------------------------------
## Downloading and tidying Czech parliamentary protocols from 2013-
## 2017
## -------------------------------------------------------------------

library(tidytext)
library(tidyverse)
library(hunspell)
library(pdftools)
library(jsonlite)
library(textmineR)
library(future.apply)
plan(multisession)
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
                                                    
writeLines(unlist(download_results), file.path(result_directory, "cz-download_results.txt"))


## ===================================================================
## Processing pdf files

newline <- function (x,y) {
    if (!y) {
        paste(x,"\n",sep="")}
    else x}

insert_newlines <- function (t) {
    words <- t$text
    spaces <- t$space
    mapply(newline, words, spaces)}

unhyphenate <- function (data) {
    trimws(data) %>%
        paste(collapse="") %>% 
        str_replace_all(pattern="-\n ?([^0-9 ]+) ",replacement="\\1\n") %>%
        str_split("\n") %>%
        sapply(trimws)}

remove_page_numbering <- function (page) {
                pageno_pos <- last(page$y)
                x <- filter(page, y != pageno_pos)
                if (length(x$text) == 0) {
                    NULL
                } else x }

## need Czech locale to properly export the debate texts
Sys.setlocale('LC_ALL','cs_CZ.UTF-8')

pdf_to_txt <- function(infile, outfile, nth, total) {
    if (! file.exists(outfile)){
        pdf_data(infile) %>%
            lapply(remove_page_numbering) %>% 
            .[sapply(.,Negate(is.null))] %>% # removing empty pages
            lapply(insert_newlines) %>%
            unlist %>%
            paste(collapse=" ") %>%
            unhyphenate %>%
            trimws %>%
            writeLines(outfile)
        return(sprintf("[%3d/%3d] Converted %s -> %s",
                       nth,total,infile,outfile))}
    else { return(sprintf("[%3d/%3d] %s already present.",nth,total,outfile))}}


convert_pdf <- function(nth,pdfs) {
    infile <- pdfs[nth]
    outfile <- str_replace_all(infile,"pdf","txt")
    total <- length(pdfs)
    return(pdf_to_txt(infile,outfile,nth,total))
    }

pdfs <- Sys.glob(file.path(pdf_directory, "cz-*.pdf"))
    
convert_results <- 
    future.apply::future_lapply(future.seed=TRUE,1:length(pdfs),
                                function (x) {
                                    return_value <- convert_pdf(x,pdfs)
                                    print(return_value)
                                    return(return_value)})

writeLines(unlist(convert_results), file.path(result_directory, "cz-convert_results.txt"))

id <- Sys.glob(file.path(txt_directory, "cz-*.txt"))

data <- sapply(id, read_file)

exported <- data.frame(id, data)

json <- toJSON(exported)

write(json, file="czechia.json")


