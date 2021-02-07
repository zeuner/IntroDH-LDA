## Description: Script for analyzing Hungarian parliamentary protocols
## from the 2014-2018 cycle
## Author: Agoston Volcz
## av71nigo@studserv.uni-leipzig.de
## For license see: https://mit-license.org
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
## Loading the parliamentary archive

parliament_archive <-
    read_lines_retrying("https://www.parlament.hu/web/guest/orszaggyulesi-naplo-2014-2018")


## -------------------------------------------------------------------
## Extracting the links for the pdf files

links <- parliament_archive[grep("/documents/10181/.*szám",parliament_archive)] %>%
    map(partial(str_replace,
                pattern=".*(/documents/[^\"]*)\".*>.*(\\d{4}.+\\d{2}.+\\d{2}).*",
                replacement="\\1\thu-\\2.pdf")) %>% # Extracting relative URL and date
    map(partial(str_replace,
                pattern=" ",
                replacement="")) %>% # Correcting malformed dates
    map(function (x) sprintf("https://www.parlament.hu%s", x)) %>%
    # Putting the URL together
    str_split(.,"\t")

## links: a list of character[2] vectors
## [0]: URL
## [1]: filename

## -------------------------------------------------------------------
## Downloading the pdf files into the pdfs directory

download_results <- 
    lapply(1:length(links),
                                function(x) {
                                    return_value <- download_pdfs(x,links)
                                    print(return_value)
                                    return(return_value)})
                                                    
writeLines(unlist(download_results), file.path(result_directory, "hu-download_results.txt"))


## -------------------------------------------------------------------
## Converting all pdf files into txt files

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
        sapply(trimws)
        }


pdf_to_txt <- function(infile,outfile,nth,total) {
    if (! file.exists(outfile)){
        pdf_data(infile) %>%
            ## Apparently all needed pages' content starts on y coordinate 67
            lapply(function (x) {
                if (length(x$y>0) && x$y[1] == 67) x
                else NULL}) %>%
            .[sapply(.,Negate(is.null))] %>%
            ## Removing page headers
            lapply(function (x) {filter (x,y!=67)}) %>%
            ## Inserting newlines and extracting text only
            lapply (insert_newlines) %>%
            unlist %>%
            paste(collapse=" ") %>%
            unhyphenate %>%
            trimws %>%
            writeLines (outfile)
        return(sprintf("[%3d/%3d] Converted %s -> %s",
                       nth,total,infile,outfile))}
    else { return(sprintf("[%3d/%3d] %s already present.",nth,total,outfile))}}

convert_pdf <- function(nth,pdfs) {
    infile <- pdfs[nth]
    outfile <- str_replace_all(infile,"pdf","txt")
    total <- length(pdfs)
    return(pdf_to_txt(infile,outfile,nth,total))
    }

pdfs <- Sys.glob(file.path(pdf_directory, "hu-*.pdf"))
    
convert_results <- 
    future.apply::future_lapply(future.seed=TRUE,1:length(pdfs),
                                function (x) {
                                    return_value <- convert_pdf(x,pdfs)
                                    print(return_value)
                                    return(return_value)})

writeLines(unlist(convert_results), file.path("hu-convert_results.txt"))

id <- Sys.glob(file.path(txt_directory, "hu-*.txt"))

data <- sapply(id, read_file)

exported <- data.frame(id, data)

json <- toJSON(exported)

write(json, file="hungary.json")

