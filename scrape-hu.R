## Description: Script for analyzing Hungarian parliamentary protocols
## from the 2014-2018 cycle
## Author: Agoston Volcz
## av71nigo@studserv.uni-leipzig.de
## For license see: https://mit-license.org
## -------------------------------------------------------------------

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

parliament_archives <- list(
    read_lines_retrying("https://www.parlament.hu/web/guest/orszaggyulesi-naplo-2014-2018"),
    read_lines_retrying("https://www.parlament.hu/web/guest/orszaggyulesi-naplo"))


## -------------------------------------------------------------------
## Extracting the links for the pdf files

links <- lapply(parliament_archives, function (parliament_archive) {
    archive <- parliament_archive %>% paste(., collapse="\n") %>%
        str_replace_all(pattern="<td", replacement="\n<td") %>%
        str_split(pattern="\n") %>% unlist
    archive[grep("/documents/10181/.*szám",archive)] %>%
    map(partial(str_replace,
                pattern=".*(/documents/[^\"]*)\".*",
                replacement="\\1")) %>% # Extracting relative URL and date
    map(function (x) sprintf("https://www.parlament.hu%s", x)) %>% rev}) %>%
    # Putting the URL together
    rev

periods <- c("2014-2018", "2018-2022")

for (i in 1:2) {
    for (j in 1:length(links[[i]])) {
        links[[i]][[j]] <- c(links[[i]][[j]], sprintf("hu-%s-%03d.pdf", periods[i], j)) }}

links <- unlist(links , recursive =FALSE)

## links: a list of character[2] vectors
## [0]: URL
## [1]: filename

## -------------------------------------------------------------------
## Downloading the pdf files into the pdfs directory

## https://stackoverflow.com/questions/66083734/handling-exceptions-using-trycatch-inside-future-applyfuture-lapply
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
##     if (! file.exists(outfile)){
    txt_output <- pdf_data(infile) %>%
            ## Apparently all needed pages' content starts on y coordinate 67
            lapply(function (x) {
                if (length(x$y>0) && x$y[1] > 65 && x$y[1] < 70) x
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
            str_split(" ") %>%
            unlist %>%
            split(., ceiling(seq_along(.) / 1000))

    lapply(1:length(txt_output), function(x) {
            write_file(x = paste(txt_output[[x]], collapse=" "),
                       file = str_replace(outfile, "\\.txt",
                                          sprintf("-%03d.txt", x))
                       )
            })
    return(sprintf("[%3d/%3d] Converted %s -> %s",
                   nth,total,infile,outfile))
}
    ## else { return(sprintf("[%3d/%3d] %s already present.",nth,total,outfile))}}

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

