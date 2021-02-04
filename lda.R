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

options(HTTPUserAgent="Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/52.0.2743.116 Safari/537.36")

## -------------------------------------------------------------------
## Loading the parliamentary archive

read_lines_retrying <- function(url, attempts = 5, throttle = 5) {
    result <- NA
    while (is.na(result) && 0 < attempts) {
        attempts <- attempts - 1
        result <- tryCatch(
            {
                readLines(url)
            },
            error = function(cond) {
                message("caught error:")
                message(cond)
                message("")
                Sys.sleep(throttle)
                return(NA)
            }
        )
    }
    if (is.na(result)) {
        stop(paste("could not get URL ", url))
    }
    return(result)
}

parliament_archive <-
    read_lines_retrying("https://www.parlament.hu/web/guest/orszaggyulesi-naplo-2014-2018")


## -------------------------------------------------------------------
## Extracting the links for the pdf files

links <- parliament_archive[grep("/documents/10181/.*szám",parliament_archive)] %>%
    map(partial(str_replace,
                pattern=".*(/documents/[^\"]*)\".*>.*(\\d{4}.+\\d{2}.+\\d{2}).*",
                replacement="\\1\t\\2.pdf")) %>% # Extracting relative URL and date
    map(partial(str_replace,
                pattern=" ",
                replacement="")) %>% # Correcting malformed dates
    map(function (x) sprintf("https://www.parlament.hu%s", x)) %>%
    # Putting the URL together
    str_split(.,"\t")

## links: a list of character[2] vectors

## -------------------------------------------------------------------
## Downloading the pdf files into the pdfs directory

if (! dir.exists("./pdfs")){
    dir.create("./pdfs")}


get_url_retrying <- function(URL, outfile, attempts = 5, throttle = 5) {
    result <- NA
    while (is.na(result) && 0 < attempts) {
        attempts <- attempts - 1
        result <- tryCatch(
            {
                download.file(URL, outfile, mode = "wb")
            },
            error = function(cond) {
                message("caught error:")
                message(cond)
                message("")
                Sys.sleep(throttle)
                return(NA)
            }
        )
    }
    if (is.na(result)) {
        stop(paste("could not get URL ", URL))
    }
    return(result)
}

download_file <- function(URL,outfile,nth,total) {
    if (file.exists(outfile)){
        outfile <- str_replace(outfile, ".pdf$", "_1.pdf")
        }
    get_url_retrying(URL, outfile)
    return(sprintf("[%3d/%3d] Downloaded file %s",
                           nth,total,outfile ))}


download_pdfs <- function(nth, links) {
    URL <- links[[nth]][1]
    outfile <- sprintf("./pdfs/%s",links[[nth]][2])
    total <- length(links)
    return(download_file(URL,outfile,nth,total))}


if (! dir.exists("./results")){
    dir.create("./results")}

## Asynchronously downloading the pdf files
#download_results <- 
#    future.apply::future_lapply(1:length(links),
#                                function(x) {
#                                    return_value <- download_pdfs(x,links)
#                                    print(return_value)
#                                    return(return_value)})

## for some reason, the tryCatch block doesn't work with future.apply
download_results <- 
    lapply(1:length(links),
                                function(x) {
                                    return_value <- download_pdfs(x,links)
                                    print(return_value)
                                    return(return_value)})
                                                    
writeLines(unlist(download_results),"./results/download_results.txt")                                  


## -------------------------------------------------------------------
## Converting all pdf files into txt files

if (! dir.exists("./txts")){
    dir.create("./txts")}

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

pdfs <- Sys.glob("./pdfs/*.pdf")
    
convert_results <- 
    future.apply::future_lapply(future.seed=TRUE,1:length(pdfs),
                                function (x) {
                                    return_value <- convert_pdf(x,pdfs)
                                    print(return_value)
                                    return(return_value)})

writeLines(unlist(convert_results),"./results/convert_results.txt")

id <- Sys.glob("./txts/*.txt")

data <- sapply(id, read_file)

exported <- data.frame(id, data)

json <- toJSON(exported)

write(json, file="hungary.json")

