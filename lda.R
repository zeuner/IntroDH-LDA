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
library(rjson)
library(textmineR)
library(future.apply)
plan(multisession)

## -------------------------------------------------------------------
## Loading the parliamentary archive

parliament_archive <-
    readLines("https://www.parlament.hu/web/guest/orszaggyulesi-naplo-2014-2018")


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


download_file <- function(URL,outfile,nth,total) {
    if (file.exists(outfile)){
        outfile <- str_replace(outfile, ".pdf$", "_1.pdf")
        }
    download.file(URL, outfile, mode = "wb")
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
download_results <- 
    future.apply::future_lapply(1:length(links),
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

result <- data.frame(id, data)

exported <- toJSON(unname(split(result,1:nrow(result))))

write(exported, file="hungary.json")

