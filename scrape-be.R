## -------------------------------------------------------------------
## Downloading and tidying Belgian parliamentary protocols from 2014-
## 2019
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

Sys.setlocale("LC_ALL", "C")
parliament_archive <-
    read_lines_retrying("https://www.senate.be/www/?MIval=/publications/ListPub&LANG=fr&COLL=H&LEG=6&START=1&END=48")

## -------------------------------------------------------------------
## Extracting the links for the pdf files

links <- grep("\"/www/webdriver[^\"]+Obj=application/pdf[^\"]+[0-9]+\"",
              parliament_archive, value=TRUE) %>%
    lapply(FUN=function(x) {
        str_replace(x, ".*\"(/www/webdriver[^\"]+Obj=application/pdf[^\"]+[0-9]+)\".*",
                    "https://www.senate.be\\1")})

for (i in 1:length(links)) {
    links[[i]] <- c(links[[i]], sprintf("be-%02d.pdf", i))
}

## Downloading

download_results <- 
    lapply(1:length(links),
           function(x) {
               return_value <- download_pdfs(x,links)
               print(return_value)
               return(return_value)})
                                                    
writeLines(unlist(download_results), file.path(result_directory, "be-download_results.txt"))


## Converting pdfs

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

remove_header <- function (page) {
    header_pos <- 41
    x <- filter(page, y > header_pos)
    if (length(x$text) == 0) {
        NULL
    } else x }

only_french <- function (page) {
    french_min_pos <- 300
    filter(page, x > french_min_pos)}

trim_content <- function (pages) {
    start <- 1
    end <- length(pages)
    for (i in 1:length(pages)) {
        first30words <- paste(pages[[i]]$text[1:30], collapse=" ")
        if (grepl("\\(La séance est ouverte à", first30words)) {
            start <- i
            break }}
    for (i in start:length(pages)) {
        if (grepl("\\(La séance est levée à", paste(pages[[i]]$text, collapse=" "))){
            end <- i
            break }}
    pages[start:end] }

pdf_to_txt <- function(infile, outfile, nth, total) {
    if (! file.exists(outfile)){
        pdf_data(infile) %>%
            lapply(remove_header) %>% 
            .[sapply(.,Negate(is.null))] %>% # removing empty pages
            lapply(only_french) %>%
            trim_content %>%
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

pdfs <- Sys.glob(file.path(pdf_directory, "be-*.pdf"))
    
convert_results <- 
    future.apply::future_lapply(future.seed=TRUE,1:length(pdfs),
                                function (x) {
                                    return_value <- convert_pdf(x,pdfs)
                                    print(return_value)
                                    return(return_value)})

writeLines(unlist(convert_results), file.path(result_directory, "be-convert_results.txt"))

id <- Sys.glob(file.path(txt_directory, "be-*.txt"))

data <- sapply(id, read_file)

exported <- data.frame(id, data)

json <- toJSON(exported)

write(json, file="belgium.json")

    
            
