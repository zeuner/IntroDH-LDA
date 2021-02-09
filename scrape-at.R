## -------------------------------------------------------------------
## Downloading and tidying Austrian parliamentary protocols from 2017-
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
## Fetching pdf urls

websites <- sapply(1:89,
    function(x){
      sprintf("https://www.parlament.gv.at/PAKT/VHG/XXVI/NRSITZ/NRSITZ_%s/",
              str_pad(x, width=5, pad="0"))})

links <- lapply(websites, read_lines_retrying) %>%
    sapply(function(x) {
        grep("/PAKT/VHG/XXVI/NRSITZ/NRSITZ_[0-9]+/fname_[0-9]+.pdf",
             x, value=TRUE) %>%
            str_replace(pattern=".*(/PAKT/VHG/XXVI/NRSITZ/NRSITZ_[0-9]+/fname_[0-9]+.pdf).*",
                        replacement="https://www.parlament.gv.at\\1")}) %>%
    as.list

for (i in 1:length(links)){
    links[[i]] <- c(links[[i]], sprintf("at-%02d.pdf", i))
}


download_results <-
    lapply(1:length(links),
           function(x) {
               return_value <- download_pdfs(x,links)
               print(return_value)
               return(return_value)})

writeLines(unlist(download_results), file.path(result_directory, "at-download_results.txt"))


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

remove_header <- function(page) {
    page <- filter(page, y > 37)
    if (length(page$text) == 0) {
        NULL
    } else page }

trim_content <- function(pages) {
    for (i in 1:length(pages)) {
        p <- pages[[i]]
        if (paste(p$text[1], p$text[2], p$text[3]) == "Beginn der Sitzung:") {
            start <- i
            break
        }}
    pages[start:length(pages)]}

pdf_to_txt <- function(infile, outfile, nth, total) {
    if (! file.exists(outfile)){
        pdf_data(infile) %>%
            lapply(remove_header) %>%
            .[sapply(.,Negate(is.null))] %>% # removing empty pages
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

pdfs <- Sys.glob(file.path(pdf_directory, "at-*.pdf"))

convert_results <-
    future.apply::future_lapply(future.seed=TRUE,1:length(pdfs),
                                function (x) {
                                    return_value <- convert_pdf(x,pdfs)
                                    print(return_value)
                                    return(return_value)})

writeLines(unlist(convert_results),"./results/at-convert_results.txt")

id <- Sys.glob("./txts/at-*.txt")

data <- sapply(id, read_file)

exported <- data.frame(id, data)

json <- toJSON(exported)

write(json, file="austria.json")