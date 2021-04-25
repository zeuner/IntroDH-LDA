## -------------------------------------------------------------------
## Downloading and tidying Italian parliamentary protocols from 2013-
## 2018
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
## Loading the websites and extracting download links

websites <- sapply(2013:2018, function(x) {sprintf("http://senato.it/static/bgt/listaresaula/17/%d/index.html", x)})

links <- lapply(websites, read_lines_retrying) %>%
    sapply(function(x) {
        grep("http://www\\.senato\\.it/service/PDF/PDFServer/BGT/[0-9]+\\.pdf",
             x, value=TRUE)}) %>%
    sapply(function(x) {
        str_replace(x, ".*(http.+pdf).*Seduta n. ([0-9]+).*",
                    "\\1\tit-\\2.pdf") %>%
            str_split(pattern="\t")}) %>%
    do.call(c, .)


## -------------------------------------------------------------------
## Actually downloading the files

download_results <- 
    lapply(1:length(links),
           function(x) {
               return_value <- download_pdfs(x,links)
               print(return_value)
               return(return_value)})
                                                    
writeLines(unlist(download_results), file.path(result_directory, "it-download_results.txt"))

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
    page <- filter(page, y > 100)
    if (length(page$text) == 0) {
        NULL
    } else page }

trim_content <- function(pages) {
    for (i in 1:length(pages)) {
        p <- pages[[i]]
        if (paste(p$text[1], p$text[2]) == "RESOCONTO STENOGRAFICO") {
            start <- i
            break
        }}
    pages[start:length(pages)]}
            

pdf_to_txt <- function(infile, outfile, nth, total) {
#    if (! file.exists(outfile)){
    txt_output <- pdf_data(infile) %>%
            lapply(remove_header) %>% 
            .[sapply(.,Negate(is.null))] %>% # removing empty pages
            trim_content %>%
            lapply(insert_newlines) %>%
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
                       nth,total,infile,outfile))}
#    else { return(sprintf("[%3d/%3d] %s already present.",nth,total,outfile))}}


convert_pdf <- function(nth,pdfs) {
    infile <- pdfs[nth]
    outfile <- str_replace_all(infile,"pdf","txt")
    total <- length(pdfs)
    return(pdf_to_txt(infile,outfile,nth,total))
    }

pdfs <- Sys.glob(file.path(pdf_directory, "it-*.pdf"))
    
convert_results <- 
    future.apply::future_lapply(future.seed=TRUE,1:length(pdfs),
                                function (x) {
                                    return_value <- convert_pdf(x,pdfs)
                                    print(return_value)
                                    return(return_value)})

writeLines(unlist(convert_results), file.path(result_directory, "it-convert_results.txt"))

id <- Sys.glob(file.path(txt_directory, "it-*.txt"))

data <- sapply(id, read_file)

exported <- data.frame(id, data)

json <- toJSON(exported)

write(json, file="italy.json")

