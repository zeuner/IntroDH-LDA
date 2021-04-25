## -------------------------------------------------------------------
## Downloading and tidying Spanish parliamentary protocols from 2011-
## 2021
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
## Loading the website

period_sessions <- list(c(10, 178),
                        c(11, 8),
                        c(12, 101),
                        c(13, 3),
                        c(14, 41))
                     

links <- lapply(period_sessions, function (period) {
    lapply(1:period[2],
                function (x) {
                    dl_link <- 
                        sprintf("https://www.senado.es/legis%d/publicaciones/pdf/senado/ds/DS_P_%d_%d.PDF",
                                period[1], period[1], x)
                    c(dl_link, sprintf("es-%d-%03d.pdf", period[1],  x)) })})

links <- unlist(links, recursive=FALSE)

## Downloading

download_results <- 
    lapply(1:length(links),
           function(x) {
               return_value <- download_pdfs(x,links)
               print(return_value)
               return(return_value)})
                                                    
writeLines(unlist(download_results), file.path(result_directory, "es-download_results.txt"))


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

remove_header_footer <- function (page) {
    header_pos <- 100
    footer_pos <- 785
    x <- filter(page, y > header_pos, y < footer_pos)
    if (length(x$text) == 0) {
        NULL
    } else x }

trim_content <- function (pages) {
    start <- 1
    for (i in 1:length(pages)) {
        first30words <- paste(pages[[i]]$text[1:30], collapse=" ")
        if (grepl("^Se .+ la sesión a las", first30words)) {
            start <- i
            break }}
    pages[start:length(pages)] }


pdf_to_txt <- function(infile, outfile, nth, total) {
#    if (! file.exists(outfile)){
    txt_output <- pdf_data(infile) %>%
            lapply(remove_header_footer) %>% 
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
 #   else { return(sprintf("[%3d/%3d] %s already present.",nth,total,outfile))}}


convert_pdf <- function(nth,pdfs) {
    infile <- pdfs[nth]
    outfile <- str_replace_all(infile,"pdf","txt")
    total <- length(pdfs)
    return(pdf_to_txt(infile,outfile,nth,total))
    }

pdfs <- Sys.glob(file.path(pdf_directory, "es-*.pdf"))
    
convert_results <- 
    future.apply::future_lapply(future.seed=TRUE,1:length(pdfs),
                                function (x) {
                                    return_value <- convert_pdf(x,pdfs)
                                    print(return_value)
                                    return(return_value)})

writeLines(unlist(convert_results), file.path(result_directory, "es-convert_results.txt"))

id <- Sys.glob(file.path(txt_directory, "es-*.txt"))

data <- sapply(id, read_file)

exported <- data.frame(id, data)

json <- toJSON(exported)

write(json, file="spain.json")
