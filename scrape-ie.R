## -------------------------------------------------------------------
## Downloading and tidying Irish parliamentary protocols from 2016-
## 2019
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


add_download_names <- function(x) {
    lapply(1:length(x), function(y) {
        c(sprintf("https://data.oireachtas.ie/ie/oireachtas/debateRecord/dail/%s/debate/mul@/main.pdf",
                  x[[y]]),
          sprintf("ie-%03d.pdf", y))})}


links <- sapply(1:4, function(x) {
    website <- read_lines_retrying(paste("https://www.oireachtas.ie/en/debates/find/?page=",
          x,
          "&datePeriod=term&debateType=dail&term=%2Fie%2Foireachtas%2Fhouse%2Fdail%2F32&resultsPerPage=100"
          )) %>%
        paste(collapse="")
    debate_dates <- unique(unlist(regmatches(website, gregexpr("201[6-9]-[0-9]{2}-[0-9]{2}", website))))}) %>%
    do.call(c, .) %>%
    sort %>%
    as.list %>%
    add_download_names


## -------------------------------------------------------------------
## Downloading pdf files into pdfs directory

download_results <- 
    lapply(1:length(links),
           function(x) {
               return_value <- download_pdfs(x,links)
               print(return_value)
               return(return_value)})
                                                    
writeLines(unlist(download_results), file.path(result_directory, "ie-download_results.txt"))



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

remove_header_footer <- function (page) {
    header_pos <- 72
    footer_pos <- 770
    x <- filter(page, y > header_pos, y < footer_pos)
    if (length(x$text) == 0) {
        NULL
    } else x }

trim_content <- function(pages) {
    pages[2:length(pages)]
    }

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
#    else { return(sprintf("[%3d/%3d] %s already present.",nth,total,outfile))}}


convert_pdf <- function(nth,pdfs) {
    infile <- pdfs[nth]
    outfile <- str_replace_all(infile,"pdf","txt")
    total <- length(pdfs)
    return(pdf_to_txt(infile,outfile,nth,total))
    }

pdfs <- Sys.glob(file.path(pdf_directory, "ie-*.pdf"))
    
convert_results <- 
    future.apply::future_lapply(future.seed=TRUE,1:length(pdfs),
                                function (x) {
                                    return_value <- convert_pdf(x,pdfs)
                                    print(return_value)
                                    return(return_value)})

writeLines(unlist(convert_results), file.path(result_directory, "ie-convert_results.txt"))

id <- Sys.glob(file.path(txt_directory, "ie-*.txt"))

data <- sapply(id, read_file)

exported <- data.frame(id, data)

json <- toJSON(exported)

write(json, file="ireland.json")

