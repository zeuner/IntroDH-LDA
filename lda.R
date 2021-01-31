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
library(textmineR)
library(future.apply)
plan(multisession)

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


## -------------------------------------------------------------------
## Tidying up the txt files


## ------------------------------
## Various tidying functions

truncate_start_end <- function(data) {
    return(data %>%
           .[grep("\\f\\d",.)[1]+2:length(.)] %>%
           .[1:grep("Az.*ülés.*véget",.)-1])
    ## Truncating the whole file to the actual parliamentary debate:
    ## it starts at the first occurence of a number following a form
    ## feed character. It ends with the term `Az ülés(nap) {time} ért
    ## véget`.
}

remove_number_lines <- function(data) {
    return(data %>%
           .[. %in% .[grep("^\\f*\\d*$",.,invert=TRUE)]])
    ## Removing lines containing only form feed and/or a number
}

remove_empty_lines <- function(data) {
    return(data %>%
           .[. != ""] %>%
           .[! is.na(.)])}


## ------------------------------
## Piping through the tidying functions

tidy_data <- function (data) {
    data <- data %>%
        truncate_start_end %>%
        remove_number_lines %>%
        remove_empty_lines
    return(data)
}

tidy_txt <- function(nth, files){
    data <- readLines(files[nth]) %>%
        tidy_data
    writeLines(data,files[nth])
    }
        
txts <- Sys.glob("./txts/*.txt")

tidy_results <- 
    future.apply::future_lapply(1:length(txts),
                                partial(tidy_txt,files=txts))


## -------------------------------------------------------------------
## Creating CSV files

if (! dir.exists("./csvs/")){
    dir.create("./csvs/")}


hunspell_stem <- partial(hunspell_stem,dict="hu_HU")

## Due to the agglutinative nature of the Hungarian language, it is
## wiser to use hunspell to stem the words first, before further
## processing the text
stem_line <- function (line) {
    line <- str_split(line," ") %>% unlist
    for (i in 1:length(line)) {
        word <- line[i]
        line[i] <- hunspell_stem(word) %>% first %>% first}
    return(paste(line[!is.na(line)],collapse=" "))}
        

tokenize <- function(data) {
    data %>%
        as.list %>%
        lapply(stem_line)
        }

text_to_tokenized_csv <- function(infile, outfile,stopwords) {
    origin <- infile %>%
        str_replace(.,".*/","") %>%
        str_replace(.,".txt$","")
    
    data <- readLines(infile) %>%
        tokenize %>%
        tibble(text=.) %>%
        mutate(line=row_number()) %>% # Adding line numbers
        unnest_tokens(word,text) %>%
        anti_join(stopwords,by="word") # Removing stopwords and common biasing words

    data$origin <- origin # Adding the file-name in one column
    
    write_csv(data,outfile)}

stopwords <- tibble(word=readLines("./stop_words.txt"))

csv_results <-
    future.apply::future_lapply(1:length(txts),
                                function(x){
                                    infile <- txts[[x]]
                                    outfile <- str_replace_all(infile,"txt","csv")
                                    text_to_tokenized_csv(infile,outfile,stopwords)})


