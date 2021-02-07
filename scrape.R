options(HTTPUserAgent="Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/52.0.2743.116 Safari/537.36")

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

pdf_directory <- file.path(getwd(), "pdfs")

if (! dir.exists(pdf_directory)){
    dir.create(pdf_directory)}

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
    outfile <- file.path(pdf_directory, links[[nth]][2])
    total <- length(links)
    return(download_file(URL,outfile,nth,total))}


result_directory <- file.path(getwd(), "results")

if (! dir.exists(result_directory)){
    dir.create(result_directory)}

txt_directory <- file.path(getwd(), "txts")

if (! dir.exists(txt_directory)){
    dir.create(txt_directory)}
