## Description: Simple language detection using stopwords
## Author: Isidor Zeuner
## dh2021@quidecco.de
## For license see: https://mit-license.org
## -------------------------------------------------------------------

## compute what fraction of the `words` are in the stopword list
##  of the `language`
language_match <- function (words, language) {
    wordlist <- stopwords::stopwords(language, source = "stopwords-iso")
    length(which(words %in% wordlist)) / length(words)
}

## compute which of the `languages` has the highest fraction of the `words`
##  in its stopword list
language_best_match <- function (words, languages) {
    languages[[
        which.max(map(languages, partial(language_match, words = words)))
    ]]
}
