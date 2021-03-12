library(tidyverse)
library(pracma)

split_at <- 1000

lines <- readLines("txts/be-1-110.txt")

words <- map(
    lines,
    function (line) {
        sapply(strsplit(line, " "), length)
    }
)

cumulated_words <- cumsum(words)

(
    1 : ceil(cumulated_words[[length(cumulated_words)]] / split_at)
) %>% map(
    function (to) {
        from <- to - 1
        from_words <- from * split_at
        to_words <- to * split_at
        lines[
            which(
                from_words < cumulated_words & cumulated_words <= to_words
            )
        ]
    }
)
