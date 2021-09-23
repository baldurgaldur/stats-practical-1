setwd("./data")
raw_bible <- scan("1581-0.txt", what = "character", skip = 156)
n <- length(raw_bible)
raw_bible <- raw_bible[- ((n - 2909):n)] ## strip license

split_punct <- function(words, punct) {
    punct_index <- grep(punct, words, fixed = TRUE)
    words_and_punct <- rep("", length(words) + length(punct_index))

    ## Each found punctuation pushes the next back by one
    new_punct_index <- punct_index + 1:length(punct_index)
    
    ## Insert the word without punctuation
    words[punct_index] <- gsub(punct, "", words[punct_index], fixed = TRUE)
    ## Insert the punctuation
    words_and_punct[new_punct_index] <- punct
    ## Insert the rest
    words_and_punct[-new_punct_index] <- words
    words_and_punct
}

processed_bible <- raw_bible
all_punctuation <- c(",", ".", ";", "!", ":", "?")
for(punctuation in all_punctuation) {
    processed_bible <- split_punct(processed_bible, punctuation)
}

## 6

## Test scenarios:
test <- c("dub:", "foo ", "bar,", "baz.", "bax,")
commas_gone <- split_punct(test, ",")
print(commas_gone)
print(split_punct(test, "."))

print(split_punct(commas_gone, ":"))
