setwd("./data")
raw_bible <- scan("1581-0.txt", what = "character", skip = 156)
n <- length(raw_bible)
raw_bible <- raw_bible[- ((n - 2909):n)] ## strip license

split_punct <- function(words, punct) {
    punct_index <- grep(punct, words, fixed = TRUE)
    words_and_punct <- rep("", length(words) + length(punct_index))

    # Each found punctuation pushes the next back by one
    new_punct_index <- punct_index + 1:length(punct_index)
    
    # Insert the word without punctuation
    words[punct_index] <- gsub(punct, "", words[punct_index], fixed = TRUE)
    # Insert the punctuation. Note we do not insert the regex match.
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
#changes all capitals to lower case
low_processed_bible <- tolower(processed_bible)

#assigns each unique word a number
unique_words <- unique(low_processed_bible)

#matches each word in bible with its unique number
match_words <- match(low_processed_bible, unique_words)

#frequency of each word
freq <- tabulate(match_words,length(unique_words))

threshold <- 100

#creates vector that will contain the unique word indices
#of commonly occurring words
common_index <- numeric()

#if a word occurs more than [threshold] times its index is added to common_index
for (i in 1:length(freq)){
    if (freq[i] >= threshold){
        common_index <- append(common_index, i)
    }
}

#vector of words that occur more than 100 times
b <- unique_words[common_index]


##Q 7

#Vector, corr, of giving corresponding element of b from
#lowercase bible
corr <- match(low_processed_bible, b)

#corr without last value and first
corr_nolast <- head(corr, -1)
corr_shift <- corr[-corr[1]]

#Matrix of corr_nolast and corr_shift
pair_ <- cbind(corr_nolast, corr_shift)

#Matrix containing only common word pairs
common_pair <- pair_[rowSums(is.na(pair_)) == 0, ]

#Initialize Matrix A (and A_new)
A <- matrix(0, length(b), length(b))
A_new <- matrix(0, length(b), length(b))

#Create A[i,j] with 1 for every ith common word
#followed by jth common word
for (n in 1:nrow(common_pair)) {
    A_new[common_pair[n,1],common_pair[n,2]] <- 1
    A <- A_new
}

#Vector containing row sums of A
row_tot <- rowSums(A)

#Initialize vector A_prob
A_prob <- matrix(0 , nrow(A), ncol(A))

#Create A_prob from A
for (i in 1:nrow(A)) {
    for (j in 1:ncol(A)) {
        A_prob[i,j] <- A[i,j] / row_tot[i]
    }
}




## Test scenarios:
test <- c("dub:", "foo ", "bar,", "baz.", "bax,")
commas_gone <- split_punct(test, ",")
print(commas_gone)
print(split_punct(test, "."))

print(split_punct(commas_gone, ":"))