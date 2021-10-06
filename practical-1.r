setwd("./data")
raw_bible <- scan("1581-0.txt", what = "character", skip = 156)
n <- length(raw_bible)
# Strip license at the back
raw_bible <- raw_bible[- ((n - 2909):n)] 

split_punct <- function(words, punct) {
    punct_index <- grep(punct, words, fixed = TRUE)
    if (length(punct_index) == 0) {
        # No match for this punc.
        return(words)
    }

    # Initialize the resulting vector
    words_and_punct <- rep("", length(words) + length(punct_index))

    # Each found punctuation pushes the next back by one
    new_punct_index <- punct_index + 1:length(punct_index)
    # Insert the word without punctuation
    words[punct_index] <- gsub(punct, "", words[punct_index], fixed = TRUE)

    # Insert the punctuation.
    words_and_punct[new_punct_index] <- punct
    # Insert the rest
    words_and_punct[-new_punct_index] <- words
    words_and_punct
}

processed_bible <- raw_bible
all_punctuation <- c(",", ".", ";", "!", ":", "?")
for (punctuation in all_punctuation) {
    processed_bible <- split_punct(processed_bible, punctuation)
}

# Q.6
# Change all capitals to lower case
low_processed_bible <- tolower(processed_bible)

# Assign each unique word a number
unique_words <- unique(low_processed_bible)

# Match each word in bible with its unique number
match_words <- match(low_processed_bible, unique_words)

# Frequency of each word
freq <- tabulate(match_words, length(unique_words))

threshold <- 90

# Create a vector that will contain the unique word indices
# of commonly occurring words
common_index <- numeric()

# If a word occurs more than [threshold] times its index is
# added to common_index
for (i in 1:length(freq)) {
    if (freq[i] >= threshold) {
        common_index <- append(common_index, i)
    }
}

# Vector of words that occur more than 90 times
b <- unique_words[common_index]

# Capitalize b
b_cap <- vector(mode = "character", length = length(b))
for (i in 1:length(b))
    b_cap[i] <- paste(toupper(substring(b[i], 1,1)), substring(b[i], 2), sep="", collapse=" ")

# Match each word in the lowercase bible if it is in b
match_b <- match(low_processed_bible, b)
# Match each word in the NOT lowercase bible if it is in b capitalized
match_b_cap <- match(processed_bible, b_cap)

# Bible frequency of each word in b
b_freq <- tabulate(match_b, length(b))
# Bible frequency of each b capitalized word
b_cap_freq <- tabulate(match_b_cap, length(b_cap))

# The list of b with regards to capitalization frequency in the bible
b_case_sensitive <- ifelse(b_cap_freq / b_freq > 0.5, b_cap, b)

# Q.7
# Match bible position of each word in b
corr <- match(low_processed_bible, b)

# Corr without last value since the last word has no next word
corr_nolast <- corr[-length(corr)]
corr_shift <- corr[-corr[1]]

# Matrix of corr_nolast and corr_shift
pair <- cbind(corr_nolast, corr_shift)

# Matrix containing _only_ common word pairs
common_pair <- pair[rowSums(is.na(pair)) == 0, ]

# Initialize Matrix A
A <- matrix(0, length(b), length(b))

# Increment A[i,j] with 1 for every ith common word
# followed by jth common word
for (n in 1:nrow(common_pair)) {
    old_val <- A[common_pair[n, 1], common_pair[n, 2]]
    A[common_pair[n, 1], common_pair[n, 2]] <- old_val + 1
}

# Vector containing row sums of A
row_sum <- rowSums(A)

# Initialize vector A_prob
A_prob <- matrix(0, nrow(A), ncol(A))

# Create A_prob from A
A_prob <- A / row_sum

# Q.8
first_word <- sample(1:length(b), 1)
sentence_index <- rep(NA, 50)
sentence_index[1] <- first_word
for (i in 2:50) {
   sentence_index[i] <- sample(1:ncol(A), size = 1, prob = A_prob[sentence_index[i-1], ])
}

cat(b_case_sensitive[sentence_index])