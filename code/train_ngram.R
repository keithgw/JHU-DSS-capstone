#uniques <- dtm$dimnames[2][[1]][dtm$v == 1]
# Process Training Corpus

# Load Libraries
library(tm)      # for text mining
library(ggplot2) # for plotting
library(dplyr)   # for data manipulation
library(stringr) # for reg exp, string replacement
library(stringi)
library(RWeka)   # for NLP, n-gram tokenizer
library(slam)    # for calcualtions on simple triplet matrices (outputs of tm)
library(data.table) # fast search

# Load training corpus
corp <- Corpus(DirSource(file.path("~", "Data Science Coursera", 
                                    "capstone-JHU-swiftkey", "JHU-DSS-capstone", 
                                    "data", "sample", "train")))

# Tokenize Training Corpus
################################################################################

# custom function to replace characters with spaces
to_space <- content_transformer(function(x, pattern) 
    str_replace_all(x, pattern, " "))

# replace /, @, |, -,  with a space
corp <- tm_map(corp, to_space, "/|@|\\||-")

# custom function to remove hashtags
rmv_hashtags <- content_transformer(function(x) 
    str_replace_all(x, "#.* |#.*$",""))
corp <- tm_map(corp, rmv_hashtags)

# make all words lowercase
corp <- tm_map(corp, content_transformer(tolower))

# remove numbers
corp <- tm_map(corp, removeNumbers)

# remove punctuation
corp <- tm_map(corp, removePunctuation)

# remove anything left that isn't letters
corp <- tm_map(corp, to_space, "[^A-Za-z]")

# strip whitespace
corp <- tm_map(corp, stripWhitespace)

# convert to plain text
corp <- tm_map(corp, PlainTextDocument)

# View: scorp[1][[1]][[1]][line#]

# Build Document Term Matrix for token frequencies
################################################################################
dtm <- DocumentTermMatrix(corp, control=list(wordLengths=c(0,Inf)))

# Replace tokens that appear only once with <unk> token for smoothing
################################################################################
# find all terms that appear only once
uniques <- dtm$dimnames[2][[1]][dtm$v == 1]

# convert unique terms into regex patterns
patterns <- sapply(uniques, 
                   function(x){str_c(' ', x, ' |', '^', x, ' |', ' ', x, '$')})

# transfrom each unique term into <unk> token
to_unk <- content_transformer(function(x, pattern) 
    str_replace(x, pattern, " <unk> "))

for (term in patterns){
    corp <- tm_map(corp, to_unk, term)
}

# strip white space
corp <- tm_map(corp, stripWhitespace)

smoothed_dtm <- DocumentTermMatrix(corp, control=list(wordLengths=c(0,Inf)))
saveRDS(smoothed_dtm, "smoothed_dtm.rds")
saveRDS(corp, 'corpus_unk.rds')
# Build bigrams and trigrams
################################################################################

# custom function for building n-grams
count_ngrams <- function(n) {
    options(mc.cores = 2)
    ctrl <- Weka_control(min = n, max = n)
    ngram_tokenizer <- function(x) NGramTokenizer(x, control = ctrl)
    dtm_ngram <- DocumentTermMatrix(corp, 
                                    control = list(tokenize = ngram_tokenizer))
    return(dtm_ngram)
}

# build bigrams
bigrams <- count_ngrams(2)
# saveRDS(bigrams, "smoothed_bigrams.rds")

# build trigrams
trigrams <- count_ngrams(3)
# saveRDS(trigrams, "smoothed_trigrams.rds")

# function to create frequency-sorted vector of ngrams
freq_sorted <- function(doc_term_mtrx) {
    return(sort(col_sums(doc_term_mtrx), decreasing=TRUE))
}

bisorted <- freq_sorted(bigrams)
trisorted <- freq_sorted(trigrams)

# convert frequency-sorted ngram vectors into data.tables
bidat <- data.table(ngram = names(bisorted), frequency = bisorted)
tridat <- data.table(ngram = names(trisorted), frequency = trisorted)
saveRDS(bidat, "bidat_unk.rds")
saveRDS(tridat, "tridat_unk.rds")

# subset only ngrams that do NOT predict <unk>
bidat <- bidat %>% 
    mutate(last = word(ngram, -1)) %>%
    filter(last != "<unk>") %>% 
    mutate(first = word(ngram, 1))

tridat <- tridat %>%
    mutate(last = word(ngram, -1)) %>%
    filter(last != "<unk>") %>%
    mutate(first = str_c(word(ngram, 1), " ", word(ngram, 2)))

# keep only the most frequent ngram given the same i-1 word(s)

## create a vector of unique first words
unique_bifirsts <- unique(bidat$first)
unique_trifirsts <- unique(tridat$first)

## initialize empty list for max frequent bigrams for each unique 1st word
max_bigrams <- vector("list", length(unique_bifirsts))
max_trigrams <- vector("list", length(unique_trifirsts))

## loop over each unique first word
max_bigrams <- lapply(seq(1, length(unique_bifirsts)), 
                      function(i) max_bigrams[[i]] <- filter(bidat, first == unique_bifirsts[i]) %>% 
                        filter(frequency == max(frequency)) %>%
                        select(ngram, first, last))

# max_trigrams <- lapply(seq(1, length(unique_trifirsts)),
#                        function(i) max_trigrams[[i]] <- filter(tridat, first == unique_trifirsts[i]) %>%
#                            filter(frequency == max(frequency)) %>%
#                            select(ngram, first, last))

prediction_bigrams <- rbindlist(max_bigrams)
# prediction_trigrams <- rbindlist(max_trigrams, use.names=fill, fill=FALSE)
saveRDS(prediction_bigrams, "prediction_bigrams.rda")

# change later
prediction_trigrams <- tridat
saveRDS(prediction_trigrams, "prediction_trigrams.rda")

