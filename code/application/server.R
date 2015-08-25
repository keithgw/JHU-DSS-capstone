# load libraries
library(stringr)
library(stringi)
library(dplyr)

# load n-gram data.tables
bigrams <- readRDS('prediction_bigrams.rda')
trigrams <- readRDS('prediction_trigrams.rda')

extracti <- function(ngram, n) {
    stem <- stri_trans_tolower(ngram)
    
    # bigram model
    if (n == 2) {
        i <- match(stem, bigrams$first)  # index of most frequent match
        if (is.na(i)){
            i <- match('<unk>', bigrams$first)  # unseen token
        }
        return(bigrams$last[i])
    }
    # trigram model
    else{
        i <- match(stem, trigrams$first)
        if (is.na(i)){
            # try 'word1 <unk>'
            j <- match(str_replace(stem, ' *.$', ' <unk>'), trigrams$first)
            if (!is.na(j)){
                i <- j
            }
            # try '<unk> word2'
            else{
                j <- match(str_replace(stem, '^*. ', '<unk> '), trigrams$first)
                if (!is.na(j)){
                    i <- j
                }
                # try '<unk> <unk>'
                else{
#                     i <- match('<unk> <unk>', trigrams$first)
                    return('the')
                }
            }
        }
        return(trigrams$last[i])
    }
}

# Shiny Server
shinyServer(
    function(input, output) {
        
        # create prediction object from input text
        predicted_word <- reactive({
            # strip white space, remove punctuation, and tokenize input
            trimmed <- str_trim(input$input_txt) %>% 
                str_replace_all("[^A-Za-z ]", "")
            sentence <- str_split(trimmed, " ")
            
            # pick 1-, 2-, 3-gram model based on length of sentence
            len <- length(sentence[[1]])
            
            # default prediction
            if (len < 1) {
                return("the")
            }
            
            # 1 or 2 word input
            else if (len <= 2) {
                # concatenate tokens for input              
                return(extracti(ngram = trimmed, n = len + 1))
            }
            
            # more than 2 words
            # input should be only last 2 words
            else {
                token <- str_c(sentence[[1]][len - 1], 
                               sentence[[1]][len], 
                               sep = " ")
                return(extracti(ngram = token, n = 3))
            }
            })
                
        # return mpg prediction 1 for printing
        output$prediction <- renderText({
            predicted_word()
        })
        
    }
)