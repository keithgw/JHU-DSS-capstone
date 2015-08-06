### To Do:
# Back-off model
# Smoothing Model <unk>
# Tetragrams


# load libraries
library(stringr)
library(stringi)
library(dplyr)

# load n-gram data.tables
# unigrams <- readRDS('unidat.rds')
bigrams <- readRDS('bidat.rds')
trigrams <- readRDS('tridat.rds')

extracti <- function(ngram, n) {
    pattern <- str_c("^", stri_trans_tolower(ngram))
    
    if (n == 2) {dat <- bidat}
    else if (n == 3) {dat <- tridat}
    else {dat <- tetradat}
    
    return(str_split(dplyr::filter(dat, 
                                   str_detect(dat$ngram, pattern)
    )$ngram[1], 
    " ")[[1]][n]
    )   
}

# Shiny Server
shinyServer(
    function(input, output) {
        
        # create prediction object with interval from inputs
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