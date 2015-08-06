# load libraries
library(stringr)
library(stringi)
library(dplyr)

# load n-gram data.tables
unigrams <- readRDS('unidat.rds')
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
            sentence <- str_trim(input$input_txt) %>% 
                str_split(" ")
            len <- length(sentence[[1]])
            if (len < 1) {
                return("the")
            }
            else if (len <= 2) {
                return(extracti(ngram = input$input_txt, n = len + 1))
            }
            })
                
        # return mpg prediction 1 for printing
        output$prediction <- renderText({
            predicted_word()
        })
        
    }
)