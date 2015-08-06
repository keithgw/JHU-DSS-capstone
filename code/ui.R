# load shiny package
library(shiny)

# Initialize Shiny UI
shinyUI(
    pageWithSidebar(
        headerPanel("Text Prediction"),
        sidebarPanel(
            h3("Enter text and click 'Predict' for suggestion"),
            textInput('input_txt', 
                        'Enter Text'
            ),
            submitButton("Predict")
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Prediction",
                         h3('Suggested Word'),
                         verbatimTextOutput("prediction")
                         ),
                tabPanel("Documentation",
                         p(                           
                             'This app is designed to predict the next word in 
                             a sentence, given the previous words. One would find
                             such an application in a texting app on a mobile 
                             phone. As the user types his/her message, the app 
                             should suggest the next most likely word to speed 
                             the user texting process.'
                         ),
                         br(),
                         p(
                             'To get a prediction, the user types one or more 
                             words, and clicks "submit." The app then suggests
                             the next most likely word, given the previous 
                             word(s).'
                         )
                )
            )
        )
    )
)