############################################################
# Capstone Project -Task 6 Data Product
# Data Scientist - Eric Bruce
# Date Apr 2016
# R 2.0
#
# UI.R
#
#############################################################
library(shiny)

shinyUI(fluidPage(theme = "bootstrap.css",
  # Set the page title
  titlePanel("Data Science Capstone: Next Word Prediction Project")
  , br()
 
  , textInput("phrase"
            , label = ("") # = h5(   "1) Enter a phrase ")
            , value = "Enter a sample phrase of two or more words")
  , tags$head(tags$style(type="text/css", "#phrase {margin-left: 13px; width: 464px}"))
  
  , sidebarPanel(
     tags$head(
       tags$style(type='text/css', ".well { height: 464px; }")
     )
    , sliderInput("num_results", 
                  ("Desired no. of next word predictions"), 
                  min=3, max=10, value=5)
    , br()
    , br()
    , "This app is created " 
    , a("by Eric Bruce", href = "https://www.linkedin.com/in/ericbrucecfa")
    , br()
    , "The source code is " 
    , a("at Github", href = "https://github.com/ericandbeethoven/DataScience-JohnsHopkins-Capstone")
    , br()
    , "The pitch presentation is " 
    , a("at RPubs", href = "https://github.com/ericandbeethoven/DataScience-JohnsHopkins-Capstone")
  ),
  
  mainPanel(
    tabsetPanel(
      
      tabPanel("Top Predictions"
               , br()
               , plotOutput('prediction_plot')
      )
      
      , tabPanel("User Docs"
                 ,  h3("Supporting Documentation")
                 , ("Instructions the user will need to get started using this application.")
      )
      
      , tabPanel("Acknowledgement"
                 ,  h3("Supporting Documentation")
                 , ("Instructions the user will need to get started using this application.")
                 , h4("")
                 , h4("")
                 , img(src="DataScientist_Sweetpea.jpg", height = 300, width = 300)
      )
      )
    
    
  )
   
)

)



