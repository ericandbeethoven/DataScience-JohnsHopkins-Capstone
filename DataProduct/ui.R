############################################################
# Capstone Project -Task 6 Data Product
# Data Scientist - Eric Bruce
# Date Apr 2016
# R 2.5
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
            , value = "Enter a sample phrase of one or more words")
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
    , a("at Github", href = "https://github.com/ericandbeethoven/DataScience-JohnsHopkins-Capstone/tree/master/DataProduct")
    , br()
    , "The pitch presentation is " 
    , a("at RPubs", href = "http://rpubs.com/ebrucecfa/DSC_DataProductPitch")
  ),
  
  mainPanel(
    tabsetPanel(
      
      tabPanel("Top Predictions"
               , br()
               , plotOutput('prediction_plot')
      )
      
      , tabPanel("User Docs"
                 ,  h3("How to Use This Data App")
                 , ("Just start typing. Experiment with Desired number of Predictions")
                 , br()
                 , ("And yes, the application appears bullet-proof. It will predict anything you type.")
      )
      
      , tabPanel("Acknowledgement"
                 ,  h3("Sweetpea - My Patient and Beloved Basset Hound")
                 , ("You were so patient waiting on me to finish that one last line of code before our walkies more times than any Machine Learning Algo can predict. You deserve lots of treats and love. Thank you.")
                 , h4("")
                 , img(src="DataScientist_Sweetpea.jpg", height = 200, width = 200)
                 ,  h3("My Fellow Data Science Learners")
                 , ("It was my honor to learn with such bright, motivated and resourceful peers. Thank you for all your valuable assistance. You deserve lots of treats too.")
                 
      )
      )
    
    
  )
   
)

)



