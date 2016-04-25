############################################################
# Capstone Project -Task 6 Data Product
# Data Scientist - Eric Bruce
# Date Apr 2016
# R 1.1
#
# UI.R
#
#############################################################
library(shiny)

# This is a UI page with three panels
threepage <- function(headerPanel,left,middle,footer) {
  bootstrapPage(div(class = "container-fluid", div(class = "row-fluid", 
  headerPanel), div(class = "row-fluid", left,  middle, footer)))}


shinyUI(fluidPage(theme = "bootstrap.css",
  # Set the page title
  titlePanel("Data Science Capstone: Swiftkey NLP Project")
  , br()
 
  , textInput("phrase"
            , label = ("") # = h5(   "1) Enter a phrase ")
            , value = "Enter a sample phrase of two or more words")
  , tags$head(tags$style(type="text/css", "#phrase {margin-left: 13px; width: 464px}"))
  
  # Set the page footer
 , footerPanel = wellPanel(
    tags$style(type = 'text/css', 
               "footer{position: absolute; bottom:5%}")
    , id = "footerPanel"
    , HTML('<footer>
          <img src="http://amor.cms.hu-berlin.de/~huangrui/images/softwares/Rlogo.png", height="120", width="120"</img>                 
          a("Eric Bruce", href = "https://www.linkedin.com/in/ericbrucecfa") 
          </footer>')
    
  )
  
  
  , sidebarPanel(
    
     tags$head(
       tags$style(type='text/css', ".well { height: 464px; }")
     )
   
    , sliderInput("num_results", 
                  h5("Desired no. of next word predictions"), 
                  min=3, max=10, value=5)
    , br()
    , radioButtons("model_selection", 
                   h5("Model selection"),
                   choices = list("Kneser Ney" = 1, "Stupid Back-off" = 2),
                   selected = 1)
    , br()
    
    , br()
    , br()
    , br()
    , br()
    
    , br()
   
    , "This app is created by " 
    , a("Eric Bruce", href = "https://www.linkedin.com/in/ericbrucecfa")
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
                 , img(src="DataScientist_Sweetpea.jpg", height = 500, width = 500)
      ))
    
    
  )
   
)

)



