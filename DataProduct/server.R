############################################################
# Capstone Project -Task 6 Data Product
# Data Scientist - Eric Bruce
# Date Apr 2016
# R 2.0
#
# Server.R
#
#############################################################
library(shiny)
source('prediction_models.R')


shinyServer(function(input, output) {

  pred_model <- reactive({
      predict.bm(input$phrase, maxResults = input$num_results)
  })  
  
  output$prediction_plot <- renderPlot({
    require(ggplot2)
    preds <- pred_model()
    p1 = ggplot(preds, aes(x=reorder(next_word, probability), y=probability)) + geom_bar(stat="identity") + coord_flip()
    # Add plot title & labels
    p1 = p1 + xlab("") +  ylab("")
    # White background and black grid lines
    p1 <- p1 + theme_bw() + theme(text = element_text(size=15),
                                  axis.text.x = element_text()) 
    print(p1)  
  })
  
 
})