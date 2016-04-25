############################################################
# Capstone Project -Task 6 Data Product
# Data Scientist - Eric Bruce
# Date Apr 2016
# R 1.1
#
# Server.R
#
#############################################################
library(shiny)
source('prediction_models.R')
profanewords <- c('ass', 'bitch', 'cunt', 'damn', 'fuck')

shinyServer(function(input, output) {
  input_tkn <- reactive({
      input_tokenize(input$phrase)
  })

  pred_model <- reactive({
    if(input$model_selection == 1){
      # Modified Kneser Ney
      predict.kn(input$phrase,
                  profanewords, 
                  ng1.df, ng2.df, ng3.df, 
                  maxResults = input$num_results)
      
    }else{
      # Stupid Backoff
      predict.sbo(input$phrase,
                  profanewords, 
                  ng1.df, ng2.df, ng3.df, 
                  maxResults = input$num_results)
    }
  })  
  
  model_sel_text <- reactive({
    if(input$model_selection == 1){
      "Next Word Predictions - Kneser Ney"
     }else{
      "Next Word Predictions - Stupid Backoff"
    }
  })  
 
  output$input_token <- renderText({
    input_tkn()
  })
  
  output$model_sel_top_pred <-  renderText({
    model_sel_text()
  })
  
  output$prediction_plot <- renderPlot({
    require(ggplot2)
    preds <- pred_model()
    t1 <- model_sel_text()
    p1 = ggplot(preds, aes(x=reorder(next_word, score), y=score)) + geom_bar(stat="identity") + coord_flip()
    # Add plot title & labels
    p1 = p1 + ggtitle(t1) + 
    xlab("") +
    ylab("")
    # White background and black grid lines
    p1 <- p1 + theme_bw() + theme(text = element_text(size=15),
                                  axis.text.x = element_text()) 
    print(p1)  
  })
  
 
})