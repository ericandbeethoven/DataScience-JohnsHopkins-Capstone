############################################################
# Capstone Project -Task 6 Data Product
# Data Scientist - Eric Bruce
# Date Apr 2016
# R 3.5
#
# Algorithims for predicting the next phrase
#
#############################################################

############################################################
# Best Model - Katz with Interpolation
############################################################
predict.bm <-function(input, maxResults){
# Process input & determine which ngram to use first
# Sanity Check for valid entry
  result = tryCatch({
    process.input(input, terms2block)
  }, warning = function(w) {
    ng1.dt
  }, error = function(e) {
    ng1.dt
  }, finally = {
    ng1.dt
  })  

if(nrow(result) > 1){
  PredictedWords <- data.frame(next_word = ng1.dt$W1, probability = ng1.dt$score, stringsAsFactors = F) 
  return(na.omit(PredictedWords)[1:maxResults,])
  
} else{
  input <- process.input(input, terms2block)  
}  
x = ncol(input)

# Make predictions  
# Start with 5gram
#=======================================================================
if(x==4){
  PredictedWords5 = ng5.search(input$W1, input$W2, input$W3, input$W4 )
  PredictedWords4 = ng4.search(input$W2, input$W3, input$W4, 0.5 )
  PredictedWords3 = ng3.search(input$W3, input$W4, 0.5^2 )
  PredictedWords2 = ng2.search(input$W4, .5^3 )
  PredictedWords = rbind(PredictedWords5
                         , PredictedWords4
                         , PredictedWords3
                         , PredictedWords2)
}
#=======================================================================  
  
# Start with 4gram
#=======================================================================
if(x==3){
  PredictedWords4 = ng4.search(input$W1, input$W2, input$W3, 1 )
  PredictedWords3 = ng3.search(input$W2, input$W3, 0.4 )
  PredictedWords2 = ng2.search(input$W3, .4^2 )
  PredictedWords = rbind(PredictedWords4
                         , PredictedWords3
                         , PredictedWords2)
}
#=======================================================================  

# Start with 3gram
#=======================================================================
if(x==2){
  PredictedWords3 = ng3.search(input$W1, input$W2, 1 )
  PredictedWords2 = ng2.search(input$W2, .4 )
  PredictedWords = rbind(PredictedWords3
                         , PredictedWords2)
}
#=======================================================================  

# Start with 2gram
#=======================================================================
if(x==1){
  PredictedWords = ng2.search(input$W1, 1 )
}
#=======================================================================  

# If no Predictions, use ng1
#=======================================================================  
if(nrow(PredictedWords) > 0){
  # Convert next_word from integer to string
  PredictedWords$next_word = names(ng1_dict[PredictedWords$next_word])
  PredictedWords <- PredictedWords[order(- PredictedWords$probability,  PredictedWords$next_word),]
}else{
  PredictedWords <- data.frame(next_word = ng1.dt$W1, probability = ng1.dt$score, stringsAsFactors = F)  
  } 
#=======================================================================  
return(na.omit(PredictedWords)[1:min(nrow(PredictedWords),maxResults),])

}



