############################################################
# Capstone Project -Task 6 Data Product
# Data Scientist - Eric Bruce
# Date Apr 2016
# R 1.1
#
# Algorithims for predicting the next phrase
#
#############################################################

############################################################
# Libraries
############################################################
library(stringr); library(stringi); library(R.utils)
library(qdap, warn.conflicts = FALSE, quietly=TRUE)
library(tm, warn.conflicts = FALSE, quietly=TRUE)
library(quanteda, warn.conflicts = FALSE, quietly=TRUE)
library(ggplot2, warn.conflicts = FALSE, quietly=TRUE)
library(stats)

load('./data/DataApp.RData')


############################################################
# Test Input Phrase
############################################################
# input <- "A nice cold glass of" # input_bigram = 'glass_of'
# profanewords <- c('ass', 'bitch', 'cunt', 'damn', 'fuck')


############################################################
# Tokenize Final 2 Words of Input Phrase
############################################################
input_tokenize <-function(input) {
  # Auto-corect
  input_bigram <- tokenize(toLower(input), 
                         removePunct = TRUE, 
                         removeNumbers = TRUE,
                         removeTwitter = TRUE,
                         ngrams = 2)
  input_bigram <- as.data.frame(as.matrix(input_bigram), stringsAsFactors = FALSE)
  input_bigram <- tail(input_bigram, 1)[1,1]
  input_bigram <- na.omit(input_bigram)
  if(length(input_bigram) == 0){ input_bigram <- "Warning: Input a valid phrase of 2 or more non-numeric words" }
  return(input_bigram)
}

############################################################
# Peter Norvig's (Research Director at Google) Spell Checker
# Resource - http://www.sumsar.net/blog/2014/12/peter-norvigs-spell-checker-in-two-lines-of-r/
############################################################
autocorrect <- function(word) {
  # Calculate the edit distance between the word and all other words in sorted_words.
  edit_dist <- adist(word, sorted_words)
  # Calculate the minimum edit distance to find a word that exists in big.txt 
  # with a limit of two edits.
  min_edit_dist <- min(edit_dist, 2)
  # Generate a vector with all words with this minimum edit distance.
  # Since sorted_words is ordered from most common to least common, the resulting
  # vector will have the most common / probable match first.
  proposals_by_prob <- c(sorted_words[ edit_dist <= min(edit_dist, 2)])
  # In case proposals_by_prob would be empty we append the word to be corrected...
  proposals_by_prob <- c(proposals_by_prob, word)
  # ... and return the first / most probable word in the vector.
  proposals_by_prob[1]
}


############################################################
# Stupid Backoff
############################################################
predict.sbo <-function(input, profanewords, ng1.df, ng2.df, ng3.df, maxResults) {
  # Process input
  input <- tokenize(toLower(input), 
                    removePunct = TRUE, 
                    removeNumbers = TRUE,
                    removeTwitter = TRUE,
                    ngrams = 2)
  input <- as.data.frame(as.matrix(input), stringsAsFactors = FALSE)
  input <- tail(input, 1)[1,1]
  if(length(input) == 0){ 
    input <- "Invalid entry" 
    return(data.frame(next_word=input,score=1.,stringsAsFactors = F))}
  
  # Initialize variables for prediction
  seektri<-grepl(paste0("^",input,"$"),ng3.df$exceptlastword)
  subtri<-ng3.df[seektri,]
  input2 <- unlist(strsplit(input,"_"))[2]
  seekbi <- grepl(paste0("^",input2,"$"),ng2.df$exceptlastword)
  subbi <- ng2.df[seekbi,]
  ng1.df$s <- ng1.df$Frequency/nrow(ng1.df)*0.16
  useuni <- ng1.df[order(ng1.df$s,decreasing = T),]
  useunia <- useuni[1:maxResults,]
  
  # Calculate Next Word and Next Word Scores
  if (sum(seektri) == 0) {
    if(sum(seekbi)==0){
      # Use ng1 because ng2 and ng3 cannot provide sol'ns
      return(head(na.omit(ng1.df[order(ng1.df$Frequency,decreasing = T),1]),maxResults))
    }
    # Use ng2 because ng3 cannot provide soln's
    subbi$s <- 0.4*subbi$Frequency/sum(seekbi)
    names <- c(subbi$Word,useunia$ng1)
    score <- c(subbi$s,useunia$s)[1:length(names)]
    predictWord <- data.frame(next_word=names,score=score,stringsAsFactors = F)
    predictWord$next_word <- word(predictWord$next_word, -1, sep = fixed('_'))
    predictWord <- aggregate(score ~ next_word, predictWord, max) 
    return(na.omit(predictWord[order(predictWord$score,decreasing = T),][1:maxResults,]))
  } 
  # Use ng2 and ng3 - best soln's
  subbi$s <- 0.4*subbi$Frequency/sum(seekbi)
  subtri$s <- subtri$Frequency/sum(subtri$Frequency)
  names <- c(subtri$Words,subbi$Words,useunia$Words)
  score <- c(subtri$s,subbi$s,useunia$s)[1:length(names)]
  predictWord <- data.frame(next_word=names,score=score,stringsAsFactors = F)
  predictWord$next_word <- word(predictWord$next_word, -1, sep = fixed('_'))
  predictWord <- aggregate(score ~ next_word, predictWord, max) 
  return(na.omit(predictWord[order(predictWord$score,decreasing = T),][1:maxResults,]))
}

############################################################
# Stupid Backoff
############################################################
predict.kn <-function(input, profanewords, ng1.df, ng2.df, ng3.df, maxResults) {
  # Process input
  input <- tokenize(toLower(input), 
                    removePunct = TRUE, 
                    removeNumbers = TRUE,
                    removeTwitter = TRUE,
                    ngrams = 2)
  input <- as.data.frame(as.matrix(input), stringsAsFactors = FALSE)
  input <- tail(input, 1)[1,1]
  if(length(input) == 0){ 
    input <- "Invalid entry" 
    return(data.frame(next_word=input,score=1.,stringsAsFactors = F))}
  
  # Initialize variables for prediction
  seektri<-grepl(paste0("^",input,"$"),ng3.df$exceptlastword)
  subtri<-ng3.df[seektri,]
  input2 <- unlist(strsplit(input,"_"))[2]
  seekbi <- grepl(paste0("^",input2,"$"),ng2.df$exceptlastword)
  subbi <- ng2.df[seekbi,]
  ng1.df$s <- ng1.df$Frequency/nrow(ng1.df)*0.16
  useuni <- ng1.df[order(ng1.df$s,decreasing = T),]
  useunia <- useuni[1:maxResults,]
  
  # Calculate Next Word and Next Word Scores
  if (sum(seektri) == 0) {
    if(sum(seekbi)==0){
      # Use ng1 because ng2 and ng3 cannot provide sol'ns
      return(head(na.omit(ng1.df[order(ng1.df$Frequency,decreasing = T),1]),maxResults))
    }
    # Use ng2 because ng3 cannot provide soln's
    subbi$s <- 0.4*subbi$Frequency/sum(seekbi)
    names <- c(subbi$Word,useunia$ng1)
    score <- c(subbi$s,useunia$s)[1:length(names)]
    predictWord <- data.frame(next_word=names,score=score,stringsAsFactors = F)
    predictWord$next_word <- word(predictWord$next_word, -1, sep = fixed('_'))
    predictWord <- aggregate(score ~ next_word, predictWord, max) 
    return(na.omit(predictWord[order(predictWord$score,decreasing = T),][1:maxResults,]))
  } 
  # Use ng2 and ng3 - best soln's
  subbi$s <- 0.4*subbi$Frequency/sum(seekbi)
  subtri$s <- subtri$Frequency/sum(subtri$Frequency)
  names <- c(subtri$Words,subbi$Words,useunia$Words)
  score <- c(subtri$s,subbi$s,useunia$s)[1:length(names)]
  predictWord <- data.frame(next_word=names,score=score,stringsAsFactors = F)
  predictWord$next_word <- word(predictWord$next_word, -1, sep = fixed('_'))
  predictWord <- aggregate(score ~ next_word, predictWord, max) 
  return(na.omit(predictWord[order(predictWord$score,decreasing = T),][1:maxResults,]))
}



