############################################################
# Capstone Project -Task 6 Data Product
# Data Scientist - Eric Bruce
# Date Apr 2016
# R 2.0
#
# Global.R
#
#############################################################

############################################################
# Libraries
############################################################
library(stringr); library(stringi); library(R.utils)
library(quanteda, warn.conflicts = FALSE, quietly=TRUE)
library(ggplot2, warn.conflicts = FALSE, quietly=TRUE)
library(dplyr, warn.conflicts = FALSE, quietly=TRUE)
library(stats)
library(data.table)

############################################################
# Load workspace
############################################################
load('./data/DataApp.RData')

############################################################
# Set ngram keys for faster searches
############################################################
setorder(ng1.dt, -score)
setorder(ng2.dt, W1, -score)
setorder(ng3.dt, W1, W2, -score)
setorder(ng4.dt, W1, W2, W3, -score)
setorder(ng5.dt, W1, W2, W3, W4, -score)

############################################################
# Process Input function
############################################################
process.input <-function(input,terms2block){
  require(qdap)
  x <- min(6,word_count(input))
  # filter profanity
  input <- gsub(terms2block, "xPROFx", input)
  input <- tokenize(toLower(input), 
                    removePunct = TRUE, 
                    removeNumbers = TRUE,
                    removeTwitter = TRUE,
                    ngrams = x)
  input <- as.data.frame(as.matrix(input), stringsAsFactors = FALSE)
  input <- tail(input, 1)[1,1]
  # Sanity check to make sure W1 is not NA (eg Exists in the unigram)
  # Function will error out if W1 is NA
  w <- 1
  
  if(x == 1){
    while(is.na(ng1_dict[unlist(Trim(tolower(input)))]))
    {x = x - 1; if (x == 0) break}
  } 
  
  if(x > 1){
    while(is.na(ng1_dict[unlist(strsplit(Trim(tolower(input)),"_"))[w]]))
    {w = w + 1; x = x - 1; if (x == 0) break}
  }
  
  # Sanity Check for Valid Phrase Entry
  if(x < 1){ 
    input <-  ng1_dict[1:5,] 
    return(input)
  }
  
  input <- word(input, w, -1, sep = "_")
  
  if(x == 6){ 
    input.df <- data.frame(
      W1 = ng1_dict[unlist(strsplit(input,"_"))[x-5]],
      W2 = ng1_dict[unlist(strsplit(input,"_"))[x-4]],
      W3 = ng1_dict[unlist(strsplit(input,"_"))[x-3]],
      W4 = ng1_dict[unlist(strsplit(input,"_"))[x-2]],
      W5 = ng1_dict[unlist(strsplit(input,"_"))[x-1]],
      W6 = ng1_dict[unlist(strsplit(input,"_"))[x]]
    )
  }
  
  if(x == 5){ 
    input.df <- data.frame(
      W1 = ng1_dict[unlist(strsplit(input,"_"))[x-4]],
      W2 = ng1_dict[unlist(strsplit(input,"_"))[x-3]],
      W3 = ng1_dict[unlist(strsplit(input,"_"))[x-2]],
      W4 = ng1_dict[unlist(strsplit(input,"_"))[x-1]],
      W5 = ng1_dict[unlist(strsplit(input,"_"))[x]]
    )
  }
  
  if(x == 4){ 
    input.df <- data.frame(
      W1 = ng1_dict[unlist(strsplit(input,"_"))[x-3]],
      W2 = ng1_dict[unlist(strsplit(input,"_"))[x-2]],
      W3 = ng1_dict[unlist(strsplit(input,"_"))[x-1]],
      W4 = ng1_dict[unlist(strsplit(input,"_"))[x]]
    )
  }
  
  if(x == 3){ 
    input.df <- data.frame(
      W1 = ng1_dict[unlist(strsplit(input,"_"))[x-2]],
      W2 = ng1_dict[unlist(strsplit(input,"_"))[x-1]],
      W3 = ng1_dict[unlist(strsplit(input,"_"))[x]]
    )
  }
  
  if(x == 2){ 
    input.df <- data.frame(
      W1 = ng1_dict[unlist(strsplit(input,"_"))[x-1]],
      W2 = ng1_dict[unlist(strsplit(input,"_"))[x]]
    )
  }
  
  if(x == 1){
    input.df <- data.frame(
      W1 = ng1_dict[unlist(Trim(tolower(input)))]
    )
  }
  
  input.df <- input.df[colSums(!is.na(input.df)) > 0]
  
  # keep last 3 columns & rename columns for housekeeping
  
  if (ncol(input.df) >= 4){
    s <- ncol(input.df)
    input.df <- input.df[1,(s-3):s] 
    colnames(input.df) <- c("W1", "W2", "W3", "W4")
  }
  
  if (ncol(input.df) == 3){
    colnames(input.df) <- c("W1", "W2", "W3")
  }
  
  if (ncol(input.df) == 2){
    colnames(input.df) <- c("W1", "W2")
  }
  
  if (ncol(input.df) == 1){
    colnames(input.df) <- c("W1")
  }
  
  return(input.df)
}

############################################################
# ngram5 Search function
############################################################
ng5.search <- function(w1,w2,w3,w4){
  # Initiate keys
  setorder(ng5.dt, W1, W2, W3, W4, -score)
  # Check for Prediction Hits
  PredictWords = na.omit(data.frame(ng5.dt[.( w1
                                              , w2
                                              , w3
                                              , w4)]))
  
  PredictWords = data.frame(next_word=PredictWords$W5, probability=PredictWords$score,stringsAsFactors = F) 
  return(PredictWords)
}


############################################################
# ngram4 Search function
############################################################
ng4.search <- function(w1,w2,w3, lambda){
  # Initiate keys
  setorder(ng4.dt, W1, W2, W3, -score)
  # Check for Prediction Hits
  PredictWords = ng4.dt[W1 == w1]
  PredictWords = PredictWords[W2 == w2]
  PredictWords = PredictWords[W3 == w3]
  PredictWords = na.omit(data.frame(PredictWords))
  PredictWords = data.frame(next_word=PredictWords$W4, probability=lambda*PredictWords$score,stringsAsFactors = F) 
  return(PredictWords)
}

############################################################
# ngram3 Search function
############################################################
ng3.search <- function(w1,w2, lambda){
  # Initiate keys
  setorder(ng3.dt, W1, W2, -score)
  # Check for Prediction Hits
  PredictWords = ng3.dt[W1 == w1]
  PredictWords = PredictWords[W2 == w2]
  PredictWords = na.omit(data.frame(PredictWords))
  PredictWords = data.frame(next_word=PredictWords$W3, probability=lambda*PredictWords$score,stringsAsFactors = F) 
  return(PredictWords)
}

############################################################
# ngram2 Search function
############################################################
ng2.search <- function(w1, lambda){
  # Initiate keys
  setorder(ng2.dt, W1, -score)
  # Check for Prediction Hits
  PredictWords = ng2.dt[W1 == w1]
  PredictWords = na.omit(data.frame(PredictWords))
  PredictWords = data.frame(next_word=PredictWords$W2, probability=lambda*PredictWords$score,stringsAsFactors = F) 
  return(PredictWords)
}