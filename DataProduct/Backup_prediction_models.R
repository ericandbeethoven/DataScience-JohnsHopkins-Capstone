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
# Libraries
############################################################
library(stringr); library(stringi); library(R.utils)
library(tm, warn.conflicts = FALSE, quietly=TRUE)
library(quanteda, warn.conflicts = FALSE, quietly=TRUE)
library(ggplot2, warn.conflicts = FALSE, quietly=TRUE)
library(stats)
library(data.table)

load('./data/DataApp.RData')

############################################################
# Test Input Phrase
############################################################
input <- "dirtie glass    " # input_bigram = 'glass_of'
maxResults = 5


input.df <- process.input(input, terms2block )


# mkn <- predict.mkn(input,
#             terms2block, 
#             ng1.df, ng2.df, ng3.df, 
#             maxResults)

############################################################
# N-gram creation functions
############################################################
makeSentences <- function(input) {
  output <- tokenize(input, what = "sentence", removeNumbers = TRUE,
                     removePunct = TRUE, removeSeparators = TRUE,
                     removeTwitter = TRUE, removeHyphens = TRUE)
  unlist(lapply(output, function(a) paste('zSz', toLower(a), 'zEz')))
}

makeTokens <- function(input, n = 1L) {
  tokenize(input, what = "word", removeNumbers = TRUE,
           removePunct = TRUE, removeSeparators = TRUE,
           removeTwitter = TRUE, removeHyphens = TRUE,
           ngrams = n, simplify = TRUE)
}


############################################################
# Process Input
############################################################
process.input <-function(input,terms2block){
  x <- min(6,word_count(input))
  # replace ordinals, money, numbers, month and profanity
  input <- gsub(terms2block, "xprofanityx", input)
  input <- gsub('\\d+(st|nd|rd|th)', "xordinalx", input)
  input <- gsub('([M|m]arch|[M|m]ar|[M|m]ay) xordinalx', "xmonthx", input)
  input <- gsub("\\$\\d+\\.\\d+", "xmoneyx", input)
  input <- gsub("\\d+","xnumberx", input)
  input <- gsub('([J|j]an|[F|f]eb|[A|a]pr|[J|j]un|[J|j]ul|[A|a]ug|[S|s]ep|[O|o]ct|[N|n]ov|[D|d]ec|[J|j]anuary|[F|f]ebruary|[A|a]pril|[J|j]une|[J|j]uly|[A|a]ugust|[S|s]eptember|[O|o]ctober|[N|n]ovember|[D|d]ecember)', "xmonthx", input)
  
  
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
    while(is.na(vocabulary_dict[unlist(Trim(tolower(input)))]))
    {x = x - 1; if (x == 0) break}
  } 
  
  if(x > 1){
    while(is.na(vocabulary_dict[unlist(strsplit(Trim(tolower(input)),"_"))[w]]))
    {w = w + 1; x = x - 1; if (x == 0) break}
  }
  
  # Sanity Check for Valid Phrase Entry
  if(x < 1){ 
    input <-  dictionary.dt[1:5,] 
    return(input)
  }

    input <- word(input, w, -1, sep = "_")
  
  if(x == 6){ 
    input.df <- data.frame(
      W1 = vocabulary_dict[unlist(strsplit(input,"_"))[x-5]],
      W2 = vocabulary_dict[unlist(strsplit(input,"_"))[x-4]],
      W3 = vocabulary_dict[unlist(strsplit(input,"_"))[x-3]],
      W4 = vocabulary_dict[unlist(strsplit(input,"_"))[x-2]],
      W5 = vocabulary_dict[unlist(strsplit(input,"_"))[x-1]],
      W6 = vocabulary_dict[unlist(strsplit(input,"_"))[x]]
    )
  }
  
  if(x == 5){ 
    input.df <- data.frame(
      W1 = vocabulary_dict[unlist(strsplit(input,"_"))[x-4]],
      W2 = vocabulary_dict[unlist(strsplit(input,"_"))[x-3]],
      W3 = vocabulary_dict[unlist(strsplit(input,"_"))[x-2]],
      W4 = vocabulary_dict[unlist(strsplit(input,"_"))[x-1]],
      W5 = vocabulary_dict[unlist(strsplit(input,"_"))[x]]
    )
  }
  
  if(x == 4){ 
    input.df <- data.frame(
      W1 = vocabulary_dict[unlist(strsplit(input,"_"))[x-3]],
      W2 = vocabulary_dict[unlist(strsplit(input,"_"))[x-2]],
      W3 = vocabulary_dict[unlist(strsplit(input,"_"))[x-1]],
      W4 = vocabulary_dict[unlist(strsplit(input,"_"))[x]]
    )
  }
  
  if(x == 3){ 
    input.df <- data.frame(
      W1 = vocabulary_dict[unlist(strsplit(input,"_"))[x-2]],
      W2 = vocabulary_dict[unlist(strsplit(input,"_"))[x-1]],
      W3 = vocabulary_dict[unlist(strsplit(input,"_"))[x]]
    )
  }
  
  if(x == 2){ 
    input.df <- data.frame(
      W1 = vocabulary_dict[unlist(strsplit(input,"_"))[x-1]],
      W2 = vocabulary_dict[unlist(strsplit(input,"_"))[x]]
    )
  }
  
  if(x == 1){
    input.df <- data.frame(
      W1 = vocabulary_dict[unlist(Trim(tolower(input)))]
    )
  }
  
  input.df <- input.df[colSums(!is.na(input.df)) > 0]
  
  # keep last 3 columns & rename columns for housekeeping
  
  if (ncol(input.df) >= 3){
    s <- ncol(input.df)
    input.df <- input.df[1,(s-2):s] # changed 3 to 4, 3 to 2
    colnames(input.df) <- c("W1", "W2", "W3")
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
# Best Model
############################################################
predict.bm <-function(input,ng1.df,ng2.df,ng3.df, maxResults){

input.df <- process.input(input, terms2block )
# Nothing to predict because user had no input in dictionary
# Return Top 5 Unigrams  
if (nrow(input) == 5){
  return((input))
}

# Else Predict  
  
  
  
  
    # get D the discounting values D1, D2, and D3+ are defined as [CG98]
  # by the total number of n-grams occurring exactly once (n1) and twice (n2)
  # This approach is based on studies by [CG98] showing that
  # sequences with a count of 1 or 2 require a different discount value than sequences
  # with a higher count.
  
  n1 <- ng1.freqfreq[1,2]
  n2 <- ng1.freqfreq[2,2]
  n3 <- ng1.freqfreq[3,2]
  n4 <- ng1.freqfreq[4,2]
  D <- n1/(n1+2.*n2)
  D1 <- 1. - 2.*gamma*(n2/n1)
  D2 <- 2. - 3.*gamma*(n3/n2) 
  D3plus <- 3. - 4. * gamma * (n4/n3)
  
  # P_continuation_w of input BiGram in TriGram
  ng3.P_continuation_w <- subset(ng3.df, ng3.df$exceptlastword == input)  
  count_w1w2 <- sum(ng3.P_continuation_w$Frequency)  
  n_w1w2 <- nrow(ng3.P_continuation_w)
  n_total <- sum(ng2.df$Frequency)
  P_continuation_word <- count_w1w2 / n_total 
  
  
  
  
  
  p3 <- D3plus *  n_w1w2 / count_w1w2 
  
  # P_continuation_w of input_lastword UniGram in BiGram
  ng2.P_continuation_w <- subset(ng2.df, ng2.df$exceptlastword == input_lastword)  
  count_w2 <- sum(ng2.P_continuation_w$Frequency)  
  n_w2 <- nrow(ng2.P_continuation_w)
  nw <- nrow(ng2.df)
  p2 <- D3plus * n_w2/(count_w2/nw)
  p1 <- D2 * n_w2/(count_w2/nw)
  
  Pkn2 <- n_w2 / nw
  Pkn3 <- 
    
    # Make predictions  
    # count_w1w2 == 0, then input was not found in trigram
    if(count_w1w2 == 0){
      # kick off to unigram if no bigram
      if(n_w2 == 0) {
        return(head(ng1.df[order(ng1.df$Frequency,decreasing = TRUE),1],maxResults))
      }
      
      # kick off to 2-gram model
      pmkn <- 
        
        
        cp <- unique(ng2.P_continuation_w$Words)
      pkn <- rep(NA,length(cp))
      
      # https://www.quora.com/What-is-the-intuition-behind-the-Kneser-Ney-smoothing        
      #     Probability(w|v) = (Count(v,w) - d)/Count(v)) + lambda * C(v)
      # lambda = d/Count(bigrams that appeared 1 time)
      # C(v) = continuous probability of all bigrams that have v as a suffix
      
      for(i in 1:length(cp)){
        # get nw3 cw3 for smooth
        nw3 <- sum(grepl(cp[1], ng2.df$exceptlastword))
        cw3 <- ng2.P_continuation_w[ng2.P_continuation_w$Words == cp[1],2]
        pkn[i] <- max((cw3-D2),0)/cw2 + p1*nw3
      }
    }
  
  cp <- unique(subbi$Words)
  pkn <- rep(NA,length(cp))
  for(i in 1:length(cp)){
    # get nw3 nw2w3 and cw1w2w3 for smooth
    nw3 <- sum(grepl(cp[i],ng2.df$exceptlastword))
    nw2w3 <- sum(grepl(paste0(input2,' ',cp[i],'$'),ng3.df$exceptlastword))
    cw1w2w3 <- subtri[subtri$name == cp[i],2]
    pkn[i] <- max((cw1w2w3-D3),0)/cw1w2 + p3*(max((nw2w3-D3),0)/W2+ p2*nw3)
  }
  
  predictWord <- data.frame(next_word=cp, probability=pkn,stringsAsFactors = F)
  predictWord$next_word <- word(predictWord$next_word, -1, sep = fixed('_'))
  predictWord <- aggregate(probability ~ next_word, predictWord, max) 
  return(na.omit(predictWord[order(predictWord$probability,decreasing = T),][1:maxResults,]))
}




############################################################
# Modified Kneser Ney
############################################################
predict.mkn <-function(input,terms2block,ng1.df,ng2.df,ng3.df, maxResults){
  input <- "qweejibo "
  # Process input
  input <- process.input(input)
  # Input did not consist of any words in Unigram, guess Top 5 Unigrams
  if(nrow(input) > 1){
    sum_score <- sum(input$Freq)
    predictWord <- data.frame(next_word=input$W1,score=input$Freq/sum_score,stringsAsFactors = F)
    return(predictWord)
  }
}

############################################################
# Katz Backoff
############################################################
predict.kbo <-function(input, ng1.dt, ng2.dt, ng3.dt, ng4.dt, ng5.dt, maxResults) {
  # Process input
 input <- process.input(input)
 # Input did not consist of any words in Unigram, guess Top 5 Unigrams
 if(nrow(input) > 1){
   sum_score <- sum(input$Freq)
   predictWord <- data.frame(next_word=input$W1,score=input$Freq/sum_score,stringsAsFactors = F)
   return(predictWord)
 }
 
 # Determine which ngram to use first
 if(ncol(input) == 4){
   
 }
 
 
 
  
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
# Katz Backoff - Start with 5-gram
############################################################
predict.kbo5 <-function(input, ng1.dt, ng2.dt, ng3.dt, ng4.dt, ng5.dt, maxResults) {
  predictWord =   
  
  sum_score <- sum(input$Freq)
    predictWord <- data.frame(next_word=input$W1,score=input$Freq/sum_score,stringsAsFactors = F)
    return(predictWord)
  }
  
  # Determine which ngram to use first
  if(ncol(input) == 4){
    
  }
  
  
  
  
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


