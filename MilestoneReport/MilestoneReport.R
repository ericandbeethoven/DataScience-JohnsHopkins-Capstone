############################################################
# Capstone Project - Milestone Report
# Data Scientist - Eric Bruce
# Date Mar 2016
# R 1.0
#############################################################

############################################################
# Libraries
############################################################
library(stringr); library(stringi); library(R.utils)
library(quanteda, warn.conflicts = FALSE, quietly=TRUE)
library(ggplot2, warn.conflicts = FALSE, quietly=TRUE)

############################################################
# Obtain Dataset
############################################################

# set wd & data/final dir - data source 
setwd("~/R/DataScience-JohnsHopkins-Capstone")
data.dir = ("~/R/DataScience-JohnsHopkins-Capstone/data/final")
# download & unzip data if needed
fileUrl <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
if (!file.exists("./data/Coursera-SwiftKey.zip")){
  download.file(fileUrl,destfile="./data/Coursera-SwiftKey.zip")
  unzip(zipfile="./data/Coursera-SwiftKey.zip",exdir="./data")
}

############################################################
# Load Data - Obtain File Info - File Content, Line Count & 
# Word Count
############################################################
data.dir <- ("~/R/DataScience-JohnsHopkins-Capstone/data/final/en_US")
file_list <- list.files(data.dir)

file_size_in_MB <- c(file.info(paste(data.dir,file_list[1], sep="/"))$size / 1024^2,
                     file.info(paste(data.dir,file_list[2], sep="/"))$size / 1024^2,
                     file.info(paste(data.dir,file_list[3], sep="/"))$size / 1024^2)

# Function to obtain line count from a file
FileLineCount <- function(x) {
  con <- file(x, "rb")
  lc <- as.numeric(countLines(con))
  close(con)
  return(lc)
}

file_line_count <- c(FileLineCount(paste(data.dir,file_list[1], sep="/")),
                     FileLineCount(paste(data.dir,file_list[2], sep="/")),
                     FileLineCount(paste(data.dir,file_list[3], sep="/")))

# Function to obtain word count from a file
FileWordCount <- function(x, N) {
  con <- file(x, "rb")
  lst <- readLines(con, n=N, skipNul=TRUE, warn=FALSE)
  lst <- gsub("[^[:alpha:][:space:]']", " ", lst)
  lst <- gsub("â ", "'", lst)
  lst <- gsub("ã", "'", lst)
  lst <- gsub("ð", "'", lst)
  lst <- Trim(clean(lst))
  wc <- sum(stri_count_words(lst))
  close(con)
  return(wc)
}

file_word_count <- c(FileWordCount(paste(data.dir,file_list[1], sep="/"), file_line_count[1] ),
                     FileWordCount(paste(data.dir,file_list[2], sep="/"), file_line_count[2] ),
                     FileWordCount(paste(data.dir,file_list[3], sep="/"), file_line_count[3])
                     )

FileInfo <- data.frame(file_list,file_size_in_MB,file_line_count, file_word_count)

############################################################
# Sample Dataset 
############################################################

samplepct = 0.05
# function to sample samplepct% of a file
FileSample <- function(x, N, s) {
  con <- file(x, "rb")
  lst <- readLines(con, n=N, skipNul=TRUE, warn=FALSE)
  close(con)
  smpl <- sample(lst, N*s)
  return(smpl)
}

set.seed(31416)
blogs.sample <-FileSample(paste(data.dir,file_list[1], sep="/"), blogs.linecount, samplepct)
news.sample <-FileSample(paste(data.dir,file_list[2], sep="/"), news.linecount, samplepct)
twitter.sample <-FileSample(paste(data.dir,file_list[3], sep="/"), twitter.linecount, samplepct)

############################################################
# Data Cleaning, nGram Tokenize & TermDocumentMatrix
############################################################

# First step is to combine blogs, news and twitter into one document
OneDoc <- (paste(blogs.sample, news.sample, twitter.sample))

# Remove URLS from OneDoc
OneDoc <- gsub('(f|ht)tp\\S+\\s*',"", OneDoc)

# Construct Profane Words List
fileUrl <- "http://www.cs.cmu.edu/~biglou/resources/bad-words.txt"
if (!file.exists("~/R/DataScience-JohnsHopkins-Capstone/data/other/bad-words.txt")){
  download.file(fileUrl,destfile="~/R/DataScience-JohnsHopkins-Capstone/data/other/bad-words.txt")
}
con <- file("~/R/DataScience-JohnsHopkins-Capstone/data/other/bad-words.txt", "r")
profanewords <- readLines(con, n=1385, skipNul=TRUE, warn=FALSE) ; close(con)
profanewords <- profanewords[profanewords != ""]

# Remove Profane Words from OneDoc
OneDoc <- removeWords(OneDoc, profanewords)

# nGram Tokenize
UniGram.tdm <- tokenize(toLower(OneDoc), 
                        removePunct = TRUE, 
                        removeNumbers = TRUE,
                        removeTwitter = TRUE,
                        ngrams = 1)

BiGram.tdm <- tokenize(toLower(OneDoc), 
                        removePunct = TRUE, 
                        removeNumbers = TRUE,
                        removeTwitter = TRUE,
                        ngrams = 2)

TriGram.tdm <- tokenize(toLower(OneDoc), 
                        removePunct = TRUE, 
                        removeNumbers = TRUE,
                        removeTwitter = TRUE,
                        ngrams = 3)

# creating the document-feature matrix
UniGram.dfm <- dfm(UniGram.tdm)
BiGram.dfm <- dfm(BiGram.tdm)
TriGram.dfm <- dfm(TriGram.tdm)

############################################################
# Exploratory Data Analysis
############################################################

# coercing the dfm into matrix.
# docfreq will get the document frequency of a feature
UniGram.df <- as.data.frame(as.matrix(docfreq(UniGram.dfm)))
BiGram.df <- as.data.frame(as.matrix(docfreq(BiGram.dfm)))
TriGram.df <- as.data.frame(as.matrix(docfreq(TriGram.dfm)))

# sorting the n-grams for plotting
UniGram.sorted <- sort(rowSums(UniGram.df), decreasing=TRUE)
BiGram.sorted <- sort(rowSums(BiGram.df), decreasing=TRUE)
TriGram.sorted <- sort(rowSums(TriGram.df), decreasing=TRUE)

# Top 30 Unigrams
Freq.df <- head(data.frame(Words=names(UniGram.sorted), Frequency = UniGram.sorted), 30)
p1 = ggplot(Freq.df, aes(x=reorder(Words, Frequency), y=Frequency)) + geom_bar(stat="identity") + coord_flip()
# Add plot title & labels
p1 = p1 + ggtitle("Top 30 Tokens - nGrams = 1") + 
  xlab("") +
  ylab("Token Frequency")
# White background and black grid lines
p1 <- p1 + theme_bw()
print(p1)  

# Top 30 Bigrams
Freq.df <- head(data.frame(Words=names(BiGram.sorted), Frequency = BiGram.sorted), 30)
p1 = ggplot(Freq.df, aes(x=reorder(Words, Frequency), y=Frequency)) + geom_bar(stat="identity") + coord_flip()
# Add plot title & labels
p1 = p1 + ggtitle("Top 30 Tokens - nGrams = 2") + 
  xlab("") +
  ylab("Token Frequency")
# White background and black grid lines
p1 <- p1 + theme_bw()
print(p1)  

# Top 30 Trigrams
Freq.df <- head(data.frame(Words=names(TriGram.sorted), Frequency = TriGram.sorted), 30)
p1 = ggplot(Freq.df, aes(x=reorder(Words, Frequency), y=Frequency)) + geom_bar(stat="identity") + coord_flip()
# Add plot title & labels
p1 = p1 + ggtitle("Top 30 Tokens - nGrams = 3") + 
  xlab("") +
  ylab("Token Frequency")
# White background and black grid lines
p1 <- p1 + theme_bw()
print(p1)  


