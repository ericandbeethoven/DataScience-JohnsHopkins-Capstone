############################################################
#
# Student - Eric Bruce
# Project - Capstone Project Quiz 1
# Task 1 - Getting and Cleaning the Data
#
# Tasks to accomplish
# 
# Tokenization - identifying appropriate tokens such as words, punctuation, and numbers. Writing a function that takes a file as input and returns a tokenized version of it.
# Profanity filtering - removing profanity and other words you do not want to predict.
#
# Tips, tricks, and hints
# 
# Loading the data in. This dataset is fairly large. We emphasize that you don't necessarily need to load the entire dataset in to build your algorithms (see point 2 below). At least initially, you might want to use a smaller subset of the data. Reading in chunks or lines using R's readLines or scan functions can be useful. You can also loop over each line of text by embedding readLines within a for/while loop, but this may be slower than reading in large chunks at a time. Reading pieces of the file at a time will require the use of a file connection in R. For example, the following code could be used to read the first few lines of the English Twitter dataset:con <- file("en_US.twitter.txt", "r") readLines(con, 1) ## Read the first line of text readLines(con, 1) ## Read the next line of text readLines(con, 5) ## Read in the next 5 lines of text close(con) ## It's important to close the connection when you are done
# See the ?connections help page for more information.
# 
# Sampling. To reiterate, to build models you don't need to load in and use all of the data. Often relatively few randomly selected rows or chunks need to be included to get an accurate approximation to results that would be obtained using all the data. Remember your inference class and how a representative sample can be used to infer facts about a population. You might want to create a separate sub-sample dataset by reading in a random subset of the original data and writing it out to a separate file. That way, you can store the sample and not have to recreate it every time. You can use the rbinom function to "flip a biased coin" to determine whether you sample a line of text or not.
#
############################################################

############################################################
# Libraries
############################################################
library(plyr)
library(tm) # text mining
library(SnowballC) # helps us use the tm library

############################################################
# Loading and preprocessing the data
############################################################

# set wd & data/final dir - data source 
setwd("~/R/DataScience-JohnsHopkins-Capstone")
data.dir = ("~/R/DataScience-JohnsHopkins-Capstone/data/final")


# create dataframe 'dfFileInfo' with file information that will optimize reading large files  
# file information from Linux 'wc' written to file.info in each EN, DE, FI, RU subdir
# File Information: lines, wordcount, bytes, filename, dir name, df name when file read
logs.dir = "~/R/DataScience-JohnsHopkins-Capstone/data/logs/"
file_list <- list.files(logs.dir)
dfFileInfo <- ldply(paste(logs.dir,file_list, sep=""), 
                    read.table, 
                    col.names = c("nrows", "wordscount", "MB", "fname"),
                    header=FALSE, quote="\"", comment.char="")
dfFileInfo$MB = round(dfFileInfo$MB/10^6, 2)
dfFileInfo$dname = strtrim(dfFileInfo$fname, 5)
dfFileInfo$dfname = strtrim(dfFileInfo$fname, nchar(as.character(dfFileInfo$fname)) - 4)
num.files = nrow(dfFileInfo)

# subset for first 10k rows of EN only 
en_US.FileInfo = subset(dfFileInfo, dfFileInfo$dname == "en_US")
en_US.num.files = nrow(en_US.FileInfo)
en_US.num.rows = 5*10^4

# for (i in 1:num.files) # # Use to build ALL dataframe, corpus, etc.
for (i in 1:en_US.num.files) # Use to build EN only dataframe, corpus, etc.  
{
  dir.file = paste(data.dir,en_US.FileInfo$dname[i],en_US.FileInfo$fname[i], sep="/")
  # create dataframe
  myCon <- file(dir.file, "rb")
  myDF <- as.data.frame(readLines(myCon, n=en_US.num.rows, skipNul=TRUE, warn=FALSE))
  close(myCon)
  # modify dataframe
  names(myDF)[1] <- "text"
  myDF$text <- as.character(myDF$text)
  assign(en_US.FileInfo$dfname[i] , myDF)
  # create corpus
  assign(paste("corpus",en_US.FileInfo$dfname[i], sep = "_"), Corpus(VectorSource(paste(en_US.FileInfo$dfname[i],"$text", sep = ""))))
}

#Housecleaning
rm(myCon, myDF)
rm(dir.file)
rm(file_list)
rm(i)
rm(logs.dir)
rm(num.files, en_US.num.files, en_US.num.rows)

############################################################
# 1 Tokenization
############################################################

# Look at corpus Twitter
corpus_en_US.twitter
corpus_en_US.twitter[[1]]

# Convert to lower-case
corpus_en_US.twitter = tm_map(corpus_en_US.twitter, tolower)
corpus_en_US.twitter[[1]]

# IMPORTANT NOTE: If you are using the latest version of the tm package, you will need to run the following line before continuing (it converts corpus_en_US.twitter to a Plain Text Document). This is a recent change having to do with the tolower function that occurred after this video was recorded.
corpus_en_US.twitter = tm_map(corpus_en_US.twitter, PlainTextDocument)

# Remove punctuation
corpus_en_US.twitter = tm_map(corpus_en_US.twitter, removePunctuation)
corpus_en_US.twitter[[1]]

# Look at stop words 
stopwords("english")[1:10]

# Remove stopwords
corpus_en_US.twitter = tm_map(corpus_en_US.twitter, removeWords, stopwords("english"))
corpus_en_US.twitter[[1]]

# Remove profanity
# Download data
fileUrl <- "http://www.cs.cmu.edu/~biglou/resources/bad-words.txt"
if (!file.exists("./data/other/bad-words.txt")){
  download.file(fileUrl,destfile="./data/other/bad-words.txt")
}

# Build Dataframe from downloaded file - Linux environment
myCon <- file("./data/other/bad-words.txt", "r")
profanity <- VectorSource(readLines(myCon, skipNul=TRUE, warn=FALSE))
close(myCon)
rm(myCon)
corpus_en_US.twitter = tm_map(corpus_en_US.twitter, removeWords, profanity)

# Stem document 
corpus_en_US.twitter = tm_map(corpus_en_US.twitter, stemDocument)
corpus_en_US.twitter[[1]]




# Video 6

# Create matrix

frequencies = DocumentTermMatrix(corpus_en_US.twitter)

frequencies

# Look at matrix 

inspect(frequencies[1000:1005,505:515])

# Check for sparsity

findFreqTerms(frequencies, lowfreq=100)

# Remove sparse terms

sparse = removeSparseTerms(frequencies, 0.995)
sparse
