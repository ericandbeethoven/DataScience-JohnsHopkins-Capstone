############################################################
#
# Student - Eric Bruce
# Project - Capstone Project Quiz 1
# Task 0 - Understanding the Problem
#
# Tasks to accomplish
# 
# Obtaining the data - Can you download the data and load/manipulate it in R?
# Familiarizing yourself with NLP and text mining - Learn about the basics of natural language processing and how it relates to the data science process you have learned in the Data Science Specialization.
# Questions to consider
# 
# What do the data look like?
# Where do the data come from?
# Can you think of any other data sources that might help you in this project?
# What are the common steps in natural language processing?
# What are some common issues in the analysis of text data?

# The tokenizer splits on all non-alphabetical characters, which includes the apostrophe. This mean that words like "Ophelia's" and '"dimm'd" will be tokenized to "Ophelia s" and "dimm d". 
# This is a common problem in NLP tasks: The simplest method works for most cases, but turns out not to work for a variety of edge cases. One way to fix this is to split on all whitespace characters, or to first remove all punctuation.

# What to do with apostrophe "'s"? -> (eg Finland, Finlands, Finland's)
# Abbreviations What're, I'm, isn't -> What are, I am, is not
# Use of dashes Hewlett-Packard -> Hewlett Packard
# state-of-the-art -> state of the art
# San Francisco -> one token or two
# What to with periods m.p.h., pHD. -> EOS or not, leave in or remove
#
# A period after a number could be a decimal point, a period after a captalized word would appear in titles (like Dr. or Ms.), and a period after an upper case word would appear in acronyms. Lower case words are the only option which are not commonly followed by periods for some other reason than a sentence boundary.
#
# Other language issues (German needs complex splitter)

# What is the relationship between NLP and the concepts you have learned in the Specialization?
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
en_US.num.rows = 10^4

# for (i in 1:num.files) # # Use to build ALL dataframe, corpus, etc.
for (i in 1:en_US.num.files) # Use to build EN only dataframe, corpus, etc.  
{
  dir.file = paste(data.dir,dfFileInfo$dname[i],dfFileInfo$fname[i], sep="/")
  # create dataframe
  myCon <- file(dir.file, "rb")
  myDF <- as.data.frame(readLines(myCon, n=en_US.num.rows, skipNul=TRUE, warn=FALSE))
  close(myCon)
  # modify dataframe
  names(myDF)[1] <- "text"
  myDF$text <- as.character(myDF$text)
  assign(dfFileInfo$dfname[i] , myDF)
  # create corpus
  assign(paste("corpus",dfFileInfo$dfname[i], sep = "_"), Corpus(VectorSource(paste(dfFileInfo$dfname[i],"$text", sep = ""))))
}

#Housecleaning
rm(myCon, myDF)
rm(dir.file)
rm(file_list)
rm(i)
rm(logs.dir)
rm(num.files)

############################################################
# 1 The en_US.blogs.txt  file is how many megabytes?
############################################################
# 200

############################################################
# 2 The en_US.twitter.txt has how many lines of text?
############################################################
# Over 200 million

############################################################
# 3 What is the length of the longest line seen in any of the three en_US data sets?
############################################################
max(nchar(en_US.news$text)) # 11384
max(nchar(en_US.blogs$text)) # Over 40 thousand in the blogs data set
max(nchar(en_US.twitter$text)) #140 d'uh

############################################################
# 4 In the en_US twitter data set, if you divide the number of lines 
# where the word "love" (all lowercase) occurs by the number of lines 
# the word "hate" (all lowercase) occurs, about what do you get?
############################################################
num.love = as.numeric(grepl("love",en_US.twitter$text,ignore.case = FALSE))
num.hate = as.numeric(grepl("hate",en_US.twitter$text,ignore.case = FALSE))
sum(num.love)/sum(num.hate) # 4.11

############################################################
# 5 The one tweet in the en_US twitter data set that matches the word "biostats" says what?
############################################################
num.biostats = as.numeric(grepl("biostats",en_US.twitter$text,ignore.case = FALSE))
num.biostats[1:5]
en_US.twitter[which(num.biostats == 1), ] # Didn't study

############################################################
# 6 How many tweets have the exact characters "A computer once beat me
# at chess, but it was no match for me at kickboxing". 
# (I.e. the line matches those characters exactly.)
############################################################
num.computer_beats = as.numeric(grepl("A computer once beat me at chess, but it was no match for me at kickboxing",en_US.twitter$text,ignore.case = FALSE))
sum(num.computer_beats) # 3

# Housecleaning
rm(num.computer_beats, num.biostats,num.love,num.hate)