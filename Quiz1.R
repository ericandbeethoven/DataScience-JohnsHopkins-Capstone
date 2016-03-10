############################################################
#
# Student - Eric Bruce
# Project - Capstone Project Quiz 1
#
############################################################

############################################################
# Libraries
############################################################
library(plyr)
library(tm)
library(SnowballC)

############################################################
# Loading and preprocessing the data
############################################################

# set wd to data/final dir - data source 
setwd("/home/ericandbeethoven/R/DataScience-JohnsHopkins-Capstone/data/final")

# create dataframe 'dfFileInfo' with file information that will optimize reading large files  
# file information from Linux 'wc' written to file.info in each EN, DE, FI, RU subdir
# File Information: lines, wordcount, bytes, filename, dir name, df name when file read
logs.dir = "/home/ericandbeethoven/R/DataScience-JohnsHopkins-Capstone/data/logs/"
file_list <- list.files(logs.dir)
dfFileInfo <- ldply(paste(logs.dir,file_list, sep=""), 
                    read.table, 
                    col.names = c("nrows", "wordscount", "MB", "fname"),
                    header=FALSE, quote="\"", comment.char="")
dfFileInfo$MB = round(dfFileInfo$MB/10^6, 2)
dfFileInfo$dname = strtrim(dfFileInfo$fname, 5)
dfFileInfo$dfname = strtrim(dfFileInfo$fname, nchar(as.character(dfFileInfo$fname)) - 4)

# Read tables using http://www.biostat.jhsph.edu/~rpeng/docs/R-large-tables.html

# Read first 5 rows of ea. type of file (blog, news, twitter) to create a vector of classes
num.files = nrow(dfFileInfo)
# #num.rows = 5

for (i in 1:num.files)
{
  dir.file = paste(getwd(),dfFileInfo$dname[i],dfFileInfo$fname[i], sep="/")
  # create dataframe
  myDF <- as.data.frame(readLines(file(dir.file, "rb")))
  # modify dataframe
  names(myDF)[1] <- "text"
  myDF$text <- as.character(myDF$text)
  assign(dfFileInfo$dfname[i] , myDF)
  # create corpus
  assign(paste("corpus",dfFileInfo$dfname[i], sep = "_"), Corpus(VectorSource(paste(dfFileInfo$dfname[i],"$text", sep = ""))))
}

#Housecleaning
rm(myDF)
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