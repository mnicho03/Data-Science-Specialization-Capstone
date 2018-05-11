#set working directory
setwd("S:/Documents/R/Data Science Capstone/final/en_US")

#load libraries
library(stringi) #for string manipulation
library(stringr) #for string manipulation
library(tm) #to filter out specific words
library(LaF) #to read specified percentage of the files
library(dplyr)
library(tidytext)
library(ggplot2)
library(gridExtra)
library(scales)
library(data.table)

#set seed for reproducibility
set.seed(16)

# load in the data
en_files <- list.files()

#save separately
blogs <- readLines(en_files[1], encoding = "UTF-8", skipNul = TRUE)
news <- readLines(en_files[2], encoding = "UTF-8", skipNul = TRUE)
twitter <- readLines(en_files[3], encoding = "UTF-8", skipNul = TRUE)

#early exploratory analysis

#identify overarching stats per .csv
blogsStats <- stri_stats_general(blogs)
newsStats <- stri_stats_general(news)
twitterStats <- stri_stats_general(twitter)

#more summary stats
#number of words by line
blogsWords <- stringi::stri_count_words(blogs)
newsWords <- stringi::stri_count_words(news)
twitterWords <- stringi::stri_count_words(twitter)

#basic description of the data
fileOverview <- data.frame(file = c("blogs", "news", "twitter"),
                           totalLines = c(stri_stats_general(blogs)[1], stri_stats_general(news)[1], stri_stats_general(twitter)[1]),
                           totalWords = c(sum(blogsWords), sum(newsWords), sum(twitterWords)),
                           totalChars = c(stri_stats_general(blogs)[3], stri_stats_general(news)[3], stri_stats_general(twitter)[3]),
                           averageWords = c(mean(blogsWords), mean(newsWords), mean(twitterWords)),
                           minWords = c(min(blogsWords), min(newsWords), min(twitterWords)),
                           maxWords = c(max(blogsWords), max(newsWords), max(twitterWords)),
                           averageChars = c(mean(stringi::stri_count_boundaries(blogs, type = "character")), mean(stringi::stri_count_boundaries(news, type = "character")), mean(stringi::stri_count_boundaries(twitter, type = "character"))),
                           minChars = c(min(stringi::stri_count_boundaries(blogs, type = "character")), min(stringi::stri_count_boundaries(news, type = "character")), min(stringi::stri_count_boundaries(twitter, type = "character"))),
                           maxChars = c(max(stringi::stri_count_boundaries(blogs, type = "character")), max(stringi::stri_count_boundaries(news, type = "character")), max(stringi::stri_count_boundaries(twitter, type = "character"))))

#tokenization: random sampling and file size reduction
#unneccessary to utilize entire files to analyze & build prediction models
#to offer significant enough size and consistency, we'll load in a random 5% of lines from each file

mini_blogs <- sample(blogs, length(blogs)*.05)
mini_news <- sample(news, length(news)*.05)
mini_twitter <- sample(twitter, length(twitter)*.05)

#profanity filtering

#load file of bad words
#list of profane words comes from Google's banned words list and comprises swear/curse/vulgar/innappropriate/offensive/dirty/rude/insulting words
temp <- tempfile()
download.file("https://www.freewebheaders.com/wordpress/wp-content/uploads/full-list-of-bad-words-csv-file_2018_03_26_26.zip",temp)
profanity <- read.table(unz(temp, "full-list-of-bad-words-csv-file_2018_03_26.csv"))
unlink(temp)

#use removeWords from tm library to remove all profane words
# removeWords(str_to_lower(mini_en_US.blogs.txt), profanity)
clean_blogs <- removeWords(str_to_lower(mini_blogs), profanity[,1])
clean_news <- removeWords(str_to_lower(mini_news), profanity[,1])
clean_twitter <- removeWords(str_to_lower(mini_twitter), profanity[,1])

#initial exploratory analysis 
#step 1: ensure text is in DF form (will be utilizing clean, subsetted version of the original datasets for at least the dev portion of this phase)
# class(clean_blogs) # <- character class

clean_blogs_df <- data.frame(line = 1:length(clean_blogs),
                             text = clean_blogs, stringsAsFactors = FALSE)

clean_news_df <- data.frame(line = 1:length(clean_news),
                             text = clean_news, stringsAsFactors = FALSE)

clean_twitter_df <- data.frame(line = 1:length(clean_twitter),
                             text = clean_twitter, stringsAsFactors = FALSE)

# #save off mini files for speedy startup
# fwrite(clean_blogs_df, "clean_blogs_df.csv")
# fwrite(clean_news_df, "clean_news_df.csv")
# fwrite(clean_twitter_df, "clean_twitter_df.csv")

