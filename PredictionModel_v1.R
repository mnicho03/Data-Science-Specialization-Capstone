# The goal of this exercise is to build and evaluate your first predictive model. You will use the n-gram and backoff models you built in previous tasks to build and evaluate your predictive model. The goal is to make the model efficient and accurate.
# 
# Tasks to accomplish
# 
# Build a predictive model based on the previous data modeling steps - you may combine the models in any way you think is appropriate.
# Evaluate the model for efficiency and accuracy - use timing software to evaluate the computational complexity of your model. Evaluate the model accuracy using different metrics like perplexity, accuracy at the first word, second word, and third word.
# Questions to consider
# 
# How does the model perform for different choices of the parameters and size of the model?
# How much does the model slow down for the performance you gain?
# Does perplexity correlate with the other measures of accuracy?
# Can you reduce the size of the model (number of parameters) without reducing performance?

#set working directory
setwd("C:/Users/mnicho03/Desktop/Capstone_Local/final/en_US")

#load libraries
library(tm) #to filter out specific words
library(stringr) #for string manipulation
library(tidytext) #for tokenization
library(dplyr)
library(data.table) #for fast read/write

#for reproducibility
set.seed(16)

# load in the full data files
en_files <- list.files()

#save each file separately
blogs <- readLines(en_files[1], encoding = "UTF-8", skipNul = TRUE)
news <- readLines(en_files[2], encoding = "UTF-8", skipNul = TRUE)
twitter <- readLines(en_files[3], encoding = "UTF-8", skipNul = TRUE)

#for efficiency sake and development purposes, we'll load a random 5% sample of the total text
mini_blogs <- sample(blogs, length(blogs)*.05)
mini_news <- sample(news, length(news)*.05)
mini_twitter <- sample(twitter, length(twitter)*.05)

#profanity filtering
#load file of bad words
#pivoted to a less comprehensive list than the original which came from Google's banned words list and comprises swear/curse/vulgar/innappropriate/offensive/dirty/rude/insulting words
temp <- tempfile()
download.file("http://www.cs.cmu.edu/~biglou/resources/bad-words.txt", temp)
profanity <- read.table(temp)
unlink(temp)

#use removeWords from tm library to remove all profane words
censored_blogs <- removeWords(str_to_lower(mini_blogs), profanity[,1])
censored_news <- removeWords(str_to_lower(mini_news), profanity[,1])
censored_twitter <- removeWords(str_to_lower(mini_twitter), profanity[,1])

#ensure text is in DF form (will be utilizing clean, subsetted version of the original datasets for at least the dev portion of this phase)
#retain file type just in case we later need it
censored_blogs_df <- data.frame(line = 1:length(censored_blogs),
                                text = censored_blogs, stringsAsFactors = FALSE,
                                file = "Blogs")

censored_news_df <- data.frame(line = 1:length(censored_news),
                               text = censored_news, stringsAsFactors = FALSE,
                               file = "News")

censored_twitter_df <- data.frame(line = 1:length(censored_twitter),
                                  text = censored_twitter, stringsAsFactors = FALSE,
                                  file = "Twitter")

#merging all the datasets 
#this diverts from the original process during the exploratory analysis
#for the final model, we'll be using a mixture of all the data, so no reason to continue analyzing separately
censored_all_df <- rbind.data.frame(censored_blogs_df, censored_news_df, censored_twitter_df)

#basic text cleansing
#removing all punctuation
censored_all_df$text <- gsub("[[:punct:] ]+", " ", censored_all_df$text)

#remove all numbers
censored_all_df$text <- gsub("[0-9]+", "", censored_all_df$text)

#remove extra whitespace
censored_all_df$text <- gsub("\\s+", " ", censored_all_df$text)

#tokenize the data frames (split each word from each document into its own row)
#will no longer remove 'stopwords': extremely common words not necessarily valuable for predictions (e.g. as, of, the , a, ..., etc.)
#however, these are vitals for creating ngrams
tokenized_all <- censored_all_df %>%
        unnest_tokens(output = word, input = text) %>%
        count(word) %>%
        select(word, n) %>%
        arrange(desc(n)) %>%
        filter(n >= 5)

# #identify all of the the most common words required to cover 95% of the total words
# token_coverage <- tokenized_all %>%
#         count(word) %>%
#         mutate(proportion = n / sum(n)) %>%
#         arrange(desc(proportion)) %>%
#         mutate(cumsum = cumsum(proportion))
# 
# #filter the text to only include the terms required to hit 95% coverage of all words (in other words, reduce the dataset by taking away the more rare terms that cover 5% of the total)
# token_95_percent <- token_coverage %>%
#         filter(cumsum <= .95)
# 
# #convert to DF
# token_95_percent <- as.data.frame(token_95_percent)
# 
# #remove single character 'words'
# token_95_percent <- token_95_percent %>%
#         filter(word %in% c("a", "i") | stringr::str_length(word) != 1)
# 
# #total terms now reduced from 330,000 terms to most significant ~15,000
# #write out file to be used in later analysis
# fwrite(token_95_percent, "tokenized_significant_words.csv")

#convert single tokens to DF
unigram_df <- as.data.frame(tokenized_all)

#n-gram building
#function to build the bigram dataframe
bigram_df_build <- function(dataset) {
        bigram_df <- dataset %>%
                        unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
                        tidyr::separate(bigram, c("word1", "word2"), sep = " ") %>%
                        na.omit() %>%
                        # filter(!word1 %in% stop_words$word,
                        #        !word2 %in% stop_words$word) %>%
                        count(word1, word2, sort = TRUE) %>%
                        mutate(bigram = paste(word1, word2, sep = " "))
        
        assign("bigram_df", bigram_df, envir = .GlobalEnv)
}

#build the DF of bigrams and calculate the runtime
system.time(bigram_df_build(censored_all_df))

#clean up bigrams
#remove single character 'words'
bigram_df <- bigram_df %>%
       #exception to keep 'a' and 'i' since these are very common actual words
        filter(word1 %in% c("a", "i") | stringr::str_length(word1) != 1) %>%
        filter(word2 %in% c("a", "i") | stringr::str_length(word2) != 1)

#trigrams
trigram_df_build <- function(dataset) {
        trigram_df <- dataset %>%
                unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
                tidyr::separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
                na.omit() %>%
                # filter(!word1 %in% stop_words$word,
                #        !word2 %in% stop_words$word,
                #        !word3 %in% stop_words$word) %>%
                count(word1, word2, word3, sort = TRUE) %>%
                mutate(trigram = paste(word1, word2, word3, sep = " "))
        
        assign("trigram_df", trigram_df, envir = .GlobalEnv)
}

#build the DF of trigrams and calculate the runtime
system.time(trigram_df_build(censored_all_df))

#clean up trigrams
#remove single character 'words'
trigram_df <- trigram_df %>%
        #exception to keep 'a' and 'i' since these are very common actual words
        filter(word1 %in% c("a", "i") | stringr::str_length(word1) != 1) %>%
        filter(word2 %in% c("a", "i") | stringr::str_length(word2) != 1) %>%
        filter(word3 %in% c("a", "i") | stringr::str_length(word3) != 1)

#quadgrams
quadgram_df_build <- function(dataset) {
        quadgram_df <- dataset %>%
                unnest_tokens(quadgram, text, token = "ngrams", n = 4) %>%
                tidyr::separate(quadgram, c("word1", "word2", "word3", "word4"), sep = " ") %>%
                na.omit() %>%
                # filter(!word1 %in% stop_words$word,
                #        !word2 %in% stop_words$word,
                #        !word3 %in% stop_words$word,
                #        !word4 %in% stop_words$word) %>%
                count(word1, word2, word3, word4, sort = TRUE) %>%
                mutate(quadgram = paste(word1, word2, word3, word4, sep = " "))
        
        assign("quadgram_df", quadgram_df, envir = .GlobalEnv)
}

#build the DF of trigrams and calculate the runtime
system.time(quadgram_df_build(censored_all_df))

#remove single character 'words'
quadgram_df <- quadgram_df %>%
        #exception to keep 'a' and 'i' since these are very common actual words
        filter(word1 %in% c("a", "i") | stringr::str_length(word1) != 1) %>%
        filter(word2 %in% c("a", "i") | stringr::str_length(word2) != 1) %>%
        filter(word3 %in% c("a", "i") | stringr::str_length(word3) != 1) %>%
        filter(word4 %in% c("a", "i") | stringr::str_length(word3) != 1)

#convert ngrams to dataframes
bigram_df <- as.data.frame(bigram_df)
trigram_df <- as.data.frame(trigram_df)
quadgram_df <- as.data.frame(quadgram_df)

#isolate the last (predicted) word from each group (unigram excluded), e.g. 'the dog jumped' and 'over' 
bigram_df <- bigram_df %>%
        select(word1, word2, n) %>%
        rename(Preceding = word1) %>%
        rename(Word = word2)

trigram_df <- trigram_df %>%
        mutate(Preceding = paste0(word1, " ", word2)) %>%
        rename(Word = word3) %>%
        select(Preceding, Word, n)

quadgram_df <- quadgram_df %>%
        mutate(Preceding = paste0(word1, " ", word2, " ", word3)) %>%
        rename(Word = word4) %>%
        select(Preceding, Word, n)

#declutter the environment
rm(list = setdiff(ls(), c("censored_all_df", "unigram_df", "bigram_df", "trigram_df", "quadgram_df")))

# #save off each ngram file for later use
# fwrite(unigram_df, "unigram_df.csv")
# fwrite(bigram_df, "bigram_df.csv")
# fwrite(trigram_df, "trigram_df.csv")
# fwrite(quadgram_df, "quadgram_df.csv")

#user input
input <- "The guy in front of me just bought a pound of bacon, a bouquet, and a case of"

#function to clean user input: matches the same text cleansing done to the corpus to ensure a match can be found
userInput_cleaner <- function(input){
        #clean input text
        #remove punctuation
        input_cleaning <- gsub('[[:punct:] ]+', ' ', input)
        
        #remove numbers
        input_cleaning <- gsub("[0-9]+", "", input_cleaning)
        
        #convert all words to lowercase
        input_cleaning <- str_to_lower(input_cleaning)
        
        #remove extra whitespace
        input_clean <- gsub("\\s+", " ", input_cleaning)
        input_clean <- str_trim(input_clean)
        
        #return the cleaned user input
        return(input_clean)
}

#save the userInput to be run in the model   
userInput <- userInput_cleaner(input)

#determine # of words in the input
userInput_words <- unlist(str_split(userInput," "))
userInput_words_length <- length(userInput_words)

#if statement to return the claned ngram based on user input after processing 
# #if it's a quadgram
# #insert the user input into a variable
# if (userInput_words_length >= 4) { 
# ngram <- paste(userInput_words[userInput_words_length-3],
#         userInput_words[userInput_words_length-2],
#         userInput_words[userInput_words_length-1],
#         userInput_words[userInput_words_length])
# 
# target_df <- quadgram_df
# 
# } else 

set_df_and_ngram <- function(userInput) {
if (userInput_words_length >= 3) {
        #if it's a trigram or more
        #insert the user input into a variable
        ngram <- paste(userInput_words[userInput_words_length-2],
                       userInput_words[userInput_words_length-1],
                       userInput_words[userInput_words_length])
        
        #data we want to predict off of: since we're looking at 3 words, we want the quadgram DF to look for the potential 4th
        target_df <- quadgram_df
        assign("ngram", ngram, envir = .GlobalEnv)
        assign("target_df", target_df, envir = .GlobalEnv)
        
} else if (userInput_words_length == 2) {
        #if it's a bigram
        #insert the user input into a variable
        ngram <- paste(userInput_words[userInput_words_length-1],
                       userInput_words[userInput_words_length])
        
        target_df <- trigram_df
        assign("ngram", ngram, envir = .GlobalEnv)
        assign("target_df", target_df, envir = .GlobalEnv)
        
} else {
        #if it's a unigram 
        #insert the user input into a variable
        ngram <- paste(userInput_words[userInput_words_length])
        
        target_df <- bigram_df
        assign("ngram", ngram, envir = .GlobalEnv)
        assign("target_df", target_df, envir = .GlobalEnv)
}
}

#run the function
set_df_and_ngram(userInput)

#take the cleaned ngram and filter based on the user input - to show top_10 predictions
next_word_Prediction <- function(ngram, target_df) {
        
        #gather predictions 
        predictions <- target_df %>%
                #currently filtered on exact matches (^ngram$)
                filter(grepl(paste0("^", ngram, "$"), Preceding)) %>%
                #remove stopwords from predictions
                filter(!Word %in% stop_words$word) %>%
                mutate(Probability = n / sum(n)) %>%
                top_n(n = 10, wt = n) %>%
                slice(row_number(1:10)) %>% #ensure 10 values max
                rename(Frequency = n) %>%
                rename(Input = Preceding)
        
        # #if the user input does not generate any matching values, we'll next look at rough matches (e.g. 'happi' may show 'happiness')
        # if (nrow(predictions) == 0) {
        #         predictions <- target_df %>%
        #                 filter(grepl(ngram, Preceding)) %>%
        #                 #remove stopwords from predictions
        #                 filter(!Word %in% stop_words$word) %>%
        #                 mutate(Probability = n / sum(n)) %>%
        #                 top_n(n = 10, wt = n) %>%
        #                 slice(row_number(1:10)) %>% #ensure 10 values max
        #                 rename(Frequency = n) %>%
        #                 rename(Input = Preceding)
        # }
        
        #if the user input + rough matches do not generate any matching values, the most common words will be brought forth
        if (nrow(predictions) == 0) {
                predictions <- unigram_df %>%
                        # #filter out stopwords
                        # filter(!word %in% stop_words$word) %>%
                        mutate(Probability = n / sum(n)) %>%
                        top_n(n = 10, wt = n) %>%
                        slice(row_number(1:10)) %>% #ensure 10 values max
                        rename(Frequency = n) %>%
                        rename(Word = word)
        }
        
        #print the most likely results
        print(predictions)
        
        #output the prediction
        print(predictions$Word[1])
}

#run the prediction function and output the predictions
#two outputs - (up to) the top 10 most likely predictions & the most likely prediction
next_word_Prediction(ngram, target_df)

