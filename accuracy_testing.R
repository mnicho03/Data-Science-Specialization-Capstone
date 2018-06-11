#accuracy testing

# #if needed <environment setup>

# #set working directory 
# setwd("C:/Users/mnicho03/Desktop/Capstone_Local

# #load ngram dataframes
# load("accuracy_testing_setup.RData")

# #load libraries
# library(tm) # to read / manipulate text 
# library(filehash) # to update / maintain perm corpus DB
# library(tau) # to create ngrams
# library(data.table) # for fast read / write
# library(stringr) # for string manipulation
# library(dplyr) # for DF manipulation
# library(parallel) #for mclapply function
# library(tidytext) #for tokenization

# #set seed for reproducibility
# set.seed(16)

#step 1 - set function to conduct the full prediction (input >> predicted word)
full_predict_next_word_function <- function(test_text) {

#cleaning unneccessary since it was already done during test_quadgram_df building

#determine # of words in the input
userInput_words <- unlist(str_split(test_text," "))
userInput_words_length <- length(userInput_words)

#determine which ngram to predict against
set_df_and_ngram <- function(test_text) {
        if (userInput_words_length >= 3) {
                #if it's a trigram or more
                #insert the user input into a variable
                ngram_input <- paste(userInput_words[userInput_words_length-2],
                                     userInput_words[userInput_words_length-1],
                                     userInput_words[userInput_words_length])
                
                #data we want to predict off of: since we're looking at 3 words, we want the quadgram DF to look for the potential 4th
                target_df <- quadgram_df
                
        } else if (userInput_words_length == 2) {
                #if it's a bigram
                #insert the user input into a variable
                ngram_input <- paste(userInput_words[userInput_words_length-1],
                                     userInput_words[userInput_words_length])
                
                #data we want to predict off of: since we're looking at 2 words, we want the trigram DF to look for the potential 3rd
                target_df <- trigram_df
                
        } else {
                #if it's a unigram 
                #insert the user input into a variable
                ngram_input <- paste(userInput_words[userInput_words_length])
                
                #data we want to predict off of: since we're looking at 1 words, we want the bigram DF to look for the potential 2nd
                target_df <- bigram_df
                
        }
        
        #ensure ngram and target_df identified in the function are saved globally
        assign("ngram_input", ngram_input, envir = .GlobalEnv)
        assign("target_df", target_df, envir = .GlobalEnv)
}

#run the function
set_df_and_ngram(test_text)

# determine if target_DF identified above will have a matching ngram
# if no match exists, search through suceeding (smaller) ngram DF's until a matching ngram exists

#variable for number of matches
ngram_matches <- sum(target_df$preceding == ngram_input)

#for future revisions: the following code should be optimized
#find if current target_DF and ngram_input have any matches
#if matches != 0, this statement will not have any impact
if(ngram_matches == 0) {
        #if matches == 0, set target_DF and ngram as n-1
        #step 1: determine current target_df
        #if we're looking for a bigram, then there will be no further action
        if(length(target_df$ngram) == length(bigram_df$ngram)) {
                
                break
                
        } else {
                
                #check to see if we're looking for a trigram
                if(length(target_df$ngram) == length(trigram_df$ngram)) {
                        
                        #update ngram and target_df to n-1
                        ngram_input <- word(ngram_input, -1)
                        target_df <- bigram_df
                        
                        #at this point even if there are no matches, there's no further updates we can make (e.g. if we can't predict off only the last word, then we have no way to make a guess)
                        
                } else {
                        #last condition will only apply if we're looking for a quadgram
                        #update ngram and target_df to n-1
                        #grabs the second to last and the last word
                        ngram_input <- word(ngram_input, -2, -1)
                        #sets the new target_df
                        target_df <- trigram_df
                        
                        #rerun ngram_matches calculation
                        ngram_matches <- sum(target_df$preceding == ngram_input)
                        
                        #rerun original if to see if we need to check against bigrams
                        if(ngram_matches == 0) {
                                #update ngram and target_df to n-1
                                #grabs  the last word
                                ngram_input <- word(ngram_input, -1)
                                #sets the new target_df
                                target_df <- bigram_df
                                
                                #final rerun of ngram_matches calculation
                                ngram_matches <- sum(target_df$preceding == ngram_input)
                                
                        }
                }
        }
}

#take the cleaned ngram and filter based on the user input - to show predicted word as well as top_10 predictions
next_word_Prediction <- function(ngram_input, target_df) {
        
        #gather predictions 
        predictions <- target_df %>%
                #filter on exact matches (^ngram$)
                filter(grepl(paste0("^", ngram_input, "$"), preceding)) %>%
                # #remove stopwords from predictions
                # filter(!predicted_word %in% stop_words$word) %>%
                mutate(Probability = frequency / sum(frequency)) %>%
                top_n(n = 10, wt = Probability) %>%
                slice(row_number(1:10)) #ensure 10 values max
        
        #if no matches can be found based on the user input: output top 10 most common words
        if (nrow(predictions) == 0) {
                predictions <- unigram_df %>%
                        # #remove stopwords from predictions
                        # filter(!predicted_word %in% stop_words$word) %>%
                        mutate(Probability = frequency / sum(frequency)) %>%
                        top_n(n = 10, wt = Probability) %>%
                        slice(row_number(1:10)) #ensure 10 values max
                
        }
        
        #save predictions DF to be used in data product visualizations
        #double check it saves properly
        predictions_df <- as.data.frame(predictions)
        
        #save the most 10 likely predictions
        assign("predictions_df", predictions, envir = .GlobalEnv)
}

#run the prediction function and output the predictions
#two outputs - (up to) the top 10 most likely predictions & the most likely prediction
next_word_Prediction(ngram_input, target_df)

return(predictions_df$predicted_word[1])

}

#step 2: run the function against the test set
#show summary of the test set
str(test_quadgram_df)
#1,229,670 test variables - for time sake, will evaluate accuracy based on a random 10,000
test_predictions_10000 <- test_quadgram_df[sample(nrow(test_quadgram_df), 10000, replace = FALSE),]

#run the function
system.time(test_predictions_10000$predicted_word <- sapply(test_predictions_10000$preceding, full_predict_next_word_function))

#determine percentage of accurately identified next_words
paste0(round(100 * with(test_predictions_10000, mean(predicted_word == actual_word)),3), "%")
# user   system  elapsed 
# 10325.68     9.67 10422.65 
# 14.21 percent accuracy