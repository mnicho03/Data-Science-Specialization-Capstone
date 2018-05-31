#user input (sample used here: will be dynamic in final model)
input <- "HOWBOUT.LEMON QUAHALI TIME"

#function to clean user input: matches similar text cleansing done to the corpus to ensure a match can be found
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

#determine which ngram to predict against
set_df_and_ngram <- function(userInput) {
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
set_df_and_ngram(userInput)