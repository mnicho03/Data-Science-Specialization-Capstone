#set working directory
setwd("C:/Users/mnicho03/Desktop/Capstone_Local")

#load libraries
library(tm) # to read / manipulate text 
library(filehash) # to update / maintain perm corpus DB
library(tau) # to create ngrams
library(data.table) # for fast read / write
library(stringr) # for string manipulation
library(dplyr) # for DF manipulation
library(parallel) #for mclapply function

#set seed for reproducibility
set.seed(16)

# load in the full data files from "en_US" folder
en_files <-  list.files(path = "final/en_US", pattern = "\\.txt$")

#save file path to reference within readLines function below
path <- "final/en_US"

#without these variables, readLines file cannot create connection properly
blogs_full_path <- paste(path, en_files[1], sep = "/")
news_full_path <- paste(path, en_files[2], sep = "/")
twitter_full_path <- paste(path, en_files[3], sep = "/")

#save each file separately -- calls full path to each file
blogs <- readLines(blogs_full_path, encoding = "UTF-8", skipNul = TRUE)
news <- readLines(news_full_path, encoding = "UTF-8", skipNul = TRUE)
twitter <- readLines(twitter_full_path, encoding = "UTF-8", skipNul = TRUE)

#random sampling and file size reduction
#unneccessary to utilize entire files to analyze & build prediction models

#for speed of development and ease of further updates, we'll create a sample factor (to determine how much of the file to load)
sample_factor <- .001

#create the random sample of each file
mini_blogs <- sample(blogs, length(blogs) * sample_factor)
mini_news <- sample(news, length(news) * sample_factor)
mini_twitter <- sample(twitter, length(twitter) * sample_factor)

#function to split each dataset into train / test (80/20)
#note in using this function: it assumes that each line within each file is random, therefore removing the need to randomize the dataset splitting (e.g. 'CreateDataPartition' from the caret package)
train_test_split <- function(file) {
#convert file into dataframe to work with row numbers
fileDF <- data.frame(
        line = 1:length(file),
        item = file, stringsAsFactors = FALSE)

#get total number of rows
rows <- nrow(fileDF)
#find row number to split the dataset on: using an 80/20 split
split_row <- round(rows * .8, 0)
#subset the data on the first 80% 
training <- fileDF[1:split_row, ]
#subset the data on the last 20% (80% row + 1 -- end of dataset)
testing <- fileDF[(split_row + 1): rows, ]

#set dynamic names for the train/test outputs
training_name <- paste(deparse(substitute(file)), "training", sep = "_")
testing_name <- paste(deparse(substitute(file)), "testing", sep = "_")

#assign variables outside of the function
assign(training_name, training, envir = .GlobalEnv)
assign(testing_name, testing, envir = .GlobalEnv)
}

#run the functions and create all train/test datasets
train_test_split(mini_blogs)
train_test_split(mini_news)
train_test_split(mini_twitter)

#merge newly created files to create complete train & test sets: no need to continue treating the data by file
training <- rbind.data.frame(mini_blogs_training, mini_news_training, mini_twitter_training)
#keep only the column of character strings
training <- training[,2]
testing <- rbind.data.frame(mini_blogs_testing, mini_news_testing, mini_twitter_testing)
#keep only the column of character strings
testing <- testing[,2]

#write out the splits to subfolders within a 'full_corpus' folder (subfolders must be created beforehand)
#training
file_connection <- file("full_corpus/training/training_corpus.txt")
writeLines(training, file_connection)
close(file_connection)

#testing
file_connection <- file("full_corpus/testing/testing_corpus.txt")
writeLines(testing, file_connection)
close(file_connection)

#set location for corpus files (as referenced above)
path_to_training <- file.path("full_corpus/training")
path_to_testing <- file.path("full_corpus/testing")

#preprocessing
#load in the corpus
training_corpus <- Corpus(DirSource(path_to_training))
testing_corpus <- Corpus(DirSource(path_to_testing))

#function to apply out-of-the-box and custom text processing 
corpus_cleaner <- function(corpus) {

#remove non-ASCII (ugly) characters
corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, "latin1", "ASCII", sub="")))

# convert all text to lower case
corpus <- tm_map(corpus, content_transformer(tolower))

# create a custom content transformer to mark end of sentence
end_sentence <- content_transformer(function(x, pattern) {return (gsub(pattern," <endofsentencemarker> ", x))})

# replace all carriage returns or new lines: this creates breakpoints between entries in each file
corpus <- tm_map(corpus, end_sentence, "[\r\n]")

# standardize sentence endings between ., !, and ?
corpus <- tm_map(corpus, end_sentence, "\\. |\\.$")
corpus <- tm_map(corpus, end_sentence, "\\? |\\?$")
corpus <- tm_map(corpus, end_sentence, "\\! |\\!$")

# create a custom content transformer to replace all specified patterns with " "
toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))})

# separate hyphenated and slashed words
corpus <- tm_map(corpus, toSpace, "-")
corpus <- tm_map(corpus, toSpace, "/")
# removes errant close brackets starting a word
corpus <- tm_map(corpus, toSpace, ">[a-z]")
# removes any remaining <> brackets 
corpus <- tm_map(corpus, toSpace, "<>")

# create a custom content transformer to remove specified patterns
stripText <- content_transformer(function(x, pattern) {return (gsub(pattern, "", x))})

# remove apostrophe but retain the words (e.g. "don't" to "dont")
corpus <- tm_map(corpus, stripText, "'")

# remove numbers
corpus <- tm_map(corpus, removeNumbers)

# remove remaining punctuation 
corpus <- tm_map(corpus, removePunctuation)

#profanity filtering
#through trial and error, decided to save off a relatively short list of 451 'bad words' separately into a TXT file
#TXT file saved to the github repo for reproducibility and can be included in the working directory as this script
profanity <- readLines("bad_words_list.txt")

#remove the badwords
corpus <- tm_map(corpus, removeWords, profanity)

# remove additional whitespace
corpus <- tm_map(corpus, stripWhitespace)

#return cleansed corpus which can then be saved off separately
return(corpus)
}

#clean the train / test corpus by running it through the function created above
#wrap in suppressWarnings function (alerts pop up when incomplete final lines are identified)
training_corpus_cleaned <- suppressWarnings(corpus_cleaner(training_corpus))
testing_corpus_cleaned <- suppressWarnings(corpus_cleaner(testing_corpus))

#*in the current process, no stemming is conducted

#clear out environment other than the corpus
rm(list = setdiff(ls(), c("training_corpus_cleaned", "testing_corpus_cleaned")))

# #set minimum word threshold
# mininum_freq <- 5
# training_unigrams <- training_unigrams %>%
#         filter(n >= mininum_freq)


#function to split ngrams
ngram <- function(n, text) {
        textcnt(text, method = "string",n=as.integer(n),
                split = "[[:space:][:digit:]]+",decreasing=T)
}

#save off only the text portion of the corpus 
training_text <- c(training_corpus_cleaned[[1]][[1]])
#reduce RAM load
rm(training_corpus_cleaned)

#unigrams
unigrams <- ngram(1, training_text)
unigram_df <- data.frame(unigram = names(unigrams), frequency = unclass(unigrams))

unigram_df$unigram <- as.character(unigram_df$unigram)
unigram_df$frequency <- as.numeric(unigram_df$frequency)
#removes mentionings of " <EOS> " (filler used to mark end of sentence/entries)
unigram_df <- unigram_df[which(unigram_df$unigram!="endofsentencemarker"),]

#ID the number of unigrams
length_unigrams <- length(unigram_df$unigram) 

#frequency table for Good-Turing smoothing
#captures the 'frequency of frequencies' (e.g. 100 instances of a word appearing 3 times)
unigram_frequency_table <- data.frame(unigram = table(unigram_df$frequency))

#bigrams
bigrams <- ngram(2, training_text)
names(bigrams) <- gsub("^\'","",names(bigrams))        
bigram_df <- data.frame(bigram = names(bigrams), frequency = unclass(bigrams))
names(bigram_df) <- c("bigram","frequency")

#removes mentionings of " <EOS> " (filler used to mark end of sentence/entries)
eosTag <- grepl("endofsentencemarker", bigram_df$bigram)
bigram_df <- bigram_df[!eosTag,]

#ID the number of bigrams
length_bigrams <- length(bigram_df$bigram) 

#frequency table for Good-Turing smoothing
#captures the 'frequency of frequencies' (e.g. 100 instances of a word appearing 3 times)
bigram_frequency_table <- data.frame(bigram = table(bigram_df$frequency))

#trigrams
trigrams <- ngram(3, training_text)
names(trigrams) <- gsub("^\'","",names(trigrams))
trigram_df <- data.frame(trigram = names(trigrams), frequency = unclass(trigrams))
names(trigram_df) <- c("trigram","frequency")

#removes mentionings of " <EOS> " (filler used to mark end of sentence/entries)
eosTag <-grepl("endofsentencemarker", trigram_df$trigram)
trigram_df <- trigram_df[!eosTag,]

#ID the number of trigrams
length_trigrams <- length(trigram_df$trigram) 

#frequency table for Good-Turing smoothing
#captures the 'frequency of frequencies' (e.g. 100 instances of a word appearing 3 times)
trigram_frequency_table <- data.frame(trigram = table(trigram_df$frequency))

#quadgrams
quadgrams <- ngram(4, training_text)
names(quadgrams) <- gsub("^\'","",names(quadgrams))
quadgram_df <- data.frame(quadgram = names(quadgrams), frequency = unclass(quadgrams))
names(quadgram_df) <- c("quadgram","frequency")

#removes mentionings of " <EOS> " (filler used to mark end of sentence/entries)
eosTag <-grepl("endofsentencemarker", quadgram_df$quadgram)
quadgram_df <- quadgram_df[!eosTag,]

#ID the number of quadgrams
length_quadgram <- length(quadgram_df$quadgram) 

#frequency table for Good-Turing smoothing
#captures the 'frequency of frequencies' (e.g. 100 instances of a word appearing 3 times)
quadgram_frequency_table <- data.frame(quadgram = table(quadgram_df$frequency))

#clear out ngram textcnt objects
rm(list = c('unigrams', 'bigrams', 'trigrams', 'quadgrams'))

#shifting to the prediction model
#convert all ngrams to character (currently factors)
unigram_df$unigram <- as.character(unigram_df$unigram)
bigram_df$bigram <- as.character(bigram_df$bigram)
trigram_df$trigram <- as.character(trigram_df$trigram)
quadgram_df$quadgram <- as.character(quadgram_df$quadgram)

#create new variables (predicted_word & preceding words) for each ngram DF, which will be used as the output in the final model

#for unigram, this will be the same word (this DF will only be used when there is no match within the larger ngram DF's)
unigram_df$predicted_word <- unigram_df$unigram

#for the remaining ngrams, predicted_word = final word in string
#word function comes from the stringr package
#grabs last word
bigram_df$predicted_word <- word(bigram_df$bigram, -1)
#grabs first word
bigram_df$preceding <- word(bigram_df$bigram, 1)

#trigrams
#grabs last word
trigram_df$predicted_word <- word(trigram_df$trigram, -1)
#grabs first to second word
trigram_df$preceding <- word(trigram_df$trigram, 1, 2)

#quadgrams
#grabs last word
quadgram_df$predicted_word <- word(quadgram_df$quadgram, -1)
#grabs first to third word
quadgram_df$preceding <- word(quadgram_df$quadgram, 1, 3)

#create uniform naming convention for ngrams throughout the 4 ngram DF's
#will ease flexibility in the prediction model used
unigram_df <- rename(unigram_df, ngram = unigram)  
bigram_df <- rename(bigram_df, ngram = bigram) 
trigram_df <- rename(trigram_df, ngram = trigram) 
quadgram_df <- rename(quadgram_df, ngram = quadgram) 

#user input (sample used here: will be dynamic in final model)
input <- "apples to apples and TESTING !@# 123 and let's do the"

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
        
        #print the most likely results
        print(predictions)
        
        #output the prediction
        print(predictions$predicted_word[1])
}

#run the prediction function and output the predictions
#two outputs - (up to) the top 10 most likely predictions & the most likely prediction
next_word_Prediction(ngram_input, target_df)