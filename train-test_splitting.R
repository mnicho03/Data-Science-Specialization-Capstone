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