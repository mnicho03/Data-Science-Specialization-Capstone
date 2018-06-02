#efficiency examination - using .001 size sample

#option 1
#function to split ngrams
ngram <- function(n, text) {
        textcnt(text, method = "string",n=as.integer(n),
                split = "[[:space:][:digit:]]+",decreasing=T)
}

#unigrams
#determine time to create the ngram
system.time(unigrams <- ngram(1, training_text))
#time ~~ user: 2.63 / elapsed: 2.65

#function to clean and create the ngram_df
ngram_df_build <- function(ngram_object) {
ngram_df <- data.frame(term = names(ngram_object), frequency = unclass(ngram_object))
rm(ngram_object)
ngram_df$term <- as.character(ngram_df$term)
ngram_df$frequency <- as.numeric(ngram_df$frequency)
#removes mentionings of " <EOS> " (filler used to mark end of sentence/entries)
ngram_df <- ngram_df[which(ngram_df$term!="<eos>"),]

return(ngram_df)
}

#determine time to create the cleaned ngram_df
system.time(unigram_df <- ngram_df_build(unigrams))
#time ~~ user: .06 / elapsed: .06

#option 2:
library(tidytext) #for tokenization

#create df to use with dplyr manipulation below
training_text_df <- data.frame(text = training_text, stringsAsFactors = FALSE)

#function to tokenize and build the df
ngram_df_build <- function(ngram_text) {
        tokenized_all <- ngram_text %>%
                unnest_tokens(output = unigram, input = text) %>%
                #filter out end of sentence markers
                filter(!grepl(paste0("^", "endofsentencemarker", "$"), unigram)) %>%
                count(unigram) %>%
                select(unigram, n) %>%
                rename(frequency = n) %>%
                #arrange in descending order
                arrange(desc(frequency))
        
        #convert single tokens to DF
        unigram_df <- as.data.frame(tokenized_all)
        
        return(unigram_df)
}

#determine time to create the cleaned ngram_df
system.time(unigram_df <- ngram_df_build(training_text_df))

#time ~~ user: .2  / elapsed: .21

#<<<<<<<option 2 much faster>>>>>>>>>>

#ID the number of unigrams
length_unigrams <- length(unigram_df$unigram) 

#frequency table for Good-Turing smoothing
#captures the 'frequency of frequencies' (e.g. 100 instances of a word appearing 3 times)
unigram_frequency_table <- data.frame(unigram = table(unigram_df$frequency))

#save off file to load in later if needed
fwrite(unigram_df, "unigram_df.txt")
#data.table marked as false to ensure it's loaded only as DF
unigram_df <- fread("unigram_df.txt", data.table = FALSE)

#option 2 for bigrams
#n-gram building
#function to build the bigram dataframe
bigram_df_build <- function(ngram_text) {
        bigram_df <- ngram_text %>%
                unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
                tidyr::separate(bigram, c("word1", "word2"), sep = " ") %>%
                na.omit() %>%
                #filter out end of sentence markers
                filter(!grepl(paste0("^", "endofsentencemarker", "$"), word1)) %>%
                filter(!grepl(paste0("^", "endofsentencemarker", "$"), word2)) %>%
                mutate(term = paste(word1, word2, sep = " ")) %>%
                count(term) %>%
                rename(frequency = n) %>%
                #remove word1 and word2
                select(term, frequency) %>%
                #arrange in descending order
                arrange(desc(frequency))
        
        return(as.data.frame(bigram_df))
}

#build the DF of bigrams and calculate the runtime
system.time(bigram_df <- bigram_df_build(training_text_df))

#ID the number of bigrams
length_bigrams <- length(bigram_df$term) 

#frequency table for Good-Turing smoothing
#captures the 'frequency of frequencies' (e.g. 100 instances of a word appearing 3 times)
bigram_frequency_table <- data.frame(bigram = table(bigram_df$frequency))

#trigrams
trigram_df_build <- function(ngram_text) {
        trigram_df <- ngram_text %>%
                unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
                tidyr::separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
                na.omit() %>%
                #filter out end of sentence markers
                filter(!grepl(paste0("^", "endofsentencemarker", "$"), word1)) %>%
                filter(!grepl(paste0("^", "endofsentencemarker", "$"), word2)) %>%
                filter(!grepl(paste0("^", "endofsentencemarker", "$"), word3)) %>%
                mutate(term = paste(word1, word2, word3, sep = " ")) %>%
                count(term) %>%
                rename(frequency = n) %>%
                #remove word1/2/3
                select(term, frequency) %>%
                #arrange in descending order
                arrange(desc(frequency))

        return(as.data.frame(trigram_df))
}

#build the DF of trigrams and calculate the runtime
system.time(trigram_df <- trigram_df_build(training_text_df))

#ID the number of trigrams
length_trigrams <- length(trigram_df$trigram) 

#frequency table for Good-Turing smoothing
#captures the 'frequency of frequencies' (e.g. 100 instances of a word appearing 3 times)
trigram_frequency_table <- data.frame(trigram = table(trigram_df$frequency))

#quadgrams
quadgram_df_build <- function(ngram_text) {
        quadgram_df <- ngram_text %>%
                unnest_tokens(quadgram, text, token = "ngrams", n = 4) %>%
                tidyr::separate(quadgram, c("word1", "word2", "word3", "word4"), sep = " ") %>%
                na.omit() %>%
                #filter out end of sentence markers
                filter(!grepl(paste0("^", "endofsentencemarker", "$"), word1)) %>%
                filter(!grepl(paste0("^", "endofsentencemarker", "$"), word2)) %>%
                filter(!grepl(paste0("^", "endofsentencemarker", "$"), word3)) %>%
                filter(!grepl(paste0("^", "endofsentencemarker", "$"), word4)) %>%
                mutate(term = paste(word1, word2, word3, word4, sep = " ")) %>%
                count(term) %>%
                rename(frequency = n) %>%
                #remove word1/2/3/4
                select(term, frequency) %>%
                #arrange in descending order
                arrange(desc(frequency))
        
        return(as.data.frame(quadgram_df))
}

#build the DF of quadgrams and calculate the runtime
system.time(quadgram_df <- quadgram_df_build(training_text_df))

#ID the number of quadgrams
length_quadgram <- length(quadgram_df$quadgram) 

#frequency table for Good-Turing smoothing
#captures the 'frequency of frequencies' (e.g. 100 instances of a word appearing 3 times)
quadgram_frequency_table <- data.frame(quadgram = table(quadgram_df$frequency))