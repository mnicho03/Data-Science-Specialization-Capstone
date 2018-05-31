
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
unigram <- ngram(1, training_text)
unigram_df <- data.frame(term = names(unigram), frequency = unclass(unigram))
rm(unigram)
unigram_df$term <- as.character(unigram_df$term)
unigram_df$frequency <- as.numeric(unigram_df$frequency)
#removes mentionings of " <EOS> " (filler used to mark end of sentence/entries)
unigram_df <- unigram_df[which(unigram_df$term!="<eos>"),]

#ID the number of unigrams
length_unigrams <- length(unigram_df$term) 

#frequency table for Good-Turing smoothing
#captures the 'frequency of frequencies' (e.g. 100 instances of a word appearing 3 times)
unigram_frequency_table <- data.frame(unigram = table(unigram_df$frequency))

#bigrams
bigram <- ngram(2, training_text)
names(bigram) <- gsub("^\'","",names(bigram))        
bigram_df <- data.frame(term = names(bigram), frequency = unclass(bigram))
names(bigram_df) <- c("term","frequency")

#removes mentionings of " <EOS> " (filler used to mark end of sentence/entries)
eosTag <- grepl("<eos>", bigram_df$term)
bigram_df <- bigram_df[!eosTag,]

#ID the number of bigrams
length_bigrams <- length(bigram_df$term) 

#frequency table for Good-Turing smoothing
#captures the 'frequency of frequencies' (e.g. 100 instances of a word appearing 3 times)
bigram_frequency_table <- data.frame(bigram = table(bigram_df$frequency))

#trigrams
trigram <- ngram(3, training_text)
names(trigram) <- gsub("^\'","",names(trigram))
trigram_df <- data.frame(term = names(trigram), frequency = unclass(trigram))
names(trigram_df) <- c("term","frequency")

#removes mentionings of " <EOS> " (filler used to mark end of sentence/entries)
eosTag <-grepl("<eos>", trigram_df$term)
trigram_df <- trigram_df[!eosTag,]

#ID the number of trigrams
length_trigrams <- length(trigram_df$term) 

#frequency table for Good-Turing smoothing
#captures the 'frequency of frequencies' (e.g. 100 instances of a word appearing 3 times)
trigram_frequency_table <- data.frame(trigram = table(trigram_df$frequency))

#quadgrams
quadgram <- ngram(4, training_text)
names(quadgram) <- gsub("^\'","",names(quadgram))
quadgram_df <- data.frame(term = names(quadgram), frequency = unclass(quadgram))
names(quadgram_df) <- c("term","frequency")

#removes mentionings of " <EOS> " (filler used to mark end of sentence/entries)
eosTag <-grepl("<eos>", quadgram_df$term)
quadgram_df <- quadgram_df[!eosTag,]

#ID the number of quadgrams
length_quadgram <- length(quadgram_df$term) 

#frequency table for Good-Turing smoothing
#captures the 'frequency of frequencies' (e.g. 100 instances of a word appearing 3 times)
quadgram_frequency_table <- data.frame(quadgram = table(quadgram_df$frequency))

#convert all ngrams to character (currently factors)
unigram_df$unigram <- as.character(unigram_df$unigram)
bigram_df$bigram <- as.character(bigram_df$bigram)
trigram_df$trigram <- as.character(trigram_df$trigram)
quadgram_df$quadgram <- as.character(quadgram_df$quadgram)

#create new variable (predicted_word) for each ngram DF
#for unigram, this will be the same word (this DF will only be used when there is no match within the larger ngram DF's)
unigram_df$predicted_word <- unigram_df$unigram
#for the remaining ngrams, predicted_word = final word in string
#word function comes from the stringr package
bigram_df$predicted_word <- word(bigram_df$bigram, -1)
#trigrams
trigram_df$predicted_word <- word(trigram_df$trigram, -1)
#quadgrams
quadgram_df$predicted_word <- word(quadgram_df$quadgram, -1)