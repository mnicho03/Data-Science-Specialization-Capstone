#set working directory
## one or the other
# setwd("S:/Documents/R/Data Science Capstone/final/en_US")
# setwd("C:/Users/mnicho03/Desktop/Capstone_Local")

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
library(kableExtra)
library(wordcloud)

#set seed for reproducibility
set.seed(16)

#need to first run 'GettingAndCleaning.R' file
#load in mini, profanity-free DF's
clean_blogs_df <- fread("clean_blogs_df.csv")
clean_news_df <- fread("clean_news_df.csv")
clean_twitter_df <- fread("clean_twitter_df.csv")

#tokenize the data frames (split each word from each document into its own row)
#also remove 'stopwords': extremely common words not valuable for an analysis (e.g. as, of, the , a, ..., etc.)

tokenized_blogs <- clean_blogs_df %>%
        unnest_tokens(output = word, input = text) %>%
        anti_join(get_stopwords())

tokenized_news <- clean_news_df %>%
        unnest_tokens(output = word, input = text) %>%
        anti_join(get_stopwords())

tokenized_twitter <- clean_twitter_df %>%
        unnest_tokens(output = word, input = text) %>%
        anti_join(get_stopwords())

# simple count of words per group (excluding stopwords)
CommonWords_blogs <- tokenized_blogs %>%
        count(word, sort = TRUE) %>%
        mutate(file = "Blogs") %>%
        top_n(n = 10, wt = n)

CommonWords_news <- tokenized_news %>%
        count(word, sort = TRUE) %>%
        mutate(file = "News") %>%
        top_n(n = 10, wt = n)

CommonWords_twitter <- tokenized_twitter %>%
        count(word, sort = TRUE) %>%
        mutate(file = "Twitter") %>%
        top_n(n = 10, wt = n)

#create full DF to show info across files
CommonWords_all <- rbind.data.frame(CommonWords_blogs, CommonWords_news, CommonWords_twitter)

#plot top 10 most common words across the files
ggplot(CommonWords_all, aes(x = reorder(word,desc(n)), y = n, fill = n)) +
        geom_bar(stat = "identity", alpha = .95) +
        scale_fill_gradient(low = "gray75", high = "darkslategray4") +
        labs(y = "", x = "", title = "Top 10 Most Common Words by File") +
        facet_grid(.~file, scales = "free_x") +
        coord_flip() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

#create DF to show proportion of words across file types
#to reduce to reasonable size, filtering on only words with 25+ occurrences and proportion of words > 1/1000
frequency <- bind_rows(mutate(tokenized_blogs, file = "Blogs"),
                       mutate(tokenized_news, file = "News"), 
                       mutate(tokenized_twitter, file = "Twitter")) %>%
        count(file, word) %>%
        group_by(file) %>%
        mutate(proportion = n / sum(n)) %>%
        tidyr::spread(file, proportion) %>% 
        tidyr::gather(file, proportion, `Blogs`,`News`, `Twitter`) %>%
        filter(n >= 25 & proportion >= .001) %>%
        arrange(desc(proportion))

#plot to show word frequencies
ggplot(frequency, aes(x = proportion, y = n, color = file)) +
        geom_point() +
        geom_text(aes(label = word), check_overlap = TRUE, vjust = -.25, hjust = .5, show.legend = FALSE) +
        scale_y_continuous(limits = c(0000, 8000)) +
        labs(title = "Word Frequencies", y = "Count", x = "Proportion") 

#n-gram analysis
#bigrams
bigram_Analysis <- function(text, filetype) {
        text %>%
                unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
                tidyr::separate(bigram, c("word1", "word2"), sep = " ") %>%
                na.omit() %>%
                filter(!word1 %in% stop_words$word,
                       !word2 %in% stop_words$word) %>%
                count(word1, word2, sort = TRUE) %>%
                top_n(n = 10, wt = n) %>% #selects only top 10 values
                slice(row_number(1:10)) %>% #prevents ties (i.e. several bigrams with the 10th highest value)
                mutate(bigram = paste(word1, word2, sep = " ")) %>%
                mutate(file = filetype)
}

blogs_bigram <- bigram_Analysis(clean_blogs_df, "Blogs")
news_bigram <- bigram_Analysis(clean_news_df, "News")
twitter_bigram <- bigram_Analysis(clean_twitter_df, "Twitter")

#combine for easy visualizations
bigram_all <- as.data.frame(rbind.data.frame(blogs_bigram, news_bigram, twitter_bigram))

#trigrams
trigram_Analysis <- function(text, filetype) {
        text %>%
                unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
                tidyr::separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
                na.omit() %>%
                filter(!word1 %in% stop_words$word,
                       !word2 %in% stop_words$word,
                       !word3 %in% stop_words$word) %>%
                count(word1, word2, word3, sort = TRUE) %>%
                top_n(n = 10, wt = n) %>%
                slice(row_number(1:10)) %>%
                mutate(trigram = paste(word1, word2, word3, sep = " ")) %>%
                mutate(file = filetype)
}

blogs_trigram <- trigram_Analysis(clean_blogs_df, "Blogs")
news_trigram <- trigram_Analysis(clean_news_df, "News")
twitter_trigram <- trigram_Analysis(clean_twitter_df, "Twitter")

#combine for easy visualizations
trigram_all <- as.data.frame(rbind.data.frame(blogs_trigram, news_trigram, twitter_trigram))

#visualize the bigrams & trigrams
ngram_plot <- function(data, num_gram) {
        label <- as.character(str_to_title(num_gram))
        
        ggplot(data, aes_string(x = num_gram)) +
                aes(y = n, fill = as.factor(file)) +
                geom_bar(stat = "identity") +
                facet_grid(file~., scales = "free_y") +
                coord_flip() +
                labs(title = paste("Most Common", label, collapse = ""), y = label, x = "Occurences", fill = "File") +
                theme(axis.text.y = element_text(size = 7))
}

top_bigrams <- ngram_plot(bigram_all, "bigram")
top_trigrams <- ngram_plot(trigram_all, "trigram")

grid.arrange(top_bigrams, top_trigrams, ncol = 2)

########################CURRENT ENDPOINT IN KNITR FILE##################################

#then look at term frequency (what # of terms consist of certain % of words)
runningTotal <- bind_rows(mutate(tokenized_blogs, file = "Blogs"),
                          mutate(tokenized_news, file = "News"), 
                          mutate(tokenized_twitter, file = "Twitter")) %>%
        count(file, word) %>%
        group_by(file) %>%
        mutate(proportion = n / sum(n)) %>%
        tidyr::spread(file, proportion) %>% 
        tidyr::gather(file, proportion, `Blogs`,`News`, `Twitter`) %>%
        na.omit() %>%
        arrange(desc(proportion)) %>%
        group_by(file) %>%
        mutate(cumsum = cumsum(proportion))

#plot of concentration of certain words (only shows terms that make up .50 of the total)
#plots with higher bars on the left have more unique terms
runningTotal_half <- runningTotal %>%
        filter(cumsum <= .5)

ggplot(runningTotal_half, aes(proportion, fill = as.factor(file))) +
        geom_histogram() +
        xlim(NA, 0.015) +
        facet_grid(.~file)

#table to show number of terms required to hit different percentages of total terms
#unique words for a frequency sorted dictionary to cover X% of all word instances
#e.g. 90% of terms from the Blogs text can be encompassed with only 16,035 terms 
percent_of_total_words <- runningTotal %>%
        select(file, cumsum) %>%
        group_by(file) %>%
        mutate(terms_to_percent = row_number()) %>% #number of terms required to hit the respective % of the total terms
        mutate(cumsum = round(cumsum, 2)) %>%
        filter(cumsum %in% c(0.10, 0.30, 0.50, 0.70, 0.90, 1.00)) %>%
        group_by(file, cumsum) %>%
        mutate(the_rank  = rank(-terms_to_percent, ties.method = "max")) %>%
        filter(the_rank == 1) %>% select(-the_rank) %>%
        arrange(file) %>%
        tidyr::spread(cumsum, terms_to_percent)

#data frame showing unique words required to hit each percentage of total words per text
percent_of_total_words <- as.data.frame(percent_of_total_words)

#print pretty table of the above
kableExtra::kable(percent_of_total_words, "html") %>%
        kable_styling("striped") %>%
        add_header_above(c(" ", "Unique Terms to Cover X Percent of Total Words" = 6))

#final visualization likely to be used with Shiny app: wordcloud
#create one aggregate tokenized file -- first need to include value for file type in each DF
#this DF includes the entire list of words from all files: this will be more useful than the above DF's used for initial plots
tokenized_all <- as.data.frame(rbind.data.frame(tokenized_blogs, tokenized_news, tokenized_twitter))

#create table of all the words
wordTable <- table(tokenized_all$word)

#wordcloud of all the words by frequency (reducing the total number of included words to a more palettable portion)
wordcloud(names(wordTable), as.numeric(wordTable), min.freq = 100, max.words = 500, colors = brewer.pal(8, "Dark2"))


#wordclouds for 2/3-gram analysis
clean_all <- as.data.frame(rbind.data.frame(clean_blogs_df, clean_news_df, clean_twitter_df))

#bigrams
#create DF of all bigrams and a count of each
bigram_all_unlimited <- 
        clean_all %>%
                unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
                tidyr::separate(bigram, c("word1", "word2"), sep = " ") %>%
                na.omit() %>%
                filter(!word1 %in% stop_words$word,
                       !word2 %in% stop_words$word) %>%
                count(word1, word2, sort = TRUE) %>%
                mutate(bigram = paste(word1, word2, sep = " "))

#bigram wordcloud
wordcloud(bigram_all_unlimited$bigram, bigram_all_unlimited$n, min.freq = 50, max.words = 250, colors = brewer.pal(8, "Dark2"), scale = c(2, .2))


#trigrams
#create DF of all trigrams and a count of each
trigram_all_unlimited <- 
        clean_all %>%
        unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
        tidyr::separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
        na.omit() %>%
        filter(!word1 %in% stop_words$word,
               !word2 %in% stop_words$word,
               !word3 %in% stop_words$word) %>%
        count(word1, word2, word3, sort = TRUE) %>%
        mutate(trigram = paste(word1, word2, word3, sep = " "))

#trigram wordcloud
wordcloud(trigram_all_unlimited$trigram, trigram_all_unlimited$n, min.freq = 10, max.words = 100, colors = brewer.pal(8, "Dark2"), scale = c(2, .2))


################ Modeling ################









