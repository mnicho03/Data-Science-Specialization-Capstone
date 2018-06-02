# frequencies of frequencies of various N-Grams

par(mfrow = c(1,3))

library(scales)   
unigram_frequency_table$unigram.Var1 <- (as.integer(unigram_frequency_table$unigram.Var1))
bigram_frequency_table$bigram.Var1 <- as.integer(bigram_frequency_table$bigram.Var1)
trigram_frequency_table$trigram.Var1 <- as.integer(trigram_frequency_table$trigram.Var1)

scatter.smooth(log10(unigram_frequency_table$Unigram.Var1), log10(unigram_frequency_table$unigram.Freq),
               ylab="log10 (freqency of freqency)",xlab="log10 (unigram count)",
               col=alpha("black",0.1),ylim=c(.000001,7),xlim=c(.000001,6))

scatter.smooth(log10(bigram_frequency_table$bigram.Var1), log10(bigram_frequency_table$bigram.Freq),
               ylab="log10 (freqency of freqency)", xlab="log10 (bigram count)",
               col=alpha("black",0.1),ylim=c(.000001,7),xlim=c(.000001,6))

scatter.smooth(log10(trigram_frequency_table$trigram.Var1), log10(trigram_frequency_table$trigram.Freq),
               ylab="log10(freqency of freqency)", xlab="log10 (trigram count)",
               col=alpha("black",0.1),ylim=c(.000001,7),xlim=c(.000001,6))


######final visualizations for Shiny app

####visualization #1
#word cloud
#wordcloud of all potential predicted outputs 
#must be dynamic to include output regardless of ngram
library(wordcloud)

#need to update the DF to show all (not just top 10) possible outputs
wordcloud_all <- wordcloud(words = predictions_all$predicted_word, freq = predictions_all$frequency, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"), scale = c(3, .3))



####visualization #2
#histogram to show top 10 most likely predictions
#flipped, with markers for probability (and shaded accordingly)

#plot top 10 most common words across the files
library(ggplot2)

# ggplot(predictions, aes(x = reorder(predicted_word, desc(frequency)), y = Probability, fill = Probability)) +
top10_hist <- ggplot(predictions, aes(x = reorder(predicted_word, frequency), y = Probability, fill = Probability)) +
        geom_text(aes(label = round(Probability, 3)), hjust = -0.03, size=3) +
        geom_bar(stat = "identity", alpha = .95) +
        scale_fill_gradient(low = "cornsilk3", high = "dodgerblue3") +
        labs(y = "", x = "", title = "Next Word Predictions", subtitle = "Includes (at most) Top 10 Most Probable") +
        coord_flip() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")


####visualization #3
#obtain general information
library(kableExtra)
library(formattable)

#what user input is being evaluated?
User_Input <- userInput
#which ngram is being used to predict? (often will be the same as above)
Ngram_Evaluated <- ngram_input

#which ngram data frame is being examined
Ngram_DF_Evaluated <- if(length_quadgram == length(target_df$ngram)) {         "Quadgrams"
}       else if(length_trigrams == length(target_df$ngram)) {
        "Trigrams"
}       else if(length_bigrams == length(target_df$ngram)) {
        "Bigrams"
}       else
        "Unigrams"

#number of total ngrams possibilities in that group
Total_Ngram_Possibilities <- length(target_df$ngram)

#how many potential matches were found in the corpus to predict based on?
Matches_Identified <- ngram_matches

#general info table
general_info <- data.frame(
        Prediction_Items = c("User Input", paste0("Ngram Evaluated", footnote_marker_number(1)), "Ngram DF Evaluated",  paste0("Total Ngram Possibilities", footnote_marker_symbol(1)), "Matches Identified"),
        Current_Evaluation_Information = c(User_Input, Ngram_Evaluated, Ngram_DF_Evaluated, Total_Ngram_Possibilities, Matches_Identified)
)

#print pretty table of the above
kableExtra::kable(general_info, "html", align = "c", escape = F) %>%
        kable_styling("striped", full_width = F) %>%
        column_spec(1, bold = TRUE, border_right = T) %>%
        column_spec(2, italic = TRUE, background = "palegreen") %>%
        footnote(number = "'Ngram Evaluated' will often equal 'User Input'",
                 symbol = "'Total Ngram Possibilities' shows the total number of Ngrams of this size in the corpus",
                 number_title = "NGram Note: ", symbol_title = "Possibilities vs Identified: ",
                 footnote_as_chunk = T)