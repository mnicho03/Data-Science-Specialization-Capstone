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