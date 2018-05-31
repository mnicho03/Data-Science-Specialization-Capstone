# Generate Good-Turing matrix for prediction model
# build empty matrix
GTCount<-matrix(c(seq(0,6,1),rep(0,21)),nrow=7,ncol=4,
                dimnames = list(c(seq(0,6,1)),c("count","unigram","bigram","trigram")))

#unigrams
#Calculate probability of unseen unigram as per Jurafsky and Martin
GTCount[1,2] <- unigram_frequency_table[1,2]
GTCount[2:7,2] <- unigram_frequency_table[1:6,2]

kFactor <- 6*GTCount[7,2] / GTCount[2,2] # for k = 5
for (c in 0:5){
        num<-((c+1)*GTCount[c+2,2]/GTCount[c+1,2])-(c)*kFactor
        den<- 1-kFactor
        GTCount[c+1,2]<-num/den
}

#bigrams
#Calculate probability of unseen bigram as per Jurafsky and Martin
GTCount[1,3] <- length_unigrams^2 - length_bigrams
GTCount[2:7,3] <- bigram_frequency_table[1:6,2]

kFactor <- 6*GTCount[7,3]/GTCount[2,3] # for k = 5
for (c in 0:5){
        num<-(c+1)*GTCount[c+2,3]/GTCount[c+1,3]-(c)*kFactor
        den<- 1-kFactor
        GTCount[c+1,3]<-num/den
}

#trigrams
# Calculate probability of unseen trigram as per Jurafsky and Martin
GTCount[1,4] <- length_unigrams^3 - length_trigrams
GTCount[2,4] <- length_trigrams
GTCount[3:7,4] <- trigram_frequency_table[1:5,2]

kFactor <- 6*GTCount[7,4]/GTCount[2,4] # for k = 5
for (c in 0:5){
        num<-(c+1)*GTCount[c+2,4]/GTCount[c+1,4]-(c)*kFactor
        den<- 1-kFactor
        GTCount[c+1,4]<-num/den
}

#prediction model
#read in prepared  datasets and ensure data types are correct

