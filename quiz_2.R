# #quiz text
# prob1 <- "When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd"
# prob2 <- "Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his"
# prob3 <- "I'd give anything to see arctic monkeys this"
# prob4 <- "Talking to your mom has the same effect as a hug and helps reduce your"
# prob5 <- "When you were in Holland you were like 1 inch away from me but you hadn't time to take a"
# prob6 <- "I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the"
# prob7 <- "I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each"
# prob8 <- "Every inch of you is perfect from the bottom to the"
# prob9 <- "I'm thankful my childhood was filled with imagination and bruises from playing"
# prob10 <- "I like how the same people are in almost all of Adam Sandler's"
        
#user input (sample used here: will be dynamic in final model)
input <- prob10

#function to clean user input: matches similar text cleansing done to the corpus to ensure a match can be found
#save the userInput to be run in the model   
userInput <- userInput_cleaner(input)

#determine # of words in the input
userInput_words <- unlist(str_split(userInput," "))
userInput_words_length <- length(userInput_words)

#determine which ngram to predict against
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
#two outputs - (up to) the top 10 most likely predictions & the most likely prediction
next_word_Prediction(ngram_input, target_df)


#run for each quiz question
#prob1 - #give = 5th most likely
#prob2 - #birthday / cocktail / dream / heritage (no quiz options)
#prob3 - #morning = 4th most likely (weekend = 5th)
#prob4 - #stress = 10th most likely
#prob5 - #picture = 1st most likely (look = 4th)
#prob6 - #case = 3rd most likely (matter = 4th)
#prob7 - #others / category / case / day... (no quiz options)
#prob8 - #top = 1st most likely
#prob9 - #hours / together / in... (no quiz options)
#prob10 - #more (no quiz options)


