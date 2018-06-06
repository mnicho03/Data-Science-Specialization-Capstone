
function(input, output, session) {
        
        #output the user text
        output$predicted_next_word <- renderText({
                
                #update page only when refresh button is selected
                if (input$startup == 0) {
                        return()
                }  else {

                #validate there is some user input prior to evaluating
                validate(
                        need(input$user_input != '', 'Please begin typing above...')
                )
                
                #run the user input through the following functions
                #clean the text
                processing_text <- userInput_cleaner(input$user_input)
                #determine which ngram to predict against
                #creates two vars [target_df & ngram_input]
                set_df_and_ngram(processing_text)
                
                # determine if target_DF identified above will have a matching ngram
                # if no match exists, search through suceeding (smaller) ngram DF's until a matching ngram exists
                target_df <- decide_df(ngram_input)

                #run the prediction function and output the predictions
                #create var for the most likely prediction
                next_word <- next_word_Prediction(ngram_input, target_df)
                
                # return predicted word
                return(next_word)
                 }
        })
        
        #plot to show histogram of top_10 most likely predictions based on user input
        #consider including highlight for the predicted word
        output$top10_hist <- renderPlot({
                
                #validate there is some user input prior to evaluating
                validate(
                        need(input$user_input != '', 'User Input Required')
                )
                
                #update page only when refresh button is selected
                if (input$startup == 0) {
                        return()
                }  else {
                        
                        ggplot(predictions, aes(x = reorder(predicted_word, frequency), y = Probability, fill = Probability == max(Probability), color = Probability == max(Probability))) +
                                geom_text(aes(label = round(Probability, 3)), color = "black", hjust = -0.07, size = 3) +
                        geom_bar(stat = "identity", alpha = .95) +
                        #highlight only the predicted value red with a gold border
                        scale_fill_manual(values = c("lightsteelblue2", "firebrick3")) +
                        scale_color_manual(values = c("white", "goldenrod")) +
                                
                        labs(y = "", x = "", title = "Next Word Predictions", subtitle = "Includes (at most) top 10 most probable next words based on the model") +
                        coord_flip() +
                        theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
                        
                }
        })
        
        #HTML text to display beneath the histogram
        output$top10_hist_text <- renderUI({
                
                #update page only when refresh button is selected
                if (input$startup == 0) {
                        return()
                }  else {
                        
                #full text description w/ breaks between bullets
                HTML("<ul><li>Horizontal bar chart displaying (up to) the top 10 most likely predictions based on the model. The predicted word, as seen above in the top left section of the page, will have a distinguishing red fill.
                        </li><li>
                     If a tie occurs, the model will randomly select a word and no red fill will be applied. These probabilities are based on millions of text string patterns analyzed from SwiftKey, which included blog posts, news articles, and tweets.
                        </li><li>
                     If the user enters text which is completely unknown to the model, the top 10 most likely words which occurred throughout the entire Swiftkey corpus will be displayed.</li></ul>")    
                }
                })
        
        #obtain general information
        output$general_info_table <- function() {
                
                #validate there is some user input prior to evaluating
                validate(
                        need(input$user_input != '', 'User Input Required')
                )
                
                #update page only when refresh button is selected
                if (input$startup == 0) {
                        return()
                }  else {
                        
                #ensure text box not blank 
                req(input$user_input)
                
                #what user input is being evaluated?
                User_Input <- input$user_input
                #which ngram is being used to predict? (often will be the same as above)
                Ngram_Evaluated <- ngram_input
                #which ngram data frame is being examined (simply count the number of words in the ngram input)
                Ngram_DF_Evaluated <- if(str_count(Ngram_Evaluated) == 3) {         
                        "Quadgrams"
                }       else if(str_count(Ngram_Evaluated) == 2) {
                        "Trigrams"
                }       else if(str_count(Ngram_Evaluated) == 1) {
                        "Bigrams"
                }       else
                        "Unigrams"
                
                #number of total ngrams possibilities in that group
                Total_Ngram_Possibilities <- length(target_df$ngram)
                
                #how many potential matches were found in the corpus to predict based on?
                Matches_Identified <- ngram_matches
                
                #general info table
                general_info_df <- data.frame(
                        Prediction_Items = c("User Input", "N-gram Evaluated", "N-gram DF Evaluated",  "Total N-gram Possibilities", "Matches Identified"),
                        Current_Evaluation_Information = c(User_Input, Ngram_Evaluated, Ngram_DF_Evaluated, Total_Ngram_Possibilities, Matches_Identified)
                ) 
                
                general_info_df %>% 
                #print pretty table of the above
                kable("html", align = "c", escape = F) %>%
                        kable_styling("striped", full_width = F) %>%
                        column_spec(1, bold = TRUE, border_right = T) %>%
                        column_spec(2, italic = TRUE, background = "lightblue") %>%
                        footnote(number = "'N-gram Evaluated' will often equal 'User Input'",
                                 symbol = "'Total N-gram Possibilities' shows the total number of N-grams of this size in the corpus",
                                 number_title = "N-Gram Note: ", symbol_title = "Possibilities vs Identified: ",
                                 footnote_as_chunk = T)
                
                }}
        
        #HTML text to display beneath the general info table
        output$general_info_table_text <- renderUI({
                
                #update page only when refresh button is selected
                if (input$startup == 0) {
                        return()
                }  else {
                        
                        #full text description w/ breaks between bullets
                        HTML("<ul><li>User Input: the exact text entered by the user in the box above
                             </li><li>
                             N-gram Evaluated: after a thorough text cleanse, removing things like capital letters, special characters, and other random punctuation, the n-gram (continuous sequence of words of 'n' length) applied to the model
                             </li><li>
                             N-gram DF Evaluated: data frame evaluated based on the number of words applied to the prediction ~ ptions include unigrams, bigrams, trigrams, and quadgrams
                             </li><li>
                             Total N-gram Possibilities: the total number of n-grams which exist based on the 'N-gram DF Evaluated' above
                             </li><li>
                             Matches Identified: number of n-grams identified which match the 'N-gram Evaluated' above ~ if 0 matches are found, the most common of all words will be predicted by the model
                             </li></ul>")
                }
        })
        
        #HTML text for an example of the prediction and key words
        #HTML text to display beneath the general info table
        output$example_text <- renderUI({
                
                #example group
                HTML(paste("<b>", "User Input: ", "</b>"), 
                     #adds line break between user input and prediction process
                     paste("'THIS IS a mighty cousy home, and'", 
                           "<b>", "Prediction Process: ", "</b>", sep = "<br/>"),
                     "<ol><li>
                        Model will scan the user input string and clean the text to 'this a mighty cousy home and'.
                        </li><li>
                        Model will determine the number of words in the string. In this case, the string exceeds 3 
words, which is the maximum amount of words the model will consider to make a prediction.
                        </li><li>
                        The last 3 words ('cousy home and') will be used to make the prediction.
                        </li><li>
                        The model will look through all previously encountered strings of 'cousy home and' in its memory and return the word that most often follows this text.
                        </li><li>
                        To no surprise, the model has never seen this exact string. The model will then strip the most recent 2 words ('home and') and return the word that most often follows this text. 
=                        </li><li>
                        Here, the predicted output would be 'XXXXXXXXXXXX.
                        </li></ol>",
                "<ul><li>
                If necessary, the model would have checked the most recent 1 word if the last 2 words did not have any matches, and if even the most recent 1 word did not have a match, the most common of all words seen by the model (regardless of past text) would be the predicted output.
                             </ul></li>",

                #key terms section
                paste("<b>", "Key Terms: ", "</b>"),
                "<ul><li>
                N-gram: (linguistics) a contiguous sequence of n items from a given sequence of text or speech [in the 
context of this model: a sequence of n words]
                </ul></li>
                <ul><li>
                Corpus: a collection of written texts, especially the entire works of a particular author or a body of 
writing on a particular subject [in the context of this model: a collection of blogs, news articles, and 
                twitter posts]
                </ul></li>"
                        )
                })
        
        #footer with Linkedin / Github links
        LinkedIn <- a("LinkedIn", href="https://www.linkedin.com/in/michaelnichols3/")

        output$LinkedIn_link <- renderUI({
                tagList("Connect with me on ", LinkedIn, ".")
        })

        GitHub <- a("GitHub Repository", href="https://github.com/mnicho03/Data-Science-Specialization-Capstone")
        
        output$GitHub_link <- renderUI({
                tagList("Access all related code and files for this application in my ", GitHub, ".")
        })
        
        
        
}
