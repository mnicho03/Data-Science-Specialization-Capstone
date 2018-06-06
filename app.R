#app.R
#loads in all necessary information for the moment the Shiny app gets called by a new user
#script includes ui.R and server.R content

#set working directory
setwd("C:/Users/mnicho03/Desktop/Capstone_Local")

#load libraries
library(data.table) # for fast read / write
library(stringr) # for string manipulation
library(dplyr) # for DF manipulation
library(wordcloud) # for visuals
library(ggplot2) # for visuals
library(kableExtra) # for visuals (cleaner tables)
library(shiny)
library(shinythemes) # for sleeker Shiny background 
library(shinycssloaders) # for plot animations

#set seed for consistency
set.seed(16)

#load in ngram data - all of which created within 'PredictionModel_final.R': 10% random sample of the entire corpus
#this was run separately and saved off via fwrite to save time
#data.table marked as false to ensure it's loaded only as DF
unigram_df <- fread("unigram_df.txt", data.table = FALSE)
bigram_df <- fread("bigram_df.txt", data.table = FALSE)
trigram_df <- fread("trigram_df.txt", data.table = FALSE)
quadgram_df <- fread("quadgram_df.txt", data.table = FALSE)

#ID the number of ngrams
length_unigrams <- length(unigram_df$ngram) 
length_bigrams <- length(bigram_df$ngram) 
length_trigrams <- length(trigram_df$ngram) 
length_quadgram <- length(quadgram_df$ngram) 

#load all functions
#user input (sample used here: will be dynamic in final model)
input <- "enter text here"

#function to clean user input: matches similar text cleansing done to the corpus to ensure a match can be found
userInput_cleaner <- function(input){
        #clean input text
        #convert all words to lowercase
        input_cleaning <- str_to_lower(input)
        
        # separate hyphenated and slashed words
        input_cleaning <- gsub("-", ' ', input_cleaning)
        input_cleaning <- gsub("/", ' ', input_cleaning)
        # removes errant close brackets starting a word
        input_cleaning <- gsub(">[a-z]", ' ', input_cleaning)
        
        # removes any remaining <> brackets
        input_cleaning <- gsub("<>", ' ', input_cleaning)
        
        # remove apostrophe but retain the words (e.g. "don't" to "dont")
        input_cleaning <- gsub("'", '', input_cleaning)
        
        #remove punctuation
        input_cleaning <- gsub('[[:punct:] ]+', ' ', input_cleaning)
        
        #remove numbers
        input_cleaning <- gsub("[0-9]+", "", input_cleaning)
        
        #remove extra whitespace
        input_cleaning <- gsub("\\s+", " ", input_cleaning)
        input_clean <- str_trim(input_cleaning)
        
        #return the cleaned user input
        return(input_clean)
        
}

# #save the userInput to be run in the model
userInput <- userInput_cleaner(input)

#determine which ngram to predict against
set_df_and_ngram <- function(userInput) {
        
        
        #determine # of words in the input - save globablly
        userInput_words <- unlist(str_split(userInput," "))
        userInput_words_length <- length(userInput_words)
        
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

# #run the function
set_df_and_ngram(userInput)

# determine if target_DF identified above will have a matching ngram
# if no match exists, search through suceeding (smaller) ngram DF's until a matching ngram exists
decide_df <- function(ngram_input) {
        #variable for number of matches
        ngram_matches <- sum(target_df$preceding == ngram_input)
        
        #for future revisions: the following code should be optimized
        #find if current target_DF and ngram_input have any matches
        #if matches != 0, this statement will not have any impact
        if(ngram_matches == 0) {
                #if matches == 0, set target_DF and ngram as n-1
                #step 1: determine current target_df
                #if we're looking for a bigram, set ngram & target_df and then there will be no further action
                if(length(target_df$ngram) == length(bigram_df$ngram)) {
                        
                        ngram_input <- ngram_input
                        target_df <- unigram_df
                        
                        ngram_matches <- 0
                        
                } else {
                        
                        #check to see if we're looking for a trigram
                        if(length(target_df$ngram) == length(trigram_df$ngram)) {
                                
                                #update ngram and target_df to n-1
                                ngram_input <- word(ngram_input, -1)
                                target_df <- bigram_df
                                
                                #rerun ngram_matches calculation
                                ngram_matches <- sum(target_df$preceding == ngram_input)
                                
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
        #ensure ngram_matches & ngram_input gets saved globally
        assign("ngram_matches", ngram_matches, envir = .GlobalEnv)
        assign("ngram_input", ngram_input, envir = .GlobalEnv)
        assign("target_df", target_df, envir = .GlobalEnv)
        
        
        #make the target_df what get's returned
        return(target_df)
}

# #run the function above to confirm the target_df
target_df <- decide_df(ngram_input)

#take the cleaned ngram and filter based on the user input - to show predicted word as well as top_10 predictions
next_word_Prediction <- function(ngram_input, target_df) {
        
        #if no matches can be found based on the user input: output top 10 most common words
        if (length(target_df$ngram) == length(unigram_df$ngram)) {
                predictions_all <- unigram_df %>%
                        # #remove stopwords from predictions
                        # filter(!predicted_word %in% stop_words$word) %>%
                        mutate(Probability = frequency / sum(frequency)) %>%
                        arrange(desc(Probability))
                
                predictions <- predictions_all %>%
                        top_n(n = 10, wt = Probability) %>%
                        arrange(desc(Probability)) %>%
                        slice(row_number(1:10)) #ensure 10 values max
                
                #convert to clean data frames
                predictions <- as.data.frame(predictions)
                predictions_all <- as.data.frame(predictions_all)
                
                #save predictions DF's to be used in data product visualizations
                assign("predictions", predictions, envir = .GlobalEnv)
                assign("predictions_all", predictions_all, envir = .GlobalEnv)
                
        } else {
        
        #gather predictions
        predictions_all <- target_df %>%
                #filter on exact matches (^ngram$)
                filter(grepl(paste0("^", ngram_input, "$"), preceding)) %>%
                # #remove stopwords from predictions
                # filter(!predicted_word %in% stop_words$word) %>%
                mutate(Probability = frequency / sum(frequency)) %>%
                arrange(desc(Probability))
        
        #save two separate DF's of the predictions (1 filtered to top 10 / 1 not)
        predictions <- predictions_all %>%
                top_n(n = 10, wt = Probability) %>%
                arrange(desc(Probability)) %>%
                slice(row_number(1:10)) #ensure 10 values max
        
        #convert to clean data frames
        predictions <- as.data.frame(predictions)
        predictions_all <- as.data.frame(predictions_all)
        
        #save predictions DF's to be used in data product visualizations
        assign("predictions", predictions, envir = .GlobalEnv)
        assign("predictions_all", predictions_all, envir = .GlobalEnv)
        }
        
        #print the most likely result only 
        # print(predictions)
        return(predictions$predicted_word[1])
}

#run the prediction function and output the predictions
#output the most likely prediction
next_word_Prediction(ngram_input, target_df)

#shiny ui
# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cosmo"),
                # Application title
                titlePanel("Predictive Text Model"),
                
                # author signature
                fluidRow(
                        column(12,
                               strong("Michael Nichols"),
                               br(),
                               em("6/6/2018")
                        )),
                
                #area for user input
                sidebarLayout(
                        sidebarPanel(
                                helpText("Insert a string or phrase of any length, and click submit to view the next word prediction."),
                                textInput("user_input", label = h5("User Input Box"), value = "Enter Text Here..."),
                                #button used to kickoff the predictions
                                actionButton("startup", "Initialize", icon("play-circle", lib = "font-awesome")),
                                hr(),
                                h5("Predicted Next Word: "), 
                                h5(textOutput("predicted_next_word"))),
                        
                        mainPanel(
                                h4("Natural Language Processing Predictive Text Model Utilizing Shiny"),
                                "Capstone project for the Johns Hopkins Data Science Specialization, in partnership with SwiftKey, a software company specializing in predictive keyboards applications.",
                                hr(),
                                h4("User Instructions:"),
                                "Type any string in the box on the left, and click the 'Initialize' button. The model will then return the predicted next word directly below. The tabs in the center of the page will also populate with updated information summarizing prediction details.",
                                h4("Model Notes:"),
                                "1: Please be patient: the model may take a few moments to load.",
                                br(),
                                "2: After clicking 'Initialize,' the model becomes dynamic and updates continuously.",
                                br(),
                                "3: If the text box is completely empty, you will receive a polite warning message and the visualizations below will disappear."
                        )
                ),
                
                fluidRow(
                        column(12,
                               #subsection for the three main visualizations
                               tabsetPanel(
                                       #tab panel 1                                       
                                       tabPanel("Top 10 Most Likely Predictions", withSpinner(plotOutput("top10_hist"), color = "red"),
                                                #html output of the full visualization description - must be called in this fashion in order to delay its appearance until the 'Initialize' button is selected
                                                htmlOutput("top10_hist_text")),        
                                       #tab panel 2
                                       tabPanel("Prediction Details", withSpinner(tableOutput("general_info_table"), color = "red"),
                                                #html output of the full visualization description - must be called in this fashion in order to delay its appearance until the 'Initialize' button is selected
                                                htmlOutput("general_info_table_text")),
                                       #tab panel 3                                        
                                       #example of how the prediction is made
                                       tabPanel("Example & Key Terms", withSpinner(htmlOutput("example_text"), color = "red")))),
                        
                        #final section / footer with links to career / portfolio sites
                        column(width = 12,
                               #add line break as a makeshift footer 
                               hr(),
                               #links to LinkedIn / Github
                               uiOutput("LinkedIn_link"),
                               uiOutput("GitHub_link"))
                        
                        
                )
)

#shiny server
server <- function(input, output, session) {
        
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
                
                #validate there is some user input prior to displaying
                validate(
                        need(input$user_input != '', '')
                )
                
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
                
                #validate there is some user input prior to displaying
                validate(
                        need(input$user_input != '', '')
                )
                
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



# Create Shiny object
shinyApp(ui = ui, server = server)
