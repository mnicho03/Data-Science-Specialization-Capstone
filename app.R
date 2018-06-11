#app.R
#loads in all necessary information for the moment the Shiny app gets called by a new user
#script includes ui.R and server.R content

##set working directory - unnecessary within published version
#setwd("C:/Users/mnicho03/Desktop/Capstone_Local/Predictive_Text_Application")

##load in ngram data - all of which created within 'PredictionModel_final.R': 5% random sample of the entire corpus ~~ this was the largest sample size i could create in order to fit within the Shiny free user size restrictions (1GB)
##this was run separately and saved off in a Data subfolder 
load('./data/ngram_data_05.RData')

#load libraries
library(stringr) # for string manipulation
library(dplyr) # for DF manipulation
library(ggplot2) # for visuals
library(kableExtra) # for visuals (cleaner tables)
library(shiny)
library(shinythemes) # for sleeker Shiny background 
library(shinycssloaders) # for plot animations

#load all functions
#user input (sample used here: will be dynamic in final model)
input <- "enter text here"

#load all functions from cleaning to prediction
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
# processing_text <- userInput_cleaner(input)

#creates two vars [target_df & ngram_input]
#determine which ngram to predict against
set_df_and_ngram <- function(userInput) {
        
        
        #determine # of words in the input - save globablly
        userInput_words <- unlist(str_split(userInput, " "))
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

# #run the function with the cleaned text
# set_df_and_ngram(processing_text)

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
# target_df <- decide_df(ngram_input)

#take the cleaned ngram and filter based on the user input - to show predicted word as well as top_10 predictions
set_top_predictions <- function(ngram_input, target_df) {
        
        #if no matches can be found based on the user input: output top 10 most common words
        if (length(target_df$ngram) == length(unigram_df$ngram)) {
                predictions <- unigram_df %>%
                        # #remove stopwords from predictions
                        # filter(!predicted_word %in% stop_words$word) %>%
                        mutate(Probability = frequency / sum(frequency)) %>%
                        arrange(desc(Probability))  %>%
                        #filter to top 10 only
                        top_n(n = 10, wt = Probability) %>%
                        arrange(desc(Probability)) %>%
                        slice(row_number(1:10)) #ensure 10 values max
                
                #convert to clean data frame
                predictions <- as.data.frame(predictions)
                
                
        } else {
                
                #gather predictions
                predictions <- target_df %>%
                        #filter on exact matches (^ngram$)
                        filter(grepl(paste0("^", ngram_input, "$"), preceding)) %>%
                        # #remove stopwords from predictions
                        # filter(!predicted_word %in% stop_words$word)
                        mutate(Probability = frequency / sum(frequency)) %>%
                        arrange(desc(Probability)) %>% 
                        #filter to top 10 only
                        top_n(n = 10, wt = Probability) %>%
                        arrange(desc(Probability)) %>%
                        slice(row_number(1:10)) #ensure 10 values max
                
                #convert to clean data frame
                predictions <- as.data.frame(predictions)
        }
        
        #save predictions DF to be used in data product visualizations
        #double check it saves properly
        predictions_df <- predictions
        
        #save the most 10 likely predictions
        assign("predictions_df", predictions, envir = .GlobalEnv)
}

# #run the function to capture the most likely predictions
# set_top_predictions(ngram_input, target_df)

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
                               em("6/8/2018")
                        )),
                
                #area for user input
                sidebarLayout(
                        sidebarPanel(
                                helpText("Insert a string or phrase of any length, and click refresh to view the next word prediction."),
                                textInput("user_input", label = h5("User Input Box"), value = "Enter Text Here..."),
                                #button used to kickoff the predictions
                                actionButton("refresh", "Refresh", icon("refresh", lib = "font-awesome")),
                                hr(),
                                h5("Predicted Next Word: "), 
                                h5(textOutput("predicted_next_word"))),
                        
                        mainPanel(
                                h4("Natural Language Processing Predictive Text Model Utilizing Shiny"),
                                "Capstone project for the Johns Hopkins Data Science Specialization, in partnership with SwiftKey, a software company specializing in predictive keyboard applications.",
                                hr(),
                                h4("User Instructions:"),
                                "Type any string in the box on the left, and click the 'Refresh' button. The model will then return the predicted next word directly below. The tabs in the center of the page will also populate with updated information summarizing prediction details.",
                                h4("Model Notes:"),
                                "1: Please be patient: the model may take a minute to fully load.",
                                br(),
                                "2: Click 'Refresh' upon editing to view the updated prediction and details.",
                                br(),
                                "3: If the text box is completely empty, you will receive a polite warning message and the visualizations below will disappear."
                        )
                ),
                fluidRow(
                        column(12,
                               #subsection for the three main visualizations
                               tabsetPanel(
                                       #tab panel 1
                                       tabPanel("Prediction Details", withSpinner(tableOutput("general_info_table"), color = "red"),
                                                #html output of the full visualization description - must be called in this fashion in order to delay its appearance until the 'Refresh' button is selected
                                                htmlOutput("general_info_table_text")),
                                       #tab panel 2
                                       tabPanel("Top 10 Most Likely Predictions", withSpinner(plotOutput("top10_hist"), color = "red"),
                                                #html output of the full visualization description - must be called in this fashion in order to delay its appearance until the 'Refresh' button is selected
                                                htmlOutput("top10_hist_text")),        
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


########## ui / server split ##################


#shiny server
server <- function(input, output, session) {
        
        #set user input as a reactive variable to use with visualizations
        #store the user_input when clicked
        user_input_reactive <- eventReactive(input$refresh, {
                input$user_input
        })
        
        #output the user text
        output$predicted_next_word <- renderText({
                
                #run after the Refresh button is selected
                if (input$refresh == 0) {
                        return()
                }  else {
                        
                        #validate there is some user input prior to evaluating
                        validate(
                                need(user_input_reactive() != '', 'Please begin typing above...')
                        )
                        
                        #run the user input through the following functions
                        #clean the text
                        processing_text <- userInput_cleaner(user_input_reactive())
                        
                        #creates two vars [target_df & ngram_input]
                        #determine which ngram to predict against
                        set_df_and_ngram(processing_text)
                        
                        # determine if target_DF identified above will have a matching ngram
                        # if no match exists, search through suceeding (smaller) ngram DF's until a matching ngram exists
                        target_df <- decide_df(ngram_input)
                        
                        #run the function and output the top 10 most likely predictions
                        #create global var for the most likely predictions (predictions_df)
                        
                        set_top_predictions(ngram_input, target_df)
                        
                        #print the most likely prediction
                        return(predictions_df$predicted_word[1])
                }
        })
        
        #plot to show histogram of top_10 most likely predictions based on user input
        output$top10_hist <- renderPlot({
                
                #validate there is some user input prior to evaluating
                validate(
                        need(user_input_reactive() != '', 'User Input Required')
                )
                
                #update page only when refresh button is selected
                if (input$refresh == 0) {
                        return()
                } else {
                        
                        #run the reactive user input through the following functions to store variables for the table                   #clean the text
                        processing_text <- userInput_cleaner(user_input_reactive())

                        #creates two vars [target_df & ngram_input]
                        #determine which ngram to predict against
                        set_df_and_ngram(processing_text)

                        # determine if target_DF identified above will have a matching ngram
                        # if no match exists, search through suceeding (smaller) ngram DF's until a matching ngram exists
                        target_df <- decide_df(ngram_input)

                        #run the function and output the top 10 most likely predictions
                        #create global var for the most likely predictions (predictions_df)

                        set_top_predictions(ngram_input, target_df)
                        
                        #create the plot
                        p <- ggplot(predictions_df, aes(x = reorder(predicted_word, frequency), y = Probability, fill = Probability == max(Probability), color = Probability == max(Probability))) +
                                geom_text(aes(label = round(Probability, 3)), color = "black", hjust = -0.07, size = 3) +
                                geom_bar(stat = "identity", alpha = .95) +
                                #highlight only the predicted value red with a gold border
                                scale_fill_manual(values = c("lightsteelblue2", "firebrick3")) +
                                scale_color_manual(values = c("white", "goldenrod")) +
                                labs(y = "", x = "", title = "Next Word Predictions", subtitle = "Includes (at most) the top 10 most probable next words based on the model") +
                                coord_flip() +
                                theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
                        #print the plot
                        print(p)
                        
                }
        })
        
        #HTML text to display beneath the histogram
        output$top10_hist_text <- renderUI({
                
                #validate there is some user input prior to displaying
                validate(
                        need(user_input_reactive() != '', '')
                )
                
                #run after the Refresh button is selected
                if (input$refresh == 0) {
                        return()
                }  else {
                        
                        #full text description w/ breaks between bullets
                        HTML("<ul><li>Horizontal bar chart displaying (up to) the top 10 most likely predictions based on the model. The predicted word, as seen above in the top left section of the page, will have a distinguishing red fill.
                             </li><li>
                             If a tie occurs, the model will randomly select a word and red fill will be applied to each word with the equivalent likelihood. These probabilities are based on millions of text string patterns analyzed from the SwiftKey corpus, including blog posts, news articles, and tweets.
                             </li><li>
                             If the user enters text which is completely unknown to the model, the top 10 most likely words which occurred throughout the entire Swiftkey corpus will be displayed.</li></ul>")    
                }
                })
        
        #obtain general information
        output$general_info_table <- function() {
                
                #validate there is some user input prior to evaluating
                validate(
                        need(user_input_reactive() != '', 'User Input Required')
                )
                
                #run after the Refresh button is selected
                if (input$refresh == 0) {
                        return()
                }  else {
                        
                        #ensure text box not blank 
                        req(user_input_reactive())
                        
                        #run the reactive user input through the following functions to store variables for the table
                        #clean the text
                        processing_text <- userInput_cleaner(user_input_reactive())

                        #creates two vars [target_df & ngram_input]
                        #determine which ngram to predict against
                        set_df_and_ngram(processing_text)

                        # determine if target_DF identified above will have a matching ngram
                        # if no match exists, search through suceeding (smaller) ngram DF's until a matching ngram exists
                        target_df <- decide_df(ngram_input)

                        #run the function and output the top 10 most likely predictions
                        #create global var for the most likely predictions (predictions_df)

                        set_top_predictions(ngram_input, target_df)
                        
                        #what user input is being evaluated?
                        User_Input <- user_input_reactive()
                        #which ngram is being used to predict? (often will be the same as above)
                        Ngram_Evaluated <- ngram_input
                        #which ngram data frame is being examined (simply count the number of words in the ngram input)
                        #"\\S+" - counts separations with a space
                        Ngram_DF_Evaluated <- if(str_count(Ngram_Evaluated, "\\S+") == 3) {         
                                "Quadgrams"
                        }       else if(str_count(Ngram_Evaluated, "\\S+") == 2) {
                                "Trigrams"
                        }       else if(str_count(Ngram_Evaluated, "\\S+") == 1) {
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
                        need(user_input_reactive() != '', '')
                )
                
                #run after the Refresh button is selected
                if (input$refresh == 0) {
                        return()
                }  else {
                        
                        #full text description w/ breaks between bullets
                        HTML("<ul><li>User Input: the exact text entered by the user in the box above
                             </li><li>
                             N-gram Evaluated: after a thorough text cleanse, removing things like capital letters, special characters, and other random punctuation, the n-gram applied to the model
                             </li><li>
                             N-gram DF Evaluated: data frame evaluated based on the number of words applied to the prediction ~ options include unigrams, bigrams, trigrams, and quadgrams
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
                     paste("'The audience was AMAZED, and filled with the...'", 
                           "<b>", "Prediction Process: ", "</b>", sep = "<br/>"),
                     "<ol><li>
                     Model will scan the user input string and clean the text to 'the audience was amazed and filled with the'.
                     </li><li>
                     Model will determine the number of words in the string. In this case, the string exceeds 3 
                     words, which is the maximum amount of words the model will consider to make a prediction.
                     </li><li>
                     The last 3 words ('filled with the') will be used to make the prediction.
                     </li><li>
                     The model will look through all previously encountered strings of 'filled with the' in its memory and return the word that most often follows this text.
                     </li><li>
                     In this case, the model recognizes this exact string and can form a prediction. If 'filled with the' was an unknown string, the model would strip the most recent 2 words ('with the') and return the word that most often follows that text. This process would repeat if the model did not recognize last 2 words, and even further, if even the most recent 1 word did not have a match, the most common of all words seen by the model (regardless of past text) would be the predicted output. 
                     </li><li>
                     Here, the predicted output would be 'spirit.'
                     </li></ol>",
                     
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
