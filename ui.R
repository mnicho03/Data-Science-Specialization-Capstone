

# first line sets theme
shinyUI(fluidPage(theme = shinytheme("cosmo"),
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
))