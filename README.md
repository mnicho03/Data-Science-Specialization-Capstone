# Data-Science-Specialization-Capstone
Capstone Project for Johns Hopkins Data Science Specialization: Natural Language Processing

### Problem Summary
- Apply all the skills developed throughout the Data Science Specialization to tackle a brand new application: analysis of text data and natural language processing. SwiftKey, developer of a smart keyboard to make it easier for people to type on their mobile devices, served as the corporate partner for the project. The dataset provided as the backbone of the project were 3 massive English-language text files (blog posts, news articles, tweets).

### Project Goal:
- Mimic the experience of being a data scientist by encountering a messy data set, a vague question, and very little instruction on exactly how to analyze the data. 
- Traverse the full spectrum of data science activities: understanding the problem, data acquisition and cleaning, exploratory analysis, statistical modeling, predictive modeling, creative exploration, creating a data product, creating a short slide deck pitching your product.

- The full project tasks and deliverables included the following:
1. An introductory quiz to test whether you have downloaded and can manipulate the data.
2. An intermediate R markdown report that describes in plain language, plots, and code your exploratory analysis of the course data set.
3. Two natural language processing quizzes, where you apply your predictive model to real data to check how it is working.
4. A Shiny app that takes as input a phrase (multiple words), one clicks submit, and it predicts the next word.
5. A 5 slide deck created with R presentations pitching your algorithm and app to your boss or investor.

### Final Deliverables
*Files explained in further detail below.

- Week2_Milestone_Report.*X* - first milestone report focused on exploratory data analysis, featuring cleaned up / merged version of the first two R scripts.
    - Full RPubs document can be viewed here: http://rpubs.com/michaelnichols16/SwiftKeyExploration 
- app.R - final data product (Shiny application) - includes server.R and ui.R scripts
    - Shiny App can be viewed here: https://michaelnichols16.shinyapps.io/Predictive_Text_Application/
- predictive_text_app_presentation.Rpres - Rstudio presentation "pitching" the final data product
    - Rstudio Presentation can be viewed here: http://rpubs.com/michaelnichols16/predictive_text_application

### All File Summaries
1. GettingAndCleaning.R - initial R script used to download the files, test random subsampling, and conduct the initial summary statistics for the full text files

2. ExploratoryDataAnalysis.R - more expansive data analysis / exploration featuring a variety of visualizations, some hopefully to be utilized within the file model data product

3. Week2_Milestone_Report.*X* - files submitted as part of the first milestone report for the capstone project, featuring cleaned up / merged version of the first two R scripts.
- Full RPubs document can be viewed here: http://rpubs.com/michaelnichols16/SwiftKeyExploration

4. PredictionModel_v1.R - initial model - did not meet sufficient accuracy metrics and could only successfully predict 2/10 practice phrases from the quiz

- Basic structure: loads in the text files, cleans them and splits them into tokens (uni-quadgrams), then separately takes a user input (text leading up to the prediction) and cleans that similar to the initial text cleansing. Then, the prediction model looks at the number of words in the user input, takes the last few words (up to a trigram) and scans all the initial file for matches. The following words of the exact matches are then sorted in decreasing order, based on the number of occurrences in the text. The model then returns the most likely word, followed by the (up-to) next 9 most likely words. If no matches occur, the model then scans the next shorter ngram for matches (e.g. if no matches for 'amazing time with', the model will then look to 'time with') and runs the same prediction process. If still no matches, the 10 most likely unigrams are predicted. 

    - Pros: very straight forward / intuitive, efficient, accurate with extremely common phrases
    - Cons: no sentiment analysis, phrases beyond tri-grams ignore potential impact from the onset of the phrase, text cleansing could likely be improved to preserve contractions and other words with punctuation within the word

- Areas for improvement / further inspection: 
    1. utilize train / test datasets - currently using the same random sample for model building and testing
    2. crowdsource text cleansing suggestions / decisions from similar models online to find smarter ways to retain as much intact ngrams as possible
    3. get smarter! - experiment with NLP predictive algorithms and strategies (Good Turing Smoothing methods, Katz's back-off models, etc.), with different text mining R packages (quanteda), evaluate NLP metrics (perplexity, accuracy, efficiency)

5. PredictionModel_v2.R - revised model - successfully predicted one of the answer options for 6/10 practice phrases from the second quiz 

- Improvements from the first file:
    - Developed 80/20 train and test set splits: after creating a 12% random sample of each file (blogs, news, twitter), the train and test sets were then split off each separate sample. This allowed an even division across the files to enter the model.
    - Experimented with Good Turing Smoothing methods but elected not to continue with this for the final model.

6. quiz_2.R - practice questions from the quiz with brief summaries of the results (as mentioned above)

7. train-test_splitting.R - process used to split the corpus evenly into train/test sets.
- Basic process: read each line (e.g. article, post, tweet) and creates data frames for each file type listing the line and line number. Under the assumption that the data is already randomized, the first 80% of entries per file get assigned to the train set, and the remaining go to the test set. This method allowed 80% of each file type (blog, news, twitter) to get saved to the train set, rather that grabbing 80% of the total corpus, where it could have had an uneven division of file types in the train/test sets (e.g. all of the Twitter posts in training, all of the blog posts in the testing). 

8. ExploratoryDataAnalysis_ctd.R - more exploratory analysis and development of visuals for the final Shiny model
- visuals included smoothed scatter plots displaying the frequency of frequencies and word clouds (both eventually excluded from the final model), as well as bar chart displaying the top 10 most likely predictions with the final prediction highlighted in red, and lastly, a detailed summary table using Kable to outline the key details of the prediction.
   
9. ngram_building.R - file used to determine best way to tokenize the corpus into ngrams (unigrams, bigrams, trigrams, quadgrams): found a combination of dplyr and tidyr to be sizably the most efficient option. Based on the findings from this file, the final model used the same methodology to tokenize user input text into the respective ngram buckets for predictions.  

10. app.R - file used to create the final data product (Shiny application) - includes server.R and ui.R scripts
- Shiny App can be viewed here: https://michaelnichols16.shinyapps.io/Predictive_Text_Application/
- Model overview: application and predictions generated based off a random 5% sample of the entire corpus (largest file which could be accommodated by Shiny free user profile size restrictions)
    - cleansed random 5% sample of the corpus created within 'PredictionModel_final.R' is first loaded in as a RData file and the prediction functions are loaded afterwards
    - model takes the user input, and upon selection of a refresh button, the user text string is run through the prediction function and the output displays directly below the user input box
    - upon selection of the refresh button, 2 dynamic tabs update to display prediction details (bar plot of top 10 most likely predictions and a table of summary details of the prediction; another static tab is also available with an example and key terms
    - model takes a few moments to first render in the web page, but generates predictions and visuals very efficiently 

10. bad_words_list.txt - file used for profanity filtering: contains 451 words which we remove from the model
- This prevents any profane words from later appearing as predictions

11. accuracy_testing.R - file used to test model accuracy
- Process / Logic & Results
    - RData file based off of 'PredictionModel_v2.R' is loaded in with the train / test set data from a random 12% of the entire corpus: this includes all ngram data frames (unigrams - quadgrams) built off the training set as well as the quadgrams from the test set
    - Only quadgrams are loaded from the test set since the model only looks at a maximum of 4 word strings (anything greater than 4 words prior do not effect the predictions) -- the quadgrams alone account for 1,229,670 potential test cases: since the predictions take a couple seconds per, only 10,000 quadgrams are tested to evaluate model accuracy
    - Accuracy results: 1,421 / 10,000 correctly predicted ~ 14.21% accuracy
    
12. predictive_text_app_presentation.X - Rstudio presentation files "pitching" the final data product (Shiny app) as if to a boss or potential investor. Slide deck consists of 5 slides that (1) explain how the model works, (2) describe its predictive performance quantitatively, and (3) shows off the app and how it works.

- Rstudio Presentation can be viewed here: http://rpubs.com/michaelnichols16/predictive_text_application
