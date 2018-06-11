# Data-Science-Specialization-Capstone
Capstone Project for Johns Hopkins Data Science Specialization: Natural Language Processing

### ...

### File Summaries
1. GettingAndCleaning.R - initial R script used to download the files, test random subsampling, and conduct the initial summary statistics for the full text files

2. ExploratoryDataAnalysis.R - more expansive data analysis / exploration featuring a variety of visualizations, some hopefully to be utilized within the file model data product

3. Week2_Milestone_Report.*X* - files submitted as part of the first milestone report for the capstone project, featuring cleaned up / merged version of the first two R scripts.
- Full RPubs document can be viewed here: http://rpubs.com/michaelnichols16/SwiftKeyExploration

4. PredictionModel_v1.R - initial model - did not meet sufficient accuracy metrics and could only successfully predict 2/10 practice phrases from the quiz

- Basic structure: loads in the text files, cleans them and splits them into tokens (uni-quadgrams), then separately takes a user input (text leading up to the prediction) and cleans that similar to the intial text cleansing. Then, the prediction model looks at the number of words in the user input, takes the last few words (up to a trigram) and scans all the initial file for matches. The following words of the exact matches are then sorted in decreasing order, based on the number of occurrences in the text. The model then returns the most likely word, followed by the (up-to) next 9 most likely words. If no matches occur, the model then scans the next shorter ngram for matches (e.g. if no matches for 'amazing time with', the model will then look to 'time with') and runs the same prediction process. If still no matches, the 10 most likely unigrams are predicted. 

    - Pros: very straight forward / intuitive, efficient, accurate with extremely common phrases
    - Cons: no sentiment analysis, phrases beyond tri-grams ignore potential impact from the onset of the phrase, text cleansing could likely be improved to preserve contractions and other words with punctuation within the word

- Areas for improvement / further inspection: 
    1. utilize train / test datasets - currently using the same random sample for model building and testing
    2. crowdsource text cleansing suggestions / decisions from similar models online to find smarter ways to retain as much intact ngrams as possible
    3. get smarter! - experiment with NLP predictive algorithms and strategies (Good Turing Smoothing methods, Katz's back-off models, etc.), with different text mining R packages (quanteda), evaluate NLP metrics (perplexity, accuracy, efficiency)

5. PredictionModel_v2.R - revised model - successfully predicted one of the answer options for 6/10 practice phrases from the second quiz 

- Improvements from the first file:
    - Developed 80/20 train and test set splits: after creating a 12% random sample of each file (blogs, news, twitter), the train and test sets were then split off of each separate sample. This allowed an even division across the files to enter into the model.
    - Experimented with Good Turing Smoothing methods but elected not to continue with this for the final model.

6. quiz_2.R - practice questions from the quiz with brief summaries of the results (as mentioned above)

7. train-test_splitting.R - process used to split the corpus evenly into train/test sets.
- Basic process: read each line (e.g. article, post, tweet) and creates data frames for each file type listing the line and line number. Under the assumption that the data is already randomized, the first 80% of entries per file get assigned to the train set, and the remaining go to the test set. This method allowed 80% of each file type (blog, news, twitter) to get saved to the train set, rather that grabbing 80% of the total corpus, where it could have had an uneven division of file types in the train/test sets (e.g. all of the Twitter posts in training, all of the blog posts in the testing). 

8. ExploratoryDataAnalysis_ctd.R - more exploratory analysis and development of visuals for the final Shiny model
- visuals included smoothed scatter plots displaying the frequency of frequencies and wordclouds (both eventually excluded from the final model), as well as bar chart displaying the top 10 most likely predictions with the final prediction highlighted in red, and lastly, a detailed summary table using Kable to outline the key details of the prediction.
   
9. ngram_building.R - file used to determine best way to tokenize the corpus into ngrams (unigrams, bigrams, trigrams, quadgrams): found a combination of dplyr and tidyr to be sizably the most efficient option. Based on the findings from this file, the final model used the same methodology to tokenize user input text into the respective ngram buckets for predictions.  

10. app.R - file used to create the final data product (Shiny application) - includes server.R and ui.R scripts
- Shiny App can be viewed here: XXXXXXXXXXXXXXXXXXX
- Model overview: application and predictions generated based off a random 5% sample of the entire corpus (largest file which could be accomodated by Shiny free user profile size restrictions)
    - cleansed random 5% sample of the corpus created within 'PredictionModel_final.R' is first loaded in as a RData file and the prediction functions are loaded afterwards
    - model takes the user input, and upon selection of a refresh button, the user text string is run through the prediction function and the output displays directly below the user input box
    - upon selecton of the refresh button, 2 dynamic tabs update to display prediction details (bar plot of top 10 most likely predictions and a table of summary details of the prediction; another static tab is also available with an example and key terms
    - model takes a few moments to first render in the web page, but generates predictions and visuals very efficiently 

10. bad_words_list.txt - file used for profantity filtering: contains 451 words which we remove from the model
- This prevents any profane words from later appearing as predictions

11. accuracy_testing.R - file used to test model accuracy
- Process / Logic & Results
    - RData file based off of 'PredictionModel_v2.R' is loaded in with the train / test set data from a random 12% of the entire corpus: this includes all ngram data frames (unigrams - quadgrams) built off the training set as well as the quadgrams from the test set
    - Only quadgrams are loaded from the test set since the model only looks at a maximum of 4 word strings (anything greater than 4 words prior do not effect the predictions) -- the quadgrams alone account for 1,229,670 potential test cases: since the predictions take a couple seconds per, only 10,000 quadgrams are tested to evaluate model accuracy
    - Accuracy results: 1,421 / 10,000 correctly predicted ~ 14.21% accuracy
    
12. predictive_text_app_presentation.Rpres - Rstudio presentation "pitching" the final data product (Shiny app) as if to a boss or potential investor. Slide deck consists of 5 slides that (1) explain how the model works, (2) describe its predictive performance quantitatively, and (3) shows off the app and how it works.
- Rstudio Presentation can be viewed here: XXXXXXXXXXXXXXXXXXX
