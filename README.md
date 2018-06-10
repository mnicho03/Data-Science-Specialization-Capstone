# Data-Science-Specialization-Capstone
Capstone Project for Johns Hopkins Data Science Specialization: Natural Language Processing

### File Summary
1. GettingAndCleaning.R - initial R script used to download the files, test random subsampling, and conduct the initial summary statistics for the full text files

2. ExploratoryDataAnalysis.R - more expansive data analysis / exploration featuring a variety of visualizations, some hopefully to be utilized within the file model data product

3. Week2_Milestone_Report.*X* - files submitted as part of the first milestone report for the capstone project, featuring cleaned up / merged version of the first two R scripts.
- Also published on RPubs here: http://rpubs.com/michaelnichols16/SwiftKeyExploration

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
   
9. 
