<style>
.small-code pre code {
  font-size: .9em;
}
</style>

Natural Language Processing Project: Predictive Text Model
========================================================
author: Michael Nichols
date: 6/11/18
autosize: true

Model Overview
========================================================
width: 1920
height: 1080
class: small-code

*Overview:*
<small>
-- Shiny app to predict the next word based a user's text input.
<br>
-- Capstone project for the Data Science Specialization from Johns Hopkins University, developed in partnership with SwiftKey.
</small>
<br>
*Strategy:*
<small>
-- Speed and simplicity: create an intuitive, efficient predictive model
</small>
<br>
*Logic & Prediction Process:*
<small>
1. Clean & tokenize SwiftKey corpus into ngrams (1-4 words) after isolating sentences and removing profanities.
<br>
2. Save the 4 separate ngram data sets.
</small>
***
<small>
3. Analyze text based on length, and return most common word following that text string.
<br>
4. For unknown strings, the process repeats against the smaller ngrams.
<br>
5. If entire string is unknown, return the most common of all words.
</small>
<br>
*Pros & Cons:*
<small>
--*Pros:* speed; straight-forward and interpretable; accurate with commonly recognized strings
<br>
-- *Cons (Areas for Improvement):* context beyond 4 words; part of speech logic (e.g. noun typically follows this); remove all proper words
</small>

Model Performance
========================================================
width: 1920
height: 1080
class: small-code

*14.21% accuracy*
<br>
<small>
-- Evaluated against 10,000 test strings
<br>
-- Predictions options include 'stop words'
</small>
<br>
*Successful Prediction Examples:*

```
  actual_word       preceding predicted_word
1          at    to just look             at
2       think       i like to          think
3        like window it looks           like
```

*Unsuccessful Prediction Examples:*

```
  actual_word             preceding predicted_word
1       rugby abolish the springbok          shirt
2      things          to see where            you
3          as continuous thought of             it
```
*Tradeoff:*
<br>
<small>
-- Speed prioritized over accuracy.
<br>
-- App includes 5% (12% in final model) of the corpus, due to size restrictions.
</small>

Product Features
========================================================
width: 1920
height: 1080

**Shiny App Basics**
<br>
-- Runs user input through prediction function upon refresh button selection, and tabs update with details.
<br>
-- Prediction displays directly below the user input box.

*To View the Full App in Action:*
<br>
-- [Shiny Application](https://michaelnichols16.shinyapps.io/Predictive_Text_Application/)
<br>
-- [GitHub Repository ~ Code and File Details](https://github.com/mnicho03/Data-Science-Specialization-Capstone)

***

**Shiny App Features**
<br>
-- Next word prediction
<br>
-- Detailed prediction summary table (*featured on next slide*)
<br>
-- Plot of top 10 most likely predictions
<br>
-- Example and key terms
<br>
-- Red loading icon as visuals render

Product Preview & Shiny Visualization
========================================================
width: 1920
height: 1080

*Example:*
<br>
-- Input Text: *"Awesome model!! It's an absolute..."*
<br>
-- *Prediction & Details:*

```
[1] "delight"
```
<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:center;"> Prediction_Items </th>
   <th style="text-align:center;"> Current_Evaluation_Information </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;font-weight: bold;border-right:1px solid;"> User Input </td>
   <td style="text-align:center;font-style: italic;background-color: lightblue;"> Awesome model!! It's an absolute... </td>
  </tr>
  <tr>
   <td style="text-align:center;font-weight: bold;border-right:1px solid;"> N-gram Evaluated </td>
   <td style="text-align:center;font-style: italic;background-color: lightblue;"> an absolute </td>
  </tr>
  <tr>
   <td style="text-align:center;font-weight: bold;border-right:1px solid;"> N-gram DF Evaluated </td>
   <td style="text-align:center;font-style: italic;background-color: lightblue;"> Trigrams </td>
  </tr>
  <tr>
   <td style="text-align:center;font-weight: bold;border-right:1px solid;"> Total N-gram Possibilities </td>
   <td style="text-align:center;font-style: italic;background-color: lightblue;"> 2052409 </td>
  </tr>
  <tr>
   <td style="text-align:center;font-weight: bold;border-right:1px solid;"> Matches Identified </td>
   <td style="text-align:center;font-style: italic;background-color: lightblue;"> 27 </td>
  </tr>
</tbody>
<tfoot>
<tr><td style="padding: 0; border: 0;" colspan="100%">
<span style="font-style: italic;">N-Gram Note: </span> <sup>1</sup> 'N-gram Evaluated' will often equal 'User Input'</td></tr>
<tr><td style="padding: 0; border: 0;" colspan="100%">
<span style="font-style: italic;">Possibilities vs Identified: </span> <sup>*</sup> 'Total N-gram Possibilities' shows the total number of N-grams of this size in the corpus</td></tr>
</tfoot>
</table>

<!-- *** -->
<!-- *Top 10 Most Likely Predictions* -->
<!-- ```{r example_3, echo = FALSE, dpi = 300, out.height = "1080px" } -->
<!-- #top 10 plot -->
<!-- ggplot(predictions_df, aes(x = reorder(predicted_word, frequency), y = Probability, fill = Probability == max(Probability), color = Probability == max(Probability))) + -->
<!--         geom_text(aes(label = round(Probability, 3)), color = "black", hjust = -0.07, size = 5) + -->
<!--         geom_bar(stat = "identity", alpha = .95) + -->
<!--         #highlight only the predicted value red with a gold border -->
<!--         scale_fill_manual(values = c("lightsteelblue2", "firebrick3")) + -->
<!--         scale_color_manual(values = c("white", "goldenrod")) + -->
<!--         labs(y = "", x = "", title = "Next Word Predictions", subtitle = "Includes (at most) the top 10 most probable next words based on the model") + -->
<!--         coord_flip(ylim = c(0, .11)) + -->
<!--         theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14), axis.text.y = element_text(size = 14), plot.title = element_text(size = 18), plot.subtitle = element_text(size = 15), legend.position = "none") -->
<!-- ``` -->
