# Data Skills 2 - R
## Fall Quarter 2024

## Homework 3
## Due: November 16 before midnight on Gradescope

Note that there is a lot of flexibility in how you approach these questions and what your final results will look like.  Being comfortable with that sort of assignment is an explicit course goal; real-world research is much more likely to come with open-ended assignments rather than explicit direction to start with X and accomplish exactly Y.  Use short comments (1-3 lines max) to explain any choices that you think need explaining.  Remember wherever possible to focus on "why" in your comments, and not "what". As usual, include a README file describing the code and output.


__Question 1 (30%):__ You are working as a research assistant at a think tank that works on global public health.  The senior researcher you work for tries to follow the Public Health Roundup from [the WHO Bulletin](https://www.who.int/publications/journals/bulletin), but they have been too busy lately to keep up with it.. They ask you to read in the January 2023 report (from the text file included in the repo, also available [here]([https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9795377/) and parse it using natural language processing:

Describe the sentiment of the article, and show which countries are discussed in the article.

Output to save to your repo for this question:
  * question1.R file with the code - summary statistics can be displayed with print or View
  * question1_plot_X.png file for the plots you generate (a minimum of 2, a maximum of 4)

__Question 2 (70%):__ Your senior researcher is very happy with the results you achieved on the most recent report, so they ask you to help them with sentiment analysis for WHO news releases from [WHO Africa](https://www.afro.who.int/news/news-releases?page=0). Create a function that uses basic web scraping to access the news releases back to a certain date. This function should take as input a month and a year, and scrape all news releases from today back to that month (including that month). It should then download and save the scraped text from the article as a separate .txt file. Make sure to remove any extraneous information beyond text: pictures, links, contact information, etc.

Use your function to pull all news releases back to September 2023. Now, analyze the overall sentiment of the WHO Africa News Releases from September 2023 to today, as well as the sentiment about a particular country of your choice. 

Output to save to your repo for this question:
  * question2.R file with the code - summary statistics can be displayed with print or View
  * A file (or collection of text documents) of the text from the reports that you scraped.
  * question2_plot_X.png files for the plot(s) you generate (a minimum of 2, a maximum of 4)
