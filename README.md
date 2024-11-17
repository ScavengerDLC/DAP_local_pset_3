[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/nG4V5G8y)
# Data Skills 2 - R
## Fall Quarter 2024

## Homework 3
## Due: November 16 before midnight on Gradescope

NOTE: I USUALLY ASSUME THAT YOU HAVE A STRONG ENOUGH COMPUTER FOR THIS BUT MAKE SURE TO CLEAR YOUR UNUSED R MEMORY AND OBJECTS WHEN GOING BETWEEN QUESTION 1 AND 2 R-SCRIPT FILES.
I FINALLY FOUND OUT WHY PROGRAMMERS OPTIMIZE.

Files:
countries.csv: A csv that contains country names. Contains more than UN recognized nations. Found at this link: https://github.com/umpirsky/country-list/blob/master/data/en_US/country.csv?plain=1

README.md: The file you're reading right now

vol101_1_publichealthroundup.txt: January 2023 Public Health Round-Up

question_1_plot_1.png: Image for Plot 1 of Question 1

question_1_plot_2.png: Image for Plot 2 of Question 1

question_2_plot_1.png: Image for Plot 1 of Question 2

question_2_plot_2.png: Image for Plot 2 of Question 2

question_3_plot_3.png: Image for Plot 3 of Question 2

n.txt: found in the Articles folder, n.txt represents the nth newest article from WHO Africa. Naming convention is for convenience and is not meant to be understandable to a human reader.

Variables and Other Named Objects from question1.R:

	Data:
		countries: dataframe containing all country names
		countries_n_word: countries that have n words in their name and are in the January Round Up
		countries_mentioned: countries that are mentioned in the January Round Up
		countries_name_words: country names made into word tokens (used to find max n for coutnries_n_word)
		countries_sentences: sentences that mention a country, parsed
		country_sentiment: average sentiment of sentences that mention countries
		filtered_sentences: sentences that only have one country in them. (used as an intermediary data frame)
		kenya_sentiment: sentiment of sentences mentioning Kenya, had an average of 0 and I wanted to investigate them specifically to see what was going on
		n_countries_mentioned: number of countries mentioned, used in print() at some point
		round_up_n: One column with January 2023 roundup broken up into n word ngrams. 
		round_up_sentences: One column with each sentence of January 2023 roundup in each cell
		round_up_whole: Entire text of January 2023 roundup in one cell
		sent_round_up: Sentiment of each sentence
		sentence_4: sentence that had more than one country mentioned. (used as intermediary data frame)
		sentiment_afinn: dataframe of sentiments defined by Finn Årup Nielsen (1.)
		
	values:
		path: File path that contains this readme and all the data
		i: index variable in for loop that creates round_up_n
		name: name value that is used in for loop that creates round_up_n
		round_up_raw: Raw Import of vol101_1_publichealthroundup.txt into R

Variables and Other Named Objects from question2.R:

	Data:
		article_n: Entire text of article_n from n.txt in one cell
		article_sentences: Every Sentence from Articles posted from September 2023 to November 16, 2023
		articles: Entire text of each article in one cell
		article_n_words: One column with all articles text broken into n word ngrams.
		countries: data frame containing all country names
		countries_n_word: countries that have n words in their name and are in the articles
		countries_mentioned: countries that are mentioned in the articles
		countries_per_sentence: Number of countries mentioned in each sentence
		countries_sentences: Evert Sentence that mentions a country, parsed
		countries_sentences_one: Sentences that mention one country
		countries_sentences_more: Sentences that mention more than one country
		country_sentences: Sentences that mention countries, untransformed
		country_sentiment: average sentiment of sentences that mention countries
		n_countries_mentioned: number of countries mentioned, used in print() at some point
		sent_articles: Sentiment of sentences, no filter
		Sentiment_afinn: dataframe of sentiments defined by Finn Årup Nielsen (1.)
		top_10: Top 10 countries mentioned by the articles with number of mentions in a column. 
		

	values:
		article_list: list of numbers from 1 to n where n is the number of articles. 
		b, c, h, h, k, u: index variables for various for loops
		data_name: placeholder value used in for loop
		name: placeholder value used in for loop
		name_countries: placeholder value used in for loop
		path: File path that contains this readme and all the data
		article_n: Raw Import of n.txt (won't be there once you finish but may be in the values depending on when you look. 

	functions:
		article_finder(month_year): Finds articles posted by WHO AFRICA to https://www.afro.who.int/news/news-releases from the current date to a specified "Month/Year". Make sure to enter month_year as "string" in the format of 		"Month/Year"


(1.) Finn Årup Nielsen A new ANEW: Evaluation of a word list for sentiment analysis in microblogs. Proceedings of the ESWC2011 Workshop on 'Making Sense of Microposts': Big things come in small packages 718 in CEUR Workshop Proceedings 93-98. 2011 May. http://arxiv.org/abs/1103.2903.



Questions: 

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



