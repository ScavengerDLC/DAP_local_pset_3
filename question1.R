library(tidyverse)
library(textdata)
library(tidytext)
library(sentimentr)

#################
# Set your Path #
#################

path <- "C:/Users/zades/Documents/GitHub/problem-set-3-ScavengerDLC/"

##############################
# Setting up the Data Frames #
##############################

### Read Text File into R
round_up_raw <- read_file(paste0(path, "vol101_1_publichealthroundup.txt"))

### Create Data Frame. Initially shared a name with round_up for simplicity, but
### I realized I needed this step in particular to be saved for the for loop. 
round_up_whole <- data.frame(text = round_up_raw)

### Separate Each word into their own cell.
round_up_1 <- unnest_tokens(round_up_whole, word_tokens, text, token = "words")

### Read in Country CSV, this will be used more extensively later, but for now
### we just need to know the country with the most words for the coming for loop

countries <- read.csv(paste0(path, "country.csv"))

### Rename columns and change IDs to be the name of the country. When we seperate 
### the word tokens out from the country names, we still want to be able to ID what word
### belongs to what country.
countries <- countries |> rename(name = value) |> mutate(id = name)

### Find Country with Most Word Tokens
country_name_words <- unnest_tokens(countries, word_tokens, name, token = "words")

### This is the check mentioned in the readme file 
country_name_words |> 
  count(id) |>
  arrange(desc(n)) |>
  head(1)

################################################################################
### Turns out it is South Georgia & South Sandwich Islands with 5 words! This 
### case probably won't turn up in the text, but it is still good to do this 
### for all the countries with 2 words in their names and 3 words. Word_tokens
### will be the column name despite them technically being ngrams so that column
### names will be standardized across the data frames. This will help later
################################################################################


### Create Dataframes that contains all combination of X words, up to 5 
### Had troubles with this step, tried a few different things but ended up using
### Google, the link can be found below:
### https://www.geeksforgeeks.org/how-do-i-rename-a-data-frame-in-a-for-loop-in-r/

for (i in c(2:5)) {
  name <- paste0("round_up_", i)
  df <- unnest_tokens(round_up_whole, word_tokens, text, 
                              token = "ngrams", n = i)
  assign(name, df)
  rm(df)
}


### Create a Data Frame that has Sentences in each cell
round_up_sentences <- unnest_tokens(round_up_whole, sentence_tokens, 
                                    text, token = "sentences")

### Create Data Frame of Mentioned Countries
# Make all country names all lower case to match word tokens
countries <- countries |> mutate(name = tolower(name))

################################################################################
### I tried making the next step more general to avoid copy and pasting. I tried
### a similar approach to the previous for loop. Why does this not work in this 
### case? Is it because it is reading name_2 as an empty dataframe rather than
### the corresponding round_up_X? If so, how would I get this for loop to work?
###
### for (i in c(1:5)) {                                                                     
###   name_2 <- paste0("round_up_", i)                                                  
###   name_3 <- paste0("countries_", i)                                                   
###   df <- inner_join(as.data.frame(name_2), countries, by = c("word_tokens" = "name"))
###   assign(name_3, df)
###   rm(df)
### }
### I learned how to do this later, but too late to remove this comment and redo
### the code. It works just as well anyways.
################################################################################

### Copy and Paste Party to get all countries mentioned

countries_1_word <- inner_join(round_up_1, countries, 
                               by = c("word_tokens" = "name"))

countries_2_word <- inner_join(round_up_2, countries, 
                               by = c("word_tokens" = "name"))

countries_3_word <- inner_join(round_up_3, countries, 
                               by = c("word_tokens" = "name"))

countries_4_word <- inner_join(round_up_4, countries, 
                               by = c("word_tokens" = "name"))

countries_5_word <- inner_join(round_up_5, countries, 
                               by = c("word_tokens" = "name"))

### Full Join and remove These placeholder dataframes and remove word token
### rows to be left with Countries in the article
countries_mentioned <- countries_1_word |>
  full_join(countries_2_word, by = "id") |>
  full_join(countries_3_word, by = "id") |>
  full_join(countries_4_word, by = "id") |>
  full_join(countries_5_word, by = "id") |>
  select(!contains("word"))

### Get AFINN Sentiment Dataframe Will be used to get sentiments about specific 
### countries mentioned

sentiment_afinn <- get_sentiments("afinn")

##################################
# Summary of Countries Mentioned #
##################################

### Run these lines of code for a quick summary of what countries are mentioned

### Number of distinct countries and what they are 
n_countries_mentioned <- distinct(countries_mentioned) |> count()

print(paste0("The number of unique countries mentioned is ", 
             n_countries_mentioned, ".", " Those countries include:")) 
distinct(countries_mentioned)

### Countries Mentioned Multiple Times
  countries_mentioned |> group_by(id) |> count() |> filter(n > 1)

################################
# General Sentiment of Article #
################################

# Get Sentiment of Sentences. This is a basic analysis for summary stats
# and a density graph
sent_round_up <- data.frame(sentiment(round_up_sentences$sentence_tokens))
  
# Filter out Second Interpretation of Sentences
sent_round_up <- sent_round_up |> filter(sentence_id == 1)

sent_round_up <- cbind(round_up_sentences, sent_round_up)

summary(sent_round_up$sentiment)

# Create Density Plot of Sentence Sentiment
sent_round_up|>
  ggplot(aes(x = sentiment)) +
  geom_density(aes(fill = as.factor(sentence_id)), alpha = 0.6, show.legend = FALSE) + 
  scale_fill_manual(values = "#52c3ff") +
  xlab("Sentiment") +
  ylab("Density") + 
  ggtitle("Density of Sentence Sentiment for January 2023 Public Health Round-Up")+
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey95"), 
  )
ggsave(paste0(path, "question_1_plot_1.png"), plot = last_plot())

#############################################
# Sentiment of Specific Countries Mentioned #
#############################################

### Filter Sentences Data Frame to one that only contains sentences that mention countries
country_sentences <- round_up_sentences |> 
  filter(
     grepl("oman", sentence_tokens) |
     grepl("switzerland", sentence_tokens) |
     grepl("sudan", sentence_tokens) |
     grepl("uganda", sentence_tokens) |
     grepl("kenya", sentence_tokens) |
     grepl("united states", sentence_tokens)
    )

### Create Country Variable that is attached to a sentence which identifies which countries
### are mentioned in a sentence. Will be helpful later

# Step 1 Make True False Variables to identify where countries are mentioned
country_sentences <- country_sentences |>
  mutate(oman = grepl("oman", sentence_tokens),
         switzerland = grepl("switzerland", sentence_tokens),
         sudan =  grepl("sudan", sentence_tokens),
         uganda = grepl("uganda", sentence_tokens),
         kenya = grepl("kenya", sentence_tokens),
         united_states = grepl("united states", sentence_tokens)
         )

# Step 2 Check for Sentences that mention multiple countries if we assign a "country"
# variable, for sentences that contain multiple countries we won't be able to track
# the average sentiment for those countries because their unsplit country variable
# would be "country1, country2"
country_sentences |> select(c("oman":"united_states")) |> rowSums()

# Sentence 4 Contains Multiple countries in one sentence. We'll need to make that
# sentence two instances in the data frame

# Create a data frame with just sentence_4
sentence_4 <- country_sentences |> filter(uganda == TRUE & sudan == TRUE)

sentence_4 <- sentence_4 |> select(sentence_tokens, uganda, sudan) |>
  pivot_longer(cols = c(uganda, sudan), names_to = "country", values_to = "extra") |>
  select(!extra)

# Filter out Sentence 4 and do a similar transformation to the country_sentences
# data frame

filtered_sentences <- country_sentences |> anti_join(sentence_4, by = "sentence_tokens")

filtered_sentences <- filtered_sentences |> 
  mutate(
    country = ifelse(oman == TRUE, "oman", 
              ifelse(switzerland == TRUE, "switzerland",
              ifelse(sudan == TRUE, "sudan",
              ifelse(uganda == TRUE, "uganda",
              ifelse(kenya == TRUE, "kenya",
              ifelse(united_states == TRUE, "united states", NA))))))
  ) |>
  select(!c(oman:united_states))

# Combine filtered_sentences and sentence_4 to country_sentences

country_sentences <- full_join(filtered_sentences, sentence_4, by = "sentence_tokens")

# Clean up joining artifacts and rename sentence_tokens to text and countries to doc_id

country_sentences <- country_sentences |>
  mutate(country = ifelse(is.na(country.x), country.y, country.x)) |>
  select(!c(country.x, country.y)) |>
  rename(text = sentence_tokens, doc_id = country)

### Country Level Sentiment Bar Graph
# Create a parsed data frame 
country_sentences <- udpipe(country_sentences, "english") 

#Join Sentiment Afinn Data into this dataframe
country_sentences <- country_sentences |> left_join(sentiment_afinn, 
                                                    by = c("lemma" = "word"))

# Get Sentiment at Country Level, rather than word level

country_sentiment <- country_sentences |> 
  group_by(doc_id) |>
  summarize(country_sent = mean(value, na.rm = TRUE))

# Create Bar Graph
country_sentiment |>
  group_by(doc_id) |>
  ggplot(aes(x = doc_id, y = country_sent)) +
  geom_col() 

################################################################################
# Kenya's  sentiment is 0? Lets take a look into this to see what is going on  
################################################################################

### Filter for Kenya only data
kenya_sentiment <- country_sentences |> ungroup() |> filter(doc_id == "kenya") |> filter(!is.na(value))

kenya_sentiment |> select(value, token) |> print()

### It does in fact average out to 0, there isn't an issue with it being all NA.

### See what is greater and worst by reading the sentence

filtered_sentences |> filter(country == "kenya") |> print()

################################################################################
### "Greater" isn't referring to a higher quality but a geographic region, adjusting
### this sentiment to be neutral. The reason that I am adjusting this manually
### is because a human reading this would understand that greater in this context
### has a neutral impact on the sentiment of the sentence. 
################################################################################

country_sentences <- country_sentences |> 
  mutate(value = ifelse(doc_id == "kenya" & token == "greater", 0, value))

### Redo bar graph
# Get Sentiment at Country Level, rather than word level
country_sentiment <- country_sentences |> 
  group_by(doc_id) |>
  summarize(country_sent = mean(value, na.rm = TRUE))

# Create Bar Graph
country_sentiment |>
  group_by(doc_id) |>
  ggplot(aes(x = doc_id, y = country_sent)) +
  geom_col()  +
  xlab("Country") +
  ylab("Sentiment") + 
  labs(subtitle = "(AFINN)", caption = "Note: Sudan and Uganda were mentioned in the same sentence") +
  ggtitle("Average Sentiment of Sentences Mentioning Countries in the January 2023 Public Health Round-Up") +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(color = "black"),
    panel.grid.minor = element_line(color = "grey75"), 
  )

ggsave(paste0(path, "question_1_plot_2.png"), plot = last_plot())

################################################################################
### Sudan and Uganda have the same averages, this is because they share a sentence.
### Uganda is mentioned in multiple sentences, however the second sentence did
### not contribute to the score in any way. I took a look at the sentence without
### using code and I can say that it is talking about reported disease cases. 
### Absolutely a negative sentiment when we read it, however the wording of it 
### is clinical in my opinion. I could see some people assigning negative sentiment 
### to it and I could see others assigning neutral. Because of that, I won't make
### any changes to the values, but note the result here. 
################################################################################







