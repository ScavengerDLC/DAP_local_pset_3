library(tidyverse)
library(stringr)
library(textdata)
library(tidytext)
library(sentimentr)
library(rvest)

#################
# Set your Path #
#################

path <- "C:/Users/zades/Documents/GitHub/problem-set-3-ScavengerDLC/"

########################
# Function Starts here #
########################

### Notes on the Function: 
### (1) Enter Month_year as "string"
### (2) It may take some time for all the web scraping to work. If you are doing
### the time range on the assignment "09/2023" expect no less than 91 files. 
### I can't guarantee the exact number because time moves in one direction and 
### there may be more articles when you run the code. 
### (3) When it is done it will say "Article Text Extraction Complete. Article 
### Finder Shutting Down" All files will be located in the subfolder /Articles in 
### the problem set folder. articles will be named n.txt where 1.txt would be the 
### most recent article published and the largest number will be the oldest. 

article_finder <- function(month_year){
  ################################################################################
  # Forming the article_info Dataframe from page 0 of search page 
  # I know this could be done in the for loop, but this was kind of my scratch
  # work of figuring out how to do this for one page. Then the for loop does it
  # for the rest
  ################################################################################
  
  ################
  # Set your URL #
  ################
  
  url <- "https://www.afro.who.int/news/news-releases?page=0" 
  
  who_link <- "https://www.afro.who.int"
  
  ### Read HTML
  html_search_page <- read_html(url)
  
  
  
  ### Get Relative Article Links on Page
  
  relative_article_links <- html_search_page |>
    html_elements("article") |>
    html_elements("h3") |>
    html_elements("a") |>
    html_attr("href") 
  
  ### Make Full Link
  article_links <- paste0(who_link, relative_article_links) %>%
    data.frame(.) %>%
    rename("link" = ".")
  
  ### Get Date that Article Released
  article_dates <- html_search_page |>
    html_elements("article") |>
    html_elements("time") |>
    html_attr("datetime") 
  
  ### Make Date Time
  article_dates <- ymd_hms(article_dates) %>%
    data.frame(.) %>%
    rename("date" = ".")
  
  ### Combine into one dataframe
  article_info <- cbind(article_dates, article_links)
  
  ################################################################################
  # The Link Gathering For Loop 
  ################################################################################
  for (i in c(1:48)) {
    
    ### Set URLS
    
    url <- paste0("https://www.afro.who.int/news/news-releases?page=", i) 
    
    who_link <- "https://www.afro.who.int"
    
    ### Read HTML
    html_search_page <- read_html(url)
    
    ### Get Relative Article Links on Page
    
    relative_article_links <- html_search_page |>
      html_elements("article") |>
      html_elements("h3") |>
      html_elements("a") |>
      html_attr("href") 
    
    ### Make Full Link
    article_links <- paste0(who_link, relative_article_links) %>%
      data.frame(.) %>%
      rename("link" = ".")
    
    ### Get Date that Article Released
    article_dates <- html_search_page |>
      html_elements("article") |>
      html_elements("time") |>
      html_attr("datetime") 
    
    ### Make Date Time
    article_dates <- ymd_hms(article_dates) %>%
      data.frame(.) %>%
      rename("date" = ".")
    
    ### Combine into one dataframe
    df <- cbind(article_dates, article_links)
    
    ### Add new dataframe to growing dataframe
    
    article_info <- rbind(article_info, df)
    
    print(paste0("Completed Page ", i))
  }
  
  print("Link Harvesting Successful")
  
  ################################################################################
  # Quick Date Filter into Text Gathering For Loop
  ################################################################################
  
  ### Filter For Specific Month/Year
  article_info <- article_info |>
    filter(date >= my({{month_year}}))
  
  ### Get rid of spaces from the end of URLs
  article_info <- article_info |>
    mutate(link = str_trim(link, "right"))
  
  ### Text Gathering For Loop
  for (j in 1:nrow(article_info)) {
    ### Set URL
    url_article <- article_info[j,2]
    
    ### Read URL
    html_article <- read_html(url_article)
    
    ### Get Text
    
    article_text <- html_article |>
      html_elements(".content") |>
      html_elements(".col-md-9") |>
      html_elements("p") |>
      html_text()
    
    ### Convert to data frame for next step
    article_text <- article_text %>% data.frame()
    
    ### Export .txt file
    file_end_point <- paste0("/Articles/", j, ".txt")
    
    write.table(article_text, paste0(path, file_end_point))
    
    print(paste0("File ", j, " exported."))
  }
  
  print("Article Text Extraction Complete. Article Finder Shutting Down")
}

### This may or may not work if you click run or press ctrl+enter? I got an error 
### when I ran this not through the console. Not sure what is up with that because  
### I can run every part of the code outside of the function.

article_finder("09/2023")

### Part one of question 2 complete!

### Read all the text files into R. I had no clue how to do this. Found the filelist
### function here. the rest of the solution is my own unless otherwise noted:
### https://stackoverflow.com/questions/3397885/how-do-you-read-multiple-txt-files-into-r
### I got the list of the files and use that to get the number of files ahead
### without needing to look into the folder

article_list <- list.files(paste0(path, "/Articles/"), pattern = ".*.txt")

### Edit the strings to be just a number

article_list <- str_remove(article_list, ".txt")

### Turn into change values to numbers

article_list <- as.numeric(article_list)

### Read in the Files with a For Loop
for (k in article_list) {
  name <- paste0("article_", k)
  
  x <- read_file(paste0(path, "/Articles/", k, ".txt"))
  
  assign(name, x)
  
  rm(x)
}


### A lot of this code is going to be copy and pasted from question 1 because
### the analysis can be more or less the same and a lot of the same steps 
### need to be performed. 

### I learned about get() here looking into an alternate way to solve the rest
### of this. I didn't end up using the original version though:
### https://stackoverflow.com/questions/59584279/referring-to-a-column-in-data-frame-with-paste0-name

##############################
# Setting up the Data Frames #
##############################

### Convert article_n objects into dataframes
for (g in 1:91) {
  data_name <- paste0("article_", g)
  
  df <- data.frame(text = get(paste0("article_", g)))
  
  assign(data_name, df)
  
  rm(df)
}

### Combine articles into one dataframe

# Create empty articles dataframe 

articles <- data.frame(text = c())

for (h in 1:90) {
  articles <- rbind(get(paste0("article_", h)), articles)
}

### Separate Each word into their own cell.
article_1_words <- unnest_tokens(articles, word_tokens, text, token = "words")

### Read in Country CSV, this will be used more extensively later, but for now
### we just need to know the country with the most words for the coming for loop

countries <- read.csv(paste0(path, "country.csv"))

### Rename columns and change IDs to be the name of the country. When we seperate 
### the word tokens out from the country names, we still want to be able to ID what word
### belongs to what country.
countries <- countries |> rename(name = value) |> mutate(id = name)

################################################################################
### Turns out it is South Georgia & South Sandwich Islands with 5 words (see
### question one for code that proves this. This case probably won't turn up in  
### the text, but it is still good to do this for all the countries with 2 words 
### in their names and 3 words. Word_tokens will be the column name despite them 
### technically being ngrams so that column names will be standardized across the 
### data frames. This will help later
################################################################################

### Create Dataframes that contains all combination of X words, up to 5 

for (u in c(2:5)) {
  name <- paste0("article_", u, "_words")
  df <- unnest_tokens(articles, word_tokens, text, 
                      token = "ngrams", n = u)
  assign(name, df)
  rm(df)
}

### Create a Data Frame that has Sentences in each cell
article_sentences <- unnest_tokens(articles, sentence_tokens, 
                                    text, token = "sentences")

### Create Data Frame of Mentioned Countries
# Make all country names all lower case to match word tokens
countries <- countries |> mutate(name = tolower(name))

# Now that I know get() I don't have to do the longer way

for(b in c(1:5)){
  name_countries <- paste0("countries_", b, "_word")
  
  df <- inner_join(get(paste0("article_", b, "_words")), countries, 
                   by = c("word_tokens" = "name"))
  
  assign(name_countries, df)
  rm(df)
}

### Create Countries Mentioned Dataframe
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

### Top 10 Countries mentioned 
top_10 <- countries_mentioned |> 
  group_by(id) |> 
  count() |> 
  filter(n > 1) |>
  arrange(desc(n)) |>
  head(10)

### Visualization of Top 10 Countries Mentioned

top_10 |>
  ggplot(aes(x = reorder(id, desc(n)), y = n)) +
  geom_col() +
  xlab("Country") +
  ylab("Number of Mentions") + 
  labs(subtitle = "From September 1, 2023 to November 16, 2024") +
  ggtitle("Top 10 Countried Mentioned by WHO Africa") +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(color = "black"),
    panel.grid.minor = element_line(color = "grey75"), 
  )

### I know plot 2 is made before plot 1. I made the code for plot one first 
### so it gets dibs
ggsave(paste0(path, "question_2_plot_2.png"), plot = last_plot())

#################################
# General Sentiment of Articles #
#################################
# Get Sentiment of Sentences. This is a basic analysis for summary stats
# and a density graph
sent_articles <- data.frame(sentiment(article_sentences$sentence_tokens))

# Filter out Second Interpretation of Sentences
sent_articles <- sent_articles |> filter(sentence_id == 1)

sent_articles <- cbind(article_sentences, sent_articles)

### I took a look here to see what the sentences data frame was looking like
### and saw it needed to be cleaned up. Something that I noticed alot of the
### problem rows shared was that the word count was na. 

sent_articles <- sent_articles |> filter(!is.na(word_count))


### I briefly skimmed through the data and the gibberish seems to be gone

summary(sent_articles$sentiment)

# Create Density Plot of Sentence Sentiment
sent_articles|>
  ggplot(aes(x = sentiment)) +
  geom_density(aes(fill = as.factor(sentence_id)), alpha = 0.6, show.legend = FALSE) + 
  scale_fill_manual(values = "#52c3ff") +
  xlab("Sentiment") +
  ylab("Density") + 
  ggtitle("Density of Sentence Sentiment for Articles Published by WHO Africa")+
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey95"), 
  )
ggsave(paste0(path, "question_2_plot_1.png"), plot = last_plot())

### What is the sentiment of the countries that are mentioned the most? 

# Create Empty Data frame for sentences about particular countries 

country_sentences <- data.frame("sentence_tokens" = c())

# Make ID column lowercase

top_10 <- top_10 |> mutate(id = tolower(id))

for (c in 1:10) {
  df <- article_sentences |> 
    filter(grepl(as.character(top_10[c, 1]), sentence_tokens))
  country_sentences <- rbind(country_sentences, df)
}

### Create Country Variable that is attached to a sentence which identifies which countries
### are mentioned in a sentence. Will be helpful later

# Step 1 Make True False Variables to identify where countries are mentioned
# can't be bothered to figure out how to make this a for loop at this point
country_sentences <- country_sentences |>
  mutate(ghana = grepl("ghana", sentence_tokens),
         kenya = grepl("kenya", sentence_tokens),
         cameroon =  grepl("cameroon", sentence_tokens),
         chad = grepl("chad", sentence_tokens),
         nigeria = grepl("nigeria", sentence_tokens),
         liberia = grepl("liberia", sentence_tokens),
         malawi = grepl("malawi", sentence_tokens),
         mozambique = grepl("mozambique", sentence_tokens),
         guinea = grepl("guinea", sentence_tokens),
         senegal = grepl("senegal", sentence_tokens)
  )

# Step 2 Check for Sentences that mention multiple countries if we assign a "country"
# variable, for sentences that contain multiple countries we won't be able to track
# the average sentiment for those countries because their unsplit country variable
# would be "country1, country2"
countries_per_sentence <- country_sentences |> select(c("ghana":"senegal")) |> 
  rowSums() %>% data.frame(.) %>% rename("n" = ".")

countries_sentences <- cbind(country_sentences, countries_per_sentence)

# Step 3 Split data frame into n = 1 and n > 1

# n = 1 path
countries_sentences_one <- countries_sentences |> filter(n == 1)

countries_sentences_one <- countries_sentences_one |>
  mutate(
    country = ifelse(ghana == TRUE, "ghana", 
              ifelse(kenya == TRUE, "kenya",
              ifelse(cameroon == TRUE, "cameroon",
              ifelse(chad == TRUE, "chad",
              ifelse(nigeria == TRUE, "nigeria",
              ifelse(liberia == TRUE, "liberia",
              ifelse(malawi == TRUE, "malawi",
              ifelse(mozambique == TRUE, "mozambique",
              ifelse(guinea == TRUE, "guinea",
              ifelse(senegal == TRUE, "senegal", NA))))))))))
  ) |>
  select(!c(ghana:senegal) & !n)

# n > 1 path
countries_sentences_more <- countries_sentences |> filter(n > 1)

countries_sentences_more <- countries_sentences_more |> select(!n) |> 
  pivot_longer(cols = c(ghana:senegal), names_to = "country", values_to = "extra") |>
  select(!extra)

# Combine Data Frames

countries_sentences <- full_join(countries_sentences_more, countries_sentences_one,
                               by = "sentence_tokens")

# Clean up joining artifacts and rename sentence_tokens to text and countries to doc_id

countries_sentences <- countries_sentences |>
  mutate(country = ifelse(is.na(country.x), country.y, country.x)) |>
  select(!c(country.x, country.y)) |>
  rename(text = sentence_tokens, doc_id = country)

### Country Level Sentiment Bar Graph
# Create a parsed data frame 
### Note: This step will take some time you should have about 95k observations
countries_sentences <- udpipe(countries_sentences, "english") 

#Join Sentiment Afinn Data into this dataframe
countries_sentences <- countries_sentences |> left_join(sentiment_afinn, 
                                                    by = c("lemma" = "word"))

# Get Sentiment at Country Level, rather than word level

country_sentiment <- countries_sentences |> 
  group_by(doc_id) |>
  summarize(country_sent = mean(value, na.rm = TRUE))

# Create Bar Graph
country_sentiment |>
  group_by(doc_id) |>
  ggplot(aes(x = reorder(doc_id, desc(country_sent)), y = country_sent)) +
  geom_col()  +
  xlab("Country") +
  ylab("Sentiment") + 
  labs(subtitle = "From September 1st, 2023 to November 16, 2024") +
  ggtitle("Average Sentiment of Sentences Mentioning Top 10 Most Mentioned Countries by WHO Africa") +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(color = "black"),
    panel.grid.minor = element_line(color = "grey75"), 
  )

ggsave(paste0(path, "question_2_plot_1.png"), plot = last_plot())

### I think it interesting that the countries tend to have a slightly positive 
### mention on average. I wonder if this is just a bi-product of the articles 
### being written by UN agencies. Many layers of bureaucracy might tone down the 
### language. On the other hand, I would imagine that the top 10 countries mentioned 
### wouldn't all  be slightly positive. If I had more time, I would look more into what's 
### going on with the data. Making this note here in case this topic ever interests
### me again. With me looking at some of the UN reports about Gaza, I wonder if
### this trend is across all UN reports or just from WHO. 