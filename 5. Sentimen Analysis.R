library(dplyr)
library(tidytext)
library(textstem)
library(sentimentr)
library(purrr)

# Load the data
reviews <- readRDS("translated_reviews.rds")

# Filter for restaurants with at least 30 reviews
reviews <- reviews %>%
  group_by(restaurant_name) %>%
  filter(n() >= 30) %>%
  ungroup()

# Eliminate stop-words and perform lemmatization
data("stop_words")
additional_stop_words <- stop_words
additional_stop_words$word <- gsub("'", "’", additional_stop_words$word)

reviews_clean <- reviews %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  anti_join(additional_stop_words, by = "word") %>%
  mutate(word = lemmatize_words(word)) 

# Aggregate text by document
reviews_aggregated <- reviews_clean %>%
  group_by(document) %>%
  summarise(text = paste(word, collapse = " ")) %>%
  ungroup()


# Perform sentiment analysis on the text column
reviews_aggregated$sentences <- get_sentences(reviews_aggregated$text)
reviews_aggregated$sentiment_score <- sentiment_by(reviews_aggregated$sentences)$ave_sentiment

# Add restaurant_name column and filter out NA text
reviews_aggregated <- reviews_aggregated %>%
  left_join(reviews %>% select(document, restaurant_name), by = "document") %>% 
  filter(text!="NA")

# Summarize sentiment score by restaurant
restaurant_sentiment <- reviews_aggregated %>%
  group_by(restaurant_name) %>%
  summarise(avg_sentiment_score = mean(sentiment_score, na.rm = TRUE))

# Save results
saveRDS(restaurant_sentiment, file = "restaurant_sentiment.rds")
saveRDS(reviews_aggregated, file = "reviews_aggregated.rds")