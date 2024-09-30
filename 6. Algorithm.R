library(tidyverse)
library(sentimentr)
library(purrr)

# Calculate popularity score based on the number of positive reviews (rating >= 3)
popularity_df <- all_reviews %>%
  group_by(restaurant_name) %>%
  summarise(
    total_reviews = n(),
    positive_reviews = sum(rating >= 3, na.rm = TRUE),
    popularity_score = (positive_reviews / total_reviews) * 10  # Scale the popularity score to a range of 0 to 10
  )


# Merge all data frames (sentiment scores, popularity, metadata and geolocation)
merged_data <- restaurant_sentiment %>%
  left_join(restaurant_data_df, by = "restaurant_name") %>% 
  left_join(popularity_df, by = "restaurant_name") %>% 
  left_join(place_data, by = "restaurant_name")

# Remove restaurants without opening hours 
merged_data <- merged_data %>% 
  filter(opening_hours!="")

# Function to normalize sentiment score
normalize_sentiment <- function(avg_sentiment_score, min_val = -1, max_val = 1, new_min = 0, new_max = 10) {
  normalized_score <- ((avg_sentiment_score - min_val) / (max_val - min_val)) * (new_max - new_min) + new_min
  # Cap the score at 10 if it is greater than or equal to 0.5
  normalized_score <- ifelse(avg_sentiment_score >= 0.85, new_max, normalized_score)
  return(normalized_score)
}

# Normalize sentiment score for the merged data
merged_data <- merged_data %>%
  mutate(normalize_sentiment = normalize_sentiment(avg_sentiment_score, min_val = -0.45, max_val = 0.85))

# Calculate final recommendation score based on sentiment and popularity
merged_data <- merged_data %>%
  mutate(
    recommendation_score = (normalize_sentiment * 0.8) + 
      (popularity_score * 0.2)
  )

# Save results
saveRDS(merged_data, file = "merged_data.rds")



