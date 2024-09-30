library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(scales)

# Count the number of reviews and proportion with a language different from "en"
non_english_reviews <- sum(reviews$language != "en", na.rm = TRUE)
total_reviews <- sum(!is.na(reviews$language))
non_english_proportion <- non_english_reviews / total_reviews

# Create the histogram of sentiment scores by review
ggplot(reviews_aggregated, aes(x = sentiment_score)) +
  geom_histogram(binwidth = 0.1, fill = brewer.pal(8, "Set2")[1], color = "white", alpha = 0.9) +
  labs(title = "Sentiment Scores by Review", 
       x = "Sentiment Score", 
       y = "Number of Reviews") +
  theme_minimal(base_size = 15) +  # Base font size for the plot
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),  
    axis.title.x = element_text(margin = margin(t = 10)),  
    axis.title.y = element_text(margin = margin(r = 10)),  
    axis.text = element_text(size = 12),  
    panel.grid.major = element_line(color = "gray85"),  
    panel.grid.minor = element_blank()  
  ) +
  scale_x_continuous(labels = label_number(accuracy = 0.01), breaks = seq(min(reviews_aggregated$sentiment_score), max(reviews_aggregated$sentiment_score), by = 0.5)) +  
  scale_y_continuous(labels= comma, expand = c(0, 0))  # Remove space between axis and bars

# Create the histogram of sentiment scores by restaurant
ggplot(restaurant_sentiment, aes(x = avg_sentiment_score)) +
  geom_histogram(binwidth = 0.1, fill = brewer.pal(8, "Set2")[2], color = "white", alpha = 0.9) +
  labs(title = "Sentiment Scores by Restaurant", 
       x = "Sentiment Score", 
       y = "Number of Restaurants") +
  theme_minimal(base_size = 15) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),  
    axis.title.x = element_text(margin = margin(t = 10)),  
    axis.title.y = element_text(margin = margin(r = 10)),  
    axis.text = element_text(size = 12),  
    panel.grid.major = element_line(color = "gray85"),  
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(labels = scales::label_number(accuracy = 0.01), 
                     breaks = seq(min(restaurant_sentiment$avg_sentiment_score), 
                                  max(restaurant_sentiment$avg_sentiment_score), by = 0.1)) +
  scale_y_continuous(expand = c(0, 0))

# Create Sentiment Score Reviews vs Restaurants
ggplot() +
  
  # Histogram for review sentiment scores
  geom_histogram(data = reviews_aggregated, 
                 aes(x = sentiment_score, y = after_stat(count / sum(count)), fill = "Reviews"), 
                 binwidth = 0.1, 
                 color = "white", alpha = 0.9) +
  
  # Histogram for restaurant sentiment scores
  geom_histogram(data = restaurant_sentiment, 
                 aes(x = avg_sentiment_score, y = after_stat(count / sum(count)), fill = "Restaurants"), 
                 binwidth = 0.1, 
                 color = "white", alpha = 0.6) +
  
  # Labels and title
  labs(title = "Sentiment Scores Proportion: Reviews vs Restaurants", 
       x = "Sentiment Score", 
       y = "Proportion") +
  
  # Custom theme settings
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),  
    axis.title.x = element_text(margin = margin(t = 10)),  
    axis.title.y = element_text(margin = margin(r = 10)),  
    axis.text = element_text(size = 12),  
    panel.grid.major = element_line(color = "gray85"),  
    panel.grid.minor = element_blank()
  ) +
  
  # Scale for x-axis
  scale_x_continuous(labels = scales::label_number(accuracy = 0.01), 
                     breaks = seq(min(c(reviews_aggregated$sentiment_score, restaurant_sentiment$avg_sentiment_score)), 
                                  max(c(reviews_aggregated$sentiment_score, restaurant_sentiment$avg_sentiment_score)), by = 0.5)) +
  
  # Scale for y-axis (%)
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +  
  
  # Add a legend
  scale_fill_manual(name = "Type",  
                    labels = c("Reviews", "Restaurants"),
                    values = c(brewer.pal(8, "Set2")[2], brewer.pal(8, "Set2")[1]))



# Create scatter plot of popularity vs sentiment score
ggplot(merged_data, aes(x = popularity_score, y = avg_sentiment_score)) +
  geom_point(color = brewer.pal(8, "Set2")[3], size = 3, alpha = 0.7) +  
  labs(title = "Popularity vs Sentiment Score", 
       x = "Popularity Score", 
       y = "Sentiment Score") +
  theme_minimal(base_size = 15) +  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),  
    axis.title.x = element_text(margin = margin(t = 10)),  
    axis.title.y = element_text(margin = margin(r = 10)),  
    axis.text = element_text(size = 12), 
    panel.grid.major = element_line(color = "gray85"),  
    panel.grid.minor = element_blank() 
  ) +
  scale_x_continuous(labels = label_number(accuracy = 0.01), breaks = seq(min(merged_data$popularity_score), max(merged_data$popularity_score), by = 0.5)) +  
  scale_y_continuous(labels = label_number(accuracy = 0.01))  

# 7 random rows from reviews_aggregated
set.seed(7874) # Set a seed for reproducibility
random_reviews <- reviews_aggregated[sample(nrow(reviews_aggregated), 7), c("text", "sentiment_score")]

# 7 random rows from merged_data
set.seed(7124) # Set a seed for reproducibility
random_restaurants <- merged_data[sample(nrow(merged_data), 7), c("restaurant_name", "normalize_sentiment")]

