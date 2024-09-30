library(dplyr)
library(lubridate)
library(stringr)
library(cld2)
library(googleLanguageR)


# Step 1: Remove rows with empty restaurant_name or text #
all_reviews <- all_reviews %>%
  filter(restaurant_name != "", text != "")

# Step 2: Convert relative dates to actual dates #
convert_relative_date <- function(relative_date) {
  # Convert the date string to lowercase and remove any whitespace
  relative_date <- tolower(trimws(relative_date))
  
  # Tokenize the date string
  parts <- str_split(relative_date, "\\s+", simplify = TRUE)
  
  if (length(parts) != 3) {
    stop("Unexpected date format: ", relative_date)
  }
  
  # Handle cases with words "un", "una"
  if (parts[2] %in% c("un", "una")) {
    number <- 1
    unit <- parts[3]
  } else {
    number <- as.numeric(parts[2])
    unit <- parts[3]
  }
  
  # Create a dictionary for unit conversion
  unit_dict <- list(
    "hora" = "hours",
    "horas" = "hours",
    "día" = "days",
    "dias" = "days",
    "día" = "days",
    "días" = "days",
    "semana" = "weeks",
    "semanas" = "weeks",
    "mes" = "months",
    "meses" = "months",
    "año" = "years",
    "años" = "years",
    "ano" = "years",
    "anos" = "years",
    "dia" = "days",
    "dias" = "days"
  )
  
  # Convert the unit to a same form using the dictionary
  unit <- unit_dict[[unit]]
  
  # Check if unit conversion was successful
  if (is.null(unit)) {
    stop("Unexpected date format: ", relative_date) 
  }
  
  # Get the current date
  current_date <- Sys.Date()
  
  # Adjust the current date based on the relative description
  adjusted_date <- switch(unit,
                          "hours" = current_date - hours(number),
                          "days" = current_date - days(number),
                          "weeks" = current_date - weeks(number),
                          "months" = current_date - months(number),
                          "years" = current_date - years(number),
                          stop("Unexpected date format: ", relative_date)
  )
  
  return(as.Date(adjusted_date))
}

# Apply the function to the date column
all_reviews$date <- as.Date(unlist(lapply(all_reviews$date, convert_relative_date)))

# Step 3: Filter out dates older than 3 years #
cutoff_date <- Sys.Date() - years(3)
all_reviews <- all_reviews %>%
  filter(date >= cutoff_date)

# Step 4: Translate reviews and add an ID to each review by restaurant #
Reviews_clean <- all_reviews %>%
  group_by(restaurant_name) %>%
  mutate(ID = row_number(),
         document = paste(restaurant_name, ID, sep = "_")) %>%
  select(document, restaurant_name, ID, date, text)

# Authenticate with Google Cloud for translation API
gl_auth("coursework-413111-c18d8eb63161.json")

# Translate non English reviews
translate_reviews <- function(text, target_lang = "en") {
  translated <- gl_translate(text, target = target_lang)
  return(translated$translatedText)
}
Reviews_clean <- Reviews_clean %>%
  mutate(language = cld2::detect_language(text),
         text = ifelse(language != "en", translate_reviews(text), text))

# Save result
saveRDS(Reviews_clean, file = "translated_reviews.rds")

