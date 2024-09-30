library(RSelenium)
library(dplyr)

# cleaning URLS to avoid Google translation
clean_urls <- sub("\\?.*", "", restaurant_urls)


# Start the RSelenium server and browser
rD <- rsDriver(browser = "firefox", port = 4545L, verbose = FALSE)
remDr <- rD[["client"]]

scrape_restaurant_data <- function(url) {
  remDr$navigate(url)
  
  # Wait for the page to load completely
  Sys.sleep(10)
  
  # Extract restaurant name
  restaurant_name <- tryCatch({
    name <- remDr$findElement(using = "css selector", ".DUwDvf")$getElementText()[[1]]
    print(paste("Restaurant Name:", name))
    name
  }, error = function(e) {
    message("Error finding restaurant name: ", e$message)
    NA
  })
  
  # Extract cuisine type
  cuisine_type <- tryCatch({
    type <- remDr$findElement(using = "css selector", ".DkEaL")$getElementText()[[1]]
    print(paste("Cuisine Type:", type))
    type
  }, error = function(e) {
    message("Error finding cuisine type: ", e$message)
    NA
  })
  
  # Extract price range
  price_range <- tryCatch({
    price <- remDr$findElement(using = "css selector", ".mgr77e > span:nth-child(1) > span:nth-child(2) > span:nth-child(1) > span:nth-child(1)")$getElementText()[[1]]
    print(paste("Price Range:", price))
    price
  }, error = function(e) {
    message("Error finding price range: ", e$message)
    NA
  })
  
  # Extract address
  address <- tryCatch({
    addr <- remDr$findElement(using = "css selector", "div.RcCsl:nth-child(3) > button:nth-child(2) > div:nth-child(1) > div:nth-child(2)")$getElementText()[[1]]
    print(paste("Address:", addr))
    addr
  }, error = function(e) {
    message("Error finding address: ", e$message)
    NA
  })
  
  # Extract webpage
  webpage <- tryCatch({
    web <- remDr$findElement(using = "css selector", "a[data-item-id='authority'][href^='http']")$getElementAttribute("href")[[1]]
    print(paste("Webpage:", web))
    web
  }, error = function(e) {
    message("Error finding webpage with primary selector: ", e$message)
    tryCatch({
      web <- remDr$findElement(using = "css selector", "div.RcCsl:nth-child(6) > a:nth-child(2)[href^='http']")$getElementAttribute("href")[[1]]
      print(paste("Webpage (Alternative):", web))
      web
    }, error = function(e) {
      message("Error finding webpage with alternative selector: ", e$message)
      NA
    })
  })
  
  # Try the first button to reveal opening hours
  opening_hours <- tryCatch({
    remDr$findElement(using = "css selector", ".OMl5r")$clickElement()
    Sys.sleep(2)  # Wait for the opening hours to be revealed
    hours <- remDr$findElement(using = "css selector", ".eK4R0e > tbody:nth-child(1)")$getElementText()[[1]]
    print(paste("Opening Hours:", hours))
    hours
  }, error = function(e) {
    message("Error finding opening hours with primary selector: ", e$message)
    # If the first button fails, try the alternative button and selector
    tryCatch({
      remDr$findElement(using = "css selector", "div.RcCsl:nth-child(4) > button:nth-child(2)")$clickElement()
      Sys.sleep(2)  # Wait for the opening hours to be revealed
      hours <- remDr$findElement(using = "css selector", "div.OqCZI:nth-child(1) > div:nth-child(2) > div:nth-child(1) > table:nth-child(1) > tbody:nth-child(1)")$getElementText()[[1]]
      print(paste("Opening Hours (Alternative):", hours))
      hours
    }, error = function(e) {
      message("Error finding opening hours with alternative selector: ", e$message)
      NA
    })
  })
  
  # Create data frame
  data.frame(
    restaurant_name = ifelse(is.na(restaurant_name), "N/A", restaurant_name),
    cuisine_type = ifelse(is.na(cuisine_type), "N/A", cuisine_type),
    price_range = ifelse(is.na(price_range), "N/A", price_range),
    address = ifelse(is.na(address), "N/A", address),
    webpage = ifelse(is.na(webpage), "N/A", webpage),
    opening_hours = ifelse(is.na(opening_hours), "N/A", opening_hours),
    stringsAsFactors = FALSE
  )
}

# Scrape data for each URL
restaurant_data <- lapply(clean_urls, scrape_restaurant_data)

# Stop the RSelenium server and browser
remDr$close()
rD$server$stop()

#Bind rows into df
restaurant_data_df <- bind_rows(restaurant_data)
saveRDS(restaurant_data_df, file = "restaurant_info.rds")

# Cleansing and Preprocessing Restaurant info 

# Replace the price_range column
restaurant_data_df <- restaurant_data_df %>%
  mutate(price_range = case_when(
    grepl("£1–10|£10–20", price_range) ~ "£",
    grepl("£20–30|£20–40|£10–30", price_range) ~ "££",
    grepl("£30–40|£30–50|£40–50", price_range) ~ "£££",
    grepl("£50–60|£60–70|£100\\+", price_range) ~ "££££",
    TRUE ~ "Unknown"  # For "N/A", hotels, and dollar signs
  ))

# Categorize cuisine types
restaurant_data_df <- restaurant_data_df %>%
  mutate(cuisine_category = case_when(
    # European Cuisines
    cuisine_type %in% c("Italian restaurant", "Italian", "Polish restaurant", 
                        "Greek restaurant", "Spanish restaurant", "Portuguese", 
                        "British restaurant", "British", "French restaurant", 
                        "Modern European restaurant", "Eastern European restaurant", 
                        "Cantabrian restaurant", "Gastropub", "Brasserie", 
                        "French Brasserie", "Tapas", "Tapas bar") ~ "European",
    
    # Asian Cuisines
    cuisine_type %in% c("Thai restaurant", "Nepalese restaurant", "Japanese restaurant", 
                        "Sri Lankan restaurant", "Korean restaurant", "Chinese restaurant", 
                        "Chinese noodle restaurant", "Tibetan restaurant", 
                        "Indian restaurant", "Indian", "Asian restaurant", 
                        "Pan-Asian", "Pan-Asian restaurant", "Modern Asian") ~ "Asian",
    
    # Middle Eastern / Mediterranean Cuisines
    cuisine_type %in% c("Lebanese restaurant", "Turkish restaurant", 
                        "Middle Eastern restaurant", "Mediterranean restaurant", 
                        "Mediterranean", "North African and eastern Mediterranean fusion") ~ "Middle Eastern / Mediterranean",
    
    # Latin American Cuisines
    cuisine_type %in% c("Caribbean restaurant", "Jamaican restaurant", 
                        "Argentinian restaurant", "Mexican restaurant") ~ "Latin American",
    
    # North American Cuisines
    cuisine_type %in% c("American restaurant", "Hamburger restaurant", 
                        "Steak house") ~ "North American",
    
    # Seafood / Fish-Based Cuisines
    cuisine_type %in% c("Seafood restaurant", "Fish & chips restaurant") ~ "Seafood / Fish-Based",
    
    # Fast Food and Casual Dining
    cuisine_type %in% c("Fast food", "Fast Food", "Fast food restaurant", 
                        "Takeout restaurant", "Chinese takeaway", 
                        "Pizza restaurant", "Pizza", "Sushi restaurant") ~ "Fast Food / Casual Dining",
    
    # Café and Informal Dining
    cuisine_type %in% c("Cafe", "Brunch restaurant", "Brunch", 
                        "Family restaurant", "Coffee shop", "Pub") ~ "Café and Informal Dining",
    
    # African Cuisines
    cuisine_type == "Ethiopian" ~ "African",
    
    # Fine Dining
    cuisine_type == "Fine dining restaurant" ~ "Fine Dining",
    
    # Default / Unknown or Miscellaneous
    TRUE ~ "Other"
  ))

# Add columns for each day with open hours as 24 hours format 
# Check AM/PM 
ensure_am_pm <- function(time_str, reference_str) {
  # If the time string doesn't have AM/PM but the reference time does, inherit AM/PM from the reference
  if (!grepl("AM|PM", time_str, ignore.case = TRUE)) {
    if (grepl("PM", reference_str, ignore.case = TRUE)) {
      return(paste0(time_str, " PM"))
    } else if (grepl("AM", reference_str, ignore.case = TRUE)) {
      return(paste0(time_str, " AM"))
    }
  }
  return(time_str)
}

# Convert time to 24-hour format
convert_to_24_hour <- function(time_str) {
  # Convert to a proper datetime object with AM/PM and format it to 24-hour time
  time_24 <- format(parse_date_time(time_str, orders = c("I:M p", "I p")), "%H:%M")
  return(time_24)
}

# Combined functions 
extract_and_convert_hours_24 <- function(text, day) {
  # Extract the time ranges using regex for the specific day
  day_pattern <- paste0(day, "\\n([^\\n]+)")
  match <- str_match(text, day_pattern)[, 2]
  
  if (!is.na(match)) {
    match <- trimws(match)  # Trim whitespace
    
    if (grepl("–", match)) {
      times <- unlist(strsplit(match, "–"))
      
      # Ensure AM/PM for both times
      start_time <- ensure_am_pm(trimws(times[1]), trimws(times[2]))
      end_time <- ensure_am_pm(trimws(times[2]), start_time)
      
      # Convert both times to 24-hour format
      start_time_24 <- convert_to_24_hour(start_time)
      end_time_24 <- convert_to_24_hour(end_time)
      
      return(paste(start_time_24, "-", end_time_24))
    } else {
      return(match)  # For cases like "Closed"
    }
  } else {
    return(NA)  # If no hours are found for that day
  }
}

# Apply functions to df
restaurant_data_df <- restaurant_data_df %>% 
  rowwise() %>% 
  mutate(
    monday = extract_and_convert_hours_24(opening_hours, "Monday"),
    tuesday = extract_and_convert_hours_24(opening_hours, "Tuesday"),
    wednesday = extract_and_convert_hours_24(opening_hours, "Wednesday"),
    thursday = extract_and_convert_hours_24(opening_hours, "Thursday"),
    friday = extract_and_convert_hours_24(opening_hours, "Friday"),
    saturday = extract_and_convert_hours_24(opening_hours, "Saturday"),
    sunday = extract_and_convert_hours_24(opening_hours, "Sunday")
  ) %>% 
  ungroup()

# Save result
saveRDS(restaurant_data_df, file = "restaurant_data_df.rds")

