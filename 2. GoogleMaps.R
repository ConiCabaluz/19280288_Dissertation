# Load required libraries
library(rvest)
library(RSelenium)
library(httr)
library(stringr)

# Start a Selenium server and browser
rD <- rsDriver(browser = "chrome", port = 4444L, chromever = "latest")
remDr <- rD[["client"]]

# Function to scrape Google Maps reviews
scrape_google_maps <- function(url) {
  remDr$navigate(url)
  Sys.sleep(10)  # Increased wait time for the page to load
  
  # Extract the restaurant name
  restaurant_name <- remDr$findElement(using = 'xpath', "//h1[contains(@class, 'DUwDvf')]")$getElementText()[[1]]
  
  # Click on the "Reviews" tab to reveal reviews
  tryCatch({
    reviews_tab_button_xpath <- "//*[@id='QA0Szd']/div/div/div[1]/div[2]/div/div[1]/div/div/div[3]/div/div/button[2]"
    reviews_tab_button <- remDr$findElement(using = 'xpath', reviews_tab_button_xpath)
    reviews_tab_button$clickElement()
    Sys.sleep(5)  # Wait for the reviews to load
  }, error = function(e) {
    message("Could not find the reviews tab button. Skipping reviews extraction.")
    return(list(restaurant_name = restaurant_name, reviews = NA))
  })
  
  # Function to scroll the side panel and load more reviews
  scroll_to_load_reviews <- function() {
    side_panel <- remDr$findElement(using = 'css selector', 'div.m6QErb.DxyBCb.kA9KIf.dS8AEf.XiKgde')
    last_height <- as.numeric(remDr$executeScript("return arguments[0].scrollHeight", list(side_panel)))
    
    while (TRUE) {
      remDr$executeScript("arguments[0].scrollTop = arguments[0].scrollHeight", list(side_panel))
      Sys.sleep(2)  # Wait for more reviews to load
      new_height <- as.numeric(remDr$executeScript("return arguments[0].scrollHeight", list(side_panel)))
      
      print(paste("last_height:", last_height, "new_height:", new_height))  # Debugging output
      
      if (new_height == last_height) {
        print("No more reviews to load.")
        break
      }
      last_height <- new_height
    }
  }
  
  scroll_to_load_reviews()
  
  # Click on all "Read more" buttons to expand long reviews
  expand_long_reviews <- function() {
    read_more_buttons <- remDr$findElements(using = 'css selector', 'button.w8nwRe.kyuRq')
    for (button in read_more_buttons) {
      button$clickElement()
      Sys.sleep(0.5)  # Wait a bit after clicking to ensure the text expands
    }
  }
  
  expand_long_reviews()  # Ensure long reviews are expanded
  
  # Get the page source after loading all reviews
  page_source <- remDr$getPageSource()[[1]]
  
  # Parse the page source
  page <- read_html(page_source)
  
  # create data frame
  reviews <- data.frame(text = character(), date = character(), rating = character(), stringsAsFactors = FALSE)
  
  # extract review elements
  review_elements <- page %>% html_nodes(".jftiEf.fontBodyMedium")
  
  for (element in review_elements) {
    review_text <- element %>% html_node(".wiI7pd") %>% html_text(trim = TRUE)
    review_date <- element %>% html_node(".rsqaWe") %>% html_text(trim = TRUE)
    review_rating <- element %>% html_node(".kvMYJc") %>% html_attr("aria-label") %>% str_extract("\\d+")
    
    reviews <- rbind(reviews, data.frame(text = review_text, date = review_date, rating = review_rating, stringsAsFactors = FALSE))
  }
  
  # Add restaurant name and reviews to the results
  result <- list(
    restaurant_name = restaurant_name,
    reviews = reviews
  )
  
  return(result)
}

# Initialize an empty list to store results
all_results <- list()

# Loop through each restaurant URL and gather data
for (url in restaurant_urls) {
  result <- scrape_google_maps(url)
  all_results <- append(all_results, list(result))
}

# Combine results into a single data frame
all_reviews <- do.call(rbind, lapply(all_results, function(x) {
  if (!is.null(x$reviews) && nrow(x$reviews) > 0) {
    cbind(restaurant_name = x$restaurant_name, x$reviews)
  }
}))

# Save results
saveRDS(all_results, "all_results.rds")
saveRDS(all_reviews, "all_reviews.rds")

# Close the browser and server
remDr$close()
rD[["server"]]$stop()
