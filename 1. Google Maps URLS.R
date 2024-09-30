library(RSelenium)
library(rvest)
library(dplyr)

# Start a Selenium server and browser
rD <- rsDriver(browser = "chrome", port = 4444L, verbose = FALSE)
remDr <- rD$client

# Navigate to Google Maps search page for restaurants in Oxford
search_url <- "https://www.google.com/maps/search/restaurants+in+Oxford/"
remDr$navigate(search_url)

# Allow the page to load completely
Sys.sleep(5)

# Function to scroll down the side panel
scroll_side_panel <- function(driver, scrolls = 10) {
  for (i in 1:scrolls) {
    webElem <- driver$findElement(using = "css selector", "div[role='feed']")
    driver$executeScript("arguments[0].scrollTop = arguments[0].scrollHeight", list(webElem))
    Sys.sleep(2)
  }
}

# Scroll down the side panel to load more results
scroll_side_panel(remDr, scrolls = 50)

# Extract the page source after scrolling
page_source <- remDr$getPageSource()[[1]]
page <- read_html(page_source)

# Extract restaurant URLs
restaurant_urls <- page %>%
  html_nodes("a[href*='/place/']") %>%
  html_attr("href") %>%
  unique()

# Close the browser
remDr$close()
rD$server$stop()

# Save results
urls <- restaurant_urls
saveRDS(urls, file = "urls.rds")
