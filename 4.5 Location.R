extract_place_name_and_coords <- function(urls) {
  # Initialize an empty data frame to store the results
  result <- data.frame(place_name = character(), lat = numeric(), lon = numeric(), stringsAsFactors = FALSE)
  
  for (url in urls) {
    # Extract the restaurant name
    place_name <- regmatches(url, regexpr("place/([^/]+)", url))
    place_name <- gsub("place/", "", place_name)  # Remove "place/" from the name
    
    # Decode any URL-encoded characters and replace "+" with spaces
    place_name <- URLdecode(place_name)
    place_name <- gsub("\\+", " ", place_name)  
    
    # Capture latitude and longitude
    lat_lon_matches <- regmatches(url, regexec("3d([0-9.-]+)!4d([0-9.-]+)", url))
    
    # Check if there was a match for coordinates
    if (length(lat_lon_matches[[1]]) >= 3) {
      lat <- as.numeric(lat_lon_matches[[1]][2])
      lon <- as.numeric(lat_lon_matches[[1]][3])
    } else {
      lat <- NA
      lon <- NA
    }
    
    # Append the results to the data frame
    result <- rbind(result, data.frame(restaurant_name = place_name, lat = lat, lon = lon, stringsAsFactors = FALSE))
  }
  
  return(result)
}

# Extract latitude and longitude from URLS
place_data <- extract_place_name_and_coords(urls)

# Save results
saveRDS(place_data, file = "place_data.rds")
