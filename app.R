library(shiny)
library(dplyr)
library(shinyjs)
library(leaflet)

# Load your full data (assuming you have a 'merged_data' dataframe)
merged_data <- readRDS("merged_data.rds")


# Reorder opening hours to start from Monday
reorder_opening_hours <- function(opening_hours) {
  days_order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  lines <- unlist(strsplit(opening_hours, "\n"))
  lines <- lines[order(match(sub(" .*", "", lines), days_order))]
  return(paste(lines, collapse = "\n"))
}

merged_data$opening_hours <- sapply(merged_data$opening_hours, reorder_opening_hours)

ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  
  # Include the custom CSS directly in the app
  tags$head(
    tags$style(HTML("
        body {
          background-color: #222222;
          color: #eeeeee;
        }
        .header-image {
          margin-bottom: 20px;
        }
        .header {
          text-align: center;
          color: #eeeeee;
        }
        .header h1 {
          font-size: 2.5em;
          font-weight: bold;
        }
        .main-content {
          display: flex;
          justify-content: space-between;
          width: 100%;
        }
        .filters {
          background-color: #333333;
          padding: 20px;
          width: 24%;
          border-radius: 8px;
          margin-right: 20px;
        }
        .recommendations {
          width: 75%;
          padding: 20px;
          background-color: #444444;
          border-radius: 8px;
        }
        table {
          width: 100%;
          margin-top: 20px;
        }
        th, td {
          padding: 10px;
          text-align: left;
        }
        th {
          background-color: #555555;
        }
        td {
          background-color: #666666;
        }
        a {
          color: #35B4DB;
          text-decoration: none;
        }
        a:hover {
          text-decoration: underline;
        }
        .header-image img {
          max-width: 100%;
          height: auto;
        }
        .google-maps-icon {
          width: 20px;
          height: 20px;
          margin-left: 5px;
        }
        .http-icon {
          width: 50px;  
          height: 50px; 
          margin-left: 5px;
        }
        .info-icon {
          width: 50px;  
          height: 50px; 
          margin-left: 5px;
        }
        .extra-info {
          display: none;
          margin-top: 10px;
          background-color: #555555;
          padding: 10px;
          border-radius: 8px;
        }
      "))
  ),
  
  # Banner Image
  div(class = "header-image",
      img(src = "imagen.png", alt = "Oxford Foodies", style = "width: 100%; height: auto;")
  ),
  
  # Header
  div(class = "header",
      h1("Find The Best Restaurants Cafes And Bars in Your City")
  ),
  
  # Main content area with filters and recommendations side by side
  div(class = "main-content",
      div(class = "filters",
          selectInput("cuisine", "Cuisine Category:", choices = unique(merged_data$cuisine_category), multiple = TRUE, selected = NULL),
          
          # Use sliderInput for Price Range with numeric values
          sliderInput("price_range", "Price Range (£):", min = 1, max = 4, value = c(1, 4), step = 1),
          
          sliderInput("time_range", "Preferred Time:", min = 0, max = 24, value = c(12, 14), step = 1),
          checkboxGroupInput("days", "Day of the Week:", choices = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
      ),
      div(class = "recommendations",
          h2("Top 5 Recommendations"),
          tableOutput("recommendationsTable")
      )
  )
)

server <- function(input, output, session) {
  
  # Helper function to convert "HH:MM" format to numeric hours (0 to 24)
  convert_time_to_numeric <- function(time_str) {
    if (grepl(":", time_str)) {
      parts <- strsplit(time_str, ":")[[1]]
      hours <- as.numeric(parts[1])
      minutes <- as.numeric(parts[2]) / 60
      return(hours + minutes)
    }
    return(NA)  # Return NA if the time string is invalid
  }
  
  # Helper function to extract opening and closing hours from a day string 
  extract_open_close_times <- function(hours_str) {
    if (is.na(hours_str) || hours_str %in% c("Closed", "closed", "", "N/A")) {
      return(c(NA, NA))
    }
    
    times <- strsplit(hours_str, "-")[[1]]
    if (length(times) == 2) {
      open_time <- convert_time_to_numeric(trimws(times[1]))
      close_time <- convert_time_to_numeric(trimws(times[2]))
      
      return(c(open_time, close_time))
    }
    return(c(NA, NA))  
  }
  
  # Function to filter data based on user inputs
  filtered_data <- reactive({
    data <- merged_data
    
    # Apply Cuisine Filter
    if (!is.null(input$cuisine)) {
      data <- data %>% filter(cuisine_category %in% input$cuisine)
    }
    
    # Apply Price Range Filter
    price_map <- c("£", "££", "£££", "££££")
    min_price <- price_map[input$price_range[1]]
    max_price <- price_map[input$price_range[2]]
    
    data <- data %>%
      filter(price_range >= min_price & price_range <= max_price)
    
    # Apply Day of the Week Filter
    if (!is.null(input$days)) {
      selected_days <- tolower(input$days)
      
      # Filter the data where any of the selected day's columns have valid open hours
      data <- data %>%
        filter_at(vars(all_of(selected_days)), any_vars(
          !(. %in% c("", "N/A", "Closed", "closed", NA))
        ))
    }
    
    # Apply Preferred Time Filter
    if (!is.null(input$time_range)) {
      min_time <- input$time_range[1]
      max_time <- input$time_range[2]
      
      # If no days are selected, check all days
      days_to_check <- if (!is.null(input$days)) tolower(input$days) else c("monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday")
      
      # Filter data where the restaurant is open during the selected time range on any of the selected days
      data <- data %>%
        filter_at(vars(all_of(days_to_check)), any_vars({
          # Extract times for each day's opening hours
          times <- map(., extract_open_close_times)
          
          # Loop through each time and filter by the time range
          valid <- map_lgl(times, function(time) {
            open_time <- time[1]
            close_time <- time[2]
            
            # Handle overnight hours or restaurants open within the time range
            !is.na(open_time) & !is.na(close_time) & (
              (open_time <= max_time & close_time >= min_time) |  # Restaurant is open within the range
                (open_time > close_time & (min_time >= open_time | max_time <= close_time))  # Handles overnight hours
            )
          })
          valid
        }))
    }
    
    # Rank the restaurants based on recommendation score
    data <- data %>%
      arrange(desc(recommendation_score)) %>%
      mutate(ranking = row_number())
    
    # Keep only the top 5 restaurants
    data <- data %>% top_n(-5, ranking)
    
    return(data)
  })
  
  # Render the filtered data in a table with "More Info" button functionality
  output$recommendationsTable <- renderTable({
    filtered_data() %>%
      mutate(
        score = paste0(sprintf("%.1f", recommendation_score), "/10"),
        webpage = paste0("<a href='", webpage, "' target='_blank'><img src='http.png' class='http-icon'></a>"),
        address = paste0(address, 
                         " <a href='https://www.google.com/maps/search/?api=1&query=", 
                         URLencode(address), 
                         "' target='_blank'>",
                         "<img src='Google_Maps_icon.png' class='google-maps-icon'></a>"
        ),
        info = paste0("<button id='more_info_", row_number(), "' onclick='Shiny.onInputChange(\"more_info_button\", ", row_number(), ")'><img src='info.png' class='info-icon'></button>")
      ) %>%
      select(
        Ranking = ranking,
        Restaurant = restaurant_name,
        Score = score, 
        CuisineType = cuisine_type, 
        Price = price_range, 
        Webpage = webpage,
        Info = info
      )
  }, sanitize.text.function = function(x) x) 
  
  # Function to format the opening hours per day
  format_opening_hours <- function(selected_row) {
    days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
    opening_hours_list <- list(
      "Monday" = selected_row$monday,
      "Tuesday" = selected_row$tuesday,
      "Wednesday" = selected_row$wednesday,
      "Thursday" = selected_row$thursday,
      "Friday" = selected_row$friday,
      "Saturday" = selected_row$saturday,
      "Sunday" = selected_row$sunday
    )
    
    formatted_hours <- sapply(days, function(day) {
      hours <- opening_hours_list[[day]]
      if (is.na(hours) || hours == "" || hours %in% c("Closed", "closed", "N/A")) {
        return(paste(day, ": Closed"))
      } else {
        return(paste(day, ": ", hours))
      }
    })
    
    return(paste(formatted_hours, collapse = "<br>"))
  }
  
  # Modal for displaying more information about the selected restaurant
  observeEvent(input$more_info_button, {
    row <- input$more_info_button
    
    # Extract the lat and lon for the selected row
    selected_row <- filtered_data()[row, ]
    lat <- selected_row$lat
    lon <- selected_row$lon
    
    # Check if coordinates are available
    if (!is.na(lat) & !is.na(lon)) {
      showModal(modalDialog(
        title = span(style = "color: black; font-weight: bold;", selected_row$restaurant_name),
        
        # Display formatted opening hours
        h4(span(style = "color: black;", "Opening Hours")),
        HTML(paste("<div style='color:black; font-family:monospace;'>", 
                   format_opening_hours(selected_row), 
                   "</div>")),
        br(),
        
        h4(span(style = "color: black;", "Address")),
        HTML(paste0("<div style='color:black;'>", selected_row$address, 
                    " <a href='https://www.google.com/maps/search/?api=1&query=", 
                    URLencode(selected_row$address), 
                    "' target='_blank'><img src='Google_Maps_icon.png' class='google-maps-icon'></a></div>")),
        
        leafletOutput("map"),  # Placeholder for the map
        easyClose = TRUE,
        footer = NULL
      ))
      
      # Render the leaflet map
      output$map <- renderLeaflet({
        leaflet() %>%
          addTiles() %>%
          setView(lng = lon, lat = lat, zoom = 15) %>%
          addMarkers(lng = lon, lat = lat, 
                     popup = selected_row$restaurant_name)
      })
    } else {
      showModal(modalDialog(
        title = "Location Unavailable",
        "The coordinates for this restaurant are unavailable.",
        easyClose = TRUE
      ))
    }
  })
  
}

shinyApp(ui = ui, server = server)  