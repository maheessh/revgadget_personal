# Load necessary libraries
library(tidyverse)
library(jsonlite)

# Function to safely parse each line of JSON
parse_json_safe <- function(line) {
  tryCatch(
    fromJSON(line, flatten = TRUE),
    error = function(e) {
      message("Error parsing line: ", line)
      NULL
    }
  )
}

# Function to read and parse JSON log file
read_and_parse_json <- function(file_path) {
  # Read JSON lines from file
  json_lines <- readLines(file_path)
  
  # Parse JSON data
  parsed_data <- lapply(json_lines, parse_json_safe)
  parsed_data <- compact(parsed_data)
  
  # Combine parsed data into a single data frame
  df <- bind_rows(parsed_data)
  
  # Return the data frame
  return(df)
}

# Function to unnest columns with multiple values
unnest_multiple_values <- function(data) {
  data %>%
    # Unnest the list columns with each value in a separate column
    mutate(rates_morpho = map(rates_morpho, ~set_names(.x, paste0("rates_morpho_", seq_along(.x)))),
           rates_morpho2 = map(rates_morpho2, ~set_names(.x, paste0("rates_morpho2_", seq_along(.x))))) %>%
    unnest_wider(rates_morpho) %>%
    unnest_wider(rates_morpho2)
}

# Path to the log file
file_path <- "simple/part_run_1.log"

# Read and parse JSON data from the log file
parsed_data <- read_and_parse_json(file_path)

# Unnest columns with multiple values
formatted_data <- unnest_multiple_values(parsed_data)

# Set options to display more rows and columns
options(max.print = 10000, width = 120)

# Display the formatted data frame in a viewer or console
View(formatted_data)  # Use View() for interactive viewing in RStudio
print(head(formatted_data, 10))  # Print the first 10 rows in the console
