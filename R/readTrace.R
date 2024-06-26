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

# Function to unnest all list columns and ensure they have consistent lengths
unnest_all <- function(df) {
  df %>%
    unnest(cols = everything(), keep_empty = TRUE)
}

# Read trace
readTrace <- function(paths,
                      format = "simple",
                      delim = "\t",
                      burnin = 0.1,
                      check.names = FALSE,
                      ...) {
  # Enforce argument matching
  if (!is.character(paths)) {
    stop("All paths must be character strings.")
  }
  
  # Check if files exist
  if (!all(file.exists(paths))) {
    missing_files <- paths[!file.exists(paths)]
    stop("The following files do not exist:\n", paste(missing_files, collapse = "\n"))
  }
  
  # Ensure format is either "simple" or "complex"
  format <- match.arg(format, choices = c("simple", "complex"))
  
  if (!is.character(delim) || nchar(delim) != 1) {
    stop("Delimiter must be a single character string.")
  }
  
  if (!is.numeric(burnin) || length(burnin) != 1 || burnin < 0) {
    stop("Burnin must be a single positive numeric value.")
  }
  
  # Helper function to read data based on file extension
  read_data <- function(path, delim, check.names, ...) {
    ext <- tools::file_ext(path)
    if (ext == "json") {
      # Read and parse the JSON file line by line
      json_lines <- readLines(path)
      parsed_data <- map(json_lines, parse_json_safe)
      
      # Filter out any NULL values that failed to parse
      parsed_data <- compact(parsed_data)
      
      # Combine all parsed JSON objects into a single data frame
      df <- bind_rows(parsed_data)
      
      # Unnest all list columns
      df_long <- unnest_all(df)
      
      # Convert columns to character to handle lists correctly
      df_long[] <- lapply(df_long, as.character)
      
      return(df_long)
    } else {
      return(utils::read.table(
        file = path,
        header = TRUE,
        sep = delim,
        check.names = check.names,
        ...
      ))
    }
  }
  
  # Function to read data and get column names
  headers <- lapply(paths, function(path) {
    data <- read_data(path, delim, check.names, nrows = 0, ...)
    colnames(data)
  })
  
  # Ensure all headers match
  unique_headers <- unique(headers)
  if (length(unique_headers) > 1) {
    stop("Not all headers of trace files match.")
  }
  
  # Read in the traces and concatenate multiple responses under the same header
  output <- lapply(seq_along(paths), function(i) {
    message(paste0("Reading in log file ", i))
    out <- read_data(paths[i], delim, check.names, ...)
    
    if (burnin >= nrow(out)) {
      stop("Burnin larger than provided trace file.")
    }
    
    if (burnin >= 1) {
      out <- out[(burnin + 1):nrow(out), ]
    } else if (burnin > 0) {
      discard <- ceiling(burnin * nrow(out))
      out <- out[(discard + 1):nrow(out), ]
    }
    
    # Concatenate multiple responses under the same header
    out <- as.data.frame(lapply(out, function(col) {
      if (is.list(col)) {
        return(paste(col, collapse = ", "))
      } else {
        return(col)
      }
    }), stringsAsFactors = FALSE)
    
    return(out)
  })
  
  return(output)
}

# Call the function
output <- readTrace(paths = c("simple/part_run_1.log", "simple/part_run_2.log"), 
                    format = "simple", 
                    delim = "\t", 
                    burnin = 0.1, 
                    check.names = FALSE)

# Display formatted output
for (i in seq_along(output)) {
  cat(paste("File", i, "\n"))
  print(output[[i]], row.names = TRUE)
  cat("\n")
}
