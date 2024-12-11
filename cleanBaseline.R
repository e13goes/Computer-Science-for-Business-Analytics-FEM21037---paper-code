# Function to normalize units and clean text
cleanTextBaseline <- function(text) {
  # Step 1: Normalize "inch" variations
  text <- gsub('(Inch|-Inch|inches|”|"|-inch| inch|inch)', 'inch', text)
  
  # Step 2: Normalize "hertz" variations
  text <- gsub('(Hertz|hertz|Hz|HZ| hz|-hz|hz)', 'hz', text)
  
  # Step 3: Convert to lowercase
  text <- tolower(text)
  
  # Step 4: Remove spaces and non-alphanumeric characters in front of units
  text <- gsub('\\s*(?=inch|hz)', '', text, perl = TRUE)
  
  return(text)
}

# Function to normalize units and clean title
cleanTitles <- function(title) {
  # Step 1: Normalize "inch" variations
  title <- gsub('(”|"|inches|-inch| inch|inch)', 'inch', title, ignore.case = TRUE)
  
  # Step 2: Normalize "hertz" variations
  title <- gsub('(hertz|hz|-hz| hz)', 'hz', title, ignore.case = TRUE)
  
  # Step 3: Convert to lowercase
  title <- tolower(title)
  
  # Step 4: Remove spaces and non-alphanumeric characters in front of units
  title <- gsub('\\s*(?=inch|hz)', '', title, perl = TRUE)
  
  # Step 5: Remove brackets
  title <- gsub("\\[|\\]|\\(|\\)|\\{|\\}|/|-|–", "", title) 
  
  # Step 6: Trim extra spaces
  title <- trimws(title)
  
  return(title)
}

cleanDatasetBaseline <- function(data) {
  for (i in seq_along(data)) {
    product <- data[[i]]
    
    # Clean title
    data[[i]]$title <- cleanTextBaseline(product$title)
    
    # Clean key-value pairs in featuresMap
    for (key in names(product$featuresMap)) {
      value <- product$featuresMap[[key]]
      cleanedValue <- cleanTextBaseline(value)
      
      # Update the key-value pair
      data[[i]]$featuresMap[[key]] <- cleanedValue

    }
  }
  
  return(data)
}

cleanDatasetNew <- function(data) {
  for (i in seq_along(data)) {
    product <- data[[i]]
    
    # Clean title
    data[[i]]$title <- cleanTitles(product$title)
    
    # Clean key-value pairs in featuresMap
    for (key in names(product$featuresMap)) {
      value <- product$featuresMap[[key]]
      cleanedValue <- cleanTextBaseline(value)
      
      # Update the key-value pair
      data[[i]]$featuresMap[[key]] <- cleanedValue
      
    }
  }
  
  return(data)
}
