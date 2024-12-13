# Function to clean text based on baseline method
cleanTextBaseline <- function(text) {
  text <- gsub('(Inch|-Inch|inches|”|"|-inch| inch|inch)', 'inch', text)
  text <- gsub('(Hertz|hertz|Hz|HZ| hz|-hz|hz)', 'hz', text)
  text <- tolower(text)
  text <- gsub('\\s*(?=inch|hz)', '', text, perl = TRUE)
  
  return(text)
}

# Function to clean title based on new method
cleanTitles <- function(title) {
  title <- gsub('(”|"|inches|-inch| inch|inch)', 'inch', title, ignore.case = TRUE)
  title <- gsub('(hertz|hz|-hz| hz)', 'hz', title, ignore.case = TRUE)
  title <- tolower(title)
  title <- gsub('\\s*(?=inch|hz)', '', title, perl = TRUE)
  title <- gsub("\\[|\\]|\\(|\\)|\\{|\\}|/|-|–", "", title) 
  title <- trimws(title)
  
  return(title)
}

# Function to clean data based on baseline method
cleanDatasetBaseline <- function(data) {
  for (i in seq_along(data)) {
    product <- data[[i]]
    data[[i]]$title <- cleanTextBaseline(product$title)
    
    for (key in names(product$featuresMap)) {
      value <- product$featuresMap[[key]]
      cleanedValue <- cleanTextBaseline(value)
      data[[i]]$featuresMap[[key]] <- cleanedValue
    }
  }
  
  return(data)
}

# Function to clean data based on new method
cleanDatasetNew <- function(data) {
  for (i in seq_along(data)) {
    product <- data[[i]]
    data[[i]]$title <- cleanTitles(product$title)
    
    for (key in names(product$featuresMap)) {
      value <- product$featuresMap[[key]]
      cleanedValue <- cleanTextBaseline(value)
      data[[i]]$featuresMap[[key]] <- cleanedValue
    }
  }
  
  return(data)
}
