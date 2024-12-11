# Regular expression for model words from titles
modelWordTitleRegexNew <- "([a-zA-Z0-9]*(([0-9]+[^0-9, ]+)|([^0-9, ]+[0-9]+))[a-zA-Z0-9]*|\\b\\d+:\\d+\\b|\\b\\d+\\s*x\\s*\\d+\\b)"
modelWordKeyValueRegexNew <- "(^\\d+(\\.\\d+)?[a-zA-Z]+$|^\\d+(\\.\\d+)?$|\\b\\d+:\\d+\\b|\\b\\d+\\s*x\\s*\\d+\\b)"

modelWordTitleRegex <- "([a-zA-Z0-9]*(([0-9]+[^0-9, ]+)|([^0-9, ]+[0-9]+))[a-zA-Z0-9]*)"
modelWordKeyValueRegex <- "(^\\d+(\\.\\d+)?[a-zA-Z]+$|^\\d+(\\.\\d+)?$)"

extractModelWords <- function(data, regex) {
  # Apply the regex to extract model words
  words <- str_extract_all(data, regex)
  
  # Flatten the list and remove duplicates
  unique(unlist(words))
}

normalizeModelWords <- function(modelWords) {
  # Remove non-numeric parts (e.g., 'lbs' from '20.8lbs')
  sapply(modelWords, function(word) {
    if (str_detect(word, "\\d+[a-zA-Z]+$")) {
      str_extract(word, "^\\d+(\\.\\d+)?")
    } else {
      word
    }
  })
}

initializeModelWordSetsBaseline <- function(data) {
  MWtitle <- c()
  MWvalue <- c()
  
  for (product in data) {
    # Extract model words from title
    titleWords <- extractModelWords(product$title, modelWordTitleRegex)
    MWtitle <- unique(c(MWtitle, titleWords))
    
    # Extract and normalize model words from key-value pairs
    for (key in names(product$featuresMap)) {
      value <- product$featuresMap[[key]]
      valueWords <- extractModelWords(value, modelWordKeyValueRegex)
      valueWords <- normalizeModelWords(valueWords)
      MWvalue <- unique(c(MWvalue, valueWords))
    }
  }
  
  list(MWtitle = MWtitle, MWvalue = MWvalue)
}

initializeModelWordSetsNew <- function(data) {
  MWtitle <- c()
  MWvalue <- c()
  
  for (product in data) {
    # Extract model words from title
    titleWords <- extractModelWords(product$title, modelWordTitleRegexNew)
    MWtitle <- unique(c(MWtitle, titleWords))
    
    # Extract and normalize model words from key-value pairs
    for (key in names(product$featuresMap)) {
      value <- product$featuresMap[[key]]
      valueWords <- extractModelWords(value, modelWordKeyValueRegexNew)
      valueWords <- normalizeModelWords(valueWords)
      MWvalue <- unique(c(MWvalue, valueWords))
    }
  }
  
  list(MWtitle = MWtitle, MWvalue = MWvalue)
}
