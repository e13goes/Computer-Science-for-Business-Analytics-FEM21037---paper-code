# Regular expressions for model words from titles and key-value pairs (new and baseline)
modelWordTitleRegexNew <- "([a-zA-Z0-9]*(([0-9]+[^0-9, ]+)|([^0-9, ]+[0-9]+))[a-zA-Z0-9]*|\\b\\d+:\\d+\\b|\\b\\d+\\s*x\\s*\\d+\\b)"
modelWordKeyValueRegexNew <- "(^\\d+(\\.\\d+)?[a-zA-Z]+$|^\\d+(\\.\\d+)?$|\\b\\d+:\\d+\\b|\\b\\d+\\s*x\\s*\\d+\\b)"
modelWordTitleRegex <- "([a-zA-Z0-9]*(([0-9]+[^0-9, ]+)|([^0-9, ]+[0-9]+))[a-zA-Z0-9]*)"
modelWordKeyValueRegex <- "(^\\d+(\\.\\d+)?[a-zA-Z]+$|^\\d+(\\.\\d+)?$)"

# Function to extract model words 
extractModelWords <- function(data, regex) {
  words <- str_extract_all(data, regex)
  unique(unlist(words))
}

# Function to remove non-numerical part of model words
normalizeModelWords <- function(modelWords) {
  sapply(modelWords, function(word) {
    if (str_detect(word, "\\d+[a-zA-Z]+$")) {
      str_extract(word, "^\\d+(\\.\\d+)?")
    } else {
      word
    }
  })
}

# Function to get model words based on baseline method
getModelWordsBaseline <- function(data) {
  MWtitle <- c()
  MWvalue <- c()
  
  for (product in data) {
    titleWords <- extractModelWords(product$title, modelWordTitleRegex)
    MWtitle <- unique(c(MWtitle, titleWords))
    
    for (key in names(product$featuresMap)) {
      value <- product$featuresMap[[key]]
      valueWords <- extractModelWords(value, modelWordKeyValueRegex)
      valueWords <- normalizeModelWords(valueWords)
      MWvalue <- unique(c(MWvalue, valueWords))
    }
  }
  
  list(MWtitle = MWtitle, MWvalue = MWvalue)
}

# Function to get model words based on new method
getModelWordsNew <- function(data) {
  MWtitle <- c()
  MWvalue <- c()
  
  for (product in data) {
    titleWords <- extractModelWords(product$title, modelWordTitleRegexNew)
    MWtitle <- unique(c(MWtitle, titleWords))
    
    for (key in names(product$featuresMap)) {
      value <- product$featuresMap[[key]]
      valueWords <- extractModelWords(value, modelWordKeyValueRegexNew)
      valueWords <- normalizeModelWords(valueWords)
      MWvalue <- unique(c(MWvalue, valueWords))
    }
  }
  
  list(MWtitle = MWtitle, MWvalue = MWvalue)
}
