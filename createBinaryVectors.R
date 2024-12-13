# Function to create characteristic vectors
createBinaryVectors <- function(data, MWtitle, MWvalue) {
  allModelWords <- unique(c(MWtitle, MWvalue))
  binaryVectors <- matrix(0, nrow = length(allModelWords), ncol = length(data))
  rownames(binaryVectors) <- allModelWords
  
  for (i in seq_along(data)) {
    product <- data[[i]]
    titleWords <- extractModelWords(product$title, modelWordTitleRegex)
    valueWords <- c()
    
    for (key in names(product$featuresMap)) {
      value <- product$featuresMap[[key]]
      valueWords <- extractModelWords(value, modelWordKeyValueRegex)
    }
    
    valueWords <- normalizeModelWords(valueWords)
    productWords <- unique(c(titleWords, valueWords))
    binaryVectors[rownames(binaryVectors) %in% productWords, i] <- 1
  }
  
  return(binaryVectors)
}
