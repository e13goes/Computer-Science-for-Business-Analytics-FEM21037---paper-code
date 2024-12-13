# Function to calculate the F1-measure
F1Measure <- function(trueDuplicates, predictedDuplicates) {
  duplicatesFound <- length(intersect(trueDuplicates, predictedDuplicates))
  totalDuplicates <- length(trueDuplicates)
  totalPredictedDuplicates <- length(predictedDuplicates)
  
  TP <- duplicatesFound
  FP <- totalPredictedDuplicates - TP
  FN <- totalDuplicates - TP
  
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  F1 <-  (2 * precision * recall) / (precision + recall)
  
  return(F1)
}

