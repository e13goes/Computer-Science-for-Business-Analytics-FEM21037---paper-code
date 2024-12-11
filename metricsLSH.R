# Function to compute evaluation metrics
metricsLSH <- function(data, trueDuplicates, candidatePairs) {
  
  duplicatesFound <- length(intersect(trueDuplicates, candidatePairs))
  totalDuplicates <- length(trueDuplicates)
  totalPredictedDuplicates <- length(candidatePairs)
  totalComparisons <- length(candidatePairs)
  
  pairQuality <- duplicatesFound / totalComparisons
  pairCompleteness <- duplicatesFound / totalDuplicates
  f1Star <- (2 * pairQuality * pairCompleteness) / (pairQuality + pairCompleteness)
  
  fracComparisons <- totalComparisons/choose(length(data),2)
  
  return(c("Pair Quality" = pairQuality, "Pair Completeness" = pairCompleteness, "F1*-Measure" = f1Star, "Fraction Comparisons" = fracComparisons))
}

