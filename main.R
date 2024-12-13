library(jsonlite)
library(stringr)
library(stringdist)
library(numbers)

# Load JSON data
data <- read_json("file_path")
data <- unlist(data, recursive = FALSE)
dataBaseline <- cleanDatasetBaseline(data)
dataNew <- cleanDatasetNew(data)

# Initialize parameters and results
nrBootstraps <- 5
resultsBaseline <- list()
resultsNew <- list()
F1Measures <- vector()
numBands <- 1 # change to get different thresholds

# Perform bootstrapping
start.time <- Sys.time()
for (b in 1:nrBootstraps) {
  cat("Bootstrap", b, "\n")
  
  set.seed(123 + b)
  trainIndices <- sample(1:length(dataBaseline), size = length(dataBaseline), replace = TRUE)
  dataBaselineTrain <- dataBaseline[trainIndices[!duplicated(trainIndices)]]
  dataNewTrain <- dataNew[trainIndices[!duplicated(trainIndices)]]
  cat("Size train data:", length(dataNewTrain), "\n")
  
  MWBaseline <- getModelWordsBaseline(dataBaselineTrain)
  MWBaselineTitle <- MWBaseline$MWtitle
  MWBaselineValue <- MWBaseline$MWvalue
  
  MWNew <- getModelWordsNew(dataNewTrain)
  MWNewTitle <- MWNew$MWtitle
  MWNewValue <- MWNew$MWvalue
  
  binaryVectorsBaseline <- createBinaryVectors(dataBaselineTrain, MWBaselineTitle, MWBaselineValue)
  binaryVectorsNew <- createBinaryVectors(dataNewTrain, MWNewTitle, MWNewValue)
  
  signMatrixBaseline <- performMinHash(binaryVectorsBaseline)
  signMatrixNew <- performMinHash(binaryVectorsNew)
  
  candidatePairsBaseline <- performLSHBanding(signMatrixBaseline, numBands = numBands)
  candidatePairsNew <- performLSHBanding(signMatrixNew, numBands = numBands)
  
  trueDuplicatesBaseline <- getTrueDuplicates(dataBaselineTrain)
  trueDuplicatesNew <- getTrueDuplicates(dataNewTrain)
  
  evalMetricsBaseline <- metricsLSH(dataBaselineTrain, trueDuplicatesBaseline, candidatePairsBaseline)
  evalMetricsNew <- metricsLSH(dataNewTrain, trueDuplicatesNew, candidatePairsNew)
  cat("LSH metrics baseline:")
  print(evalMetricsBaseline)
  cat("LSH metrics new:")
  print(evalMetricsNew)
  
  resultsBaseline[[b]] <- evalMetricsBaseline
  resultsNew[[b]] <- evalMetricsNew
  
  M <- dissimilarityMatrixNew(dataNewTrain, binaryVectorsNew, candidatePairsNew)
  predictedDuplicates <- adaptedHierarchicalClustering(M, threshold = 0.5)
  predictedDuplicatePairs <- getDuplicatePairs(predictedDuplicates)
  F1 <- F1Measure(trueDuplicatesNew, predictedDuplicatePairs)
  F1Measures[b] <- F1
  cat("F1-measure:", F1, "\n")
}

end.time <- Sys.time()
time.taken <- end.time - start.time

resultsBaselineM <- matrix(unlist(resultsBaseline), ncol = 4, byrow = TRUE)
avgResultsBaseline <- colMeans(resultsBaselineM)
cat("Avg PQ baseline:", avgResultsBaseline[1], "\n")
cat("Avg PC baseline:", avgResultsBaseline[2], "\n")
cat("Avg F1* baseline:", avgResultsBaseline[3], "\n")
cat("Avg fraction of comparisons baseline:", avgResultsBaseline[4], "\n")

resultsNewM <- matrix(unlist(resultsNew), ncol = 4, byrow = TRUE)
avgResultsNew <- colMeans(resultsNewM)
cat("Avg PQ new:", avgResultsNew[1], "\n")
cat("Avg PC new:", avgResultsNew[2], "\n")
cat("Avg F1* new:", avgResultsNew[3], "\n")
cat("Avg fraction of comparisons new:", avgResultsNew[4], "\n")
cat("Avg F1", mean(F1Measures), "\n")
cat("Run time:", time.taken)
