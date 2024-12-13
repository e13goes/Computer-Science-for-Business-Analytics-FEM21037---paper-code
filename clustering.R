# Function to perform adapted hierarchical single linkage clustering
adaptedHierarchicalClustering <- function(dissimilarityMatrix, threshold) {
  n <- nrow(dissimilarityMatrix)
  clusters <- as.list(1:n)  # Each product starts in its own cluster
  diag(dissimilarityMatrix) <- Inf
  
  while (TRUE) {
    minDist <- Inf
    clusterPair <- NULL
    
    for (i in seq_along(clusters)) {
      for (j in seq_along(clusters)) {
        if (i >= j) next  
        clusterI <- clusters[[i]]
        clusterJ <- clusters[[j]]
        
        clusterDistances <- dissimilarityMatrix[clusterI, clusterJ, drop = FALSE]
        
        if (any(clusterDistances == Inf)) {
          dist <- Inf 
        } else {
          dist <- min(clusterDistances)
        }
        if (dist < minDist) {
          minDist <- dist
          clusterPair <- c(i, j)
        }
      }
    }
    if (minDist > threshold) break
    
    clusterI <- clusters[[clusterPair[1]]]
    clusterJ <- clusters[[clusterPair[2]]]
    mergedCluster <- c(clusterI, clusterJ)
    
    clusters <- clusters[-clusterPair]
    clusters <- append(clusters, list(mergedCluster))
  }
  return(clusters)
}

# Function to get duplicate pairs from clusters
getDuplicatePairs <- function(clusters) {
  duplicatePairs <- list() 
  
  for (cluster in clusters) {
    if (length(cluster) > 1) {
      pairs <- combn(cluster, 2, simplify = FALSE) 
      duplicatePairs <- append(duplicatePairs, pairs)  
    }
  }
  return(duplicatePairs)
}

