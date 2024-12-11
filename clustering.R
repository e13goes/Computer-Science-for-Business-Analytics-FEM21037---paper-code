# Function to perform adapted hierarchical single linkage clustering
adaptedHierarchicalClustering <- function(dissimilarityMatrix, threshold) {

  n <- nrow(dissimilarityMatrix)
  clusters <- as.list(1:n)  # Each product starts in its own cluster
  
  # Replace dissimilarity matrix diagonal with Inf to avoid self-merging
  diag(dissimilarityMatrix) <- Inf
  
  # Iteratively merge clusters
  while (TRUE) {
    # Find the pair of clusters with the smallest distance
    minDist <- Inf
    clusterPair <- NULL
    
    for (i in seq_along(clusters)) {
      for (j in seq_along(clusters)) {
        if (i >= j) next  # Avoid redundant comparisons
        
        # Extract distances between clusters i and j
        clusterI <- clusters[[i]]
        clusterJ <- clusters[[j]]
        
        clusterDistances <- dissimilarityMatrix[clusterI, clusterJ, drop = FALSE]
        
        # Determine minimum distance between the two clusters
        if (any(clusterDistances == Inf)) {
          dist <- Inf  # If any distance is Inf, cluster distance is Inf
        } else {
          dist <- min(clusterDistances)
        }
        
        # Update if this is the smallest distance found
        if (dist < minDist) {
          minDist <- dist
          clusterPair <- c(i, j)
        }
      }
    }
    
    # Stop if the smallest distance exceeds the threshold
    if (minDist > threshold) break
    
    # Merge the two nearest clusters
    clusterI <- clusters[[clusterPair[1]]]
    clusterJ <- clusters[[clusterPair[2]]]
    mergedCluster <- c(clusterI, clusterJ)
    
    # Update the cluster list
    clusters <- clusters[-clusterPair]
    clusters <- append(clusters, list(mergedCluster))
  }
  
  # Return the final clusters
  return(clusters)
}

getDuplicatePairs <- function(clusters) {
  duplicatePairs <- list()  # Initialize an empty list
  
  for (cluster in clusters) {
    if (length(cluster) > 1) {
      # Generate all possible pairs of products in the cluster
      pairs <- combn(cluster, 2, simplify = FALSE)  # Returns a list of pairs
      duplicatePairs <- append(duplicatePairs, pairs)  # Append each pair to the list
    }
  }
  
  return(duplicatePairs)
}

