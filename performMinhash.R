performMinHash <- function(vectors) {
  nrHashes <- round(0.5*nrow(vectors)/500)*500
  signMatrix <- matrix(Inf, nrow = nrHashes, ncol = ncol(vectors))
  
  set.seed(123)
  p <- nextPrime(nrow(vectors))
  a <- sample(0:(p-1), nrHashes)
  b <- sample(1:(p-1), nrHashes)

  hash <- matrix(0, nrHashes)
  
  # Perform MinHashing
  for (r in 1:nrow(vectors)) {
    for (h in 1:nrHashes) {
      hash[h] <- (a[h] + b[h] * r) %% p 
    }
    for (c in 1:ncol(vectors)) {
      if (vectors[r, c] == 1) {
        for (i in 1:nrHashes) {
          if (hash[i] < signMatrix[i, c]) {
            signMatrix[i, c] = hash[i]
          }
        }
      }
    }
  }
  
  return(signMatrix)
}



