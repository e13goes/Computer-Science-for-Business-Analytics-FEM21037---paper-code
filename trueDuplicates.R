# To uncover the true duplicates
getTrueDuplicates <- function(data) {
  trueDuplicates <- list()
  
  for (i in 1:(length(data) - 1)) { 
    for (j in (i + 1):length(data)) { 
      if (data[[i]]$modelID == data[[j]]$modelID) {
        pair <- list(c(i, j))  
        trueDuplicates <- append(trueDuplicates, pair)
      }
    }
  }
  
  return(trueDuplicates)
}

