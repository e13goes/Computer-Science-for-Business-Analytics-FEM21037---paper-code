# Function to calculate the Jaccard similarity
jaccardSimilarity <- function(a, b) {
  intersection <- sum(a & b) 
  union <- sum(a | b)       
  return(intersection / union)          
}

# Function to check if two products are from the same Webshop
sameWebshop <- function(product1, product2) {
  product1$shop == product2$shop
}

# Predefined list of known brands
knownBrands <- c("samsung", "philips", "sony", "sharp", "nec", "hp", "toshiba", "hisense", "sony", "lg", "sanyo", "coby", "panasonic", "sansui", "vizio", "viewsonic", "sunbritetv", "haier", "optoma", "proscan", "jvc", "pyle", "sceptre", "magnavox", "mitsubishi", "supersonic", "compaq", "hannspree", "upstar", "seiki", "rca", "craig", "affinity", "naxa", "westinghouse", "epson", "elo", "sigmac")

# Function to extract brand from key-value pairs or title
extractBrand <- function(product) {
  
  if ("Brand" %in% names(product$featuresMap)) {
    return(product$featuresMap$Brand)
  }
  
  title <- product$title
  for (brand in knownBrands) {
    if (grepl(brand, title, ignore.case = TRUE)) {
      return(brand)
    }
  }
  
  return(NA)
}

# Function to check if two products share same brand
sameBrand <- function(product1, product2) {
  brand1 <- extractBrand(product1)
  brand2 <- extractBrand(product2)

  !is.na(brand1) && !is.na(brand2) && (tolower(brand1) == tolower(brand2))
}

# Function to set up dissimilarity matrix
dissimilarityMatrixNew <- function(data, binaryVectors, candidatePairs) {
  dissimilarityMatrix <- matrix(Inf, nrow = length(data), ncol = length(data))
  
  for (pair in candidatePairs) {
    i <- pair[1]
    j <- pair[2]
    product1 <- data[[i]] 
    product2 <- data[[j]] 
    
    if (sameWebshop(product1, product2) || !sameBrand(product1, product2)) {
      next
    }
    
    dissimilarityMatrix[i, j] <- 1 - jaccardSimilarity(binaryVectors[,i], binaryVectors[,j])
    dissimilarityMatrix[j, i] <- dissimilarityMatrix[i, j] # Symmetric matrix
  }
  return(dissimilarityMatrix)
}


