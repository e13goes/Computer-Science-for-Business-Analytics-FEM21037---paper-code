# Computer-Science-for-Business-Analytics-FEM21037---paper-code

## cleanBaseline.R
This file contains the functions needed to do the cleaning of the data for both the baseline method and the methods presented in the paper.

**cleanTextBaseline(text)** : Function to clean given text (titles and key-value pairs) according to the baseline method.

**cleanTitles(title)** : Function to clean given title according to the method presented in my paper.

**cleanDatasetBaseline(data)** : Function to clean given data according to the baseline method.

**cleanDatasetNew(data)** : Function to clean given data according to the method presented in my paper.

## clustering.R
This file contains the functions to perform adapted hierarchical single linkage clustering and obtain the duplicate pairs from the clusters.

**adaptedHierarchicalClustering(dissimilarityMatrix, threshold)** : Function to perform adapted hierarchical single linkage clustering and obtain clusters based on the given dissimilarity matrix and threshold.

**getDuplicatePairs(clusters)** : Function to extract the duplicate pairs from given the clusters.

## createBinaryVectors.R
This file contains the function to create the binary characteristic vectors of the products.

**createBinaryVectors(data, MWtitle, MWvalue)** : Function to create binary characteristic vectors of the products given the data and the model words extracted from the titles and key-value pairs.

## extractMW.R
This file contains the different regular expressions and functions to extract model words from the titles and key-value pairs.

**extractModelWords(data, regex)** : Function to extract model words from the given data based on the given regex. 

**normalizeModelWords(modelWords)** : Function to remove the non-numerical parts of the given model words.

**initializeModelWordSetsBaseline(data)** : Function to extract the model words from the titles and key-value pairs of the given data based on the baseline method.

**initializeModelWordSetsNew(data)** : Function to extract the model words from the titles and key-value pairs of the given data based on the method presented in my paper.

## F1Measure.R
This file contains the function to calculate the F1-measure (harmonic mean between precision and recall).

**F1Measure(trueDuplicates, predictedDuplicates)** : Function to calculate the F1-measure based on the given set of true and predicted duplicates. 

## jaccardDissimilarityMatrix.R
This file contains the functions to calculate the Jaccard similarity and create a dissimilarity matrix based on these similarities.

**jaccardSimilarity(a, b)** : Function to calculate the Jaccard similarity between the given vectors/sets a and b.

**sameWebshop(product1, product2)** : Function to check whether the given two product share the same Web shop.

**extractBrand(product)** : Function to extract the brand of the given product. This can be extracted from the key-value pairs or from the title based on the set **knownBrands** also defined in this file.

**sameBrand(product1, product2)** : Function to check whether the given two products share the same brand.

**dissimilarityMatrixNew(data, binaryVectors, candidatePairs)** : Function to set up the dissimilarity matrix based on the given data, binary characteristic vectors, and candidate pairs from LSH. The Jaccard similarity is used as similarity measure.

## metricsLSH.R
This file contains the function to calculate the different performance measures for LSH.

**metricsLSH(data, trueDuplicates, candidatePairs)** : Function to calculate pair quality, pair completeness, and the F1*-measure of the given data, true duplicates and candidate pairs from LSH.

## performLSHBanding.R
This file contains the function to perform LSH.

**performLSHBanding(signatureMatrix, numBands)** : Function to perform LSH and obtain candidate pairs using the given signature matrix and number of bands. The procedure and hash functions are as described in my paper.

## performMinhash.R
This file contains the function to perform MinHashing.

**performMinHash(vectors)** : Function to perform MinHashing and obtain a signature matrix of the given characteristic verctors. The procedure and hash functions are as described in my paper.

## trueDuplicates.R
This file contains the function to extract the true duplicates.

**getTrueDuplicates(data)** : Function the get the true duplicates from the given data by checking modelID's.

