# TF(t) = (Number of times term t appears in a document) / (Total number of terms in the document).
getTF <- function(ct, td) {
  return(ct / td)
}

# IDF(t) = log_e(Total number of documents / Number of documents with term t in it).
getIDF <- function(tds, ndt) {
  return(log(tds / ndt))
}

getFeatureVec <- function(data, wordList) {
  featureSet <- data.frame(matrix(NA, nrow=length(data), ncol=length(wordList)))
  tds <- length(data)
  for (i in 1:tds) {
    lineoffeature <- list()
    print(data[[i]])
    items <- unlist(strsplit(data[[i]], "[, ]+"))
    td <- length(items)
    counts <- table(items)
    for ( j in 1:length(wordList)) {
      ct <-  as.integer(counts[names(counts) == wordList[j]]);
      if (length(ct) == 0) {
        ct <- 0
      }
      # count ndt
      ndt <- 0
      for ( k in 1:length(data)) {
        if(grepl(wordList[j], data[[k]], fixed = TRUE))
          ndt <- ndt + 1
      }
      if (ndt == 0)
        ndt <- 1
      print(i)
      print(paste("ct, td, tds, ndt", ct, td, tds, ndt))
      print(getTF(ct, td))
      print(getIDF(tds, ndt))
      lineoffeature[length(lineoffeature) + 1] <- (getTF(ct, td) * getIDF(tds, ndt))
    }
    featureSet[i,] <- lineoffeature
  }
  return(featureSet)
}

# Require dataframe : whole dataset
getWordList <- function(rawData) {
  wordList <- list()
  print(paste("rows:", length(rawData)))
  for (i in 1:length(rawData)) {
    print(rawData[[i]])
    items <- unlist(strsplit(rawData[[i]], "[, ]+"))
    items <- lapply(items, trim)
    items <- lapply(items, tolower)
    items <- unique(items)
    print(paste("This line has :", length(items)))
    haveOrNot <- items %in% wordList
    for (j in 1: length(haveOrNot)) {
      if (!haveOrNot[j]) wordList[length(wordList) + 1] <- items[j]
    }
    
    print(paste("word list length :", length(wordList)))
  }
  return(wordList)
}