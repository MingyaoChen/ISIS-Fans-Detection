myadasvmtrain <- function(sampleTrain) {

  # Initialize weight 4 each row of value
  freq <- c(table(sampleTrain$parkinson))
  freqLen <- length(freq)
  for (i in 1: freqLen) {
    value <- (1/(freq[i]*freqLen))
    freq[i] <- value
  }
  
  weights <- list()
  for (j in 1: nrow(sampleTrain)) {
    for ( k in 1:length(freq)) {
      if (sampleTrain[j,"parkinson"] == as.numeric(names(freq[k]))) {
        weights[length(weights) + 1] <- freq[k]
        break
      } 
    }
  }
  weights<- data.frame(matrix(unlist(weights), nrow=length(weights), byrow=T))
  weights$rowId <- rownames(sampleTrain)
  colnames(weights) <- c("quanzhong", "rowId")
  
  adaList <- list()
  numofSamples <- as.integer(nrow(sampleTrain) / 5)
  for (i in 1:5) {
    # Sampling based on weight
    weights <- weights[order(weights$quanzhong), ]
    weightsSum <- sum(as.numeric(weights$quanzhong))
    seeds <- runif(numofSamples, 0.0, weightsSum)
    
    trainIndex <- c()
    for ( i in 1:length(seeds)) {
      addup <- 0.000000
      for (j in 1:nrow(weights)) {
        addup <- addup + weights$quanzhong[j]
        if (addup > seeds[i]) {
          trainIndex <- c(trainIndex, weights$rowId[j])
          break
        }
      }
    }
    
    # Train SVM
    adaTrain <- sampleTrain[trainIndex, ]
    svmModel <- svm(parkinson~., data=adaTrain, kernel="radial")
    summary(svmModel)
    pred <- predict(svmModel, sampleTrain[,1:9])
    confusion <- table(pred, sampleTrain$parkinson)
    
    # calcu the weight of classifier
    error <-  (nrow(sampleTrain) - (confusion[1, 1] + confusion[2, 2] + confusion[3, 3]))/ nrow(sampleTrain)
    alpha = (0.5*log((1.0-error)/max(error,1e-16)))
    
    key <- as.character(alpha)
    value <- svmModel
    adaList[[ key ]] <- value
    
    # update the weight of each row of data
    for (i in 1:length(pred)) {
      rowid <- rownames(sampleTrain)[i]
      if (pred[i] == sampleTrain$parkinson[i]) {
        weights[weights$rowId == rowid, ]$quanzhong <- (weights[weights$rowId == rowid, ]$quanzhong * exp(-error)) / weightsSum
      } else {
        weights[weights$rowId == rowid, ]$quanzhong <- (weights[weights$rowId == rowid, ]$quanzhong * exp(error)) / weightsSum
      }
    }
  }
  return(adaList)
}

myadasvmpredict <- function(sampleTest, adaList) {
  results <- list()
  svmWeights <- names(adaList)
  for (i in 1:nrow(sampleTest)) {
    svmWeights <- names(adaList)
    result <- 0.0
    for (j in 1:length(adaList)) {
      pred <- predict(adaList[j], sampleTest[i,1:9])
      result <- result + (as.numeric(svmWeights[j]) * (as.numeric(unlist(pred)) -1 ))
    }
    results[length(results) + 1] <- as.integer(result)
  }
  
  confusion <- table(unlist(results), sampleTest$parkinson)
  return(confusion)
}
