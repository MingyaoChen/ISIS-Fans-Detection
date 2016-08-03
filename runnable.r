# Initialzation
rm(list = ls(all = TRUE))
#install.packages("skmeans")
#install.packages("e1071")

# Packages Requires
require("e1071")
require("gdata")
require("dplyr")
require("foreign")
require("streamR")
require("FSelector")
require("RWeka")
require("DMwR")
require("adabag")

source("preprocess.r")
source("featureExtraction.r")
source("myadasvm.r")
setwd("/home/mingyao/Dropbox/R/ImbalancedClassification/")

# ------------------------------------------------------------------------------------------------- Step 0: Load data

# ------------------------------------------------- 1.1 Word Dictionary read in and clean

adj <- read.csv("dataset/dict/data.adj", sep = "", header = FALSE)
adj <- data.frame(matrix(unlist(adj$V5), nrow=length(adj$V5), byrow=T))
verb <- read.csv("dataset/dict/data.verb", sep = "", header = FALSE)
verb <- data.frame(matrix(unlist(verb$V5), nrow=length(verb$V5), byrow=T))
noun <- read.csv("dataset/dict/data.noun", sep = "", header = FALSE)
noun <- data.frame(matrix(unlist(noun$V5), nrow=length(noun$V5), byrow=T))
adv <- read.csv("dataset/dict/data.adv", sep = "", header = FALSE)
adv <- data.frame(matrix(unlist(adv$V5), nrow=length(adv$V5), byrow=T))

names(adj) <- "word"
names(verb) <- "word"
names(noun) <- "word"
names(adv) <- "word"

wordDic <- rbind(adj, verb, noun, adv)
rm(adj, verb, noun, adv)
wordDic <- data.frame(lapply(wordDic, as.character), stringsAsFactors=FALSE)
wordDic <- data.frame(matrix(unlist(lapply(wordDic$word, cleanDic))), stringsAsFactors=FALSE)
names(wordDic) <- "word"
wordDic <-  data.frame(wordDic[!(wordDic$word ==""),])
wordDic <- data.frame(wordDic[!duplicated(wordDic), ])

# Finished. Word Dictionary read in and clean

# ------------------------------------------------- 1.2 Stopwords read in and clean

stopwords <- read.csv("dataset/stopwords.csv", sep = ",", header = FALSE, stringsAsFactors=FALSE)
stopwords <- as.list(stopwords)
stopwords <- data.frame(matrix(unlist(stopwords), nrow=length(stopwords), byrow=T))
names(stopwords) <- "stopword"

# Finished. Stopwords read in and clean

# ------------------------------------------------- 2. ISIS data read in and clean

dataset <- read.csv("dataset/isis.csv", header = TRUE, sep = ",", fill = TRUE)
isisDataset <- data.frame(matrix(unlist(dataset$tweets), nrow=length(dataset$tweets), byrow=T))
isisName <- data.frame(matrix(unlist(dataset$username), nrow=length(dataset$tweets), byrow=T))
isisDataset <- cbind(isisDataset, isisName)
rm(isisName)
names(isisDataset) <- c("tweets", "username")
#rm(dataset)

# 3. Clean ISIS data
# isisDataset$tweets <- lapply(isisDataset$tweets, myClean)  
# isisDataset <- isisDataset[!(isisDataset$tweets==""),]
# isisDataset$tweets <- lapply(isisDataset$tweets, tolower)

# Grouping methods implemented by myself
# usernames <- as.vector(unique(isisDataset$username))
# t <-list()
# for (i in 1:length(usernames)) {
#   tmp <- isisDataset$tweets[isisDataset$username == usernames[i]]
#   print(usernames[i])
#   t[length(t) + 1] <- paste0(tmp, collapse = ",")
# }
# rm(tmp)
# rm(i)
# isisDataset <- do.call(rbind, Map(data.frame, tweets=t, username=usernames)) 
# isisDataset <- data.frame(lapply(isisDataset, tolower))
# rm(t)
# rm(usernames)

# isisLabel <- data.frame(matrix(unlist(as.list(rep(1, length(isisDataset$tweets))))))
# names(isisLabel) <- "label"

# Finished. ISIS data read in and clean

# ------------------------------------------------- 3. Normal twitter data collection

negaDataset <- parseTweets("dataset/normal.json", verbose=TRUE)
tmptweets <- data.frame(matrix(unlist(negaDataset$text), nrow=length(negaDataset$text), byrow=T))
tmpName <- data.frame(matrix(unlist(negaDataset$screen_name), nrow=length(negaDataset$screen_name), byrow=T))
negaDataset <- cbind(tmptweets, tmpName)
names(negaDataset) <- c("tweets", "username")
rm(tmptweets, tmpName)

# negaDataset$tweets <- lapply(negaDataset$tweets, myClean)  
# negaDataset <- negaDataset[!(negaDataset$tweets==""),]
# negaDataset$tweets <- lapply(negaDataset$tweets, tolower)


# ------------------------------------------------------------------------------------------------- Step 1: feature extraction & feature selection

# Prepare data
sub <- sample(1:nrow(negaDataset), floor(1000))
sampleNega <- negaDataset[sub, ]
negaLabel <- data.frame(matrix(unlist(as.list(rep(0, length(sampleNega$tweets))))))
names(negaLabel) <- "label"
sampleNega <- cbind(sampleNega, negaLabel)
rm(negaLabel)

sub <- sample(1:nrow(isisDataset), floor(100))
samplePosi <- isisDataset[sub, ]
posiLabel <- data.frame(matrix(unlist(as.list(rep(1, length(samplePosi$tweets))))))
names(posiLabel) <- "label"
samplePosi <- cbind(samplePosi, posiLabel)
rm(posiLabel)

# Train set Clean
trainset <- rbind(samplePosi, sampleNega)
trainset <- trainset[sample(nrow(trainset)),]


trainset$tweets <- lapply(trainset$tweets, myClean)
trainset <- trainset[!(trainset$tweets==""),]
trainset$tweets <- lapply(trainset$tweets, tolower)
rm(samplePosi, sampleNega)

# ----------------------------------------------------- BOW features
# Extract features
wordList <- getWordList(as.character(trainset$tweets))
featureSet <- getFeatureVec(as.character(trainset$tweets), wordList)
label <- trainset$label
featureSet <- cbind(featureSet, label)
featureSet$label <- as.factor(featureSet$label)
write.csv(featureSet, "dataset/featureset.csv", sep = ",", col.names = TRUE, row.names = FALSE)
  

# feature selection
featureSet <- read.csv("dataset/featureset.csv", sep = "," , header = TRUE)
featureSet$label <- as.factor(featureSet$label)
weights <- InfoGainAttributeEval(label~.,featureSet)
weights <- data.frame(weights)
subset <- cutoff.k(weights, 500)
label <- featureSet$label
featureSet <- cbind(featureSet[,subset], label)
featureSet$label <- as.factor(featureSet$label)

# ------------------------------------------------------------------------------------------------- Step 2: Training & evaluation
for (i in 1:5) {
  sub <- sample(1:nrow(featureSet), floor(nrow(featureSet) * (0.5)))

# ----- First fold
  tr <- featureSet[sub, ]
  te <- featureSet[-sub, ]
  # SMOTE
  smoted <- SMOTE(label~., tr, k=5, perc.over=600, perc.under=0)
  tr <- rbind(tr, smoted)
  tr <- tr[sample(nrow(tr)),]
  rm(smoted)
  # Adaboost Train
  BC.adaboost <- boosting(label ~ ., data=tr, mfinal=30)
  BC.adaboost.pred <- predict.boosting(BC.adaboost, newdata=te, newmfinal=20)
  print(BC.adaboost.pred$confusion)
  if (i == 1) {
    confusion <- BC.adaboost.pred$confusion
  } else {
    confusion <- confusion + BC.adaboost.pred$confusion
  }

# ------ Second fold
  tr <- featureSet[-sub, ]
  te <- featureSet[sub, ]
  # SMOTE
  smoted <- SMOTE(label~., tr, k=5, perc.over=600, perc.under=0)
  tr <- rbind(tr, smoted)
  tr <- tr[sample(nrow(tr)),]
  rm(smoted)
  # Adaboost Train
  BC.adaboost <- boosting(label ~ ., data=tr, mfinal=30)
  BC.adaboost.pred <- predict.boosting(BC.adaboost, newdata=te, newmfinal=10)
  print(BC.adaboost.pred$confusion)
  confusion <- confusion + BC.adaboost.pred$confusion
}

confusion <- confusion / 10
TP <- confusion[2,2]
TN <- confusion[1,1]
FP <- confusion[2,1]
FN <- confusion[1,2]
precision <- TP / (FP + TP)
recall <- TP / (TP + FN)
fmeasure <- 2 * precision * recall / ( precision + recall) 

print(confusion)
print(paste("Precision:", precision))
print(paste("Recall:", recall))
print(paste("F Measure:", fmeasure))


# ------------------------------------------------------------------------------------------------- Attachment

# SVM train
svmModel <- svm(label~., data=tr, kernel="radial")
summary(svmModel)
pred <- predict(svmModel, te[,1:310])
confusion <- table(pred, te$label)
print(confusion)

# DT Train
dtModel <- J48(label~., data=tr, options = "-R")
e <- evaluate_Weka_classifier(dtModel, cost = matrix(c(0,2,1,0), ncol = 2), numFolds = 10, complexity = TRUE,seed = 123, class = TRUE)
print(e)
pred <- predict(dtModel, te[,1:500])
confusion <- table(pred, te$label)
print(confusion)
TP <- confusion[2,2]
TN <- confusion[1,1]
FP <- confusion[2,1]
FN <- confusion[1,2]
precision <- TP / (FP + TP)
recall <- TP / (TP + FN)
fmeasure <- 2 * precision * recall / ( precision + recall) 
print(fmeasure)
print((confusion[1,1] + confusion[2,2]) / (confusion[1,1] + confusion[1,2] + confusion[2,1] + confusion[2,2]))

# -------------------------------------------------------------------------------------------------- Backup

# SVM train
#svmModel <- svm(label~., data=tr, kernel="radial")
# dtModel <- J48(label~., data=tr, options = "-R")
# pred <- predict(dtModel, te[,1:500])
# #pred <- predict(svmModel, te[,1:500])
# result <- table(pred, te$label)
# print(result)
# if (i == 1) {
#     confusion <- result
# } else {
#   confusion <- confusion + result
# }
