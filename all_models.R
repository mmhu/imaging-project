# (1)

library(knitr)
opts_chunk$set(cache = TRUE)
opts_knit$set(root.dir = "C:/Users/bensadis/Desktop/BDSI/imaging-project")
library(devtools)
library(ggbiplot)
library(ada)
library(randomForest)
library(e1071)
library(tidyverse)
library(mgcv)
library(gbm)
library(nnet)


nTrain <- 500
sampleidx <- sample(1:700,nTrain)

print("all features")
### get features ###
features <- read.csv("features.csv", header = TRUE)
features <- features[, -8] # get rid of glcm_correlation with all 1 values
firstorder <- read.csv("firstorder.csv", header = TRUE)
colors <- read.csv("colors.csv", header = TRUE)
colors <- colors[, 2:13]
circ <- read.csv("circularity.csv", header = TRUE) %>% as_vector
asym <- read.csv("asym.csv", header = TRUE)
asym <- asym[,2] %>% as_vector
truth <- read.csv("training_set_truth.csv", header = FALSE)
truth$V3 = as.factor(truth$V3)
truth = truth$V3
all.features <- cbind(firstorder, features, colors, circ, asym)
all.features.scaled <- scale(all.features, center=TRUE, scale=TRUE)
all.features.scaled = as.data.frame(all.features.scaled)

train.x <- all.features[sampleidx,]
train.xs <- all.features.scaled[sampleidx,]
train.y <- truth[sampleidx]
test.x <- all.features[-sampleidx,]
test.xs <- all.features.scaled[-sampleidx,]
test.y <- truth[-sampleidx]


svm.modelr1 <- svm(train.y ~ ., data = train.xs, kernel = "radial", cost = 10^2, gamma = 10^-3, class.weights = c("benign" = 0.2, "malignant" = 0.8))
svm.predr1 <- predict(svm.modelr1, newdata = test.xs, type = "class")
r1.tb <- table(test.y, svm.predr1)
#print(r1.tb)
sens <- r1.tb[2,2] / (r1.tb[2,2] + r1.tb[2,1])
spec <- r1.tb[1,1] / (r1.tb[1,1] + r1.tb[1,2])
acc <- (sens + spec) / 2
#print(sens)
#print(spec)
print(acc)


for (round in 1:50) {
  ### get features ###
  features <- read.csv("features.csv", header = TRUE)
  features <- features[, -8] # get rid of glcm_correlation with all 1 values
  feat.idx <- sample(42, 20)
  print(feat.idx)
  features <- features[, feat.idx]
  firstorder <- read.csv("firstorder.csv", header = TRUE)
  colors <- read.csv("colors.csv", header = TRUE)
  colors <- colors[, 2:13]
  circ <- read.csv("circularity.csv", header = TRUE) %>% as_vector
  asym <- read.csv("asym.csv", header = TRUE)
  asym <- asym[,2] %>% as_vector
  truth <- read.csv("training_set_truth.csv", header = FALSE)
  truth$V3 = as.factor(truth$V3)
  truth = truth$V3
  all.features <- cbind(firstorder, features, colors, circ, asym)
  all.features.scaled <- scale(all.features, center=TRUE, scale=TRUE)
  all.features.scaled = as.data.frame(all.features.scaled)
  
  train.x <- all.features[sampleidx,]
  train.xs <- all.features.scaled[sampleidx,]
  train.y <- truth[sampleidx]
  test.x <- all.features[-sampleidx,]
  test.xs <- all.features.scaled[-sampleidx,]
  test.y <- truth[-sampleidx]
  
  
  svm.modelr1 <- svm(train.y ~ ., data = train.xs, kernel = "radial", cost = 10^2, gamma = 10^-3, class.weights = c("benign" = 0.2, "malignant" = 0.8))
  svm.predr1 <- predict(svm.modelr1, newdata = test.xs, type = "class")
  r1.tb <- table(test.y, svm.predr1)
  #print(r1.tb)
  sens <- r1.tb[2,2] / (r1.tb[2,2] + r1.tb[2,1])
  spec <- r1.tb[1,1] / (r1.tb[1,1] + r1.tb[1,2])
  acc <- (sens + spec) / 2
  #print(sens)
  #print(spec)
  print(acc)
}





svm.modelr1 <- svm(train.y ~ ., data = train.xs, kernel = "radial", cost = 10^2, gamma = 10^-3, class.weights = c("benign" = 0.2, "malignant" = 0.8))
svm.modelr2 <- svm(train.y ~ ., data = train.xs, kernel = "radial", cost = 10^3*5, gamma = 10^-5*5, class.weights = c("benign" = 0.2, "malignant" = 0.8))
svm.modelr3 <- svm(train.y ~ ., data = train.xs, kernel = "radial", cost = 10^2*5, gamma = 10^-4*5, class.weights = c("benign" = 0.2, "malignant" = 0.8))
svm.modelr4 <- svm(train.y ~ ., data = train.xs, kernel = "radial", cost = 10^3*5, gamma = 10^-5, class.weights = c("benign" = 0.2, "malignant" = 0.8))
svm.modelr5 <- svm(train.y ~ ., data = train.xs, kernel = "radial", cost = 10^4, gamma = 10^-5, class.weights = c("benign" = 0.2, "malignant" = 0.8))
svm.models1 <- svm(train.y ~ ., data = train.xs, kernel = "sigmoid", coef0 = 10^-1, cost = 10^2, gamma = 10^-4*5, class.weights = c("benign" = 0.2, "malignant" = 0.8))
svm.models2 <- svm(train.y ~ ., data = train.xs, kernel = "sigmoid", coef0 = 10^-6, cost = 10^2, gamma = 10^-4*5, class.weights = c("benign" = 0.2, "malignant" = 0.8))
svm.models3 <- svm(train.y ~ ., data = train.xs, kernel = "sigmoid", coef0 = 10^0, cost = 10^4*5, gamma = 10^-5, class.weights = c("benign" = 0.2, "malignant" = 0.8))
svm.models4 <- svm(train.y ~ ., data = train.xs, kernel = "sigmoid", coef0 = 10^-3, cost = 10^2, gamma = 10^-4*5, class.weights = c("benign" = 0.2, "malignant" = 0.8))
svm.models5 <- svm(train.y ~ ., data = train.xs, kernel = "sigmoid", coef0 = 10^-6, cost = 10^3, gamma = 10^-5*5, class.weights = c("benign" = 0.2, "malignant" = 0.8))

svm.predr1 <- predict(svm.modelr1, newdata = test.xs, type = "class")
svm.predr2 <- predict(svm.modelr2, newdata = test.xs, type = "class")
svm.predr3 <- predict(svm.modelr3, newdata = test.xs, type = "class")
svm.predr4 <- predict(svm.modelr4, newdata = test.xs, type = "class")
svm.predr5 <- predict(svm.modelr5, newdata = test.xs, type = "class")
svm.preds1 <- predict(svm.models1, newdata = test.xs, type = "class")
svm.preds2 <- predict(svm.models2, newdata = test.xs, type = "class")
svm.preds3 <- predict(svm.models3, newdata = test.xs, type = "class")
svm.preds4 <- predict(svm.models4, newdata = test.xs, type = "class")
svm.preds5 <- predict(svm.models5, newdata = test.xs, type = "class")

#svm.pred <- predict(svm.model, newdata = test.xs, type = "class")
#nnet.pred <- predict(nnet.model, newdata = test.xs, type = "class")
#ada.pred <- predict(ada.model, newdata = test.x)
#rf.pred <- predict(rf.model, newdata = test.x)

r1.tb <- table(test.y, svm.predr1)
r2.tb <- table(test.y, svm.predr2)
r3.tb <- table(test.y, svm.predr3)
r4.tb <- table(test.y, svm.predr4)
r5.tb <- table(test.y, svm.predr5)
s1.tb <- table(test.y, svm.preds1)
s2.tb <- table(test.y, svm.preds2)
s3.tb <- table(test.y, svm.preds3)
s4.tb <- table(test.y, svm.preds4)
s5.tb <- table(test.y, svm.preds5)

r1.tb
r2.tb
r3.tb
r4.tb
r5.tb
s1.tb
s2.tb
s3.tb
s4.tb
s5.tb


##################VVVVVVVVVVVVVVVVVVV CROSS VALIDATION VVVVVVVVVVVVVVVVVVV###################
num_rounds <- 10

svm.sensitivity <- rep(0,num_rounds)
nnet.sensitivity <- rep(0,num_rounds)
ada.sensitivity <- rep(0,num_rounds)
rf.sensitivity <- rep(0,num_rounds)

svm.specificity <- rep(0,num_rounds)
nnet.specificity <- rep(0,num_rounds)
ada.specificity <- rep(0,num_rounds)
rf.specificity <- rep(0,num_rounds)

svm.avg_accuracy <- rep(0,num_rounds)
nnet.avg_accuracy <- rep(0,num_rounds)
ada.avg_accuracy <- rep(0,num_rounds)
rf.avg_accuracy <- rep(0,num_rounds)


for (n in 1:num_rounds) {
  print(paste0("n: ", n))
  
  ss <- sample(700,replace=F)
  
  ### svm ###
  print("svm")
  svm.pred <- rep(0,700)
  for (i in seq(1,700,by=70)) {
    svm.cv <- svm(truth[-ss[i:(i+69)]] ~ ., data = all.features.scaled[-ss[i:(i+69)],], kernel = "sigmoid", cost = 10^2, coef0 = 10^-1, gamma = 10^-4*5, class.weights = c("benign" = 0.2, "malignant" = 0.8))
    svm.pred[ss[i:(i+69)]] = predict(svm.cv, newdata = all.features.scaled[ss[i:(i+69)],])
  }
  svm.table = table(truth, svm.pred)

  ### concurrency tables ###
  svm.table

  svm.sensitivity[[n]] = svm.table[2,2] / 135

  svm.specificity[[n]] = svm.table[1,1] / 565

  svm.avg_accuracy[[n]] = (svm.sensitivity[[n]] + svm.specificity[[n]]) / 2
}
svm.sensitivity.mean <- mean(svm.sensitivity)
svm.specificity.mean <- mean(svm.specificity)
svm.avg_accuracy.mean <- mean(svm.avg_accuracy)
svm.sensitivity.mean
svm.specificity.mean
svm.avg_accuracy.mean










for (n in 1:num_rounds) {
  print(paste0("n: ", n))
  
  ss <- sample(700,replace=F)
  
  ### svm ###
  #print("svm")
  #svm.pred <- rep(0,700)
  #for (i in seq(1,700,by=70)) {
  #  svm.cv <- svm(truth[-ss[i:(i+69)]] ~ ., data = all.features.scaled[-ss[i:(i+69)],], kernel = "radial", cost = 10^2*5, gamma = 10^-5*5, class.weights = c("benign" = 0.2, "malignant" = 0.8))
  #  svm.pred[ss[i:(i+69)]] = predict(svm.cv, newdata = all.features.scaled[ss[i:(i+69)],])
  #}
  #svm.table = table(truth, svm.pred)
  
  ### nueral net ###
  #print("nnet")
  #truth.mxn = as.numeric(truth) - 1
  #mxn.pred <- rep(0,700)
  #nnet.pred <- rep(0,700)
  #for (i in seq(1,700,by=70)) {
    #mxn.cv = mx.mlp(all.features.scaled[-ss[i:(i+69)],], truth.mxn[-ss[i:(i+69)]], hidden_node = 10, out_node = 2, out_activation = "softmax", learning.rate = 0.1, eval.metric = mx.metric.accuracy)
    #mxn.pred[ss[i:(i+69)]] = predict(mxn.cv, newdata = all.features.scaled[ss[i:(i+69)],])
  #  nnet.cv <- nnet(truth[-ss[i:(i+69)]] ~ ., data = all.features.scaled[-ss[i:(i+69)],], size = 5, decay = 1.0e-5, maxit = 1000, trace=FALSE)
  #  nnet.pred[ss[i:(i+69)]] = predict(nnet.cv, newdata = all.features.scaled[ss[i:(i+69)],], type = "class")
  #}
  #mxn.table = table(truth.mxn, mxn.pred)
  #nnet.table = table(truth, nnet.pred)
  
  ### adaboost ###
  print("ada")
  ada.pred <- rep(0,700)
  for (i in seq(1,700,by=70)) {
    ada.cv <- ada(truth[-ss[i:(i+69)]] ~ ., data = all.features.scaled[-ss[i:(i+69)],], loss = "e", type = "discrete", iter=50, nu=0.08, rpart.control(maxdepth = 4))
    ada.pred[ss[i:(i+69)]] = predict(ada.cv, newdata = all.features.scaled[ss[i:(i+69)],])
  }
  ada.table = table(truth, ada.pred)
  
  ### random forest ###
  print("rf")
  rf.pred <- rep(0,700)
  for (i in seq(1,700,by=70)) {
    rf.cv <- randomForest(as.factor(truth[-ss[i:(i+69)]]) ~ ., data = all.features.scaled[-ss[i:(i+69)],], ntree = 4, mtry = 54, sampsize = 630, maxnodes = 20, classwt=(c("benign" = 0.2, "malignant" = 0.8)))
    rf.pred[ss[i:(i+69)]] = predict(rf.cv, newdata = all.features.scaled[ss[i:(i+69)],])
  }
  rf.table = table(truth, rf.pred)
  
  ### concurrency tables ###
  svm.table
  nnet.table
  ada.table
  rf.table
  
  svm.sensitivity[[n]] = svm.table[2,2] / 135
  nnet.sensitivity[[n]] = nnet.table[2,2] / 135
  ada.sensitivity[[n]] = ada.table[2,2] / 135
  rf.sensitivity[[n]] = rf.table[2,2] / 135
  
  svm.specificity[[n]] = svm.table[1,1] / 565
  nnet.specificity[[n]] = nnet.table[1,1] / 565
  ada.specificity[[n]] = ada.table[1,1] / 565
  rf.specificity[[n]] = rf.table[1,1] / 565
  
  svm.avg_accuracy[[n]] = (svm.sensitivity[[n]] + svm.specificity[[n]]) / 2
  nnet.avg_accuracy[[n]] = (nnet.sensitivity[[n]] + nnet.specificity[[n]]) / 2
  ada.avg_accuracy[[n]] = (ada.sensitivity[[n]] + ada.specificity[[n]]) / 2
  rf.avg_accuracy[[n]] = (rf.sensitivity[[n]] + rf.specificity[[n]]) / 2
}

svm.sensitivity.mean <- mean(svm.sensitivity)
nnet.sensitivity.mean <- mean(nnet.sensitivity)
ada.sensitivity.mean <- mean(ada.sensitivity)
rf.sensitivity.mean <- mean(rf.sensitivity)

svm.specificity.mean <- mean(svm.specificity)
nnet.specificity.mean <- mean(nnet.specificity)
ada.specificity.mean <- mean(ada.specificity)
rf.specificity.mean <- mean(rf.specificity)

svm.avg_accuracy.mean <- mean(svm.avg_accuracy)
nnet.avg_accuracy.mean <- mean(nnet.avg_accuracy)
ada.avg_accuracy.mean <- mean(ada.avg_accuracy)
rf.avg_accuracy.mean <- mean(rf.avg_accuracy)

svm.sensitivity.mean
nnet.sensitivity.mean
ada.sensitivity.mean
rf.sensitivity.mean

svm.specificity.mean
nnet.specificity.mean
ada.specificity.mean
rf.specificity.mean

svm.avg_accuracy.mean
nnet.avg_accuracy.mean
ada.avg_accuracy.mean
rf.avg_accuracy.mean

