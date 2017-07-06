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

# VVV kNN_best_model.R / voting.R VVV
# -----------------------------------

# (3)

svm.model <- svm(train.y ~ ., data = train.xs, kernel = "radial", cost = 10^2, gamma = 10^-3, class.weights = c("benign" = 0.2, "malignant" = 0.8))
nnet.model <- nnet(train.y ~ ., data = train.xs, size = 5, decay = 1.0e-5, maxit = 1000)
ada.model <-  ada(train.y ~ ., data = train.x, loss = "e", type = "discrete")
rf.model <- randomForest(as.factor(train.y) ~ ., data = train.x, ntree = 4, mtry = 54, sampsize = 500, maxnodes = 20, classwt=(c("benign" = 0.2, "malignant" = 0.8)))

svm.pred <- predict(svm.model, newdata = test.xs, type = "class")
nnet.pred <- predict(nnet.model, newdata = test.xs, type = "class")
ada.pred <- predict(ada.model, newdata = test.x)
rf.pred <- predict(rf.model, newdata = test.x)


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
    svm.cv <- svm(truth[-ss[i:(i+69)]] ~ ., data = all.features.scaled[-ss[i:(i+69)],], kernel = "radial", cost = 10^2, gamma = 10^-3, class.weights = c("benign" = 0.2, "malignant" = 0.8))
    svm.pred[ss[i:(i+69)]] = predict(svm.cv, newdata = all.features.scaled[ss[i:(i+69)],])
  }
  svm.table = table(truth, svm.pred)
  
  ### nueral net ###
  print("nnet")
  #truth.mxn = as.numeric(truth) - 1
  #mxn.pred <- rep(0,700)
  nnet.pred <- rep(0,700)
  for (i in seq(1,700,by=70)) {
    #mxn.cv = mx.mlp(all.features.scaled[-ss[i:(i+69)],], truth.mxn[-ss[i:(i+69)]], hidden_node = 10, out_node = 2, out_activation = "softmax", learning.rate = 0.1, eval.metric = mx.metric.accuracy)
    #mxn.pred[ss[i:(i+69)]] = predict(mxn.cv, newdata = all.features.scaled[ss[i:(i+69)],])
    nnet.cv <- nnet(truth[-ss[i:(i+69)]] ~ ., data = all.features.scaled[-ss[i:(i+69)],], size = 5, decay = 1.0e-5, maxit = 1000, trace=FALSE)
    nnet.pred[ss[i:(i+69)]] = predict(nnet.cv, newdata = all.features.scaled[ss[i:(i+69)],], type = "class")
  }
  #mxn.table = table(truth.mxn, mxn.pred)
  nnet.table = table(truth, nnet.pred)
  
  ### adaboost ###
  print("ada")
  ada.pred <- rep(0,700)
  for (i in seq(1,700,by=70)) {
    ada.cv <- ada(truth[-ss[i:(i+69)]] ~ ., data = all.features[-ss[i:(i+69)],], loss = "e", type = "discrete")
    ada.pred[ss[i:(i+69)]] = predict(ada.cv, newdata = all.features[ss[i:(i+69)],])
  }
  ada.table = table(truth, ada.pred)
  
  ### random forest ###
  print("rf")
  rf.pred <- rep(0,700)
  for (i in seq(1,700,by=70)) {
    rf.cv <- randomForest(as.factor(truth[-ss[i:(i+69)]]) ~ ., data = all.features[-ss[i:(i+69)],], ntree = 4, mtry = 54, sampsize = 630, maxnodes = 20, classwt=(c("benign" = 0.2, "malignant" = 0.8)))
    rf.pred[ss[i:(i+69)]] = predict(rf.cv, newdata = all.features[ss[i:(i+69)],])
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

