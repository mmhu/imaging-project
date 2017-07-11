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
library(ROCR)


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
all.features <- cbind(firstorder, colors, circ)
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
    svm.cv <- svm(truth[-ss[i:(i+69)]] ~ ., data = all.features.scaled[-ss[i:(i+69)],], kernel = "radial", cost = 10^2*5, gamma = 10^-5*5, class.weights = c("benign" = 0.2, "malignant" = 0.8))
    svm.pred[ss[i:(i+69)]] = predict(svm.cv, newdata = all.features.scaled[ss[i:(i+69)],])
  }
  svm.table = table(truth, svm.pred)
  
  ### nueral net ###
  print("nnet")
  nnet.pred <- rep(0,700)
  for (i in seq(1,700,by=70)) {
    nnet.cv <- nnet(truth[-ss[i:(i+69)]] ~ ., data = all.features.scaled[-ss[i:(i+69)],], size = 5, decay = 1.0e-5, maxit = 1000, trace=FALSE)
    nnet.pred[ss[i:(i+69)]] = predict(nnet.cv, newdata = all.features.scaled[ss[i:(i+69)],], type = "class")
  }
  nnet.table = table(truth, nnet.pred)
  
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
    rf.cv <- randomForest(as.factor(truth[-ss[i:(i+69)]]) ~ ., data = all.features.scaled[-ss[i:(i+69)],], ntree = 4, mtry = 18, sampsize = 630, maxnodes = 20, classwt=(c("benign" = 0.2, "malignant" = 0.8)))
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

##ROC curve##
for (n in 1:num_rounds) {
  print(paste0("n: ", n))
  
  ss <- sample(700,replace=F)
  
  ### svm ###
  print("svm")
  svm.pred <- rep(0,700)
  
  for (i in seq(1,700,by=70)) {
    svm.cv <- svm(truth[-ss[i:(i+69)]] ~ ., data = all.features.scaled[-ss[i:(i+69)],], probability=TRUE,kernel = "sigmoid", cost = 10^2, coef0 = 10^-1, gamma = 10^-4*5, class.weights = c("benign" = 0.2, "malignant" = 0.8))
    svm.pred[ss[i:(i+69)]] = predict(svm.cv, newdata = all.features.scaled[ss[i:(i+69)],])
  }
  svm.table = table(truth, svm.pred)
  
  ### concurrency tables ###
  svm.table
  
  svm.sensitivity[[n]] = svm.table[2,2] / 135
  
  svm.specificity[[n]] = svm.table[1,1] / 565
  
  svm.avg_accuracy[[n]] = (svm.sensitivity[[n]] + svm.specificity[[n]]) / 2
}



##ada ROC curve##
<<<<<<< HEAD
ada.pred <- matrix(data=NA,nrow=700,ncol=2)
ada.features <- cbind(firstorder, colors, circ)

for (i in seq(1,700,by=70)) {
  ada.cv <- ada(truth[-ss[i:(i+69)]] ~ ., data = ada.features[-ss[i:(i+69)],], loss = "e", type = "discrete", iter=50, nu=0.08, rpart.control(maxdepth = 4))
  ada.pred[ss[i:(i+69)],] = predict(ada.cv, newdata = ada.features[ss[i:(i+69)],],type="prob")
=======
num_rounds <- 100

ada.pred <- matrix(data=0,nrow=700,ncol=2)

for (r in 1:num_rounds) {
  print(r)
  ss <- sample(700,replace=F)
  for (i in seq(1,700,by=70)) {
    ada.cv <- ada(truth[-ss[i:(i+69)]] ~ ., data = all.features[-ss[i:(i+69)],], loss = "e", type = "discrete", iter=50, nu=0.08, rpart.control(maxdepth = 4))
    ada.pred[ss[i:(i+69)],] = ada.pred[ss[i:(i+69)],] + predict(ada.cv, newdata = all.features[ss[i:(i+69)],],type="prob")
  }
>>>>>>> 550f2c1fd3fb4caafc06566baa6fdcf0206bee98
}
ada.pred <- ada.pred / num_rounds

ada.predictions <- prediction(ada.pred[,2],labels=truth)

ada.perf <- performance(ada.predictions,"tpr","fpr")

plot(ada.perf,main="ROC Curve for AdaBoost",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

<<<<<<< HEAD
ada_avg_acc <- (ada.predictions@tp[[1]]/135+ada.predictions@tn[[1]]/565)/2
which.max(ada_avg_acc)
ada.predictions@cutoffs[[1]][362]
=======
avg_accuracies <- (ada.predictions@tp[[1]]/135 + ada.predictions@tn[[1]]/565) / 2
threshold <- ada.predictions@cutoffs[[1]][which.max(avg_accuracies)]

>>>>>>> 550f2c1fd3fb4caafc06566baa6fdcf0206bee98
###RandomForest Curve###
ss <- sample(700,replace=F)

rf.pred <- matrix(data=NA,nrow=700,ncol=2)
for (i in seq(1,700,by=70)) {
  rf.cv <- randomForest(as.factor(truth[-ss[i:(i+69)]]) ~ ., data = all.features[-ss[i:(i+69)],], ntree = 4, mtry = 18, sampsize = 630, maxnodes = 20, classwt=(c("benign" = 0.2, "malignant" = 0.8)))
  rf.pred[ss[i:(i+69)],] = predict(rf.cv, newdata = all.features[ss[i:(i+69)],],type="prob")
}

rf.predictions <- prediction(rf.pred[,2],labels=truth)

rf.perf <- performance(rf.predictions,"tpr","fpr")

plot(rf.perf,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

## svm ROC ##

svm.pred <- matrix(data=NA,nrow=700,ncol=2)
for (i in seq(1,700,by=70)) {
  svm.cv <- svm(truth[-ss[i:(i+69)]] ~ ., data = all.features.scaled[-ss[i:(i+69)],], probability=TRUE, kernel = "radial", cost = 10^2*5, gamma = 10^-5*5, class.weights = c("benign" = 0.2, "malignant" = 0.8))
  svm.pred.class <- predict(svm.cv, newdata = all.features.scaled[ss[i:(i+69)],], probability=TRUE)
  svm.pred[ss[i:(i+69)],] <- attr(svm.pred.class,"probabilities")
}
svm.table = table(truth, svm.pred)

svm.predictions <- prediction(svm.pred[,2],labels=truth)

svm.perf <- performance(svm.predictions,"tpr","fpr")

plot(svm.perf,main="ROC Curve for SVM",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

## neural network ROC ##
ss <- sample(700,replace=F)
nnet.pred <- rep(0,700)
for (i in seq(1,700,by=70)) {
  nnet.cv <- nnet(truth[-ss[i:(i+69)]] ~ ., data = all.features.scaled[-ss[i:(i+69)],], size = 5, decay = 1.0e-5, maxit = 1000, trace=FALSE)
  nnet.pred[ss[i:(i+69)]] = predict(nnet.cv, newdata = all.features.scaled[ss[i:(i+69)],], type = "raw")
}

nn.predictions = prediction(nnet.pred,truth)

nn.perf = performance(nn.predictions,"tpr","fpr")

plot(nn.perf,main="ROC Curve for NN",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")
