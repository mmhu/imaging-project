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
truth <- read.csv("training_set_truth.csv", header = FALSE)
truth$V3 = as.factor(truth$V3)
truth = truth$V3
all.features <- cbind(firstorder, features, colors, circ)
all.features.scaled <- scale(all.features, center=TRUE, scale=TRUE)
all.features.scaled = as.data.frame(all.features.scaled)
ss = sample(700,replace=F)

### nueral net ###
#truth.mxn = as.numeric(truth) - 1
#mxn.pred = rep(0,700)
nnet.pred = rep(0,700)
for (i in seq(1,700,by=70)) {
  #mxn.cv = mx.mlp(all.features.scaled[-ss[i:(i+69)],], truth.mxn[-ss[i:(i+69)]], hidden_node = 10, out_node = 2, out_activation = "softmax", learning.rate = 0.1, eval.metric = mx.metric.accuracy)
  #mxn.pred[ss[i:(i+69)]] = predict(mxn.cv, newdata = all.features.scaled[ss[i:(i+69)],])
  nnet.cv <- nnet(truth[-ss[i:(i+69)]] ~ ., data = all.features.scaled[-ss[i:(i+69)],], size = 5, decay = 1.0e-5, maxit = 1000)
  nnet.pred[ss[i:(i+69)]] = predict(nnet.cv, newdata = all.features.scaled[ss[i:(i+69)],], type = "class")
}
#mxn.table = table(truth.mxn, mxn.pred)
nnet.table = table(truth, nnet.pred)

### svm ###
svm.pred = rep(0,700)
for (i in seq(1,700,by=70)) {
  svm.cv <- svm(truth[-ss[i:(i+69)]] ~ ., data = all.features.scaled[-ss[i:(i+69)],], kernel = "radial", cost = 10^2, gamma = 10^-3, class.weights = c("benign" = 0.2, "malignant" = 0.8))
  svm.pred[ss[i:(i+69)]] = predict(svm.cv, newdata = all.features.scaled[ss[i:(i+69)],])
}
svm.table = table(truth, svm.pred)

### adaboost ###
ada.pred = rep(0,700)
for (i in seq(1,700,by=70)) {
  ada.cv = ada(truth[-ss[i:(i+69)]] ~ ., data = all.features[-ss[i:(i+69)],], loss = "e", type = "discrete")
  ada.pred[ss[i:(i+69)]] = predict(ada.cv, newdata = all.features[ss[i:(i+69)],])
}
ada.table = table(truth, ada.pred)

### random forest ###
rf.pred = rep(0,700)
for (i in seq(1,700,by=70)) {
  rf.cv <- randomForest(as.factor(truth[-ss[i:(i+69)]]) ~ ., data = all.features[-ss[i:(i+69)],], ntree = 10)
  rf.pred[ss[i:(i+69)]] = predict(rf.cv, newdata = all.features[ss[i:(i+69)],])
}
rf.table = table(truth, rf.pred)

### concurrency tables ###
svm.table
ada.table
rf.table