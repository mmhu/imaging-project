library(knitr)
opts_chunk$set(cache = TRUE)
opts_knit$set(root.dir = "C:/Users/bensadis/Desktop/BDSI/imaging-project")
library(tidyverse)
library(nnet)
library(mxnet)

### get train features ###
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

num_runs <- 1
ss <- 500

s.values <- c(1,2,3,4,5,6,7,8,9,10) # s = number of hidden layers
d.values <- c(1.0e-1,1.0e-2,1.0e-3,1.0e-4,1.0e-5,1.0e-6,1.0e-7) # d = weight decay
accuracy <- matrix(0, nrow = 10, ncol = 7)
rownames(accuracy) <- c("1","2","3","4","5","6","7","8","9","10")
colnames(accuracy) <- c("1","2","3","4","5","6","7")
sensitivity <- matrix(0, nrow = 10, ncol = 7)
rownames(sensitivity) <- c("1","2","3","4","5","6","7","8","9","10")
colnames(sensitivity) <- c("1","2","3","4","5","6","7")
specificity <- matrix(0, nrow = 10, ncol = 7)
rownames(specificity) <- c("1","2","3","4","5","6","7","8","9","10")
colnames(specificity) <- c("1","2","3","4","5","6","7")
avg_accuracy <- matrix(0, nrow = 10, ncol = 7)
rownames(avg_accuracy) <- c("1","2","3","4","5","6","7","8","9","10")
colnames(avg_accuracy) <- c("1","2","3","4","5","6","7")

for (z in 1:num_runs) {
  print(z)
  sampleidx <- sample(700, ss)
  train.x <- all.features.scaled[sampleidx,]
  train.y <- truth[sampleidx]
  test.x <- all.features.scaled[-sampleidx,]
  test.y <- truth[-sampleidx]
  for (i in 1:10) {
    print(i)
    s <-  s.values[[i]]
    for (j in 1:7) {
      print(j)
      d <- d.values[[j]]
      nnet.model = nnet(train.y ~ .,data=train.x, size = s, decay = d, maxit = 1000, trace=FALSE)
      nnet.pred = predict(nnet.model, newdata=test.x, type="class")
      cm <- table(nnet.pred, test.y)
      if (dim(cm)[1]==2) {
        accuracy[i,j] <- accuracy[i,j] + (cm[1,1] + cm[2,2]) / 200
        sensitivity[i,j] <- sensitivity[i,j] + cm[2,2] / (cm[1,2] + cm[2,2])
        specificity[i,j] <- specificity[i,j] + cm[1,1] / (cm[1,1] + cm[2,1])
      }
      else {
        accuracy[i,j] <- accuracy[i,j] + 0
        sensitivity[i,j] <- sensitivity[i,j] + 0
        specificity[i,j] <- specificity[i,j] + 0
      }
      rm(nnet.model, nnet.pred, cm)
    }
  }
  rm(train.x, train.y, test.x, test.y)
}

avg_accuracy = (sensitivity + specificity) / 2
accuracy = accuracy / num_runs
sensitivity = sensitivity / num_runs
specificity = specificity / num_runs
avg_accuracy = avg_accuracy / num_runs

sensitivity
specificity
avg_accuracy














scaled <- scale(features, center=TRUE, scale=TRUE)
scaled = as.matrix(scaled)
sampleidx = sample(1:700,500)
train.x = scaled[sampleidx,]
train.y = truth[sampleidx]
test.x = scaled[-sampleidx,]
test.y = truth[-sampleidx]
nn <- nnet(truth ~ ., data = scaled, subset = sampleidx, size = 5, decay = 1.0e-5, maxit = 1000)
cm.nn <- table(test.y, predict(nn, test.x, type = "class"))
truth = as.numeric(truth) - 1
train.y = truth[sampleidx]
test.y = truth[-sampleidx]
mxn = mx.mlp(train.x, train.y, hidden_node = 10, out_node = 2, out_activation = "softmax", learning.rate = 0.1, eval.metric = mx.metric.accuracy)
cm.mxn <- table(test.y, predict(mxn, test.x))
cm.nn
cm.mxn
