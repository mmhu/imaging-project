setwd("/Users/bensadis/Desktop/BDSI")
library(class)
library(FNN)

features <- read.csv("features.csv", header=TRUE)
scaled <- scale(features, center=TRUE, scale=TRUE)
scaled = as.matrix(scaled)

labels = read.csv("training_set_truth.csv", header=FALSE)
labels$V3 = as.factor(labels$V3)
labels = labels$V3

nTrain <- 500
sampleidx <- sample(1:700,nTrain)
train.x <- scaled[sampleidx,]
train.y <- labels[sampleidx]
test.x <- scaled[-sampleidx,]
test.y <- labels[-sampleidx]

k = 10
neighbors <- get.knnx(data = train.x, query = test.x, k = k)
indices <- neighbors$nn.index
distances <- neighbors$nn.dist

models <- c("nnet","svm","adaboost","rforest")
num.models <- length(models)
test.y <- rep(0, 200)
test.y = factor(x = test.y, levels = c(0,1), labels = c("benign", "malignant"))

for (i in 1:length(test.x)) {
  point <- test.x[[i]]
  model.acc <- matrix(0, nrow = k, ncol = num.models)
  for (j in 1:k) {
    idx <- indices[[i,j]]
    dist <- distances[[i,j]]
    for (m in 1:num.models) {
      model <- models[[m]]
      pred <- model(train.x[[idx]])
      bool.val <- pred == train.y[[idx]]
      model.acc[[j,m]] <- bool.val
    }
  }
  counts <- rep(0, num.models)
  for (m in 1:num.models) {
    counts[[m]] = sum(model.acc[[,m]] == TRUE)
  }
  model.idx <- which.max(counts)
  test.y[[i]] = models[[model.idx]](point)
}










