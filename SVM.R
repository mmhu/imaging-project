# SVM #
library(knitr)
opts_chunk$set(cache = TRUE)
opts_knit$set(root.dir = "C:/Users/bensadis/Desktop/BDSI/imaging-project")
library(e1071)
library(tidyverse)

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
all.features <- cbind(firstorder, features, colors, circ)
all.features.scaled <- scale(all.features, center=TRUE, scale=TRUE)
all.features.scaled = as.data.frame(all.features.scaled)

num_rounds <- 500
ss <- 500
#r.values <- c(10^-6,10^-5,10^-4,10^-3,10^-2,10^-1,10^0,10^1,10^2)
C.values <- c(10^1,10^1*5,10^2,10^2*5,10^3,10^3*5,10^4,10^4*5,10^5,10^5*5)
g.values <- c(10^-5,10^-5*5,10^-4,10^-4*5,10^-3,10^-3*5,10^-2,10^-2*5,10^-1,10^-1*5)
#accuracy <- array(rep(0,9*10*10), dim=c(9,10,10))
accuracy <- matrix(0, nrow = 10, ncol = 10)
rownames(accuracy) <- c("10^1","10^1*5","10^2","10^2*5","10^3","10^3*5","10^4","10^4*5","10^5","10^5*5")
colnames(accuracy) <- c("10^-5","10^-5*5","10^-4","10^-4*5","10^-3","10^-3*5","10^-2","10^-2*5","10^-1","10^-1*5")
#sensitivity <- array(rep(0,9*10*10), dim=c(9,10,10))
sensitivity <- matrix(0, nrow = 10, ncol = 10)
rownames(sensitivity) <- c("10^1","10^1*5","10^2","10^2*5","10^3","10^3*5","10^4","10^4*5","10^5","10^5*5")
colnames(sensitivity) <- c("10^-5","10^-5*5","10^-4","10^-4*5","10^-3","10^-3*5","10^-2","10^-2*5","10^-1","10^-1*5")
#specificity <- array(rep(0,9*10*10), dim=c(9,10,10))
specificity <- matrix(0, nrow = 10, ncol = 10)
rownames(specificity) <- c("10^1","10^1*5","10^2","10^2*5","10^3","10^3*5","10^4","10^4*5","10^5","10^5*5")
colnames(specificity) <- c("10^-5","10^-5*5","10^-4","10^-4*5","10^-3","10^-3*5","10^-2","10^-2*5","10^-1","10^-1*5")
#avg_accuracy <- array(rep(0,9*10*10), dim=c(9,10,10))
avg_accuracy <- matrix(0, nrow = 10, ncol = 10)
rownames(avg_accuracy) <- c("10^1","10^1*5","10^2","10^2*5","10^3","10^3*5","10^4","10^4*5","10^5","10^5*5")
colnames(avg_accuracy) <- c("10^-5","10^-5*5","10^-4","10^-4*5","10^-3","10^-3*5","10^-2","10^-2*5","10^-1","10^-1*5")
for (n in 1:num_rounds) {
  print(n)
  sampleidx <- sample(700, ss)
  train.x <- all.features[sampleidx,]
  train.y <- truth[sampleidx]
  test.x <- all.features[-sampleidx,]
  test.y <- truth[-sampleidx]
  for (i in 1:10) {
    c <- C.values[[i]]
    for (j in 1:10) {
      g <- g.values[[j]]
      svm.model <- svm(train.y ~ ., data = train.x, kernel = "radial", cost = c, gamma = g, class.weights = c("benign" = 0.2, "malignant" = 0.8))
      svm.pred <- predict(svm.model, test.x)
      cm <- table(svm.pred, test.y)
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
    }
  }
}




avg_accuracy = (sensitivity + specificity) / 2
accuracy = accuracy / 300
sensitivity = sensitivity / 300
specificity = specificity / 300
avg_accuracy = avg_accuracy / 300

sensitivity
specificity
avg_accuracy


### best results - 100 rounds - radial kernel ###
# 1. 10^2*5 x 10^-4*5, .6243828, .6571245, .6407536
# 2. 10^3*5 x 10^-5*5, .6412094, .6391456, .6401775
# 3. 10^3   x 10^-5*5, .6641631, .6160684, .6401157
# 4. 10^3*5 x 10^-5
# 5. 10^4   x 10^-5
# 6. 10^3   x 10^-4
# 7. 10^2   x 10^-3
# 8.
# 9.
# 10.

### best result - 100 rounds - sigmoid kernel ###
# 1. 10^-1 x 10^2   x 10^-4*5, .6411147
# 2. 10^-6 x 10^2   x 10^-4*5, .6400624
# 3. 10^-3 x 10^2   x 10^-4*5, .6399361
# 4. 10^-4 x 10^2   x 10^-4*5, .6399309
# 5. 10^-5 x 10^2   x 10^-4*5, .6398037
# 6. 10^-2 x 10^2   x 10^-4*5, .6396728
# 7. 10^0  x 10^3*5 x 10^-5*5, .6379248
# 8. 10^0  x 10^4*5 x 10^-5,   .6377676
# 9. 10^-6 x 10^3   x 10^-5*5, .6376574
#10. 10^-5 x 10^3   x 10^-5*5, .6375716
#10. 10^-3 x 10^3   x 10^-5*5, .6375716

