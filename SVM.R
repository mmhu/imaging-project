# SVM #
library(e1071)

features <- scale(features, center=TRUE, scale=TRUE)
features = as.matrix(features)
sampleidx <- sample(700, 500)
train.x <- features[sampleidx,]
train.y <- truth[sampleidx]
test.x <- features[-sampleidx,]
test.y <- truth[-sampleidx]

C.values <- c(10^-3,10^-2,10^-1,10^0,10^1,10^2,10^3)
g.values <- c(10^-3,10^-2,10^-1,10^0,10^1,10^2,10^3)
accuracy <- matrix(0, nrow = 7, ncol = 7)
rownames(accuracy) <- c("10^-3","10^-2","10^-1","10^0","10^1","10^2","10^3")
colnames(accuracy) <- c("10^-3","10^-2","10^-1","10^0","10^1","10^2","10^3")
sensitivity <- matrix(0, nrow = 7, ncol = 7)
rownames(sensitivity) <- c("10^-3","10^-2","10^-1","10^0","10^1","10^2","10^3")
colnames(sensitivity) <- c("10^-3","10^-2","10^-1","10^0","10^1","10^2","10^3")
specificity <- matrix(0, nrow = 7, ncol = 7)
rownames(specificity) <- c("10^-3","10^-2","10^-1","10^0","10^1","10^2","10^3")
colnames(specificity) <- c("10^-3","10^-2","10^-1","10^0","10^1","10^2","10^3")
avg_accuracy <- matrix(0, nrow = 7, ncol = 7)
rownames(avg_accuracy) <- c("10^-3","10^-2","10^-1","10^0","10^1","10^2","10^3")
colnames(avg_accuracy) <- c("10^-3","10^-2","10^-1","10^0","10^1","10^2","10^3")
for (i in 1:7) {
  c <-  C.values[[i]]
  for (j in 1:7) {
    g <- g.values[[j]]
    # winner: benign = 0.2, malignant = 0.8
    svm.model <- svm(train.y ~ ., data = train.x, kernel = "radial", cost = c, gamma = g, class.weights = c("benign" = 0.2, "malignant" = 0.8))
    svm.pred <- predict(svm.model, test.x)
    cm <- table(svm.pred, test.y)
    accuracy[i,j] <- (cm[1,1] + cm[2,2]) / 200
    sensitivity[i,j] <- cm[2,2] / (cm[1,2] + cm[2,2])
    specificity[i,j] <- cm[1,1] / (cm[1,1] + cm[2,1])
  }
}
avg_accuracy = (sensitivity + specificity) / 2
accuracy
sensitivity
specificity
avg_accuracy
