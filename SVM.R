# SVM #
library(e1071)

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

num_rounds <- 10
ss <- 500
C.values <- c(10^1,10^1*5,10^2,10^2*5,10^3,10^3*5,10^4,10^4*5,10^5,10^5*5)
g.values <- c(10^-5,10^-5*5,10^-4,10^-4*5,10^-3,10^-3*5,10^-2,10^-2*5)
accuracy <- matrix(0, nrow = 10, ncol = 8)
rownames(accuracy) <- c("10^1","10^1*5","10^2","10^2*5","10^3","10^3*5","10^4","10^4*5","10^5","10^5*5")
colnames(accuracy) <- c("10^-5","10^-5*5","10^-4","10^-4*5","10^-3","10^-3*5","10^-2","10^-2*5")
sensitivity <- matrix(0, nrow = 10, ncol = 8)
rownames(sensitivity) <- c("10^1","10^1*5","10^2","10^2*5","10^3","10^3*5","10^4","10^4*5","10^5","10^5*5")
colnames(sensitivity) <- c("10^-5","10^-5*5","10^-4","10^-4*5","10^-3","10^-3*5","10^-2","10^-2*5")
specificity <- matrix(0, nrow = 10, ncol = 8)
rownames(specificity) <- c("10^1","10^1*5","10^2","10^2*5","10^3","10^3*5","10^4","10^4*5","10^5","10^5*5")
colnames(specificity) <- c("10^-5","10^-5*5","10^-4","10^-4*5","10^-3","10^-3*5","10^-2","10^-2*5")
avg_accuracy <- matrix(0, nrow = 10, ncol = 8)
rownames(avg_accuracy) <- c("10^1","10^1*5","10^2","10^2*5","10^3","10^3*5","10^4","10^4*5","10^5","10^5*5")
colnames(avg_accuracy) <- c("10^-5","10^-5*5","10^-4","10^-4*5","10^-3","10^-3*5","10^-2","10^-2*5")
for (n in 1:num_rounds) {
  print(n)
  sampleidx <- sample(700, ss)
  train.x <- all.features[sampleidx,]
  train.y <- truth[sampleidx]
  test.x <- all.features[-sampleidx,]
  test.y <- truth[-sampleidx]
  for (i in 1:10) {
    c <-  C.values[[i]]
    for (j in 1:8) {
      g <- g.values[[j]]
      svm.model <- svm(train.y ~ ., data = train.x, kernel = "radial", cost = c, gamma = g, class.weights = c("benign" = 0.2, "malignant" = 0.8))
      svm.pred <- predict(svm.model, test.x)
      cm <- table(svm.pred, test.y)
      accuracy[i,j] <- accuracy[i,j] + (cm[1,1] + cm[2,2]) / 200
      sensitivity[i,j] <- sensitivity[i,j] + cm[2,2] / (cm[1,2] + cm[2,2])
      specificity[i,j] <- specificity[i,j] + cm[1,1] / (cm[1,1] + cm[2,1])
    }
  }
}

avg_accuracy = (sensitivity + specificity) / 2
accuracy = accuracy / num_rounds
sensitivity = sensitivity / num_rounds
specificity = specificity / num_rounds
avg_accuracy = avg_accuracy / num_rounds

sensitivity
specificity
avg_accuracy

