library(tidyverse)
library(caret)
library(ada)
setwd("/Users/catscomputer/Documents/Mine New/bdsi/imaging/imaging_git")
features <- read.csv("features.csv", header = TRUE)
View(features)
features <- features[, -8] # get rid of glcm_correlation with all 1 values

pca=prcomp(~., data = features, scale = TRUE)
gs_pca <- data.frame(pca$x[,1:6])

firstorder <- read.csv("firstorder.csv", header = TRUE)
colors <- read.csv("color_grayscale_fin.csv", header = TRUE)
colors <- colors[, 2:13]

circ <- read.csv("circularity.csv", header = TRUE) %>% as_vector
truth <- read.csv("training_set_truth.csv", header = FALSE)
truth$V3 = as.factor(truth$V3)
truth = truth$V3

all.features <- cbind(firstorder, gs_pca, colors, circ)
all.features.scaled <- scale(all.features, center=TRUE, scale=TRUE)
all.features.scaled = as.data.frame(all.features.scaled)
all.features.scaled.truth <- cbind(all.features.scaled,truth)

colnames(all.features.scaled.truth)

avg_accuracy <- function(data,lev,model){
  avg <- (sum(data[,"obs"]==lev[1]&data[,"pred"]==lev[1])/sum(data[,"obs"]==lev[1])+sum(data[,"obs"]==lev[2]&data[,"pred"]==lev[2])/sum(data[,"obs"]==lev[2]))/2
  c(mean_acc = avg)
  }

control <- trainControl(method="cv",number=10,summaryFunction = avg_accuracy)
tunegrid <- expand.grid(.nu=1:10/100,.iter=c(40,50,60,70),.maxdepth=1:8)
ada_gridsearch <- train(truth~., data=all.features.scaled.truth[,c(1:13,32,33)], method="ada", tuneGrid=tunegrid, trControl=control,metric="mean_acc")

summary(ada_gridsearch)

#iter = 60, maxdepth= 6, nu = 0.07

#just firstorder+circularity: iter = 50, maxdepth=4, nu=0.08
ss = sample(700,replace=F)
ada.pred = rep(0,700)
for (i in seq(1,700,by=70)) {
  ada.cv = ada(truth[-ss[i:(i+69)]] ~ ., data = all.features.scaled[-ss[i:(i+69)],c(1:13,32)], loss = "e", type = "discrete",rpart.control(maxdepth = 4),nu=0.08,iter=50)
  ada.pred[ss[i:(i+69)]] = predict(ada.cv, newdata = all.features.scaled[ss[i:(i+69)],c(1:13,32)])
}
ada.table = table(truth, ada.pred)

spec <- sum(truth == "benign" & ada.pred == 1)/sum(truth=="benign")
sens <- sum(truth == "malignant" & ada.pred == 2)/sum(truth=="malignant")
avg_acc <- (spec+sens)/2
#mu - shrinkage parameter 0<mu<1, mu < 0.1 dramatic improvement in generalization
#iter between 40 and 70
#maxdepth 5-8


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