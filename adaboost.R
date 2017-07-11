library(knitr)
opts_chunk$set(cache = TRUE)
opts_knit$set(root.dir = "C:/Users/bensadis/Desktop/BDSI/imaging-project")
library(devtools)
library(ggbiplot)
library(ada)
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
all.features <- cbind(firstorder, colors, circ, asym)
all.features.scaled <- scale(all.features, center=TRUE, scale=TRUE)
all.features.scaled = as.data.frame(all.features.scaled)

ada.predictions <- rep(0, 700)
ada.predictions = factor(x = ada.predictions, levels = c(0,1), labels = c("benign", "malignant"))
ada.pred.prob <- matrix(data=0,nrow=700,ncol=2)

num_rounds <- 10

for (r in 1:num_rounds) {
  print(r)
  ss <- sample(700,replace=F)
  for (i in seq(1,700,by=70)) {
    ada.cv <- ada(truth[-ss[i:(i+69)]] ~ ., data = all.features[-ss[i:(i+69)],], loss = "e", type = "discrete", iter=50, nu=0.08, rpart.control(maxdepth = 4))
    ada.pred.prob[ss[i:(i+69)],] = ada.pred.prob[ss[i:(i+69)],] + predict(ada.cv, newdata = all.features[ss[i:(i+69)],],type="prob")
  }
}
ada.pred.prob <- ada.pred.prob / num_rounds

for (i in 1:700) {
  if (ada.pred.prob[[i,2]] > 0.14) {
    ada.predictions[[i]] = "malignant"
  }
  else {
    ada.predictions[[i]] = "benign"
  }
}
tb <- table(truth, ada.predictions)
tb
sens <- tb[2,2] / (tb[2,2] + tb[2,1])
spec <- tb[1,1] / (tb[1,1] + tb[1,2])
acc <- (sens + spec) / 2
sens
spec
acc



















###### previous ######

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

ss = sample(700,replace=F)

control <- trainControl(method="cv",number=10,search="grid")
tunegrid <- expand.grid(.nu=1:10/100,.iter=c(40,50,60,70),.maxdepth=1:8)
ada_gridsearch <- train(truth~., data=all.features.scaled.truth[,c(1:20,32,33)], method="ada", metric="Accuracy", tuneGrid=tunegrid, trControl=control)

results_ada <- train(x=all.features.scaled, y=truth, method="ada", iter=60,maxdepth=6,nu=0.07)
results_ada

#iter = 60, maxdepth= 6, nu = 0.07

ada.pred = rep(0,700)
for (i in seq(1,700,by=70)) {
  ada.cv = ada(truth[-ss[i:(i+69)]] ~ ., data = all.features.scaled[-ss[i:(i+69)],c(1:13,68)], loss = "e", type = "discrete",iter=60,nu=0.07,rpart.control(maxdepth=6))
  ada.pred[ss[i:(i+69)]] = predict(ada.cv, newdata = all.features.scaled[ss[i:(i+69)],c(1:13,68)])
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