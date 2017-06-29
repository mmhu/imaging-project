library(dplyr)
library(jpeg)
library(png)
library(neuralnet)
setwd("~/Documents/Mine New/bdsi/imaging/training_set")

colorgs= read.csv("~/Documents/Mine New/bdsi/imaging/training_set/color_grayscale_fin.csv", head=T)
color = colorgs[,10:13]
gray = colorgs[,13:56]

truth = read.csv("~/Documents/Mine New/bdsi/imaging/training_set/training_set_truth.csv", head=F)
truth <- truth[,3]
truth <- recode(truth, benign = "0")
truth <- recode(truth, malignant="1")
truth <- as.numeric(levels(truth))[truth]

##PCA of graysclae
pca=prcomp(~., data = gray, scale = TRUE)
gs_pca <- data.frame(pca$x[,1:6])
features <- bind_cols(color, gs_pca)

##asymmetry
asymmetry <-  read.csv("~/Documents/Mine New/bdsi/imaging/training_set/asym.csv", head=T)
asymmetry <- asymmetry[,2]
features <- bind_cols(features,data.frame(asymmetry))
colnames(features)[11] <- "asym"

##logistic model
model <- glm(truth~.,data=features, family=binomial)
summary(model)


## 10-fold cross-validation
fitted = rep(0,700)
for (j in 1:100) {
  ss = sample(700,replace=F)
  for (i in seq(1,700,by=70)) {
    model.tmp = glm(truth[-ss[i:(i+69)]] ~ .,data=features[-ss[i:(i+69)],],family=binomial())
    fitted[ss[i:(i+69)]] = predict(model.tmp,newdata=features[ss[i:(i+69)],],type="response")
  }
}

##table
xtabs(~truth + ifelse(fitted<0.2,0,1))

##specificty - sensitivity
spec.sens = matrix(0,101,2)
for (i in 0:100) {
  spec.sens[i+1,1] = sum(truth == 0 & fitted< i/100)/sum(truth==0)
  spec.sens[i+1,2] = sum(truth == 1 & fitted > i/100)/sum(truth==1)	
}

plot(1-spec.sens[,1],spec.sens[,2],ylab="Sensitivity",xlab="1-Specificity",col=4,type="s");abline(0,1,col=1)

##weighted accuracy
accuracy = spec.sens[,1]*(565/700)+spec.sens[,2]*(135/700)
plot(accuracy)

avg_acc <- rowMeans(spec.sens)
plot(avg_acc)


AUC_log <- sum(abs(diff(1-spec.sens[,1]))*spec.sens[1:100,2])

