library(nnet)
library(mxnet)

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

### cross validation
pred = rep(0,700)
#AUC = rep(0,100) 
sizes = c(1,2,3,4,5,6,7,8,9,10)
for(s in 1:1) {
  for (j in 1:1) {
    ss = sample(700,replace=F)
    for (i in seq(1,700,by=70)) {
      nn = nnet(truth[-ss[i:(i+69)]] ~ .,data=scaled[-ss[i:(i+69)],],size = sizes[s], decay = 1.0e-5, maxit = 1000, trace=TRUE)
      pred[ss[i:(i+69)]] = predict(nn,newdata=scaled[ss[i:(i+69)],],type="class")
    }
    cm <- table(truth, pred)
    sizes[s]
    cm
    # cross classify truth and prediction
    #xtabs(~truth + ifelse(pred1<0.5,0,1))
    
    
    # create specificity vs. sensitivity table at different probability thresholds
    #spec.sens = matrix(0,101,2)
    #for (i in 0:100) {
    #  spec.sens[i+1,1] = sum(truth == 0 & !ifelse(pred1< i/100,0,1))/sum(truth==0)
    #  spec.sens[i+1,2] = sum(truth == 1 & ifelse(pred1< i/100,0,1))/sum(truth==1)	
    #}
    
    # which gives maximum average correct classification
    #spec.sens[which.max(apply(spec.sens,1,mean)),]
    
    # plot ROC curve
    #plot(1-spec.sens[,1],spec.sens[,2],ylab="Sensitivity",xlab="1-Specificity",col=4,type="s");abline(0,1,col=1)
    
    # calculate area under ROC curve
    #(AUC[j] = sum(abs(diff(1-spec.sens[,1]))*spec.sens[1:100,2]))
  }
}
