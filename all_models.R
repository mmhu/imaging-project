
ss = sample(700,replace=F)

# svm


# adaboost
ada.pred = rep(0,700)
for (i in seq(1,700,by=70)) {
  ada.cv = ada(truth[-ss[i:(i+69)]] ~ ., data = all.features[-ss[i:(i+69)],], loss = "e", type = "discrete")
  ada.pred[ss[i:(i+69)]] = predict(ada.cv, newdata = all.features[ss[i:(i+69)],])
}
ada.table = table(truth, ada.pred)

# random forest
rf.pred = rep(0,700)
for (i in seq(1,700,by=70)) {
  rf.cv <- randomForest(as.factor(truth[-ss[i:(i+69)]]) ~ ., data = all.features[-ss[i:(i+69)],], ntree = 10)
  rf.pred[ss[i:(i+69)]] = predict(rf.cv, newdata = all.features[ss[i:(i+69)],])
}
rf.table = table(truth, rf.pred)

