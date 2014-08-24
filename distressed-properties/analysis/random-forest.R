# This file is for training and testing the random forest classifier

# Load libraries
library(randomForest)
library(pROC)

# Load the dataset with all features of interest about each property
# load("data/data13.RData")

# Select features for the model
formula = blighted ~ rtotapr + yrblt + outofstate + corp + 
  sqft + no_utilities + aprland + cdu + asmtchng + rmchng + sqftchng + total_permit +
  whitecollar + bluecollar + service + lowincomerate + midincomerate + highincomerate +
  fc_rate_lag1 + fc_rate_lag2 + fc_rate_lag3 + fc_rate_lag4 + tract_gross_sales + 
  weeds + we_charge1 + latefee + tb_bankrupt + tb_badcheck + saleyear + saleprice

# Set the training and test sets for the model
train = sample(length(data$parcelid),100000)
test = (1:length(data$cond))[-train]

# Train model and predict classes for the test set (if you have labels for the test samples). 
# If testing/predicting on a year other than the training set, you will need to call newdata on a different dataset
rf.fit = randomForest(formula,data=data[train,],do.trace=50,ntree=200)
rf.probs = (predict(rf.fit,newdata=data[test,],type='prob'))[,1]

# Evaluate the model with an ROC curve
roc = roc(data$blighted[test],rf.probs,ci=TRUE)
plot(roc,print.auc=TRUE)
ci = ci.se(roc)
plot(ci)

# Measure the relationship between the predicted and empirical probabilities that a 
# property is distressed. 'bin' is the range of probabilities in each bin
acc = NULL
ts = NULL
bin = .03
ps = seq(0,(1-bin),bin)
for(p in ps){
  t = table((data$blighted[test])[which(rf.probs>=p & rf.probs<(p+bin))])
  acc = rbind(acc,as.numeric(t[1]/sum(t)))
  ts = rbind(ts,sum(t))
}
plot((ps+bin/2),acc,lwd=3,xlim=c(0,1),ylim=c(0,1),cex=ts/800,
     xlab='Predicted Probability Blighted',ylab='Empirical Probability Blighted')
abline(a=0,b=1,lwd=1,col='red')

# Observe the importance of each variable in the trained random forest
imp = importance(rf.fit)
cbind(row.names(imp),rev(imp[order(imp)]))
barplot(rev(imp[order(imp)]),names=row.names(imp))

# Save your restuls in the desired format and location
# results11 = data.frame(data$parcelid,rf.probs)
# save(results11,file='../data/results11.RData')
# write.csv(results12,file='../data/results12.csv')
