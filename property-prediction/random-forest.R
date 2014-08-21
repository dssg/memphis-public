load("~/Documents/Yale/Summer 2014/DSSG/data/parceldat/data13.RData")

library(randomForest)
library(pROC)

formula = blighted ~ rtotapr + yrblt + outofstate + corp + 
  sqft + no_utilities + aprland + cdu + asmtchng + rmchng + sqftchng + total_permit +
  whitecollar + bluecollar + service + lowincomerate + midincomerate + highincomerate +
  fc_rate_lag1 + fc_rate_lag2 + fc_rate_lag3 + fc_rate_lag4 + tract_gross_sales + 
  weeds + we_charge1 + latefee + tb_bankrupt + tb_badcheck + saleyear + saleprice

train = sample(length(data$parcelid),100000)
test = (1:length(data$cond))[-train]
# test = which(!is.na(data$blighted))

rf.fit = randomForest(formula,data=data[train,],do.trace=50,ntree=200)
rf.probs = (predict(rf.fit,newdata=data[test,],type='prob'))[,1]

roc = roc(data$blighted[test],rf.probs,ci=TRUE)
plot(roc,print.auc=TRUE)
ci = ci.se(roc)
plot(ci)


acc = NULL
ts = NULL
by = .03
ps = seq(0,(1-by),by)
for(p in ps){
  t = table((data$blighted[test])[which(rf.probs>=p & rf.probs<(p+by))])
  acc = rbind(acc,as.numeric(t[1]/sum(t)))
  ts = rbind(ts,sum(t))
}
plot((ps+by/2),acc,lwd=3,xlim=c(0,1),ylim=c(0,1),cex=ts/800,
     xlab='Predicted Probability Blighted',ylab='Empirical Probability Blighted')
abline(a=0,b=1,lwd=1,col='red')

imp = importance(rf.fit)
cbind(row.names(imp),rev(imp[order(imp)]))
barplot(rev(imp[order(imp)]),names=row.names(imp))

# results11 = data.frame(data$parcelid,rf.probs)
# save(results11,file='~/Documents/Yale/Summer 2014/DSSG/data/results/results11.RData')
# write.csv(results12,file='~/Documents/Yale/Summer 2014/DSSG/data/results/results12.csv')
