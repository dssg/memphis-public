library(adabag)

formula = blighted ~ log(rtotapr) + I(2014 - yrblt) + I((1-outofstate)*(1-corp)) + 
  sqft + no_utilities + heatsys  + popchg + pop2010 + pop2000 + wbfp_o + wbfp_pf +
  vacant + whitecollar + bluecollar + service + whitecollar + lowincomerate + midincomerate + highincomerate +
  fc_rate_07 + fc_rate_06 + fc_rate_05 + fc_rate_04 + tract_gross_sales + 
  weeds + sidewalks + we_charge1  +
  latefee + tb_bankrupt + tb_badcheck

train = 1:1000
test = 20000:20010

ok = which(data$blighted[train]=='ok')
distressed = which(data$blighted[train]=='distressed')
ok = sample(ok,length(ok)/2)
train = union(ok,distressed)
length(train)
barplot(c(length(ok),length(distressed),length(blighted)),
        names.arg = c('ok','distressed','blighted'))

ada.fit = boosting(formula,data[train,], mfinal=10, control=rpart.control(cp = 0))
ada.pred = predict.boosting(ada.fit,data[test,])
ada.probs = apply(ada.pred$prob,1,max)


predacc = NULL
l = length(ada.probs)
r = rank(ada.probs,ties.method='first')
acc = data$condition[test]==ada.pred$class
for(i in 1:length(test)){
  predacc = rbind(predacc,mean(acc[(l-r)<i]))
}

table(ada.pred$class[which((l-r)<10000)])

ps = seq(0,1,.01)
fp = rep(NA,length(ps))
tp = rep(NA,length(ps))
for(p in ps){
  problem_prob = (1-ada.pred$prob[,1])>p
  fpos = sum(problem_prob==T & data$condition[test]=='ok')/sum(data$condition[test]=='ok')
  tpos = sum(problem_prob==T & data$condition[test]!='ok')/sum(data$condition[test]!='ok')
  fp[which(ps==p)] = fpos
  tp[which(ps==p)] = tpos
}
plot(fp,tp,type='l',lwd=5,cex.lab=1.5,cex.main=3,
     xlab='False Positive Rate',
     ylab='True Positive Rate',
     main='ROC Curve for Boosting Model')

fpb=fp;tpb=tp

barplot(boost$imp)
table(ada.pred$class,data$condition[test])

