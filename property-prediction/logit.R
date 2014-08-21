formula = blighted ~ rtotapr + I(2014 - yrblt) + outofstate + corp + 
  sqft + no_utilities + aprland + cdu + asmtchng + rmchng + sqftchng +
  whitecollar + bluecollar + service + lowincomerate + midincomerate + highincomerate +
  fc_rate_lag1 + fc_rate_lag2 + fc_rate_lag3 + fc_rate_lag4 + tract_gross_sales + 
  weeds + we_charge1 + latefee + tb_bankrupt + tb_badcheck + total_permit

glm.fit=glm(formula,family = binomial, data = data[train,], na.action = na.omit)

summary(glm.fit)

glm.probs = predict.glm(glm.fit,newdata=data[test,])
glm.pred = glm.probs>0.5
table(glm.pred,data$blighted[test])

predacc = NULL
l = length(glm.probs)
r = rank(glm.probs,ties.method='first')
for(i in 1:2000){
  predacc = rbind(predacc,mean(test$blighted[(l-r)<i]))
}
plot(predacc,type='l')

ps = seq(0,1,.01)
fp = rep(NA,length(ps))
tp = rep(NA,length(ps))
for(p in ps){
  glm.pred=glm.probs>p
  fpos = sum(glm.pred==T & data$blighted[test]==F,na.rm=T)/sum((data$blighted[test])[!is.na(glm.pred)]==F)
  tpos = sum(glm.pred==T & data$blighted[test]==T,na.rm=T)/sum((data$blighted[test])[!is.na(glm.pred)]==T)
  fp[which(ps==p)] = fpos
  tp[which(ps==p)] = tpos
}
plot(fp,tp,type='l',lwd=5,cex.lab=1.5,cex.main=3,xlim=c(0,1),ylim=c(0,1),
     xlab='False Positive Rate',
     ylab='True Positive Rate',
     main='ROC Curve Comparison')


lines(fpd,tpd,lwd=5,col='blue')
lines(fprf,tprf,lwd=5,col='darkgreen')
legend('bottomright',c('Logit','Decision Tree','Random Forest'),
       col=c('black','blue','darkgreen'),lty=1,lwd=5,cex=1.5,inset=.05)
