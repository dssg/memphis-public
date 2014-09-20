  # retrieving data: 

  # load_data <- function (sample=F) {
  # Pull down our features from the database
  
  library(RPostgreSQL)
  dbdrv <- dbDriver('PostgreSQL')
  
  # run this over an ssh tunnel
  
  con <- dbConnect(dbdrv,
                   user="memphis",
                   password="P4c3l10",
                   dbname="memphis",
                   host="dssgsummer2014postgres.c5faqozfo86k.us-west-2.rds.amazonaws.com");  
    
  data <- dbGetQuery(con, "SELECT anon.anonid, aud.strprob, 
                       rtotasmt, asmt.aprland, asmt.aprbldg, asmt.rtotapr as rtotapr, asmt.class as class, asmt.luc as luc, land.sf as sfland,
                       dd.yrblt as yrblt, dd.sfla as sqft, upper(dd.user1)='Y' as ownocc, dd.extwall, dd.style, dd.stories, dd.rmtot, dd.rmfam, dd.rmbed,
                       dd.fixbath, dd.fixhalf, dd.fixaddl, dd.bsmt, dd.heat, dd.fuel, dd.heatsys, dd.intext, dd.wbfp_o, dd.wbfp_s, dd.wbfp_pf, dd.bsmtcar,
                       dd.grade, dd.cdu, dd.cond, dd.rooftype, dd.roofcover,
                       
                       -- flag variables about the owner
                       od.statecode != 'TN' AS outofstate,
                       position('pobox' in lower(regexp_replace(regexp_replace(od.adrstr, '[:punct:]', '', 'g'), '\\W', '', 'g'))) > 0 AS pobox,
                       lower(regexp_replace(od.own1, '[:punct:]', '', 'g')) ~ 'corp|inc|incorporated|la|el|srl|limited|llc|co|corporation|company|ltd|ltda|lda|de|sa|pty|ste|sarl|gmhb|sprl'
                       AS corp,
                       position('church' in lower(od.own1)) AS church,
                       
                       -- info about the last sale
                       sal.yrsold saleyear, sal.stype saletype, sal.sval saleval, sal.steb steb, sal.price saleprice,
                       
                       -- building permits
                       prmt.total total_permit,
                       
                       -- flag indicating no utilities in 2008
                       par.parcelid IN (SELECT parcelid FROM invt.no_utilities_2013_by_parcel WHERE EXTRACT(year FROM last_bill) < 2008) AS no_utilities,
                       
                       -- outbuildings
                       'PRC' = ANY(oby_code) OR 'PIF' = ANY (oby_code) OR 'PIG' = ANY (oby_code) OR 'PRV' = ANY (oby_code) OR 'PIV' = ANY (oby_code) OR 'PRF' = ANY (oby_code)
                       OR 'PRG' = ANY (oby_code) OR 'PIC' = ANY (oby_code) AS pool
                       
                       
                       FROM parcels.parcels2008 par
                       LEFT JOIN nbn.panda aud USING (parcelid)
                       
                       LEFT JOIN tax2008_asmt asmt  ON (parid = par.parcelid)
                       LEFT JOIN tax2008_land land  ON (land.parid = par.parcelid)
                       LEFT JOIN tax2008_dweldat dd ON (dd.parid = par.parcelid)
                       LEFT JOIN tax2008_owndat od  ON (od.parid = par.parcelid)
                       LEFT JOIN tax2008_pardat pd  ON (pd.parid = par.parcelid)
                       LEFT JOIN tax2008_owndat own ON (own.parid = par.parcelid)
                       
                       -- get the most recent sale
                       LEFT JOIN (  
                       select m.parid parid, EXTRACT (year FROM m.mrt_saledt) yrsold, m.mrt_saledt dt, a.saletype stype, a.saleval sval, a.steb steb, a.price price FROM tax2008_sales a inner join 
                       ( select max(s.saledt) mrt_saledt, s.parid FROM tax2008_sales s group by s.parid) m ON (a.parid = m.parid and a.saledt = m.mrt_saledt)
                       ) sal ON (sal.parid = par.parcelid)
                       
                       -- aggregate of recent permits
                       LEFT JOIN (SELECT parid, SUM(amount) AS total FROM tax2008_permit WHERE EXTRACT(YEAR FROM permdt) >= 2008 - 10 GROUP BY parid) prmt ON (prmt.parid = par.parcelid)
                       
                       -- aggregate of outbuildings
                       LEFT JOIN (SELECT parid, ARRAY_AGG(code) AS oby_code, ARRAY_AGG(yrblt) AS oby_yrblt FROM tax2008_oby GROUP BY parid) oby ON (oby.parid = par.parcelid)
                       
                       LEFT JOIN parcels.all_parcels_anon anon ON (par.parcelid = anon.parcelid)
                       
                       WHERE pd.\"class\" = 'R' -- only residential
                       AND ST_Within(centroid, (SELECT ST_Union(the_geom) FROM muniboundaries_shelby WHERE name='Memphis'))
                       AND anonid < 20000
                       ORDER BY anonid; -- squelch any potentially meaningful order (wouldn't want someone to grab assessor files and see that they're in the same order, then they'd have utility disconnects)
                       ")
      
  # anything that was in NxN is considered blighted
  
  data$blighted <- !is.na(data$strprob)
  
  # TODO: why do some have class M (mixed-use?)
  
  data$vacant <- data$luc == '000'
  data$extwall <- as.factor(data$extwall)
  data$style <- as.factor(data$style)
  data$roofcover <- as.factor(data$roofcover)
  data$rooftype <- as.factor(data$rooftype)
  data$basement <- data$bsmt > 2 # 3,4 are partial and full basement, 1 and 2 are no basement and crawlspace
  data$heat <- as.factor(data$heat)
  data$fuel <- as.factor(data$fuel)
  data$heatsys <- as.factor(data$heatsys)
  data$intext <- as.ordered(data$intext)
  
  # Per AEDIT, cond is defined two ways, one by numbers and one by letters. They are the same.
  data$cond[data$cond=='F'] <- 2 # 2: Fair
  data$cond[data$cond=='A'] <- 3 # 3: Average
  data$cond[data$cond=='G'] <- 4 # 4: Good
  data$cond[data$cond=='E'] <- 5 # 5: Excellent
  data$cond[data$cond=='P'] <- 1 # 1: Poor
  data$cond[data$cond==''] <- NA
  
  # we throw in an as.character, otherwise as.ordered picks up the original levels
  # (this type of text data comes in by default as a factor)
  data$cond <- as.ordered(as.character(data$cond))
  

##################################################################################################
##################################################################################################
##################################################################################################
#########################################  Build Models ##########################################

  # Logit model:
  # glm.fit is a model object
  
  glm.fit=glm(blighted~log(rtotapr)+I(2014-yrblt)+outofstate+pobox+corp+
              sqft+no_utilities+heatsys+rmbed,data=data, family=binomial, 
              na.action=na.omit)
  sumFit <- summary(glm.fit)
  glm.probs=predict(glm.fit,type='response')
  glm.pred=glm.probs>0.5
  table(glm.pred,glm.fit$y)
  mean(glm.pred==glm.fit$y)
  
  # Axis
  ps = seq(0,1,.01)
  # False-true Positive rate data lists 
  fp = rep(NA,length(ps))
  tp = rep(NA,length(ps))
  
  
  for(p in ps){
    glm.pred=glm.probs>p
    fpos = sum(glm.pred==T & glm.fit$y==0)/sum(glm.fit$y==0)
    tpos = sum(glm.pred==T & glm.fit$y==1)/sum(glm.fit$y==1)
    fp[which(ps==p)] = fpos
    tp[which(ps==p)] = tpos
  }
  
  plot(fp,tp,type='l',lwd=5,cex.lab=1.5,cex.main=3,
  xlab='False Positive Rate',
  ylab='True Positive Rate',
  main='ROC Curve for Logit Model')
  
  #SVM Model
  #Install packages
  
  install.packages("e1071")
  library(e1071)
  
  # Preparing model and testing
  glm.fit=glm(blighted~log(rtotapr)+I(2014-yrblt)+outofstate+pobox+corp+
                sqft+no_utilities+heatsys+rmbed,data=data, family=binomial, 
              na.action=na.omit)
  
  # Plug features here!
  features <- as.data.frame(cbind(data$rtotapr,I(2014-data$yrblt), data$outofstate, data$pobox, data$corp,
                    data$sqft,data$no_utilities,data$heatsys, data$rmbed))
  
  svm.fit <- svm(features, as.factor(data$blighted), probability=TRUE, na.action=na.omit)    
    
  nas <- apply(is.na(features), 1, sum) > 0
  a <- which(nas)
  
  # separate vectors for predicted and actual output. 
  svm.pred <- predict(svm.fit, features, probability = TRUE)       

  svm.actual <- data$blighted[-a]          
  svm.probs <- as.data.frame(attr(svm.pred, "probabilities"))
  svm.probs.true <- 
  head(svm.probs)
    
  #Mosdel parameters 
  print(svm.fit)
  summary(svm.fit)
  
  #confusion matrix
  table(svm.pred, svm.actual)  
  
  
  ############################################################################################
  ############################################################################################
  
  # Axis
  p.svm = seq(0,1,.01)
  # False-true Positive rate data lists 
  fp.svm = rep(NA,length(ps))
  tp = rep(NA,length(ps))
  
  
  for(p in p.svm){
    svm.pred=glm.probs>p
    fpos = sum(glm.pred==T & glm.fit$y==0)/sum(glm.fit$y==0)
    tpos = sum(glm.pred==T & glm.fit$y==1)/sum(glm.fit$y==1)
    fp[which(ps==p)] = fpos
    tp[which(ps==p)] = tpos
  }
  
  plot(fp,tp,type='l',lwd=5,cex.lab=1.5,cex.main=3,
       xlab='False Positive Rate',
       ylab='True Positive Rate',
       main='ROC Curve for Logit Model')
  
  
  
  
  
  
  