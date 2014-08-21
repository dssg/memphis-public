#Going to take the relationship files made in get_frayser_hypo_sales.R
#Then bring in the data from the database
#Then we can type in a parcel and an amount
#Then we'll 

library(RPostgreSQL)
setwd('<directory>')

#Connect to database
drv <- dbDriver("PostgreSQL")
# run this over an ssh tunnel
con <- dbConnect(drv, 'localhost', port='5431', user='*****', password='******')

## Query 1: Get pools, porches, patios ##
rs = dbSendQuery(con, "select * from mconway.hedonicfeatures")
hedon = fetch(rs, n = -1)
dbClearResult(rs)

#Combine porch, patio, deck because only single digits of the first and last
#But actually I think we're missing too much to be useful
hedon$patio = hedon$patio | hedon$deck | hedon$porch
table(hedon$patio) #Only 756 with anything in the whole city
hedon = hedon[,c('parcelid','pool')]


## Query 2: Get main property information ##

rs = dbSendQuery(con,"select par.parcelid as parid,ST_X(ST_TRANSFORM(par.centroid,4326)) as lon, ST_Y(ST_TRANSFORM(par.centroid,4326)) as lat,
                 t13.aprland aprland13, t13.aprbldg aprbldg13, t13.class class13, t13.luc luc13, t13.asmtland asmtland13, t13.asmtbldg asmtbldg13, t13.rtotapr rtotapr13, t13.rtotasmt rtotasmt13,
                 dd.yrblt as yrblt, dd.sfla as sqft, dd.user1 as ownocc, dd.extwall, dd.style, dd.stories, dd.rmtot, dd.rmfam, dd.rmbed,
                 dd.fixbath, dd.fixhalf, dd.fixaddl, dd.bsmt, dd.heat, dd.fuel, dd.heatsys, dd.intext, dd.wbfp_o, dd.wbfp_s, dd.wbfp_pf, dd.bsmtcar,
                 dd.grade, dd.cdu, dd.cond, dd.rooftype, dd.roofcover, dd.card
                 
                 from parcels.parcels2013 par left join tax2013_asmt t13 on par.parcelid = t13.parid
                 left join tax2013_dweldat dd on dd.parid = par.parcelid
                 where ST_Within(centroid, (SELECT the_geom FROM cbana_neighborhoods WHERE gid=311))")           


#Frayser CDC is gid = 311      
#Binghampton is gid = 234

propdat = fetch(rs, n=-1)  
dbClearResult(rs)

dim(propdat) #14357
propdat = propdat[is.na(propdat$card) | propdat$card == 1,] #First take just ones that are either the first improvement on the property or the only 
propdat = propdat[propdat$luc13 == '062' & !is.na(propdat$luc13) & propdat$class13 == "R" & !is.na(propdat$class13),] #Now limit to SFR
dim(propdat) #12095
sum(duplicated(propdat[!is.na(propdat$parid),'parid'])) #No duplicates

#Merge on hedonic features
propdat = merge(propdat, hedon, by.x = 'parid',by.y = 'parcelid',all.x = T)
propdat[is.na(propdat$pool),'pool'] = F
propdat$fires = propdat$wbfp_o >0
propdat[is.na(propdat$fires),'fires'] = F
propdat[is.na(propdat$rmfam),'rmfam'] = 0


## Query 3: Get relevant sales data ##
#2013 sales
rs = dbSendQuery(con, "select t13.* from parcels.parcels2013 par inner join tax2013_sales t13
                 on par.parcelid = t13.parid
                 where t13.saledt >= '2010-01-01 00:00:00' and t13.saledt < '2013-01-01 00:00:00' 
                 and ST_Within(centroid, (SELECT the_geom FROM cbana_neighborhoods WHERE gid=311))")
sales13 = fetch(rs, n = -1)
dbClearResult(rs)

dim(sales13) #5072
sales13 = sales13[sales13$price >0 & !is.na(sales13$price),] #Remove sales with zero price bc shouldn't impact assessments
dim(sales13) #3467
sales13[!is.na(sales13$parid) &(duplicated(sales13[,c('parid','transno','saledt','price')])|duplicated(sales13[,c('parid','transno','saledt','price')],fromLast = T)),c('parid','transno','saledt','price','saleval','saletype')] #7 duplicates
sales13 = sales13[(sales13$transno != '11060164' | sales13$saleval == "A") , ]  #Keep the A valued one when possible
sales13 = sales13[(sales13$transno != '12012357' | sales13$saleval == "A") , ]  #Keep the A valued one when possible
dim(sales13) #3465
sales13 = sales13[!duplicated(sales13[,c('parid','transno','saledt','price')]),] #The rest of the duplicates seem totally identical
dim(sales13) #3460
sum(duplicated(sales13[,c('parid','transno')])) #Parid + transno are unique here
sales13 = sales13[!is.na(sales13$price),] 
dim(sales13) #3459

#Only keep most recent sale per property
stopifnot(sales13$saledt<as.POSIXct('2014-01-01') ) 
sales13 = sales13[order(sales13$saledt,decreasing = T),]
sales13 = sales13[order(sales13$parid, sales13$saledt, decreasing = T),]
options("scipen"=100, "digits"=4)
head(sales13[duplicated(sales13$parid)|duplicated(sales13$parid,fromLast = T),c('parid','transno','saledt','price','saletype','saleval')],20) #Multi-sale properties
sales13 = sales13[!duplicated(sales13$parid),]
dim(sales13) #Final list of 2072
sales13 = sales13[!is.na(sales13$saleval) & sales13$saleval == "A",]
dim(sales13) #Final 223 that go into the regression

# merge and model
bdat= merge(propdat, sales13, by= "parid", all.x = T)
bdat$age = 2013 - bdat$yrblt
bdat[is.na(bdat$rmfam),'rmfam'] = 0
mod = lm((price) ~., data = bdat[,c('price','age','rmbed','rmtot','fixbath','sqft','grade','cdu','rooftype','heat','fires','pool')])

summary(mod) 


## Query 4: Get list of Frayser potential rehabs plus addresses
rs = dbSendQuery(con, 'select parcelid,countytaxyrs,parceladd from frayser.exportdata f where f.countytaxyrs = 6')
fray = fetch(rs, n=-1)
fray$dfname = ''
dbClearResult(rs)


#Bring in all the match files and assign them
files = Sys.glob("output/frayparmatch/parmatch_*.csv")
mdfs = vector()
mparnums = vector() 
  for(f in 1:length(files)){
    print(f)
    parname = gsub('scripts/analysis/output/frayparmatch/parmatch_','',gsub('.csv','',files[f]))
    m = read.csv(files[f])
  if(dim(m)[1]>0 & dim(propdat[propdat$parid == parname,])[1] > 0 & dim(table(m$matchedparid))>1){
      
      m$without = m$matchedparid != parname
      m$with = duplicated(m$parcelid, fromLast = T)
    
      # Make Comparables Dataset - with #
      m = merge(m,propdat[,c('lat','lon','rtotapr13','parid','yrblt','sqft','grade','cdu','heat','rooftype','style','extwall','fires','rmbed','rmfam','rmtot','fixbath','fixhalf','fixaddl','bsmt','pool')],by.x = 'parcelid',by.y = 'parid',all.x = T)
      m = merge(m,propdat[,c('parid','yrblt','grade','cdu','heat','rooftype','style','extwall','fires','rmbed','rmfam','rmtot','fixbath','fixhalf','fixaddl','bsmt','pool')],by.x = 'matchedparid',by.y = 'parid',all.x=T,suffix = c('_a','_s'))
      m = m[order(m$parcelid,m$rnum),]
      assign(paste('m',f,sep = ''),m)  
      fray[fray$parcelid == parname,'dfname'] = paste('m',f,sep = '')
  }  
  }

rm(m,files,mdfs,mparnums,hedon, con, drv,f,rs,sales13)

frayparcelsplot = merge(fray[fray$dfname!='',],propdat,by.x = 'parcelid',by.y = 'parid',all.x = T)

save.image(file = 'frayser_app/data/frayser_data.RData')










#072018  00022 This propety is in this new dataset and not in the one I was using before. It had been being droped as a duplicate.
#test = merge(bs[bs$saleval == 'A' & !is.na(bs$saleval),],sales13, by = 'parid',all = T)

###### Done making Add on dataset ######



###### Bring in the property list for the prop of int ######
# 
# runcompare = function(parofint, pricehypo){
# #Import base data
# files <- (Sys.glob("scripts/analysis/output/frayparmatch/parmatch_*.csv"))
# m = read.csv(paste('scripts/analysis/output/frayparmatch/parmatch_',parofint,'.csv', sep = ''))
# 
# if(dim(m)[1]==0){
#   return('That property does not match with any others. It might not be SFR')
# }
# else{
#   
#   # Change price to what we want #
#   m[m$matchedparid == parofint,'price'] = pricehypo
#   
#   # Make with and without datasets #
#   m$without = m$matchedparid != parofint
#   m$with = duplicated(m$parcelid, fromLast = T)
# 
#   # Make Comparables Dataset - with #
#   m = merge(m,propdat[,c('rtotapr13','parid','yrblt','sqft','grade','cdu','heat','rooftype','style','extwall','fires','rmbed','rmfam','rmtot','fixbath','fixhalf','fixaddl','bsmt','pool')],by.x = 'parcelid',by.y = 'parid')
#   m = merge(m,propdat[,c('parid','yrblt','grade','cdu','heat','rooftype','style','extwall','fires','rmbed','rmfam','rmtot','fixbath','fixhalf','fixaddl','bsmt','pool')],by.x = 'matchedparid',by.y = 'parid',suffix = c('a','s'))
#   m = m[order(m$parcelid),]
# 
#   #New prediction
#   m$newpred = (m$price)
#   m$newpred = m$newpred + mod$coefficients['sqft'] * (m$sqft.y - m$sqft.x)
#   m$newpred = m$newpred + mod$coefficients['rmtot'] * (m$rmtota - m$rmtots)
#   m$newpred = m$newpred + mod$coefficients['rmbed'] * (m$rmbeda - m$rmbeds)
#   m$newpred = m$newpred + mod$coefficients['fixbath'] * (m$fixbatha - m$fixbaths)
#   m$newpred = m$newpred + mod$coefficients['firesTRUE'] * (m$firesa - m$firess)
#   m$newpred = m$newpred - mod$coefficients['heat3'] * (m$heats == 3 & (m$heata ==2 | m$heata == 1))
#   m$newpred = m$newpred - mod$coefficients['heat4'] * (m$heats == 4 & (m$heata ==2 | m$heata == 1))
#   m$newpred = m$newpred + mod$coefficients['heat3'] * (m$heata == 3 & (m$heats ==2 | m$heats == 1))
#   m$newpred = m$newpred + mod$coefficients['heat4'] * (m$heata == 4 & (m$heats ==2 | m$heats == 1))
#   m$newpred = m$newpred + (mod$coefficients['heat3'] -mod$coefficients['heat4']) * (m$heata == 3 & m$heats ==4)
#   m$newpred = m$newpred + (mod$coefficients['heat4'] -mod$coefficients['heat3']) * (m$heata == 4 & m$heats ==3)
#   m$newpred = m$newpred + (mod$coefficients['poolTRUE']) * (m$poola - m$pools)
#   m$newpred = (m$newpred)
#   
#   stopifnot(sum(is.na(m$newpred)) == 0)
#   
#   with_agg = aggregate(m[m$with,c('rtotapr13','newpred')], by=list(m[m$with,'parcelid']),FUN = mean)
#   names(with_agg) = c('parid','rtotapr13','newpred')
# 
#   wo_agg = aggregate(m[m$without,c('rtotapr13','newpred')], by=list(m[m$without,'parcelid']),FUN = mean)
#   names(wo_agg) = c('parid','rtotapr13','newpred')
#   
#   compare = merge(with_agg, wo_agg, by = 'parid',suffix = c('_w','_wo'))
#   compare$dif = compare$newpred_w - compare$newpred_wo
# 
#   return(cat(paste('Current Assessed Value: ',propdat[propdat$parid == parofint, 'rtotapr13'], 
#                '\n Proposed Sale Value: ',pricehypo,'\n Total Assessment Change From Sale: ',
#                round(sum(compare$dif)),sep = '')))
# }
# }

