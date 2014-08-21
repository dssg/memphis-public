#Estimating the impact of Frayser CDC renovations in 2013-2014 on property appraisals

library(RPostgreSQL)
setwd('<directory>')


## Get data from database ##
drv <- dbDriver("PostgreSQL")
# run this over an ssh tunnel
con <- dbConnect(drv, 'localhost', port='5431', user='******', password='******')


#Get pools, porches, patios
rs = dbSendQuery(con, "select * from mconway.hedonicfeatures")
hedon = fetch(rs, n = -1)
dbClearResult(rs)

#Only use pool because too few instances of the other features
hedon = hedon[,c('parcelid','pool')]


#Get comparable sales for Frayser
rs = dbSendQuery(con,"select * from 
(select par.parcelid, sales.paridsale as matchedparid, st_distance(par.centroid,sales.centroid) dist,sales.transno, sales.saledt, sales.price, sales.saleval, sales.sfla sqft, row_number() over(partition by par.parcelid order by st_distance(par.centroid,sales.centroid)) as rnum
 
                 from parcels.parcels2013 par left join tax2013_asmt t13 on par.parcelid = t13.parid
                 left join tax2013_dweldat td on par.parcelid = td.parid
                 
                 left join
                 (select t.*, par.centroid as centroid, ts.parid paridsale, ts.price price, ts.saledt saledt, ts.saletype saletype, ts.saleval saleval, ts.steb steb, ts.numpars numpars, ts.transno transno 
                 from parcels.parcels2013 par inner join 
                 
                 (
                 
                 select * from
                 (select *, row_number() over(partition by parid order by saledt desc) rownum from tax2013_sales  
                 where price >0
                 and saledt >= '2010-01-01 00:00:00' and saledt < '2013-01-01 00:00:00' 
                 and saleval = 'A'
                 ) tx
                 where tx.rownum =1
                 order by tx.parid, tx.rownum
                 
                 ) ts
                        
                 on par.parcelid = ts.parid
                 left join tax2013_dweldat t on par.parcelid = t.parid
                 where t.card = 1
                 and ST_Within(centroid, (SELECT the_geom FROM cbana_neighborhoods WHERE gid=311))) as sales
                 
                 on td.fuel = sales.fuel
                 and td.grade = sales.grade
                 and td.cdu = sales.cdu
                 and abs(td.yrblt - sales.yrblt) < 5
                 and abs(td.sfla - sales.sfla) < 500
                 
                 where td.card = 1
                 and ST_Within(par.centroid, (SELECT the_geom FROM cbana_neighborhoods WHERE gid=311))
) master
                 where master.rnum<=6")

matching = fetch(rs, n = -1)
dbClearResult(rs)


#Get property details

rs = dbSendQuery(con,"select par.parcelid as parid, t8.aprland aprland8, t8.aprbldg aprbldg8, t8.class class8, t8.luc luc8, t8.asmtland asmtland8, t8.asmtbldg asmtbldg8, t8.rtotapr rtotapr8, t8.rtotasmt rtotasmt8,
                 t13.aprland aprland13, t13.aprbldg aprbldg13, t13.class class13, t13.luc luc13, t13.asmtland asmtland13, t13.asmtbldg asmtbldg13, t13.rtotapr rtotapr13, t13.rtotasmt rtotasmt13,
                 dd.yrblt as yrblt, dd.sfla as sqft, dd.user1 as ownocc, dd.extwall, dd.style, dd.stories, dd.rmtot, dd.rmfam, dd.rmbed,
                 dd.fixbath, dd.fixhalf, dd.fixaddl, dd.bsmt, dd.heat, dd.fuel, dd.heatsys, dd.intext, dd.wbfp_o, dd.wbfp_s, dd.wbfp_pf, dd.bsmtcar,
                 dd.grade, dd.cdu, dd.cond, dd.rooftype, dd.roofcover, dd.card
                 
                 from parcels.parcels2013 par left join tax2008_asmt t8 on par.parcelid = t8.parid
                 left join tax2013_asmt t13 on par.parcelid = t13.parid
                 left join tax2013_dweldat dd on dd.parid = par.parcelid
                 where ST_Within(centroid, (SELECT the_geom FROM cbana_neighborhoods WHERE gid=311)) ")           


#Frayser CDC is 311      
#Binghampton is 234

propdat = fetch(rs, n=-1)       
dim(propdat) #14357 - 1956 have dweldat, lots of missings it turns out
propdat = propdat[propdat$card == 1 & !is.na(propdat$card),] #Just get the first building for each property
dim(propdat) #12486, 
dim(propdat[!is.na(propdat$parid),]) #12486
propdat = propdat[!is.na(propdat$parid),]
dim(propdat) #12486, 
sum(duplicated(propdat[!is.na(propdat$parid),'parid'])) #No duplicates
sum(duplicated(propdat$parid))
dbClearResult(rs)

#Limit to SFR
propdat = propdat[propdat$luc13 == '062' & !is.na(propdat$luc13) & propdat$class13 == "R" & !is.na(propdat$class13),]
dim(propdat) #12091, 

#Merge on hedonic features
propdat = merge(propdat, hedon, by.x = 'parid',by.y = 'parcelid',all.x = T)
propdat[is.na(propdat$pool),'pool'] = F
propdat$age = 2013 - propdat$yrblt
propdat$fires = propdat$wbfp_o >0
propdat[is.na(propdat$rmfam),'rmfam'] = 0
dim(propdat) #12091


#Get 2013 sales
rs = dbSendQuery(con, "select t13.* from parcels.parcels2013 par inner join tax2013_sales t13
                 on par.parcelid = t13.parid
                 where t13.saledt >= '2010-01-01 00:00:00' and t13.saledt < '2013-01-01 00:00:00' 
                 and t13.saleval = 'A'
                 and ST_Within(centroid, (SELECT the_geom FROM cbana_neighborhoods WHERE gid=311))")
bs13 = fetch(rs, n = -1)
dbClearResult(rs)
dbDisconnect(con)

bs13 = bs13[bs13$price >0,] #Remove sales with zero price bc shouldn't impact assessments
bs13 = bs13[!duplicated(bs13[,c('parid','transno','saledt','price')]),]
sum(duplicated(bs13[,c('parid','transno')])) #Parid + transno are unique here

bs13[bs13$saledt >= as.POSIXct("2014-01-01"),] #One property from 2951, drop
bs = bs13[bs13$saledt  < as.POSIXct("2014-01-01"),]
bs = bs[!is.na(bs$saledt),]

#Only keep most recent sale per property
stopifnot(bs$saledt<as.POSIXct('2014-01-01')) 
bs = bs[order(bs$saledt,decreasing = T),]
bs = bs[order(bs$parid, bs$saledt, decreasing = T),]
options("scipen"=100, "digits"=4)
head(bs[duplicated(bs$parid)|duplicated(bs$parid,fromLast = T),c('parid','saledt','price','saletype','saleval')],20)
bs = bs[!duplicated(bs$parid),] #Final 1513 newest sales. Many duplicated ones were foreclosures that were sold a month or so later. We keep the latter sale, which is often much cheaper

# merge and model
bdat= merge(propdat, bs, by= "parid", all.x = T)

mod = lm((price) ~., data = bdat[,c('price','age','rmbed','rmtot','fixbath','sqft','grade','cdu','rooftype','heat','fires','pool')])

summary(mod)


## Calculate impact of rehabilitations ##

#Merge full info on for the comparable sales
f = merge(matching,propdat[,c('rtotapr13','parid','yrblt','sqft','grade','cdu','heat','rooftype','style','extwall','fires','rmbed','rmfam','rmtot','fixbath','fixhalf','fixaddl','bsmt','pool')],by.x = 'parcelid',by.y = 'parid',all.x = T, suffix = c('_s','_a'))
f = merge(f,propdat[,c('parid','yrblt','grade','cdu','heat','rooftype','style','extwall','fires','rmbed','rmfam','rmtot','fixbath','fixhalf','fixaddl','bsmt','pool')],by.x = 'matchedparid',by.y = 'parid',all.x=T,suffix = c('_a','_s'))
f = f[order(f$parcelid,f$rnum),]
f = f[f$rnum != 6 & !is.na(f$rnum),]

#Fix nas
f[is.na(f$fires_s),'fires_s' ] =F
f[is.na(f$fires_a),'fires_a' ] =F

#New prediction
f$newpred = (f$price)
f$newpred = f$newpred + mod$coefficients['sqft'] * (f$sqft_a - f$sqft_s)
f$newpred = f$newpred + mod$coefficients['rmtot'] * (f$rmtot_a - f$rmtot_s)
f$newpred = f$newpred + mod$coefficients['rmbed'] * (f$rmbed_a - f$rmbed_s)
f$newpred = f$newpred + mod$coefficients['fixbath'] * (f$fixbath_a - f$fixbath_s)
f$newpred = f$newpred + mod$coefficients['firesTRUE'] * (f$fires_a - f$fires_s)
f$newpred = f$newpred - mod$coefficients['heat3'] * (f$heat_s == 3 & (f$heat_a ==2 | f$heat_a == 1))
f$newpred = f$newpred - mod$coefficients['heat4'] * (f$heat_s == 4 & (f$heat_a ==2 | f$heat_a == 1))
f$newpred = f$newpred + mod$coefficients['heat3'] * (f$heat_a == 3 & (f$heat_s ==2 | f$heat_s == 1))
f$newpred = f$newpred + mod$coefficients['heat4'] * (f$heat_a == 4 & (f$heat_s ==2 | f$heat_s == 1))
f$newpred = f$newpred + (mod$coefficients['heat3'] -mod$coefficients['heat4']) * (f$heat_a == 3 & f$heat_s ==4)
f$newpred = f$newpred + (mod$coefficients['heat4'] -mod$coefficients['heat3']) * (f$heat_a == 4 & f$heat_s ==3)
f$newpred = f$newpred + (mod$coefficients['poolTRUE']) * (f$pool_a == T & f$pool_s ==F)
f$newpred = (f$newpred)

g = aggregate(f[,c('rtotapr13','newpred')], by=list(f[,'parcelid']),FUN = mean)
names(g) = c('parid','rtotapr13','pred')
plot(g[,c('rtotapr13','pred')],pch = '.')

#correlation is 0.74
cor(g$rtotapr13,g$pred, use = 'complete.obs')


#Set property to drop
#Two Frayser CDC properties in Frayser were rehabbed and sold with saleval = A during 2010-2013
#072107  00106 is 3026 Spring Hill Dr
#072071  00032 is 3240 Debby Street

for(frayprop in c('072071  00032','072107  00106')){
matches = matching[(matching$matchedparid == frayprop & !is.na(matching$matchedparid)),'parcelid']
adjmatch = matching[(matching$matchedparid != frayprop) & !is.na(matching$matchedparid) & matching$parcelid %in% matches,]

#Merge full info on for the comparable sales
#Swtiching from evaluation of Frayser to eval the model
f = merge(adjmatch,propdat[,c('parid','rtotapr13','yrblt','sqft','grade','cdu','heat','rooftype','style','extwall','fires','rmbed','rmfam','rmtot','fixbath','fixhalf','fixaddl','bsmt','pool')],by.x = 'parcelid',by.y = 'parid', suffix = c('_s','_a'))
f = merge(f,propdat[,c('parid','yrblt','grade','cdu','heat','rooftype','style','extwall','fires','rmbed','rmfam','rmtot','fixbath','fixhalf','fixaddl','bsmt','pool')],by.x = 'matchedparid',by.y = 'parid',suffix = c('_a','_s'))
f = f[order(f$parcelid),]

#Fix nas
f[is.na(f$fires_s),'fires_s' ] =F
f[is.na(f$fires_a),'fires_a' ] =F

#New prediction
f$newpred = (f$price)
f$newpred = f$newpred + mod$coefficients['sqft'] * (f$sqft_a - f$sqft_s)
f$newpred = f$newpred + mod$coefficients['rmtot'] * (f$rmtot_a - f$rmtot_s)
f$newpred = f$newpred + mod$coefficients['rmbed'] * (f$rmbed_a - f$rmbed_s)
f$newpred = f$newpred + mod$coefficients['fixbath'] * (f$fixbath_a - f$fixbath_s)
f$newpred = f$newpred + mod$coefficients['firesTRUE'] * (f$fires_a - f$fires_s)
f$newpred = f$newpred - mod$coefficients['heat3'] * (f$heat_s == 3 & (f$heat_a ==2 | f$heat_a == 1))
f$newpred = f$newpred - mod$coefficients['heat4'] * (f$heat_s == 4 & (f$heat_a ==2 | f$heat_a == 1))
f$newpred = f$newpred + mod$coefficients['heat3'] * (f$heat_a == 3 & (f$heat_s ==2 | f$heat_s == 1))
f$newpred = f$newpred + mod$coefficients['heat4'] * (f$heat_a == 4 & (f$heat_s ==2 | f$heat_s == 1))
f$newpred = f$newpred + (mod$coefficients['heat3'] -mod$coefficients['heat4']) * (f$heat_a == 3 & f$heat_s ==4)
f$newpred = f$newpred + (mod$coefficients['heat4'] -mod$coefficients['heat3']) * (f$heat_a == 4 & f$heat_s ==3)
f$newpred = f$newpred + (mod$coefficients['poolTRUE']) * (f$pool_a == T & f$pool_s ==F)
f$newpred = (f$newpred)

h = aggregate(f[,c('rtotapr13','newpred')], by=list(f[,'parcelid']),FUN = mean)
names(h) = c('parid','rtotapr13','pred')

check = merge(g, h, by = 'parid', all = T, suffix = c('_withrehab','_worehab'))
check$dif = check$pred_withrehab - check$pred_worehab
#check2 = check[c]

print(paste("For parcel ",frayprop,' total increase in assessed value is ',sum(check$dif, na.rm = T),sep  =''))
}

# [1] "For parcel 072071  00032 total increase in assessed value is 137934.209124527"
# [1] "For parcel 072107  00106 total increase in assessed value is 294871.968659399"
