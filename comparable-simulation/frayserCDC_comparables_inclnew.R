#Estimating the impact of Frayser CDC renovations in 2013-2014 on property appraisals

library(RPostgreSQL)
setwd('<directory>')


## Get data from database ##
drv <- dbDriver("PostgreSQL")
# run this over an ssh tunnel
con <- dbConnect(drv, 'localhost', port='5431', user='memphis', password='P4c3l10')


#Get pools, porches, patios
rs = dbSendQuery(con, "select * from mconway.hedonicfeatures")
hedon = fetch(rs, n = -1)
dbClearResult(rs)

#Only use pool because too few instances of the other features
hedon = hedon[,c('parcelid','pool')]


#Get property details
rs = dbSendQuery(con,"select par.parcelid as parid, t8.aprland aprland8, t8.aprbldg aprbldg8, t8.class class8, t8.luc luc8, t8.asmtland asmtland8, t8.asmtbldg asmtbldg8, t8.rtotapr rtotapr8, t8.rtotasmt rtotasmt8,
                 t13.aprland aprland13, t13.aprbldg aprbldg13, t13.class class13, t13.luc luc13, t13.asmtland asmtland13, t13.asmtbldg asmtbldg13, t13.rtotapr rtotapr13, t13.rtotasmt rtotasmt13,
                 --t10.aprland aprland10, t10.aprbldg aprbldg10, t10.class class10, t10.luc luc10, t10.asmtland asmtland10, t10.asmtbldg asmtbldg10, t10.rtotapr rtotapr10, t10.rtotasmt rtotasmt10,
                 dd.yrblt as yrblt, dd.sfla as sqft, dd.user1 as ownocc, dd.extwall, dd.style, dd.stories, dd.rmtot, dd.rmfam, dd.rmbed,
                 dd.fixbath, dd.fixhalf, dd.fixaddl, dd.bsmt, dd.heat, dd.fuel, dd.heatsys, dd.intext, dd.wbfp_o, dd.wbfp_s, dd.wbfp_pf, dd.bsmtcar,
                 dd.grade, dd.cdu, dd.cond, dd.rooftype, dd.roofcover, dd.card
                 
                 from parcels.parcels2013 par left join tax2008_asmt t8 on par.parcelid = t8.parid
                 left join tax2013_asmt t13 on par.parcelid = t13.parid
                 left join tax2013_dweldat dd on dd.parid = par.parcelid
                 --      left join tax2010_asmt t10 on par.parcelid = t10.parid
                 where ST_Within(centroid, (SELECT the_geom FROM cbana_neighborhoods WHERE gid=311)) -- and dd.card = 1")           


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


#Get Frayser CDC sales
frayprops = read.csv('frayser_sold.csv')
frayprops$dt = as.POSIXct(frayprops$date,format = '%m/%d/%y')
parsofint = frayprops[frayprops$dt>"2009-12-31",]
parsofint$num_affected = 0
parsofint$total_increase = 0

#Get comparable sales for Frayser
for(parofint in parsofint$parid){
      
    query = paste("select * from 
                  (select par.parcelid, sales.paridsale as matchedparid, st_distance(par.centroid,sales.centroid) dist,sales.transno, sales.saledt, sales.price, sales.saleval, sales.sfla sqft, row_number() over(partition by par.parcelid order by st_distance(par.centroid,sales.centroid)) as rnum
                  
                  from parcels.parcels2013 par left join tax2013_asmt t13 on par.parcelid = t13.parid
                  left join tax2013_dweldat td on par.parcelid = td.parid
                  
                  left join
                  (select t.*, par.centroid as centroid, par.parcelid paridsale, ts.price price, ts.saledt saledt, ts.saletype saletype, ts.saleval saleval, ts.steb steb, ts.numpars numpars, ts.transno transno 
                  from parcels.parcels2013 par left join 
                  
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
                  and (ts.price is not null or par.parcelid = '",parofint,"')
                  and ST_Within(centroid, (SELECT the_geom FROM cbana_neighborhoods WHERE gid=311)))  as sales
                  
                  on td.fuel = sales.fuel
                  and (td.grade = sales.grade or (td.grade = '30' and sales.grade not in ('25','30') and sales.paridsale = '",parofint,"'))
                  and (td.cdu = sales.cdu or (td.cdu = 'FR' and sales.cdu != 'FR' and sales.paridsale = '",parofint,"'))
                  and (td.cond = sales.cond or (td.cond = '3' and sales.cond != '3' and sales.paridsale = '",parofint,"'))
                  and abs(td.yrblt - sales.yrblt) < 5
                  and abs(td.sfla - sales.sfla) < 500
                  
                  where td.card = 1
                  and ST_Within(par.centroid, (SELECT the_geom FROM cbana_neighborhoods WHERE gid=311))
                  ) master
                  where master.rnum<=6", sep = "")


rs = dbSendQuery(con,query)
rawmatch = fetch(rs, n = -1)
dbClearResult(rs)
matching = rawmatch[rawmatch$parcelid %in% rawmatch[rawmatch$matchedparid == parofint,'parcelid'],]
if(dim(matching)[1]==0){
  next
}
matching = matching[order(matching$parcelid, matching$rnum),]
matching$without = matching$matchedparid != parofint
matching$with = matching$rnum != 6 

matching[matching$matchedparid==parofint, 'price']= parsofint[parsofint$parid == parofint, 'price']

## Calculate impact of rehabilitations ##

#Merge full info on for the comparable sales
f = merge(matching,propdat[,c('rtotapr13','parid','yrblt','sqft','grade','cdu','heat','rooftype','style','extwall','fires','rmbed','rmfam','rmtot','fixbath','fixhalf','fixaddl','bsmt','pool')],by.x = 'parcelid',by.y = 'parid',all.x = T, suffix = c('_s','_a'))
f = merge(f,propdat[,c('parid','yrblt','grade','cdu','heat','rooftype','style','extwall','fires','rmbed','rmfam','rmtot','fixbath','fixhalf','fixaddl','bsmt','pool')],by.x = 'matchedparid',by.y = 'parid',all.x=T,suffix = c('_a','_s'))
f = f[order(f$parcelid,f$rnum),]

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

#Aggregate results for each property
with_agg = aggregate(f[f$with,c('rtotapr13','newpred')], by=list(f[f$with,'parcelid']),FUN = mean)
names(with_agg) = c('parid','rtotapr13','newpred')

wo_agg = aggregate(f[f$without,c('rtotapr13','newpred')], by=list(f[f$without,'parcelid']),FUN = mean)
names(wo_agg) = c('parid','rtotapr13','newpred')

#Compare with and without rehab
compare = merge(with_agg, wo_agg, by = 'parid',suffix = c('_w','_wo'))
compare$dif = compare$newpred_w - compare$newpred_wo
names(compare) = c('parcel_id','appraised_val','predicted_withsale','rtotapr13_wo','predicted_withoutsale','difference')

parsofint[parsofint$parid == parofint,'num_affected'] = dim(compare)[1]
parsofint[parsofint$parid == parofint,'total_increase'] = sum(compare$difference, na.rm = T)
print(paste(parsofint[parsofint$parid == parofint,'address'],' was sold for ',parsofint[parsofint$parid == parofint,'price']," dollars. This sale is estimated to have affected the appraisals of ",dim(compare)[1],' properties, and the total increase in assessed value due to its sale is estimated to be ',sum(compare$difference, na.rm = T),sep  =''))
}

#[1] "For parcel 072071  00032 total increase in assessed value is 321449.82960105"
#[1] "For parcel 072107  00106 total increase in assessed value is 179039.838330761"
write.csv(parsofint, 'output/fray_rehabs_13_14.csv')
dbDisconnect(con)

