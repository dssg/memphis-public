#Take a list of parcel ids for real or potential sales and extract the properties for 
#which they would serve as comparables.

library(RPostgreSQL)
setwd('<directory>')

#Connect to database
drv <- dbDriver("PostgreSQL")
# run this over an ssh tunnel
con <- dbConnect(drv, 'localhost', port='5431', user='******', password='******')

#Get Frayser data
rs = dbSendQuery(con, 'select parcelid,countytaxyrs from frayser.exportdata')
fray = fetch(rs, n=-1)
dbClearResult(rs)

#Limit to properties with six years of tax delinquency
sixyears = fray[fray$countytaxyrs == 6,'parcelid'] 



#Each comparison will use 5 matches. But select 6 because we will remove the 
#property of interest from one of the comparisons.

#Becasue the conditions of non-rehabed buildings are not what they will be later 
#We don't require that the properties of interest match on condition but limit our anlaysis
#to high quality properties

for(parofint in sixyears[15:length(sixyears)]){

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
and (td.grade = '25' or td.grade = '30') and (td.grade = sales.grade or sales.paridsale = '",parofint,"')
and (td.cdu = 'FR') and (td.cdu = sales.cdu or sales.paridsale = '",parofint,"')
and (td.cond = '3') and (td.cond = sales.cond or sales.paridsale = '",parofint,"')
and abs(td.yrblt - sales.yrblt) < 5
and abs(td.sfla - sales.sfla) < 700

where td.card = 1
and ST_Within(par.centroid, (SELECT the_geom FROM cbana_neighborhoods WHERE gid=311))
) master
where master.rnum<=6", sep = "")


rs = dbSendQuery(con,query)
matching = fetch(rs, n = -1)
dbClearResult(rs)

matches = matching[matching$parcelid %in% matching[matching$matchedparid == parofint,'parcelid'],]
length(unique(matches$parcelid))
write.csv(matches, paste('output/frayparmatch/parmatch_',parofint,'.csv',sep=''))
print(paste(parofint,which(sixyears==parofint)))
}

#There are a lot of blank properties returned because they don't have a dweldat table
#Which means there's no building on the property. These are dropped in the data creation script
