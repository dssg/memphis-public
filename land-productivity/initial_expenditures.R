# Try to get the total amount of roads in each census tract.

library(RPostgreSQL)
library(ggplot2)
library(doBy)
library(rgdal)

library(ggmap)
library(sp)
library(maptools)
library(RColorBrewer)


setwd('~/projects/memphis')
# census block
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host="dssgsummer2014postgres.c5faqozfo86k.us-west-2.rds.amazonaws.com", user="memphis", password="memphis", dbname="memphis", port="5432")

#Total street length by census tract
rs <- dbSendQuery(con, "SELECT clipped.tract, sum(st_length(clipped_geom)) as tot_st_length
FROM (SELECT s.gid, s.name, t.geo_id2 as tract, (ST_Dump(ST_Intersection(s.the_geom, t.the_geom))).geom As clipped_geom
FROM streets_county s
	INNER JOIN census2010_sf1dp1_withgeom t
	ON ST_Intersects(s.the_geom, t.the_geom))  As clipped group by tract");	
roads <- fetch(rs, n = -1); dbClearResult(rs) 

#Total MATA length by census tract
rs <- dbSendQuery(con, "SELECT clipped.tract, sum(st_length(clipped_geom)) as tot_mata_length
FROM (SELECT s.gid, s.name, t.geo_id2 as tract, (ST_Dump(ST_Intersection(s.the_geom, t.the_geom))).geom As clipped_geom
FROM mataroutes s
	INNER JOIN census2010_sf1dp1_withgeom t
	ON ST_Intersects(s.the_geom, t.the_geom))  As clipped group by tract");	
mata <- fetch(rs, n = -1); dbClearResult(rs) 	
	
#Area of census tracts for fire and pop
rs = dbSendQuery(con, 'select geo_id2 as tract, hd01_s001 as pop, st_area(the_geom) as area from census2010_sf1dp1_withgeom')	
area <- fetch(rs, n = -1); dbClearResult(rs) 

#Assign to police
rs <- dbSendQuery(con, "SELECT s.name as police, t.geo_id2 as tract FROM policeboundaries s
	INNER JOIN census2010_sf1dp1_withgeom t
	ON ST_Intersects(st_centroid(t.the_geom), s.the_geom)");	
police <- fetch(rs, n = -1); dbClearResult(rs) 

#Divvy up police officers
for(i in unique(police$police)){
	police[police$police == i,'officers'] = 145 / dim(police[police$police == i,])[1]
	if(i == "South Main Station"){
		police[police$police == i,'officers'] = 175 / dim(police[police$police == i,])[1]	
	}
}

	
#Area Inside of city
rs <- dbSendQuery(con, "SELECT clipped.tract, sum(st_area(clipped_geom)) as cityarea
FROM (SELECT s.gid, t.geo_id2 as tract, (ST_Dump(ST_Intersection(s.the_geom, t.the_geom))).geom As clipped_geom
FROM memphisannexation_dissolved s
	INNER JOIN census2010_sf1dp1_withgeom t
	ON ST_Intersects(s.the_geom, t.the_geom))  As clipped group by tract");	
cityarea <- fetch(rs, n = -1); dbClearResult(rs) 
	
#Assign parks to tracts	
rs <- dbSendQuery(con, "SELECT count(*) as parks, t.geo_id2 as tract FROM parks s
	INNER JOIN census2010_sf1dp1_withgeom t
	ON ST_Intersects(t.the_geom, st_centroid(s.the_geom)) group by tract");	
parks <- fetch(rs, n = -1); dbClearResult(rs) 

#Assign libraries to tracts
rs <- dbSendQuery(con, "SELECT count(*) as libraries, t.geo_id2 as tract FROM mem_libraries s
	INNER JOIN census2010_sf1dp1_withgeom t
	ON ST_Intersects(t.the_geom,s.the_geom) group by tract");	
libraries <- fetch(rs, n = -1); dbClearResult(rs) 

#Total revenues in 2014
rs <- dbSendQuery(con, "select t.geo_id2 as tract, sum(partax.rtotasmt) as asmt from census2010_sf1dp1_withgeom t 
	inner join (select * FROM parcels.parcels2013 s
	inner join tax2013_asmt t on s.parcelid = t.parid) partax on st_intersects(t.the_geom, partax.centroid)
	group by t.geo_id2;");	
taxes <- fetch(rs, n = -1); dbClearResult(rs)  

#Total taxes are 534383412 - seems high? They're only supposed to account for 50% of revenues, right? 

dbDisconnect(con) # Close the connection

#Merge into one frame

dim(area)
dat = merge(area,roads, by = 'tract', all = T)
dat = merge(dat,mata, by = 'tract', all = T)
dat = merge(dat,police, by = 'tract', all = T)
dat = merge(dat,cityarea, by = 'tract', all = T)
dat = merge(dat,parks, by = 'tract', all = T)
dat = merge(dat,libraries, by = 'tract', all = T)
dat = merge(dat,taxes, by = 'tract', all = T)

#NAs to 0s
dat[is.na(dat)] = 0

#Set expenditures to 0 based on centroids
dat$incity = dat$police != '0'
for(i in c(4,5,7,8,9,10)){
	dat[dat$incity == F,i] = 0
}

##Now add expenditures
dat$fire = dat$cityarea / sum(dat$cityarea) * 155672508
dat$pubworks = dat$tot_st_length / sum(dat$tot_st_length) * 21791144
dat$mata = dat$tot_mata_length / sum(dat$tot_mata_length) * 20221000
dat$libcost = dat$libraries / sum(dat$libraries) * 17566268
dat$parkcost = dat$parks / sum(dat$parks) * 30081378
dat$polcost = dat$officers / sum(dat$officers) * 234055868
dat$totexp = apply(dat[,c('fire','pubworks','mata','libcost','parkcost','polcost')], 1, sum)
dat$taxes = dat$asmt * 0.034
dat[dat$incity == F, 'taxes'] = 0
dat$exppp = dat$totexp / dat$pop
dat[dat$pop < 500,'exppp'] = NA
dat$exparea = dat$totexp / dat$area	

#Raw net, though this doesn't make that much sense arithmetically
dat$net = dat$taxes - dat$totexp
dat[!dat$incity,'net'] = 0

#Try normalizing net by saying "We have 300 mil of rev and $480 mil of spending. Two ways. 
#One is to assume the difference just gets paid. Then we divide spending by 300/480 and compare
#Other is to assume the difference is distributed like this. Then we multiply rev by 480/300

dat$net_difpaid = dat$taxes - dat$totexp * (sum(dat[dat$incity, 'taxes'])/sum(dat$totexp))
dat$net_samedist = dat$taxes * sum(dat$totexp)/sum(dat$taxes) - dat$totexp

write.csv(dat,'~/projects/memphis/scripts/analysis/output/tract_exp.csv', row.names = F)








#Mapping
tracts = readShapePoly('Non-Programming/shapes/memphistracts.shp')
t2 = sp::merge(tracts, dat, by.x = 'geo_id2',by.y ='tract')

#Colors
colpal = colorRampPalette(brewer.pal(9,"Reds"))(100)

maxtot = max(t2$totexp)
maxpp = max(t2$totexp[t2$pop>500]/t2$pop[t2$pop>500])
maxarea = max(t2$totexp/t2$area)

divcolpal = colorRampPalette(brewer.pal(9,"RdYlGn"))(100)

colmap = function(cm, maxval, pal) sapply(cm, function(x) pal[round((x / maxval)*99)+1])
divcolmap = function(cm, maxval, pal) sapply(cm, function(x) pal[round(( (x + maxval) / (2*maxval))*99)+1])
getcol = function(column){
	modcol = column
	m = max(column, na.rm = T)
	modcol[t2$incity == F] = NA
	return(sapply(modcol, function(x) colpal[round((x / m)*99)+1]))
}

getcolarea = function(column){
	modcol = column / t2$area
	modcol[t2$incity == F] = NA
	m = max(modcol, na.rm = T)
	return(sapply(modcol, function(x) colpal[round((x / m)*99)+1]))
}

getcolpop = function(column){
	modcol = column / t2$pop
	modcol[t2$pop <500] = NA
	modcol[t2$incity == F] = NA
	m = max(modcol, na.rm = T)
	return(sapply(modcol, function(x) colpal[round((x / m)*99)+1]))
}

getdivcol = function(column){
	modcol = column
	modcol[t2$incity == F] = NA
	m = max(abs(modcol), na.rm = T)
	return(divcolmap(modcol, m, divcolpal))
}

getdivcolarea = function(column){
	modcol = column / t2$area
	modcol[t2$incity == F] = NA
	m = max(abs(modcol), na.rm = T)
	return(divcolmap(modcol, m, divcolpal))
}

getdivcolpop = function(column){
	modcol = column / t2$pop
	modcol[t2$pop <500] = NA
	modcol[t2$incity == F] = NA
	m = max(abs(modcol), na.rm = T)
	return(divcolmap(modcol, m, divcolpal))
}

#Plot

#Total Expenditures
pdf('scripts/analysis/output/expenditure_maps/totalexp.pdf')
plot(t2,col = getcol(t2$totexp))
title(main = "Total Expenditures by Tract, FY 2014")
dev.off()

pdf('scripts/analysis/output/expenditure_maps/exppp.pdf')
plot(t2,col = getcolpop(t2$totexp))
title(main = "Per Capita Expenditures by Tract, FY 2014")
dev.off()

pdf('scripts/analysis/output/expenditure_maps/exparea.pdf')
plot(t2,col = getcolarea(t2$totexp))
title(main = "Density of Expenditures, FY2014")
dev.off()

#Police costs are about 1 mil/tract
#libraries are each 1 mil
#Parks are each 200k
#Fire is around 500k but highly skewed
#MATA is around 100k but highly skewed
#Public works are about $100k and not that skewed

#Police is driving the high per area tracts


#Fire
pdf('scripts/analysis/output/expenditure_maps/firetot.pdf')
plot(t2,col = getcol(t2$fire))
title(main = "Fire Services Expenditures by Tract, FY 2014")
dev.off()

pdf('scripts/analysis/output/expenditure_maps/firepp.pdf')
plot(t2,col = getcolpop(t2$fire))
title(main = "Per Capita Fire Services Expenditures by Tract, FY 2014")
dev.off()

pdf('scripts/analysis/output/expenditure_maps/firearea.pdf')
plot(t2,col = getcolarea(t2$fire))
title(main = "Density of Fire Services Expenditures, FY2014")
dev.off()

#Public works
pdf('scripts/analysis/output/expenditure_maps/pubtot.pdf')
plot(t2,col = getcol(t2$pubworks))
title(main = "Public Works Expenditures by Tract, FY 2014")
dev.off()

pdf('scripts/analysis/output/expenditure_maps/pubpp.pdf')
plot(t2,col = getcolpop(t2$pubworks))
title(main = "Per Capita Public Works Expenditures by Tract, FY 2014")
dev.off()

pdf('scripts/analysis/output/expenditure_maps/pubarea.pdf')
plot(t2,col = getcolarea(t2$pubworks))
title(main = "Density of Public Works Expenditures, FY2014")
dev.off()

#Police
pdf('scripts/analysis/output/expenditure_maps/policetot.pdf')
plot(t2,col = getcol(t2$polcost))
title(main = "Police Services Expenditures by Tract, FY 2014")
dev.off()

pdf('scripts/analysis/output/expenditure_maps/policepp.pdf')
plot(t2,col = getcolpop(t2$polcost))
title(main = "Per Capita Police Services Expenditures by Tract, FY 2014")
dev.off()

pdf('scripts/analysis/output/expenditure_maps/policearea.pdf')
plot(t2,col = getcolarea(t2$polcost))
title(main = "Density of Police Services Expenditures, FY2014")
#legend(x = 'topleft',legend = c(max(t2$polcost)))
dev.off()

#MATA
pdf('scripts/analysis/output/expenditure_maps/matatot.pdf')
plot(t2,col = getcol(t2$mata))
title(main = "MATA Expenditures by Tract, FY 2014")
dev.off()

pdf('scripts/analysis/output/expenditure_maps/matapp.pdf')
plot(t2,col = getcolpop(t2$mata))
title(main = "Per Capita MATA Expenditures by Tract, FY 2014")
dev.off()

pdf('scripts/analysis/output/expenditure_maps/mataarea.pdf')
plot(t2,col = getcolarea(t2$mata))
title(main = "Density of MATA Expenditures, FY2014")
dev.off()

#Taxes
pdf('scripts/analysis/output/expenditure_maps/taxestot.pdf')
plot(t2,col = getcol(t2$taxes))
title(main = "Tax Revenues by Tract, FY 2014")
dev.off()

pdf('scripts/analysis/output/expenditure_maps/taxespp.pdf')
plot(t2,col = getcolpop(t2$taxes))
title(main = "Per Capita Tax Revenues by Tract, FY 2014")
dev.off()

pdf('scripts/analysis/output/expenditure_maps/taxesarea.pdf')
plot(t2,col = getcolarea(t2$taxes))
title(main = "Density of Tax Revenues, FY2014")
dev.off()

#Net
pdf('scripts/analysis/output/expenditure_maps/nettot.pdf')
plot(t2,col = getdivcol(t2$net))
title(main = "Tax Revenues by Tract, FY 2014")
dev.off()

pdf('scripts/analysis/output/expenditure_maps/netpp.pdf')
plot(t2,col = getdivcolpop(t2$net))
title(main = "Per Capita Net Revenues by Tract, FY 2014")
dev.off()

pdf('scripts/analysis/output/expenditure_maps/netarea.pdf')
plot(t2,col = getdivcolarea(t2$net))
title(main = "Density of Net Revenues, FY2014")
dev.off()

#Net Norm 1
pdf('scripts/analysis/output/expenditure_maps/netdifpaidtot.pdf')
plot(t2,col = getdivcol(t2$net_difpaid))
title(main = "Net Revenues by Tract (Normalized), FY 2014")
dev.off()

pdf('scripts/analysis/output/expenditure_maps/netdifpaidpp.pdf')
plot(t2,col = getdivcolpop(t2$net_difpaid))
title(main = "Per Capita Net Revenues by Tract (Normalized), FY 2014")
dev.off()

pdf('scripts/analysis/output/expenditure_maps/netdifpaidarea.pdf')
plot(t2,col = getdivcolarea(t2$net_difpaid))
title(main = "Density of Net Revenues (Normalized), FY2014")
dev.off()



##### Notes #####
#Do a spatial join of centroids of tracts to police boundaries to get police costs. Then each precinct has 145 officers except south main w 175. Divvy up police expenditures by that.

#Calculate the area of each tract and use that to divvy up fire.

#Main budget categories http://www.memphistn.gov/Portals/0/pdf_forms/fy2014_adopted_op/GeneralFundSummary.pdf
#Fire - 155672508
#Police - 234055868
#Grants and agencies - 69 million, of which 20221000 is to MATA and 29 mil to pensioners  
#Parks and Neighborhoods - 47647646
	#of which libraries 17566268
	#Golf 4399487
	#recreation 8854551 - senior, athletics, tennics, aquatics, community centers 
	#zoo 2535910
	#park facilities 3280350
	#Animal shelter 2.5 mil
	#Fairgrounds 2 mil
#Public works - 21791144
	#of which street maintenance 6285233
	#street lighting 6003367
	#neighborhood improvements = cleaning, litter, weeds, housing violations 8274374

#Total here is 479388166, just under 80%

#Final categories
	#Fire - 155672508
	#Police - 234055868
	#Libraries - 17566268
	#MATA - 20221000
	#Public Works - 21791144
	#Parks - 30081378



