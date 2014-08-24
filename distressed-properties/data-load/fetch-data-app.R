# This file fetches and cleans data to be displayed in the online application
# Repeat this process for each year predicted in the app, and save all datasets 
# at the end into one RData file.

# Connect to the database
library(RPostgreSQL)
dbdrv <- dbDriver('PostgreSQL')
con <- dbConnect(dbdrv, 'localhost', port='5431', user='-----', password='-----')

# Select the file with the desired SQL query and send it to the database
qfile = 'app-data-2013.sql'
query = readChar(qfile,file.info(qfile)$size)
data <- dbGetQuery(con, query)

# Clean the data to appear nicely and get rid of unnecessary columns 
data$rtotapr = paste('$',prettyNum(data$rtotapr,big.mark=',',preserve.width='none'),sep='')
data$sqft = prettyNum(data$sqft,big.mark=',',preserve.width='none')
data$sfland = prettyNum(data$sfland,big.mark=',',preserve.width='none')

data[is.na(data)] = ''
data$address = gsub('  ',' ',paste(data$adrno,data$adrdir,data$adrstr,data$adrsuf,data$zip1))
data$ownaddress = gsub('  ',' ',paste(data$oadrno,data$oadrdir,data$oadrstr,data$oadrsuf,data$ocity,data$ostate,data$ozip1))

data$risk = data$blightpred
data$blightpred = NULL
data$risk = as.numeric(data$risk)
data = data[order(data$risk, decreasing=TRUE),]

data$adrno = NULL
data$adrdir = NULL
data$adrstr = NULL
data$adrsuf = NULL
data$zip1 = NULL
data$oadrno = NULL
data$oadrdir = NULL
data$oadrstr = NULL
data$oadrsuf = NULL
data$ozip = NULL
data$ocity = NULL
data$ostate = NULL

data$cdu[which(data$cdu=='EX')] = "Excellent"
data$cdu[which(data$cdu=='VG')] = "Very Good"
data$cdu[which(data$cdu=='GD')] = "Good"
data$cdu[which(data$cdu=='AV')] = "Average"
data$cdu[which(data$cdu=='FR')] = "Fair"
data$cdu[which(data$cdu=='PR')] = "Poor"
data$cdu[which(data$cdu=='VP')] = "Very Poor"
data$cdu[which(data$cdu=='UN')] = "Unsound"

data$luc[which(data$luc=='000')] = "Vacant Land"
data$luc[which(data$luc=='001')] = "Accessory Imp"
data$luc[which(data$luc=='002')] = "Apartment Complex"
data$luc[which(data$luc=='008')] = "Religious"
data$luc[which(data$luc=='009')] = "Community Center"
data$luc[which(data$luc=='011')] = "Country Club"
data$luc[which(data$luc=='019')] = "Hangar"
data$luc[which(data$luc=='025')] = "Mobile Home Park"
data$luc[which(data$luc=='032')] = "Service Garage"
data$luc[which(data$luc=='034')] = "Store/Retail"
data$luc[which(data$luc=='035')] = "Store/Apartment"
data$luc[which(data$luc=='051')] = "Zero Lot Line"
data$luc[which(data$luc=='052')] = "Planning-Unit Development"
data$luc[which(data$luc=='057')] = "Condo Parking"
data$luc[which(data$luc=='058')] = "Condo Unit"
data$luc[which(data$luc=='059')] = "Duplex"
data$luc[which(data$luc=='060')] = "Condo Common"
data$luc[which(data$luc=='061')] = "Triplex"
data$luc[which(data$luc=='062')] = "Single Family"
data$luc[which(data$luc=='063')] = "Town House"
data$luc[which(data$luc=='064')] = "Mobile Home"
data$luc[which(data$luc=='067')] = "Apartment Garden"
data$luc[which(data$luc=='068')] = "Office Low"
data$luc[which(data$luc=='069')] = "Office High"
data$luc[which(data$luc=='073')] = "Utility/RR"
data$luc[which(data$luc=='078')] = "Bar/Lounge"
data$luc[which(data$luc=='082')] = "Day Care Center"
data$luc[which(data$luc=='093')] = "Warehouse Pre-Engineered"
data$luc[which(data$luc=='099')] = "Planned Unit Development Common"
data$luc[which(data$luc=='200')] = "Common Area Land"
data$luc[which(data$luc=='201')] = "Marina Parking"
data$luc[which(data$luc=='251')] = "Zero Lot Line-Attached"
data$luc[which(data$luc=='252')] = "Planned Unit Development Attached"
data$luc[which(data$luc=='255')] = "Cemetery"

# Pass the new data to another variable indicating the proper year
data13 = data

# Save all datasets into a single file for easy access from the app
# save(data11,data12,data13, file='../data/data.RData')
