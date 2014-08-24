# Pull down our features from the database
library(RPostgreSQL)
dbdrv <- dbDriver('PostgreSQL')
# run this over an ssh tunnel
con <- dbConnect(dbdrv, 'localhost', port='5431', user='-----', password='-----')

qfile = 'parcel-query-2008.sql'
query = readChar(qfile,file.info(qfile)$size)
data <- dbGetQuery(con, query)


# 2008 distressed truth based on CBANA NxN survey. Anything that was in NxN is considered distressed
data$distressed = rep('ok',length(data$strprob))
data$distressed[which(!is.na(data$strprob))] = 'distressed'
data$distressed = factor(data$blighted)

# 2013 blighted truth test for Frayser. Anything with a grade other than 'A' is considered distressed
# data$distressed = rep(NA,length(data$condgrade))
# data$distressed[which(data$condgrade=='A')] = 'ok'
# data$distressed[which(!is.na(data$condgrade) & data$condgrade!='A')] = 'distressed'
# data$distressed = factor(data$distressed)
# test = which(!is.na(data$distressed)) to get Frayser as test set

data$age = 2008-data$yrblt
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
data$popchg <- data$pop2010 / data$pop2000
data$popchg[data$pop2000 < 10] <- NA
data$popchg[which(is.infinite(data$popchg))] <- NA

# NA means not in the LODES data, which means 0
data$bluecollar[is.na(data$bluecollar)] <- 0
data$service[is.na(data$service)] <- 0
data$whitecollar[is.na(data$whitecollar)] <- 0
data$lowincomerate[is.na(data$lowincomerate)] <- 0
data$midincomerate[is.na(data$midincomerate)] <- 0
data$highincomerate[is.na(data$highincomerate)] <- 0

# Per AEDIT, cond is defined two ways, one by numbers and one by letters. They are the same.
data$cond[data$cond=='F'] <- 2 # 2: Fair
data$cond[data$cond=='A'] <- 3 # 3: Average
data$cond[data$cond=='G'] <- 4 # 4: Good
data$cond[data$cond=='E'] <- 5 # 5: Excellent
data$cond[data$cond=='P'] <- 1 # 1: Poor
data$cond[data$cond==''] <- NA
data$cond <- as.ordered(as.character(data$cond))

data$cdu[which(data$luc=='000')] = 'vac'
data$cdu = factor(data$cdu)

data$fc_rate_lag1 = data$fc1/data$parcels; data$fc_rate_lag1[which(is.na(data$fc_rate_lag1))]=0
data$fc_rate_lag2 = data$fc2/data$parcels; data$fc_rate_lag2[which(is.na(data$fc_rate_lag2))]=0
data$fc_rate_lag3 = data$fc3/data$parcels; data$fc_rate_lag3[which(is.na(data$fc_rate_lag3))]=0
data$fc_rate_lag4 = data$fc4/data$parcels; data$fc_rate_lag4[which(is.na(data$fc_rate_lag4))]=0

data$weeds[is.na(data$weeds)] = 0
data$sidewalks[is.na(data$sidewalks)] = 0
data$sanitation[is.na(data$sanitation)] = 0
data$neglect[is.na(data$neglect)] = 0
data$we_charge1[is.na(data$we_charge1)] = 0
data$latefee[is.na(data$latefee)] = 0
data$tb_bankrupt[is.na(data$tb_bankrupt)] = 0
data$tb_badcheck[is.na(data$tb_badcheck)] = 0
data$total_permit[is.na(data$total_permit)] = 0
data$unemployed[is.na(data$unemployed)] = 0
data$adrdir[is.na(data$adrdir)] = ''
data$adrsuf[is.na(data$adrsuf)] = ''
data$address = gsub('  ',' ',paste(data$adrno,data$adrdir,data$adrstr,data$adrsuf,data$zip1))

# Measure how the home has changed over the past four years
data$asmtchng = data$rtotasmt/data$rtotasmt4
data$asmtchng[which(is.na(data$asmtchng))] = 1
data$asmtchng[which(is.infinite(data$asmtchng))] = 2
data$rmchng = data$rmtot/data$rmtot4
data$rmchng[which(is.na(data$rmchng))] = 1
data$rmchng[which(is.infinite(data$rmchng))] = 2
data$sqftchng = data$sqft/data$sqft4
data$sqftchng[which(is.na(data$sqftchng))] = 1
data$sqftchng[which(is.infinite(data$sqftchng))] = 2

### remove NAs from vacant properties 
data$outofstate = factor(data$outofstate)
data$ownocc = factor(data$ownocc)
data$corp = factor(data$corp)
data$yrblt[which(data$luc=='000')] = mean(data$yrblt,na.rm=TRUE)
data$saleyear[which(data$luc=='000')] = mean(data$saleyear,na.rm=TRUE)
data$saleprice[which(data$luc=='000')] = 0
data$sqft[which(data$luc=='000')] = 0
data$sqft4[which(data$luc=='000')] = 0
data$stories[which(data$luc=='000')] = 0
data$rmtot[which(data$luc=='000')] = 0
data$rmtot4[which(data$luc=='000')] = 0

# reduce data to only include complete cases without NAs. The model will not run on samples with missing data.
cols = c('rtotapr', 'yrblt', 'outofstate', 'corp', 'sqft', 'no_utilities', 'aprland', 'cdu',
         'asmtchng', 'rmchng', 'sqftchng', 'total_permit', 'whitecollar', 'bluecollar',
         'service', 'lowincomerate', 'midincomerate', 'highincomerate', 'fc_rate_lag1', 
         'fc_rate_lag2', 'fc_rate_lag3', 'fc_rate_lag4', 'tract_gross_sales', 'weeds', 
         'we_charge1', 'latefee', 'tb_bankrupt', 'tb_badcheck', 'saleyear', 'saleprice')

set = 1:length(data$parcelid)
X = data[set,cols]
set = (as.numeric(row.names(X)))[which(complete.cases(X))]
data = data[set,]

# Save data to a file for later reference
save(data08, file='../data/data08.RData')
