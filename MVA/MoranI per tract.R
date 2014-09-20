# This code estimates the autorrelation of property assessed vale per tract.

library(ape)

drv <- dbDriver("PostgreSQL");
con <- dbConnect(drv,
                 user= username,               
                 password=*******,
                 dbname="dbname",
                 host="hostname");

# Convert degrees to radians 
deg2rad <- function(deg) 
  return(deg*pi/180) 

# Calculates the geodesic distance between two points specified by 
# radian latitude/longitude using the Haversine formula 

gcd.hf <- function(long1, lat1, long2, lat2) {     
  R <- 6371 # Earth mean radius [km]   
  delta.long <- (long2 - long1) 
  delta.lat <- (lat2 - lat1) 
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2 
  c <- 2 * asin(min(1,sqrt(a))) 
  d = R * c 
  return(d) # Distance in km 
} 

query <- dbSendQuery(con, "SELECT a.tractid, a.parcelid, b.rtotasmt, c.longitude, c.latitude FROM tax2008_asmt b
                     LEFT JOIN parcels.parcels2008 a ON(a.parcelid=b.parid) 
                     LEFT JOIN feature_sel c ON (a.parcelid=c.parcelid) ")

data <- fetch(query,n=-1)

query <- dbSendQuery(con, "SELECT tractid FROM mva.cl_summary") 

tractid <- fetch(query,n=-1)
tractid <- tractid[1:221,1]

a <- which(is.na(data$rtotasmt))    
b <- which(data$rtotasmt==0)
# Drop null values 
data <- data[-a,]
data <- data[-b,]


moranI_tract <- data.frame(matrix(NA, nrow = 221, ncol = 4))
row.names(moranI_tract) <- tractid


for(k in tractid){
  b <- which(data$tractid==k)    
  sub_data <- data[b,]  
  n=nrow(sub_data)    
  longGrad1<-as.data.frame(deg2rad(sub_data$longitude))
  latGrad1<-as.data.frame(deg2rad(sub_data$latitude))
  distance <- data.frame(matrix(NA, nrow = n, ncol = n))
  
  for (i in 1:n){
    for (j in 1:n){
      if (longGrad1[i,] == longGrad1[j,] | latGrad1[i,] == latGrad1[j,] ) { 
        distance[j,i] <- 0
      } else {               
        distance[j,i] <- gcd.hf(longGrad1[i,],latGrad1[i,],longGrad1[j,],latGrad1[j,])  
      }    
    }  
  }  

  invDistance <- 1/distance
  invDistance[invDistance==Inf] <- 1
  gravityDistance <- 1/(distance^2)
  gravityDistance[gravityDistance==Inf] <- 1
  moranI <- Moran.I(sub_data$rtotasmt,invDistance)
  
  moranI_tract[k,1] <- k
  moranI_tract[k,2] <- moranI$observed
  moranI_tract[k,3] <- moranI$expected
  moranI_tract[k,4] <- moranI$p.value

  rm(sub_data)
  
  
}



