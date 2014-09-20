
# install.packages("RPostgreSQL")
library(RPostgreSQL)

# Connecting to the database: 

drv <- dbDriver("PostgreSQL");
con <- dbConnect(drv,
                 user= username,               
                 password=*******,
                 dbname="dbname",
                 host="hostname");

query <- dbSendQuery(con,"SELECT * FROM asessment08_13_persf WHERE (class='R' AND appr_2008 > 5
                     AND appr_2009 > 5 AND appr_2010 > 5 AND appr_2011 > 5 AND appr_2012 > 5)")

# Upload data into a data frame. 

df <-fetch(query,n=-1)
data <- as.data.frame(cbind(df$is_blight,log(df$appr_2008),log(df$appr_2009),log(df$appr_2010),log(df$appr_2011),log(df$appr_2012),log(df$appr_2013)))
colnames(data) <- c("isBlight", "2008", "2009", "2010", "2011", "2012","2013")

# Sample some observations from the data set:

library(random)
n <- 1000
r <- randomNumbers(n, max = nrow(data), col = 1)
dataSample <- data[r,]

# Plot parallel coordinates:

library(MASS)
blight.colors<-ifelse(test = dataSample$isBlight==1, yes = "red", no = "blue")

# Line type by condition:
parcoord(dataSample,col=blight.colors, var.label=TRUE)
