
# install.packages("clValid")
install.packages("kohonen")
library(clValid)
library(kohonen)

drv <- dbDriver("PostgreSQL");
con <- dbConnect(drv,
                 user= username,               
                 password=*******,
                 dbname="dbname",
                 host="hostname");

clist <- c("2008","2009","2010","2011","2012","2013")
for (i in clist) {
  query <- dbSendQuery(con, paste("SELECT * FROM mva.features_",i, sep=""))
  assign(paste("data_",i, sep=""),fetch(query,n=-1))
}

query <- dbSendQuery(con,"SELECT * FROM mva.static_features")
data_static <- fetch(query,n=-1)
data_static[is.na(data_static)] <- 0
d_stat <- data_static[,c("perc_blight", "perc_vac_housing", "perc_houstenure_owner")]



# Fix NAs for the data from sales tax. Theres no sales data for those tracts where there is less than 4 business. 
data_2008[is.na(data_2008)] <- 0

data_2008$sales_business <- data_2008$sales_2008/data_2008$freq_2008
data_2008$sales_business[data_2008$sales_business == "NaN"] <- 0

data_2008$tax_business <- data_2008$tax_2008/data_2008$freq_2008
data_2008$tax_business[data_2008$tax_business == "NaN"] <- 0
data_2008$tax_sales <- data_2008$tax_2008/data_2008$sales_2008
data_2008$tax_sales[data_2008$tax_sales == "NaN"] <- 0

----------------------------------------------------------------------------------------
data_2009[is.na(data_2009)] <- 0
data_2009$sales_business <- data_2009$sales_2009/data_2009$freq_2009
data_2009$sales_business[data_2009$sales_business == "NaN"] <- 0
data_2009$tax_business <- data_2009$tax_2009/data_2009$freq_2009
data_2009$tax_business[data_2009$tax_business == "NaN"] <- 0
data_2009$tax_sales <- data_2009$tax_2009/data_2009$sales_2009
data_2009$tax_sales[data_2009$tax_sales == "NaN"] <- 0

----------------------------------------------------------------------------------------
data_2010[is.na(data_2010)] <- 0
data_2010$sales_business <- data_2010$sales_2010/data_2010$freq_2010
data_2010$sales_business[data_2010$sales_business == "NaN"] <- 0
data_2010$tax_business <- data_2010$tax_2010/data_2010$freq_2010
data_2010$tax_business[data_2010$tax_business == "NaN"] <- 0
data_2010$tax_sales <- data_2010$tax_2010/data_2010$sales_2010
data_2010$tax_sales[data_2010$tax_sales == "NaN"] <- 0

----------------------------------------------------------------------------------------
data_2011[is.na(data_2011)] <- 0
data_2011$sales_business <- data_2011$sales_2011/data_2011$freq_2011
data_2011$sales_business[data_2011$sales_business == "NaN"] <- 0
data_2011$tax_business <- data_2011$tax_2011/data_2011$freq_2011
data_2011$tax_business[data_2011$tax_business == "NaN"] <- 0
data_2011$tax_sales <- data_2011$tax_2011/data_2011$sales_2011
data_2011$tax_sales[data_2011$tax_sales == "NaN"] <- 0

----------------------------------------------------------------------------------------
data_2012[is.na(data_2012)] <- 0
data_2012$sales_business <- data_2012$sales_2012/data_2012$freq_2012
data_2012$sales_business[data_2012$sales_business == "NaN"] <- 0
data_2012$tax_business <- data_2012$tax_2012/data_2012$freq_2012
data_2012$tax_business[data_2012$tax_business == "NaN"] <- 0
data_2012$tax_sales <- data_2012$tax_2012/data_2012$sales_2012
data_2012$tax_sales[data_2012$tax_sales == "NaN"] <- 0

----------------------------------------------------------------------------------------
data_2013[is.na(data_2013)] <- 0
data_2013$sales_business <- data_2013$sales_2013/data_2013$freq_2013
data_2013$sales_business[data_2013$sales_business == "NaN"] <- 0

data_2013$tax_business <- data_2013$tax_2013/data_2013$freq_2013
data_2013$tax_business[data_2013$tax_business == "NaN"] <- 0

data_2013$tax_sales <- data_2013$tax_2013/data_2013$sales_2013
data_2013$tax_sales[data_2013$tax_sales == "NaN"] <- 0

# ----------------------------------------------------------------------------------------
# Cluster estimation and validation
  
d_08 <- data_2008[,c("sales_business","tax_business", "value_ratio_2008", "mean_valuesqft_2008")]
d_09 <- data_2009[,c("sales_business","tax_business", "value_ratio_2009", "mean_valuesqft_2009")]
d_10 <- data_2010[,c("sales_business","tax_business", "value_ratio_2010", "mean_valuesqft_2010")]
d_11 <- data_2011[,c("sales_business","tax_business", "value_ratio_2011", "mean_valuesqft_2011")]
d_12 <- data_2012[,c("sales_business","tax_business", "value_ratio_2012", "mean_valuesqft_2012")]
d_13 <- data_2013[,c("sales_business","tax_business", "value_ratio_2013", "mean_valuesqft_2013")]

# --------------------------------------------------------------------------------------------
# 2008 Analysis:
internal_08  <- clValid(d_08, 2:6, clMethods=c("kmeans", "hierarchical", "model","diana"), validation ="internal")
stability_08 <- clValid(d_08, 2:6, clMethods=c("kmeans", "hierarchical", "model","diana"), validation ="stability")

summary(internal_08)
summary(stability_08)


outcome_08_kmeans_6 <- internal_08@clusterObjs$kmeans$`6`

centers_08 <- as.data.frame(outcome_08_kmeans_6$centers)
centers_08$clustersize <- as.data.frame(outcome_08_kmeans_6$size)
centers_08

plot(internal_08)
plot(stability_08)

# Store clusters is a data frame. 
clusters_2008 <- data.frame(matrix(NA, nrow = 221, ncol = 3))
clusters_2008[,1] <- data_2008$tractid
clusters_2008[,2] <- outcome_08_kmeans_6$cluster
colnames(clusters_2008) <- c("tractid","original_cluster","cluster_2008_code")

# Remane clusters
# high sales and high value ratio = A. 
# (residential properties are waaaaaay more expensive than comercial (i dont get this but...)
clusters_2008$cluster_2008_code[clusters_2008$original_cluster==1] <- "A"
# normal sales and tax, commercial prop not too expensive = B
clusters_2008$cluster_2008_code[clusters_2008$original_cluster==2] <- "B"
# extreely high sales per business with extremely average value per sq ft. debe ser un suburbio con pocos negocios 
# o algo mal en el tax sales
# es el tract id=47157005300
clusters_2008$cluster_2008_code[clusters_2008$original_cluster==3] <- "c"
# high sales per business and a little higer residential prices than commercial ones.
clusters_2008$cluster_2008_code[clusters_2008$original_cluster==4] <- "D"
# high sales per business and high commercial value, but low property value is just one.
# tractid =  47157022600
clusters_2008$cluster_2008_code[clusters_2008$original_cluster==5] <- "E"
# Extremely low value per sq ft.
# tractid =  47157980200
clusters_2008$cluster_2008_code[clusters_2008$original_cluster==6] <- "F"

# Write the tables to postgres:
dbWriteTable(con,"clusters_2008", clusters_2008)

# --------------------------------------------------------------------------------------------
# 2009 Analysis:
internal_09  <- clValid(d_09, 2:6, clMethods=c("kmeans", "hierarchical", "model","diana"), validation ="internal")
stability_09 <- clValid(d_09, 2:6, clMethods=c("kmeans", "hierarchical", "model","diana"), validation ="stability")

summary(internal_09)
summary(stability_09)

outcome_09_kmeans_6 <- internal_09@clusterObjs$kmeans$`6`

centers_09 <- as.data.frame(outcome_09_kmeans_6$centers)
centers_09$clustersize <- as.data.frame(outcome_09_kmeans_6$size)
centers_09

plot(internal_09)
plot(stability_09)

# Store clusters is a data frame. 
clusters_2009 <- data.frame(matrix(NA, nrow = 221, ncol = 3))
clusters_2009[,1] <- data_2009$tractid
clusters_2009[,2] <- outcome_09_kmeans_6$cluster
colnames(clusters_2009) <- c("tractid","original_cluster","cluster_2009_code")

# Remane clusters
# high sales and high value ratio = A. 
# (residential properties are waaaaaay more expensive than comercial (i dont get this but...)
clusters_2009$cluster_2009_code[clusters_2009$original_cluster==5] <- "A"
# normal sales and tax, commercial prop not too expensive = B
clusters_2009$cluster_2009_code[clusters_2009$original_cluster==3] <- "B"
# extreely high sales per business with extremely average value per sq ft. debe ser un suburbio con pocos negocios 
# o algo mal en el tax sales
# es el tract id=47157005300
clusters_2009$cluster_2009_code[clusters_2009$original_cluster==4] <- "c"
# high sales per business and a little higer residential prices than commercial ones.
clusters_2009$cluster_2009_code[clusters_2009$original_cluster==1] <- "D"
# high sales per business and high commercial value, but low property value is just one.
# tractid =  47157022600
clusters_2009$cluster_2009_code[clusters_2009$original_cluster==6] <- "E"
# Extremely low value per sq ft.
# tractid =  47157980200
clusters_2009$cluster_2009_code[clusters_2009$original_cluster==2] <- "F"

# Write the tables to postgres:
dbWriteTable(con,"clusters_2009", clusters_2009)


# --------------------------------------------------------------------------------------------
# 2010 Analysis:
internal_10  <- clValid(d_10, 2:6, clMethods=c("kmeans", "hierarchical", "model","diana"), validation ="internal")
stability_10 <- clValid(d_10, 2:6, clMethods=c("kmeans", "hierarchical", "model","diana"), validation ="stability")

summary(internal_10)
summary(stability_10)

outcome_10_kmeans_6 <- internal_10@clusterObjs$kmeans$`6`

centers_10 <- as.data.frame(outcome_10_kmeans_6$centers)
centers_10$clustersize <- as.data.frame(outcome_10_kmeans_6$size)
centers_10

plot(internal_10)
plot(stability_10)

# Store clusters is a data frame. 
clusters_2010 <- data.frame(matrix(NA, nrow = 221, ncol = 3))
clusters_2010[,1] <- data_2010$tractid
clusters_2010[,2] <- outcome_10_kmeans_6$cluster
colnames(clusters_2010) <- c("tractid","original_cluster","cluster_2010_code")

# Remane clusters
# high sales and high value ratio = A. 
# (residential properties are waaaaaay more expensive than comercial (i dont get this but...)
clusters_2010$cluster_2010_code[clusters_2010$original_cluster==5] <- "A"
# normal sales and tax, commercial prop not too expensive = B
clusters_2010$cluster_2010_code[clusters_2010$original_cluster==3] <- "B"
# extreely high sales per business with extremely average value per sq ft. debe ser un suburbio con pocos negocios 
# o algo mal en el tax sales
# es el tract id=47157005300
clusters_2010$cluster_2010_code[clusters_2010$original_cluster==4] <- "c"
# high sales per business and a little higer residential prices than commercial ones.
clusters_2010$cluster_2010_code[clusters_2010$original_cluster==1] <- "D"
# high sales per business and high commercial value, but low property value is just one.
# tractid =  47157022600
clusters_2010$cluster_2010_code[clusters_2010$original_cluster==6] <- "E"
# Extremely low value per sq ft.
clusters_2010$cluster_2010_code[clusters_2010$original_cluster==2] <- "F"

# Write the tables to postgres:
dbWriteTable(con,"clusters_2010", clusters_2010)


# --------------------------------------------------------------------------------------------
# 2011 Analysis:
internal_11  <- clValid(d_11, 2:6, clMethods=c("kmeans", "hierarchical", "model","diana"), validation ="internal")
stability_11 <- clValid(d_11, 2:6, clMethods=c("kmeans", "hierarchical", "model","diana"), validation ="stability")

summary(internal_11)
summary(stability_11)

outcome_11_kmeans_6 <- internal_11@clusterObjs$kmeans$`6`

centers_11 <- as.data.frame(outcome_11_kmeans_6$centers)
centers_11$clustersize <- as.data.frame(outcome_11_kmeans_6$size)
centers_11

plot(internal_11)
plot(stability_11)

# Store clusters is a data frame. 
clusters_2011 <- data.frame(matrix(NA, nrow = 221, ncol = 3))
clusters_2011[,1] <- data_2011$tractid
clusters_2011[,2] <- outcome_11_kmeans_6$cluster
colnames(clusters_2011) <- c("tractid","original_cluster","cluster_2011_code")

# Remane clusters
# high sales and high value ratio = A. 
# (residential properties are waaaaaay more expensive than comercial (i dont get this but...)
clusters_2011$cluster_2011_code[clusters_2011$original_cluster==1] <- "A"
# normal sales and tax, commercial prop not oo expensive = B
clusters_2011$cluster_2011_code[clusters_2011$original_cluster==3] <- "B"
# extreely high sales per business with extremely average value per sq ft. debe ser un suburbio con pocos negocios 
# o algo mal en el tax sales
# es el tract id=47157005300
clusters_2011$cluster_2011_code[clusters_2011$original_cluster==5] <- "c"
# high sales per business and a little higer residential prices than commercial ones.
clusters_2011$cluster_2011_code[clusters_2011$original_cluster==4] <- "D"
# high sales per business and high commercial value, but low property value is just one.
# tractid =  47157022600
clusters_2011$cluster_2011_code[clusters_2011$original_cluster==2] <- "E"
# Extremely low value per sq ft.
clusters_2011$cluster_2011_code[clusters_2011$original_cluster==6] <- "F"

# Write the tables to postgres:
dbWriteTable(con,"clusters_2011", clusters_2011)


# --------------------------------------------------------------------------------------------
# 2012 Analysis:
internal_12  <- clValid(d_12, 2:6, clMethods=c("kmeans", "hierarchical", "model","diana"), validation ="internal")
stability_12 <- clValid(d_12, 2:6, clMethods=c("kmeans", "hierarchical", "model","diana"), validation ="stability")

summary(internal_12)
summary(stability_12)

outcome_12_kmeans_6 <- internal_12@clusterObjs$kmeans$`6`

centers_12 <- as.data.frame(outcome_12_kmeans_6$centers)
centers_12$clustersize <- as.data.frame(outcome_12_kmeans_6$size)
centers_12

plot(internal_12)
plot(stability_12)

# Store clusters is a data frame. 
clusters_2012 <- data.frame(matrix(NA, nrow = 221, ncol = 3))
clusters_2012[,1] <- data_2012$tractid
clusters_2012[,2] <- outcome_12_kmeans_6$cluster
colnames(clusters_2012) <- c("tractid","original_cluster","cluster_2012_code")

# Remane clusters
# high sales and high value ratio = A. 
# (residential properties are waaaaaay more expensive than comercial (i dont get this but...)
clusters_2012$cluster_2012_code[clusters_2012$original_cluster==1] <- "A"
# normal sales and tax, commercial prop not oo expensive = B
clusters_2012$cluster_2012_code[clusters_2012$original_cluster==2] <- "B"
# extreely high sales per business with extremely average value per sq ft. debe ser un suburbio con pocos negocios 
# o algo mal en el tax sales
# es el tract id=47157005300
clusters_2012$cluster_2012_code[clusters_2012$original_cluster==4] <- "c"
# high sales per business and a little higer residential prices than commercial ones.
clusters_2012$cluster_2012_code[clusters_2012$original_cluster==3] <- "D"
# high sales per business and high commercial value, but low property value is just one.
# tractid =  47157022600
clusters_2012$cluster_2012_code[clusters_2012$original_cluster==5] <- "E"
# Extremely low value per sq ft.
clusters_2012$cluster_2012_code[clusters_2012$original_cluster==6] <- "F"

# Write the tables to postgres:

dbWriteTable(con,"clusters_2012", clusters_2012)

# --------------------------------------------------------------------------------------------
# 2013 Analysis:
internal_13  <- clValid(d_13, 2:6, clMethods=c("kmeans", "hierarchical", "model","diana"), validation ="internal")
stability_13 <- clValid(d_13, 2:6, clMethods=c("kmeans", "hierarchical", "model","diana"), validation ="stability")

summary(internal_13)
summary(stability_13)

outcome_13_kmeans_6 <- internal_13@clusterObjs$kmeans$`6`

centers_13 <- as.data.frame(outcome_13_kmeans_6$centers)
centers_13$clustersize <- as.data.frame(outcome_13_kmeans_6$size)
centers_13

plot(internal_13)
plot(stability_13)

# Store clusters is a data frame. 
clusters_2013 <- data.frame(matrix(NA, nrow = 221, ncol = 3))
clusters_2013[,1] <- data_2013$tractid
clusters_2013[,2] <- outcome_13_kmeans_6$cluster
colnames(clusters_2013) <- c("tractid","original_cluster","cluster_2013_code")

# Remane clusters
# high sales and high value ratio = A. 
# (residential properties are waaaaaay more expensive than comercial (i dont get this but...)
clusters_2013$cluster_2013_code[clusters_2013$original_cluster==1] <- "A"
# normal sales and tax, commercial prop not oo expensive = B
clusters_2013$cluster_2013_code[clusters_2013$original_cluster==2] <- "B"
# extreely high sales per business with extremely average value per sq ft. debe ser un suburbio con pocos negocios 
# o algo mal en el tax sales
# es el tract id=47157005300
clusters_2013$cluster_2013_code[clusters_2013$original_cluster==4] <- "c"
# high sales per business and a little higer residential prices than commercial ones.
clusters_2013$cluster_2013_code[clusters_2013$original_cluster==3] <- "D"
# high sales per business and high commercial value, but low property value is just one.
# tractid =  47157022600
clusters_2013$cluster_2013_code[clusters_2013$original_cluster==6] <- "E"
# Extremely low value per sq ft.
clusters_2013$cluster_2013_code[clusters_2013$original_cluster==5] <- "F"

# Write the tables to postgres:
dbWriteTable(con,"clusters_2013", clusters_2013)

