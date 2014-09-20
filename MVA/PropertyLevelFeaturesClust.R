 
# This code downloads and cleans the data from the database implements various 
# clustering algorithms, assess them. Labels the data and biulds a classifier with a 
# Random forest. Then it plots the results. 

# Download libraries
library(clValid)
library(kohonen)
library(MASS)
library(randomForest)

# Connect to the database
drv <- dbDriver("PostgreSQL");
con <- dbConnect(drv,
                 user= username,               
                 password=*******,
                 dbname="dbname",
                 host="hostname");

# Download the data:
  query <- dbSendQuery(con, "SELECT * FROM mva.demographic_features")
  data <- fetch(query,n=-1)
  data <- as.data.frame(data[,1:5])

# Fix NAs for the data from sales tax. Theres no sales data for those tracts where there is less than 4 business. 

x <-which(is.na(data$perc_employed))   
data <- data[-x,]
x <-which(is.na(data$perc_below_povline))   
data <- data[-x,]
x <-which(is.na(data$median_family_income))   
data <- data[-x,]

# Select variable names, clusters and validates the data: 
dd <- data[,c("prop_tax_per_parcel", "perc_blight",  "perc_fc",
              "walkscore_avg",	"resid_value_2008",	"comm_value_2008")]

internal  <- clValid(dd, 3:5, clMethods=c("kmeans", "model", "pam", "clara"), validation ="internal")
stability <- clValid(dd, 3:5, clMethods=c("kmeans", "model", "pam", "clara"), validation ="stability")
summary(internal)
summary(stability)
plot(internal)
plot(stability)

op <- par(no.readonly = TRUE)
par(mfrow = c(1, 1), mar=c(3, 2, 2, 1))
plot(internal, legend = FALSE)
plot(nClusters(internal), measures(internal, "Dunn")[, , 1], type = "n", axes = F, xlab = "", ylab = "")
legend("bottom", clusterMethods(internal), col = 1:5, lty=1:9, y.intersp=0.5, bty="n", hor=T)
par(op)

op <- par(no.readonly = TRUE)
par(mfrow = c(1, 4), mar=c(3, 2, 2, 1))
plot(stability, measures=c("APN","AD","ADM"), legend = FALSE)
plot(nClusters(stability), measures(stability,"APN")[, , 1], type = "n", axes = F, xlab = "", ylab = "")
legend("bottom", clusterMethods(stability), col = 1:9, lty=1:9, y.intersp=0.5, bty="n",horiz=F)
par(op)

# store the output in a data frame object.
outcome_pam_3 <- internal@clusterObjs$pam$`3`
outcome_pam_3$clustering
outcome_pam_3$clusinfo

outcome_pam_5 <- internal@clusterObjs$pam$`5`
outcome_pam_5$clustering
outcome_pam_5$clusinfo

clusters <- data
clusters$pam_3 <- as.factor(outcome_pam_3$clustering)
clusters$pam_5 <- as.factor(outcome_pam_5$clustering)

# write the output in the database
query <- dbSendQuery(con, "DROP TABLE IF EXISTS pam_clusters")
dbWriteTable(con,"pam_clusters", clusters)

#  Tis part of the codes builds a RF and and SVM to label the data. 
install.packages( 'e1071' )
library( 'e1071' )

drv <- dbDriver("PostgreSQL");
con <- dbConnect(drv,
                 user="memphis",
                 password="P4c3l10",
                 dbname="memphis",
                 host="dssgsummer2014postgres.c5faqozfo86k.us-west-2.rds.amazonaws.com");

query <- dbSendQuery(con, "SELECT a.tractid,
                     a.prop_tax_per_parcel, 
                     a.perc_blight,  a.perc_fc, a.perc_vac_housing,
                     a.mean_age, a.walkscore_avg, a.resid_value_2008, a.comm_value_2008,
                     b.perc_employed, b.perc_below_povline,
                     b.perc_incomeover_100,
                     b.median_family_income, 
                     b.pam_3, b.pam_5
                     FROM mva.cl_summary a left join pam_clusters b using(tractid)
                     WHERE a.tractid IS NOT NULL")

other_data <- fetch(query,n=-1)

x <-which(is.na(other_data$mean_age))     
other_data <- other_data[-x,]
y <-which(is.na(other_data$perc_fc))     
other_data <- other_data[-y,]
other_data$pam_3 <-  as.factor(other_data$pam_3)
other_data$pam_5 <-  as.factor(other_data$pam_5)

# This piece of code could work if instead of a multiple classifier we decide to build a binary meta classifier
for (i in 1:nrow(other_data)){
if (!is.na(other_data$pam_3[i]) && other_data$kmeans_3[i]==1){ 
    other_data$binkm3_1[i] <-1
    }else {other_data$binkm3_1[i] <-0} 
if (!is.na(other_data$kmeans_3[i]) && other_data$kmeans_3[i]==2){ 
    other_data$binkm3_2[i] <-2
    }else {other_data$binkm3_2[i] <-0} 
if (!is.na(other_data$kmeans_3[i]) && other_data$kmeans_3[i]==3){ 
    other_data$binkm3_3[i] <-3
    }else {other_data$binkm3_3[i] <-0} 
    }

z <-which(is.na(other_data$pam_5))     
train <- other_data[-z,]
test <-  other_data[z,]
  
a <- train[,c("prop_tax_per_parcel", "perc_blight",  "perc_fc", "perc_vac_housing",
                "mean_age",  "walkscore_avg",  "resid_value_2008",	"comm_value_2008")]

#### predictors: 

svm3 <- svm(a, train$pam_3, type="C-classification")
predict_svm <- as.data.frame(predict(svm3))

# RF predictions: 

b <- c("pam_3","prop_tax_per_parcel", "perc_blight",  "perc_fc", "perc_vac_housing",
       "mean_age",  "walkscore_avg",  "resid_value_2008",  "comm_value_2008")

rf3 <- randomForest(pam_3~., data=train[,b], proximity=F, nodesize=5, do.trace=20, 
                   ntree=100000)
b_prime <- c("prop_tax_per_parcel", "perc_blight",  "perc_fc", "perc_vac_housing",
             "mean_age",  "walkscore_avg",  "resid_value_2008",  "comm_value_2008")
predictions<- as.data.frame(predict(rf3, newdata=test[,b_prime], type="response"))
colnames(predictions) <- c("predicted")
my_test_data <- merge(test, predictions,by=c("row.names"), all=TRUE)
my_test_data$Row.names <- NULL
train$predicted <- train$pam_3

# Append data sets: 
install.packages("gtools")
library(gtools)
mydata <- smartbind(my_test_data, train)
rownames(mydata) <- mydata$tractid

query <- dbSendQuery(con, "DROP TABLE IF EXISTS complete_clusters")
dbWriteTable(con,"complete_clusters", mydata)


# ------------------------------------------------------------------------------------------ #
# Now the plot: 
install.packages("ggplot2")
library(ggplot2)

c <- c(
"prop_tax_per_parcel",  
"perc_blight",
"perc_vac_housing", 
"resid_value_2008", 
"comm_value_2008")

d <- c("perc_employed", 
"perc_below_povline", 
"perc_incomeover_100", 
"median_family_income")

colors <- ifelse(test = mydata$predicted=='1', yes = "tomato", 
                 ifelse(test = mydata$predicted=='2', yes = "slateblue2",                  
                 no = "turquoise"))
pairs(mydata[,c],col=colors, pch=19)
pairs(mydata[,d],col=colors, pch=19)


# ------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------ #

ggplot(mydata[,c], aes(x= mydata$prop_tax_per_parcel, fill=mydata$predicted)) +
  geom_density(alpha = 0.5, na.rm = T)+xlab("Average tax per property") + 
  scale_fill_discrete(guide=FALSE)
ggplot(mydata[,c], aes(x= mydata$perc_vac_housing, fill=mydata$predicted)) +
  geom_density(alpha = 0.5, na.rm = T)+xlab("% of vacant housing") + 
  scale_fill_discrete(guide=FALSE)
ggplot(mydata[,c], aes(x= mydata$resid_value_2008, fill=mydata$predicted)) +
  geom_density(alpha = 0.5, na.rm = T)+xlab("Residential value per sq ft.") + 
  scale_fill_discrete(guide=FALSE)
ggplot(mydata[,c], aes(x= mydata$comm_value_2008, fill=mydata$predicted)) +
  geom_density(alpha = 0.5, na.rm = T)+xlab("Commercial value per sq ft.") + 
  scale_fill_discrete(guide=FALSE)
ggplot(mydata[,c], aes(x= mydata$perc_employed, fill=mydata$predicted)) +
  geom_density(alpha = 0.5, na.rm = T)+xlab("% Employment") + 
  scale_fill_discrete(guide=FALSE)
ggplot(mydata[,c], aes(x= mydata$perc_below_povline, fill=mydata$predicted)) +
  geom_density(alpha = 0.5, na.rm = T)+xlab("% Families below the Poverty Line") + 
  scale_fill_discrete(guide=FALSE)
ggplot(mydata[,c], aes(x= mydata$perc_incomeover_100, fill=mydata$predicted)) +
  geom_density(alpha = 0.5, na.rm = T)+xlab("% Families with anual income over 100k") + 
  scale_fill_discrete(guide=FALSE)
ggplot(mydata[,c], aes(x= mydata$median_family_income, fill=mydata$predicted)) +
  geom_density(alpha = 0.5, na.rm = T)+xlab("Median family income") + 
  scale_fill_discrete(guide=FALSE)

