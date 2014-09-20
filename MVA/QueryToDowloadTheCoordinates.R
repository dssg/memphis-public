library(RPostgreSQL)
dbdrv <- dbDriver('PostgreSQL')
# run this over an ssh tunnel

drv <- dbDriver("PostgreSQL");
con <- dbConnect(drv,
                 user= username,               
                 password=*******,
                 dbname="dbname",
                 host="hostname");


data <- dbGetQuery(con,'SELECT 
                        tiger2010_bg_geom.gid, 
                        tiger2010_bg_geom.intptlat10 infilelong, 
                        tiger2010_bg_geom.intptlon10 infilelat, 
                        ST_X(st_transform(ST_Centroid(the_geom), 4326))as mylongitude, 
                        ST_Y(st_transform(ST_Centroid(the_geom),4326)) as mylatitude,
                        tiger2010_bg_geom.the_geom
                        FROM 
                        public.tiger2010_bg_geom')


write.table(data, 'C:/Users/Alejandra/dssg_files/memphis/blockCoord.csv', quote = FALSE, sep = ",", row.names = T)
