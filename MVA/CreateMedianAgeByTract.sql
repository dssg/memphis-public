DROP TABLE  IF EXISTS  mean_bldgage ;
CREATE TABLE mean_bldgage AS
SELECT tractid, median(parage) AS median_age
FROM parcel_age
group by tractid

CREATE TABLE  mean_bldgage_withgeom AS
SELECT tractid, median_age, the_geom AS tract_geom
FROM mean_bldgage 
LEFT OUTER JOIN census2010_sf1dp1_withgeom 
ON mean_bldgage.tractid=census2010_sf1dp1_withgeom.geo_id2


