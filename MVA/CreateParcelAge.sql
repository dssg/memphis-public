

CREATE TABLE parcel_age AS 
SELECT 		tax2013_oby.parid, 
		yrblt AS yearbuilt, 2014-yrblt AS parage, zoning, 
		parcels2013.centroid as par_centroid,
		parcels2013.the_geom as par_geom, 
		tractid
FROM tax2013_oby 
LEFT OUTER JOIN tax2013_pardat ON tax2013_oby.parid=tax2013_pardat.parid
LEFT OUTER JOIN parcels.parcels2013 ON parcels2013.parcelid=tax2013_oby.parid


