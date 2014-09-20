
CREATE TABLE Bdc_problems_withgeom AS
SELECT 
  bdcproblems."PARID", 
  bdcproblems."OWN1", 
  bdcproblems."ZONING", 
  bdcproblems.problem_code, 
  parcels2008.the_geom, 
  parcels2008.centroid
FROM 
  bdcproblems
LEFT OUTER JOIN parcels.parcels2008 ON (bdcproblems."PARID" = parcels2008.parcelid)
