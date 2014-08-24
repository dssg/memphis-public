SELECT par.parcelid, ST_X(ST_TRANSFORM(par.centroid,4326)) as lon, ST_Y(ST_TRANSFORM(par.centroid,4326)) as lat, 
asmt.rtotapr, asmt.luc as luc, ST_Area(the_geom) as sfland,
dd.yrblt as yrblt, dd.sfla as sqft, dd.stories, dd.rmtot, dd.cdu, 
pd.adrno, pd.adrdir, pd.adrstr, pd.adrsuf, pd.zip1,
od.own1 as ownername, od.adrno as oadrno, od.adrdir as oadrdir, od.adrstr as oadrstr, od.adrsuf as oadrsuf, od.zip1 as ozip, od.cityname as ocity, od.statecode as ostate,
bp.blightprob


FROM parcels.parcels2012 par
LEFT JOIN blightpred.results2012 bp ON (bp.parcelid=par.parcelid)
LEFT JOIN tax2012_asmt asmt  ON (asmt.parid = par.parcelid)
LEFT JOIN tax2012_pardat pd  ON (pd.parid = par.parcelid)
LEFT JOIN tax2012_owndat od  ON (od.parid = par.parcelid)
LEFT JOIN (SELECT *, ROW_NUMBER() OVER (PARTITION BY parid ORDER BY sfla DESC) rn FROM tax2012_dweldat) dd ON (dd.parid = par.parcelid AND dd.rn=1)
WHERE pd."class" = 'R' -- only residential
AND ST_Within(centroid, (SELECT ST_Union(the_geom) FROM muniboundaries_shelby WHERE name='Memphis'))
