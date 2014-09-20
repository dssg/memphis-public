ALTER TABLE nbn.panda
ADD blight int NOT NULL DEFAULT(1);

DROP TABLE IF EXISTS feature_sel;

CREATE TABLE feature_sel AS
select 	par.parcelid, COALESCE(aud.blight,0), 
	ST_X(st_transform(centroid, 4269))as longitude, ST_Y(st_transform(centroid,4269)) as latitude, rtotasmt, 
	asmt.aprland, asmt.aprbldg, asmt.rtotapr as rtotapr, asmt.class as class, asmt.luc as luc, land.sf as sfland,
	dd.yrblt as yrblt, dd.sfla as sqft, dd.user1 as ownocc, dd.extwall, dd.style, dd.stories, dd.rmtot, dd.rmfam, dd.rmbed,
	dd.fixbath, dd.fixhalf, dd.fixaddl, dd.bsmt, dd.heat, dd.fuel, dd.heatsys, dd.intext, dd.wbfp_o, dd.wbfp_s, dd.wbfp_pf, dd.bsmtcar,
	dd.grade, dd.cdu, dd.cond, dd.rooftype, dd.roofcover, own.own1 "owner", own.statecode ownstate
FROM parcels.parcels2008 par
LEFT JOIN nbn.panda aud USING (parcelid)
LEFT JOIN tax2008_asmt asmt on (parid = par.parcelid)
left join tax2008_land land on land.parid = par.parcelid
LEFT JOIN tax2008_dweldat dd on (dd.parid = par.parcelid)
left join tax2008_owndat own on own.parid = par.parcelid

ALTER TABLE feature_sel
RENAME COLUMN coalesce TO is_blight;