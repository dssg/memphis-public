DROP TABLE appraisal_time_prime;
CREATE TABLE appraisal_time_prime AS
select 	DISTINCT parcelid, is_blight, 
	(asmt08.rtotapr/land08.sf) as appr_2008,
	(asmt09.rtotapr/land09.sf) as appr_2009,	
	(asmt10.rtotapr/land10.sf) as appr_2010,
	(asmt11.rtotapr/land11.sf) as appr_2011,
	(asmt12.rtotapr/land12.sf) as appr_2012,
	(asmt13.rtotapr/land13.sf) as appr_2013	
FROM feature_sel 
LEFT OUTER JOIN tax2008_asmt asmt08 on (parcelid = asmt08.parid)
LEFT OUTER JOIN tax2008_land land08 on (parcelid = land08.parid)
LEFT OUTER JOIN tax2009_asmt asmt09 on (parcelid = asmt09.parid)
LEFT OUTER JOIN tax2009_land land09 on (parcelid = land09.parid)
LEFT OUTER JOIN tax2010_asmt asmt10 on (parcelid = asmt10.parid)
LEFT OUTER JOIN tax2010_land land10 on (parcelid = land10.parid)
LEFT OUTER JOIN tax2011_asmt asmt11 on (parcelid = asmt11.parid)
LEFT OUTER JOIN tax2011_land land11 on (parcelid = land11.parid)
LEFT OUTER JOIN tax2012_asmt asmt12 on (parcelid = asmt12.parid)
LEFT OUTER JOIN tax2012_land land12 on (parcelid = land12.parid)
LEFT OUTER JOIN tax2013_asmt asmt13 on (parcelid = asmt13.parid)
LEFT OUTER JOIN tax2013_land land13 on (parcelid = land13.parid)
WHERE (land08.sf <> 0 
AND land09.sf <> 0 
AND land10.sf <> 0 
AND land11.sf <> 0
AND land12.sf <> 0 
AND land13.sf <> 0
);



DROP TABLE asessment08_13_persf;
CREATE TABLE asessment08_13_persf as
select 	DISTINCT parcelid, is_blight, feature_sel.class,
	asmt08.rtotapr/land08.sf AS appr_2008,  
	asmt09.rtotapr/land08.sf AS appr_2009,	
	asmt10.rtotapr/land08.sf as appr_2010,
	asmt11.rtotapr/land08.sf as appr_2011,
	asmt12.rtotapr/land08.sf as appr_2012,
	asmt13.rtotapr/land08.sf as appr_2013	
FROM feature_sel 
INNER JOIN tax2008_asmt asmt08 on (parcelid = asmt08.parid)
LEFT OUTER JOIN tax2008_land land08 on (parcelid = land08.parid)
LEFT OUTER JOIN tax2009_asmt asmt09 on (parcelid = asmt09.parid)
LEFT OUTER JOIN tax2010_asmt asmt10 on (parcelid = asmt10.parid)
LEFT OUTER JOIN tax2011_asmt asmt11 on (parcelid = asmt11.parid)
LEFT OUTER JOIN tax2012_asmt asmt12 on (parcelid = asmt12.parid)
LEFT OUTER JOIN tax2013_asmt asmt13 on (parcelid = asmt13.parid)
WHERE (land08.sf <> 0 AND asmt08.rtotapr>0);


