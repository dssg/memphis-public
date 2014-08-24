SELECT par.parcelid, ST_X(ST_TRANSFORM(par.centroid,4326)) as lon, ST_Y(ST_TRANSFORM(par.centroid,4326)) as lat, 
asmt.rtotasmt, asmt.aprland, asmt.aprbldg, asmt.rtotapr as rtotapr, asmt.class as class, asmt.luc as luc, ST_Area(the_geom) as sfland,
dd.yrblt as yrblt, dd.sfla as sqft, upper(dd.user1)='Y' as ownocc, dd.extwall, dd.style, dd.stories, dd.rmtot, dd.rmfam, dd.rmbed,
dd.fixbath, dd.fixhalf, dd.fixaddl, dd.bsmt, dd.heat, dd.fuel, dd.heatsys, dd.intext, dd.wbfp_o, dd.wbfp_s, dd.wbfp_pf, dd.bsmtcar,
dd.grade, dd.cdu, dd.cond, dd.rooftype, dd.roofcover,

asmt4.rtotasmt as rtotasmt4, dd4.rmtot as rmtot4, dd4.sfla as sqft4,

-- flag variables about the owner
od.statecode != 'TN' AS outofstate,
position('pobox' in lower(regexp_replace(regexp_replace(od.adrstr, '[:punct:]', '', 'g'), '\\W', '', 'g'))) > 0 AS pobox,
lower(regexp_replace(od.own1, '[:punct:]', '', 'g')) ~ 'corp|inc|incorporated|la|el|srl|limited|llc|co|corporation|company|ltd|ltda|lda|de|sa|pty|ste|sarl|gmhb|sprl'
AS corp,
position('church' in lower(od.own1)) AS church,
pd.adrno, pd.adrdir, pd.adrstr, pd.adrsuf, pd.zip1,

-- info about the last sale
sal.yrsold saleyear, sal.stype saletype, sal.sval saleval, sal.steb steb, sal.price saleprice,

-- building permits
prmt.total total_permit,

-- flag indicating no utilities in 2008
par.parcelid IN (SELECT parcelid FROM invt.no_utilities_2013_by_parcel WHERE EXTRACT(year FROM last_bill) < 2012) AS no_utilities,

-- outbuildings
'PRC' = ANY(oby_code) OR 'PIF' = ANY (oby_code) OR 'PIG' = ANY (oby_code) OR 'PRV' = ANY (oby_code) OR 'PIV' = ANY (oby_code) OR 'PRF' = ANY (oby_code)
OR 'PRG' = ANY (oby_code) OR 'PIC' = ANY (oby_code) AS pool,

-- data about nearby foreclosures
fc.parcels, fc.f09 as fc4, fc.f10 as fc3, fc.f11 as fc2, fc.f12 as fc1,

-- census population change stuff
-- note that we do not do the calculation here, because we want to exclude blocks that had minimimal
-- 2000 population from the count.
cchg.P0010001 AS pop2010, cchg.P0010001_2000 AS pop2000,

-- sales tax
stax.gross_sales AS tract_gross_sales,

-- LODES
-- CE01: # of jobs with earnings < $1250/mo, CE02: 1250-3333, C000: total jobs
l7.ce01 / l7.c000::numeric AS lowincomerate, l7.ce02 / l7.c000::numeric AS midincomerate,
l7.ce03 / l7.c000::numeric AS highincomerate,

-- CNS03: Utilities, 04: Construction, 05: manufacturing, 08: transportation/warehousing
(l7.cns03 + l7.cns04 + l7.cns05 + l7.cns08) / l7.c000::numeric AS bluecollar,
-- CNS07: retail, 18: accomodation/foodservice
(l7.cns07 + l7.cns18) / l7.c000::numeric AS service,

-- CNS09: Information, 10: Finance/Insurance, 11: Real Estate, 12: Prof/Scientific/Technical Svcs, 13: Management,
-- 16: healthcare
(l7.cns09 + l7.cns10 + l7.cns11 + l7.cns12 + l7.cns13 + l7.cns16) / l7.c000::numeric AS whitecollar,

-- ACS 2009 demographic characteristics
acs09_3.HC02_EST_VC06 AS unemployed,

-- postprediction aggregation variable
bglook.geoid AS blockgroup,
blklook.geoid AS block,

cca.weeds, cca.sidewalks, cca.sanitation, cca.neglect, cca.we_charge1, 
ctd.tb_latefee as latefee, ctd.tb_bankrupt, ctd.tb_badcheck 

FROM parcels.parcels2012 par
LEFT JOIN parcels.par12_bg_lookup bglook ON (bglook.parid=par.parcelid)
LEFT JOIN parcels.par12_blk_lookup blklook ON (blklook.parid=par.parcelid)

LEFT JOIN tax2012_asmt asmt  ON (asmt.parid = par.parcelid)
LEFT JOIN tax2012_owndat od  ON (od.parid = par.parcelid)
LEFT JOIN tax2012_pardat pd  ON (pd.parid = par.parcelid)

LEFT JOIN tax2008_asmt asmt4 ON (asmt4.parid = par.parcelid)
LEFT JOIN (SELECT *, ROW_NUMBER() OVER (PARTITION BY parid ORDER BY sfla DESC) rn FROM tax2008_dweldat) dd4 ON (dd4.parid = par.parcelid AND dd4.rn=1)

-- this grabs dweldat for only the largest property
LEFT JOIN (SELECT *, ROW_NUMBER() OVER (PARTITION BY parid ORDER BY sfla DESC) rn FROM tax2012_dweldat) dd ON (dd.parid = par.parcelid AND dd.rn=1)

-- get the most recent sale
LEFT JOIN (SELECT parid, EXTRACT (year FROM saledt) yrsold, saletype stype, saleval sval, steb, price,
                  ROW_NUMBER() OVER (PARTITION BY parid ORDER BY saledt DESC) rn
           FROM tax2012_sales) sal ON (sal.parid = par.parcelid AND sal.rn = 1)    

-- aggregate of recent permits
LEFT JOIN (SELECT parid, SUM(amount) AS total FROM tax2012_permit WHERE EXTRACT(YEAR FROM permdt) >= 2012 - 10 GROUP BY parid) prmt ON (prmt.parid = par.parcelid)

-- aggregate of outbuildings
LEFT JOIN (SELECT parid, ARRAY_AGG(code) AS oby_code, ARRAY_AGG(yrblt) AS oby_yrblt FROM tax2012_oby GROUP BY parid) oby ON (oby.parid = par.parcelid)
LEFT JOIN parcels.all_parcels_anon anon ON (par.parcelid = anon.parcelid)
LEFT JOIN census2000to2010 cchg ON (blklook.geoid=cchg.geoid)
LEFT JOIN acs_09_5yr_dp5yr3 acs09_3 ON (acs09_3.geo_id2 = par.tractid)
LEFT JOIN (SELECT tractid, sum(gross_sales) as gross_sales from sales.tax where date<'01-01-2012' and date>'12-31-2009' group by tractid) stax ON (par.tractid=stax.tractid)
LEFT JOIN lodes.lodes2007_rac l7 ON (blklook.geoid = l7.h_geocode)
LEFT JOIN foreclosures.fc_blk_counts fc on (blklook.geoid = fc.blk_id)
LEFT JOIN (SELECT sum((we_type='W')::int) as weeds, sum((we_type='I')::int) as sidewalks, sum((we_type='A')::int) as sanitation, sum((we_type='N')::int) as neglect,
		sum(we_charge1) as we_charge1, we_mapno FROM com.com_charges_all 
		WHERE cast(we_fullyear as int)<=2012 and cast(we_fullyear as int)>=2009 GROUP BY we_mapno) cca on (par.parcelid=cca.we_mapno)
LEFT JOIN (SELECT sum(tb_latefee) as tb_latefee, tb_mapno, sum((length(tb_bankrupt)>0)::int) as tb_bankrupt, sum((length(tb_badcheck)>0)::int) as tb_badcheck 
		FROM com.com_tax_delinquent WHERE cast(tb_fullyear as int)<=2012 and cast(tb_fullyear as int)>=2009 GROUP BY tb_mapno) ctd on (par.parcelid=ctd.tb_mapno)


WHERE pd."class" = 'R' -- only residential
AND ST_Within(centroid, (SELECT ST_Union(the_geom) FROM muniboundaries_shelby WHERE name='Memphis'))
