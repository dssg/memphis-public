drop table if exists demograph_ann;
create table demograph_ann as
select 
geo_id, geo_id2, geo_display_label,
(HD01_S002+HD01_S003+HD01_S004)/nullif(HD01_S076,0) as under14_perc,
(HD01_S005+HD01_S006+HD01_S007+HD01_S008+HD01_S009+HD01_S010+HD01_S011+HD01_S012+HD01_S013+HD01_S014)/nullif(HD01_S076,0) as from15to60_perc,
(HD01_S015+HD01_S016+HD01_S017+HD01_S018+HD01_S019)/nullif(HD01_S076,0) as more61_perc,
HD01_S078/nullif(HD01_S076,0) as white_perc,
HD01_S079/nullif(HD01_S076,0) as afram_perc,
HD01_S081/nullif(HD01_S076,0) as asian_perc,
(HD01_S080+HD01_S089+HD01_S094+HD01_S095)/nullif(HD01_S076,0) as otherace_perc,
HD02_S107/100 as hisplat_perc,
HD02_S170/100 as hoccupan_perc,
HD02_S171/100 as hvaccanc_perc,
HD02_S181/100 as occowner_perc,
HD02_S184/100 as occrente_perc,
HD02_S151/100 as hhfamily_perc,
HD02_S159/100 as hhnonfam_perc,
the_geom
from census2010_sf1dp1_withgeom
