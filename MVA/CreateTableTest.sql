CREATE TABLE Assmt_dynamics AS
SELECT parid, tax2001_asmt.rtotasmt AS asmt_2001, tax2002_asmt.rtotasmt AS asmt_2002, 
FROM tax2001_asmt FULL OUTER JOIN tax2002_asmt
ON tax2001_asmt.parid=tax2002_asmt.parid
