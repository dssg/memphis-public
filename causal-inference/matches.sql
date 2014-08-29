CREATE TABLE mconway.matches (treatment varchar, control varchar);

\copy mconway.matches FROM 'spatial.csv' CSV HEADER;

CREATE TABLE mconway.matches_geom AS
SELECT ST_MakeLine(p1.centroid, p2.centroid) the_geom
FROM mconway.matches
LEFT JOIN parcels.parcels2013 p1 ON (p1.parcelid=treatment)
LEFT JOIN parcels.parcels2013 p2 ON (p2.parcelid=control);

ALTER TABLE mconway.matches_geom ADD COLUMN fid SERIAL PRIMARY KEY;

CREATE INDEX matches_geom_gix ON mconway.matches_geom USING gist (the_geom);