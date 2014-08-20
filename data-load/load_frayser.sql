CREATE SCHEMA frayser;

-- We use an enum to enforce the types
CREATE TYPE condgrade AS ENUM ('A', 'B', 'C', 'D', 'E');

CREATE TABLE frayser.exportdata (
  PARCELID varchar,
  PARCELADD varchar,
  PARCELACRE numeric,
  WARD int2,
  BLOCK int2,
  PARCEL int4,
  CONDGRADE condgrade,
  CONDVACANT boolean,
  CONDOPEN boolean,
  OWNOCC boolean,
  COUNTYTAX numeric,
  COUNTYTAXYRS int2,
  OWNER1 varchar,
  OWNER2 varchar,
  OWNADRNO numeric,
  OWNADRDR varchar,
  OWNADRST varchar,
  OWNADRSUFF varchar,
  OWNADRCITY varchar,
  OWNADRST1 varchar,
  OWNADRUN_1 varchar,
  OWNADRUNIT varchar,
  OWNADRZIP varchar,
  OWNADRZIP4 varchar,
  PARADRNO numeric,
  PARADRADD varchar,
  PARADRDIR varchar,
  PARADRSTR varchar,
  PARADRSUF varchar,
  PARADRUNIT varchar,
  PARADRUN_1 varchar,
  PARADRZIP varchar,
  LUC varchar,
  ZONING varchar
);

\copy frayser.exportdata FROM 'frayser_exportdata.csv' WITH CSV HEADER

ALTER TABLE frayser.exportdata ADD COLUMN fid SERIAL PRIMARY KEY;

CREATE TABLE frayser.key (
  f1 varchar,
  parcel varchar,
  note1 varchar,
  note2 varchar
);

\copy frayser.key FROM 'frayser_key.csv' WITH CSV HEADER

CREATE TABLE frayser.cond (
  grade condgrade,
  classification varchar,
  "desc" varchar
);

\copy frayser.cond FROM 'frayser_cond.csv' WITH CSV HEADER

CREATE TABLE frayser.luc (
  luc varchar,
  "type" varchar,
  description varchar
);

\copy frayser.luc FROM 'frayser_luc.csv' WITH CSV HEADER

VACUUM ANALYZE frayser;
