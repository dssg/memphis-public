#!/usr/bin/env python

# Load Census CSVs into a postgres database
# This is for the annotated variety from American FactFinder
# usage: load_census.py file.csv database_connection_string

import sys, csv
import psycopg2

conn = psycopg2.connect(sys.argv[2])

with open(sys.argv[1],'r') as infile:
  cur = conn.cursor()
  f = csv.reader(infile)
  h1 = f.next()
  for i,field in enumerate(h1):
    h1[i]=field.replace('.','_')
  for i,field in enumerate(h1):
    h1[i]=field.replace('-','_')
  h2 = f.next()

  cur.execute('DROP TABLE IF EXISTS '+sys.argv[2]+'_descr')

  sql_create = 'CREATE TABLE '+sys.argv[2]+'_descr ( key VARCHAR, descr VARCHAR )'
  cur.execute(sql_create)
  for key, val in zip(h1,h2):
    cur.execute('INSERT INTO '+sys.argv[2]+'_descr VALUES ( %s, %s )',(key,val))

  cur.execute('DROP TABLE IF EXISTS '+sys.argv[2]+' CASCADE')

  sql_create = 'CREATE TABLE '+sys.argv[2]+' ( '
  for i,field in enumerate(h1):
    if i < 3:
      sql_create += field+' VARCHAR, '
    else:
      sql_create += field+' FLOAT, '
  sql_create = sql_create[:-2] + ');'
  cur.execute(sql_create)

  sql_populate = 'INSERT INTO '+sys.argv[2]+' VALUES ( '
  for i,field in enumerate(h2):
    sql_populate += '%s, '
  sql_populate = sql_populate[:-2] + ');'

  for row in f:
    for i, field in enumerate(row):
      row[i] = row[i] if row[i] not in [' ( X ) ', '(X)', '-', '**', 'N', '***', \
        '2,500-','1,000+','10,000-','100-','9.0+','2,000+'] else None
    cur.execute(sql_populate, row)

  cur.execute('CREATE VIEW '+sys.argv[2]+'_withgeom AS SELECT * FROM '+sys.argv[2]+' c JOIN tiger2010_tract_geom t ON c.geo_id2=t.geoid10')

  conn.commit()

