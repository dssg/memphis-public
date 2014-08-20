#!/usr/bin/env python

# Load the BDC sales data to the database
# Run like so: DBCONN=postgresql://user@db/database load_bdc.py

import csv, psycopg2, re
from os import environ

conn = psycopg2.connect(environ['DBCONN'])

cursor = conn.cursor()

# Create types and tables
cursor.execute('''
CREATE TYPE bdc.status AS ENUM ('Sold/Transfered', 'Rented',
'Rehab Process', 'New Construction', 'Vacant Structure',
'Vacant Lot', 'Pending Purchase', 'Lease Purchase');
''')

cursor.execute('''
CREATE TYPE bdc.disposition AS ENUM ('Rent', 'Sold', 'Lease Purchase', 'Construction', 'Other');
''')

cursor.execute('''
CREATE TYPE bdc.improvement AS ENUM ('Rehab', 'New Construction', 'Land');
''')

cursor.execute('''
CREATE TABLE bdc.properties (rownum int2 PRIMARY KEY, adr VARCHAR, parcelid VARCHAR,
dateacquired DATE, statusnote VARCHAR, status bdc.status, saledt DATE,
units int2, improvement bdc.improvement, diposition bdc.disposition,
numdemolitions int2);
''')

for row in csv.DictReader(open('Property Database Core.csv')):
    # Clean up the parcel ID's
    parid = row['Parcel ID'].replace(' ', '')
    
    numericParid = re.sub('[A-Z]', '', parid)
    
    if len(numericParid) == 10:
        parid = '0' + parid

    if len(numericParid) == 9:
        parid = '0' + parid[0:6] + '0' + parid[6:]

    row['Parcel ID'] = parid[0:6] + '  ' + parid[6:]
    
    # anonymize names but leave useful stuff like park, &c.
    if row['Status Code'] in ('1', '2', '8'):
        row['Current Status'] = re.sub(' \(.*\)', '', row['Current Status'])

    row['Status Code'] = row['Status Code']\
                         .replace('1', 'Sold/Transfered')\
                         .replace('2', 'Rented')\
                         .replace('3', 'Rehab Process')\
                         .replace('4', 'New Construction')\
                         .replace('5', 'Vacant Structure')\
                         .replace('6', 'Vacant Lot')\
                         .replace('7', 'Pending Purchase')\
                         .replace('8', 'Lease Purchase')

    row['Type'] = row['Type']\
                  .replace('R', 'Rehab')\
                  .replace('N', 'New Construction')\
                  .replace('L', 'Land')

    row['Disposition'] = row['Disposition']\
                         .replace('RE', 'Rent')\
                         .replace('S', 'Sold')\
                         .replace('LP', 'Lease Purchase')\
                         .replace('C', 'Construction')\
                         .replace('O', 'Other')

    if row['Units'] != None:
        row['Units'] = int(row['Units'])

    if row['Demolished'] == '':
        row['Demolished'] = None

    if row['Demolished'] != None:
        row['Demolished'] = int(row['Demolished'])

    if row['Date Acquired'] == '':
        row['Date Acquired'] = None

    if row['Date'] == '':
        row['Date'] = None

    cursor.execute('''INSERT INTO bdc.properties VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s)''',
                                        (row['rownum'], row['Address'],
                                         row['Parcel ID'], row['Date Acquired'], row['Current Status'],
                                         row['Status Code'], row['Date'], row['Units'], row['Type'],
                                         row['Disposition'], row['Demolished']))

conn.commit()

    
    
