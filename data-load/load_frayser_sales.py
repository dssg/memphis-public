#!/usr/bin/env python
# Load the Frayser sales data and deduce parcel IDs.

# Run like so: DBCONN=postgresql://user@db/database loadFrayserSales.py
# expects the Frayser sales to be in the same directory as the script,
# and be called hsold_frayser.csv

# Note: you will need to manually edit the column names so that they are
# Address, Type, Date, Num, Name, Memo, Split, Amount

# Also, addresses don't always match, so you may need to manually edit some of
# the addresses in the CSV to match a variant of the street name in the parcel
# data.

import psycopg2
import re
from csv import DictReader
from time import strptime
import datetime
from sys import stderr
from os import environ

conn = psycopg2.connect(environ['DBCONN'])
cursor = conn.cursor()

cursor.execute('DROP TABLE IF EXISTS frayser.sold;')

cursor.execute('''CREATE TABLE frayser.sold (
    fid SERIAL PRIMARY KEY,
    address VARCHAR,
    net numeric(16, 2),
    lastactiveyr int2,
    parcelid varchar
);''')

# Read in the Frayser file
with open('hsold_frayser.csv') as hsraw:
    hsold = DictReader(hsraw)

    currentAddress = None
    net = None
    syr = None

    for row in hsold:
        if row['Address'].startswith('Total'):
            net = float(row['Amount'].replace(',', ''))
            continue

        if row['Address'] is not None and row['Address'] != '':
            # new house, great!
            if currentAddress != None:
                # save the current house
                cursor.execute('INSERT INTO frayser.sold (address, net, lastactiveyr) VALUES (%s, %s, %s);',
                               (currentAddress, net, syr))

            currentAddress = row['Address']
            # ensure there is not accidental propagation
            net = None
            syr = None
            continue
            

        try:
            curyr = int(row['Date'].split('/')[2])

            # sale date is just the most recent date a property was active, to a first approximation
            if syr is None or curyr > syr:
                syr = curyr

        except IndexError:
            print >> stderr, 'Could not parse date %s for row %s' % (row['Date'], currentAddress)

conn.commit()

# Now match the houses to parcel IDs, semi-interactively
cursor = conn.cursor()
updcurs = conn.cursor()
parcurs = conn.cursor()
cursor.execute('SELECT fid, address FROM frayser.sold')

for row in cursor:
    address = row[1]
    fid = row[0]

    # parse out the address
    # This regexp won't work if there is no street suffix. there was only one such address in the data,
    # so we simply edited it manually
    mo = re.search(r'([0-9]+)(?:\W+)([A-Za-z\W]+)(?:\W+[A-Za-z\.]+)', address)
    
    if mo is None:
        print >> stderr, 'Unable to parse address %s, not matching to parcel' % address
        continue
        
    adrno = mo.group(1)
    adrstr = mo.group(2).upper().replace('.', '')
        
    print 'Parsed as %s | %s' % (adrno, adrstr)
    
    # search for a matching parcel
    regexp = '%s(\W[A-Za-z]*)?' % adrstr

    parcurs.execute('SELECT parid, adrno, adrstr FROM tax2008_pardat WHERE adrno=%s AND adrstr~%s', (adrno, regexp))

    if parcurs.rowcount == 0:
        print 'No parcel found for address %s in tax2008_pardat' % address
    
    elif parcurs.rowcount == 1:
        # easy
        row = parcurs.fetchone()
        updcurs.execute('UPDATE frayser.sold SET parcelid=%s WHERE fid=%s', (row[0], fid))
    else:
        rows = parcurs.fetchall()

        print '%s addresses matched address %s, choose one:' % (len(rows), address)

        for i in range(len(rows)):
            print i, row[0], row[1], row[2]

        correct = rows[int(raw_input())]
        updcurs.execute('UPDATE frayser.sold SET parcelid=%s WHERE fid=%s', (correct[0], fid))

conn.commit()
