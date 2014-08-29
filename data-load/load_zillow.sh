# Load data from Zillow into a Postgres DB
# Usage: HOST=... USER=... DB=... SCHEMA=... TABLE=... load_zillow.sh *.csv
# It expects the files to be already downloaded and extracted for
# your preferred level of geographic disaggregation from
# http://www.zillow.com/research/data/ (at the very bottom)
# Note that it is bad news to put spaces in the schema/table

# This is used to keep track of what variables we want to select
select=''

# This is the from clause from the giant SQL query that will eventually join all of the stuff together
from=''

psql -h $HOST -U $USER $DB <<EOF
CREATE SCHEMA zillow_tmp;
EOF

for file in $@; do
    lname=`echo $file | sed -e 's/^[^_]*_//' -e 's/.csv//' | tr A-Z a-z | tr -d '-'`
    echo Loading $file to $lname

    # reshape the files
    python <<EOF
import csv

inf = csv.DictReader(open('$file'))
outf = csv.DictWriter(open('$lname.reshaped.csv', 'w'), ['RegionName', 'City', 'State', 'Metro', 'CountyName', 'date', '$lname'])

outf.writeheader()

# This gets rid of the RegionName, &c.
dates = [fname for fname in inf.fieldnames if fname not in outf.fieldnames]

for row in inf:
    if row['State'] != 'TN':
        continue

    rows = []
    for date in dates:
        rows.append(dict(
                RegionName=row['RegionName'],
                City=row['City'],
                State=row['State'],
                Metro=row['Metro'],
                CountyName=row['CountyName'],
                date=date
                ))
        rows[-1]['$lname'] = row[date]
    outf.writerows(rows)
EOF
    
     # Now load them (temporarily) to postgres
     psql -h $HOST -U $USER $DBNAME <<EOF
CREATE TABLE zillow_tmp.$lname (
    region varchar,
    city varchar,
    state varchar,
    metro varchar,
    county varchar,
    date varchar,
    $lname float
);

\copy zillow_tmp.$lname from '$lname.reshaped.csv' CSV HEADER
EOF

     rm $lname.reshaped.csv

     if [ -z "$from" ]; then
	 from="FROM zillow_tmp.$lname"
	 # We are using medianvaluepersqft_allhomes here because it contains
	 # the most data, so there won't be nulls.
	 select="medianvaluepersqft_allhomes.region, medianvaluepersqft_allhomes.city, medianvaluepersqft_allhomes.state, medianvaluepersqft_allhomes.metro, medianvaluepersqft_allhomes.county, medianvaluepersqft_allhomes.date, $lname"
     else
	 from="$from FULL OUTER JOIN zillow_tmp.$lname USING (region, date)"
	 select="$select, $lname"
     fi

done

# Create the single table and clean up
psql -h $HOST -U $USER $DBNAME <<EOF
CREATE TABLE $SCHEMA.$TABLE AS
SELECT $select $from;

ALTER TABLE $SCHEMA.$TABLE ADD COLUMN fid SERIAL;
ALTER TABLE $SCHEMA.$TABLE ADD PRIMARY KEY (fid);

DROP SCHEMA zillow_tmp CASCADE;
EOF
