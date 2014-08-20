#!/bin/sh
# Load all the shapefiles in the data directory.
# Usage: HOST=database_host USER=database_user DB=database_name SCHEMA=schema load_shapefiles.sh [directory]
# You can also set PGPASSWORD if needed, although ~/.pgpass is probably a better alternative

if [ -n "$1" ]; then
  SHPFILE_DIR="$1"
else
  SHPFILE_DIR=/mnt/data/memphis/GDB_Conversion
fi

echo "Processing shapefiles in $SHPFILE_DIR"

cd $SHPFILE_DIR

# Loop over all .shp files
for SHPFILE in $(ls *.shp) ; do

  # Define the table name
  TABLE=$(echo $SHPFILE | sed 's/.shp//' | awk '{print tolower($1);}')

  # Drop the table if it already exists.
  psql -h $HOST -U $USER $DB <<EOF
DROP TABLE IF EXISTS ${SCHEMA}.${TABLE};
EOF

  # Turn the shapefile data into SQL statements to create and populate a table,
  # and pipe it into the database.
  shp2pgsql $SHPFILE ${SCHEMA}.$TABLE | psql -h $HOST -U $USER $DB

  # Set the SRID and make a spatial index on the new table.
  # Note that we use an underscore rather than a period in the index
  # because indices are not schema-specific
  psql -h $HOST -U $USER $DB <<EOF
SELECT UpdateGeometrySRID('${SCHEMA}', '$TABLE','the_geom',2274);
CREATE INDEX ${TABLE}_gix ON ${SCHEMA}.$TABLE USING gist (the_geom);
EOF

done

