# load files from an MDB into Postgres
# Usage: HOST=database_host USER=database_user DB=database_name load_mdb.sh input.mdb schema
# You can also set PGPASSWORD if needed, although ~/.pgpass is probably a better alternative

# Process arguments
MDB=$1
SCHEMA=$2

# load the schema
echo Loading schema
mdb-schema --no-relations --no-not-null "$MDB" postgres | psql -h $HOST -U $USER $DB

# load the data
echo Loading tables
TABLES=`mdb-tables -d , "$MDB"`
OLDIFS=$IFS
IFS=','
for table in $TABLES; do
    echo Loading table $table
    mdb-export "$MDB" "$table" | psql -h $HOST -U $USER $DB -c "\copy \"$table\" from stdin CSV HEADER"
done

# move the tables to the correct schema and patch up the names
echo Creating schema $SCHEMA
psql -h $HOST -U $USER $DB <<EOF
  CREATE SCHEMA $SCHEMA;
EOF

for table in $TABLES; do
    echo Cleaning table $table
    psql -h $HOST -U $USER $DB <<EOF
      ALTER TABLE "$table" SET SCHEMA $SCHEMA;
      ALTER TABLE $SCHEMA."$table" RENAME TO `echo $table | sed 's/\W//g' | tr '[:upper:]' '[:lower:]'`;
EOF
done

OLDIFS="$IFS"
