#!/bin/bash
PG_BASE="" # Replace by your own value
PG_USER="" # Replace by your own value
PG_PASSWORD="" # Replace by your own value

# Simple tables of 5 attributes
number_of_tables=(1 100 200 300 400 500 600 700 800 900 1000)
for nb in "${number_of_tables[@]}"
do
	schema="benchmark_${nb}_tables"
	echo "$nb tables in schema $schema"
	echo `PGPASSWORD=${PG_PASSWORD} psql -U ${PG_USER} ${PG_BASE} -c "DROP SCHEMA IF EXISTS $schema CASCADE"` > /dev/null
	echo `PGPASSWORD=${PG_PASSWORD} psql -U ${PG_USER} ${PG_BASE} -c "CREATE SCHEMA $schema"`
	for (( i=1; i<=$nb; i++ ))
	do
		table="$schema.table_$i"
		echo `PGPASSWORD=${PG_PASSWORD} psql -U ${PG_USER} ${PG_BASE} -c "CREATE TABLE $table(att1 INTEGER, att2 TEXT, att3 DATE, att4 TEXT, att5 BOOLEAN)"` > /dev/null
	done
done

# For a fixed number of tables, a fixed number of attributes and a fixed number of primary keys, varying the number of foreign keys
number_of_fks=(0 1 2 3 4 5 6 7 8 9 10)
nb_attributes=20
nb_tables=100
for nb in "${number_of_fks[@]}"
do
	schema="benchmark_${nb}_fks_${nb_tables}_tables_${nb_attributes}_attributes"
	echo "$nb fks in schema $schema"
	echo `PGPASSWORD=${PG_PASSWORD} psql -U ${PG_USER} ${PG_BASE} -c "DROP SCHEMA IF EXISTS $schema CASCADE"` > /dev/null
	echo `PGPASSWORD=${PG_PASSWORD} psql -U ${PG_USER} ${PG_BASE} -c "CREATE SCHEMA $schema"`
	attributes="att0 INTEGER PRIMARY KEY"
	for (( i=1; i<$nb_attributes; i++))
	do
		attributes="$attributes, att$i INTEGER"
	done
	for (( i=1; i<=$nb_tables; i++ ))
	do
		table="$schema.table_$i"
		echo `PGPASSWORD=${PG_PASSWORD} psql -U ${PG_USER} ${PG_BASE} -c "CREATE TABLE $table($attributes)"` > /dev/null
	done
	# Add foreign keys
	for (( i=1; i<=$nb_tables; i++ ))
	do
		table="$schema.table_$i"
		for (( fk=1; fk<=$nb; fk++ ))
		do
			if [[ $i -ne $fk+1 ]]; then 
				num_table=$(( ((i+fk)%nb_tables)+1 ))
				referenced_table="$schema.table_${num_table}"
				echo `PGPASSWORD=${PG_PASSWORD} psql -U ${PG_USER} ${PG_BASE} -c "ALTER TABLE $table ADD FOREIGN KEY(att$fk) REFERENCES ${referenced_table}(att0)"` > /dev/null
			fi
		done
	done
done

