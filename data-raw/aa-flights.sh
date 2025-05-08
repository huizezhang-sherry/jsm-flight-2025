#!/bin/bash

# Output CSV file
output_file="AA_flights.csv"

# Remove existing file if it exists
rm -f "$output_file"

# Use DuckDB to write filtered data with header
duckdb <<EOF
COPY (
  SELECT Reporting_Airline, FlightDate, DepTime, ArrTime, Origin, Dest
  FROM read_parquet('../Year=*/data_*.parquet')
  WHERE Reporting_Airline = 'AA'
    AND (Origin IN ('DFW', 'CLT', 'ORD', 'MIA') OR Dest IN ('DFW', 'CLT', 'ORD', 'MIA'))
) TO '$output_file' (FORMAT CSV, HEADER TRUE, DELIMITER ',', QUOTE '"', FORCE_QUOTE *);
EOF
