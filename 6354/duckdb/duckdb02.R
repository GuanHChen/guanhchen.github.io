## EPPS 6354 Information Management
## Connecting DuckDB to SQLite
## Packages: DBI, RPostgres, duckdb
#install.packages("RSQLite")
library(DBI)
library(duckdb)
library(RSQLite)

con <- dbConnect(duckdb::duckdb())

# For first run, be sure to install the SQLite extension
# Install the SQLite extension
dbExecute(con, "INSTALL sqlite")

# Check if the extension has been loaded
dbExecute(con, "LOAD sqlite")

# Connect to the SQLite file using DuckDB
duckdb_conn <- dbConnect(duckdb::duckdb(), "nba.db") # file on same folder or provide path

# Query the data using DuckDB
result <- dbGetQuery(duckdb_conn, "SELECT * FROM Player")
print(result)

# Close connection
dbDisconnect(duckdb_conn)
