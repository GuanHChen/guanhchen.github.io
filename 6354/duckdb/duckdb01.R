## EPPS 6354 Information Management
## Connecting PostgreSQL database to DuckDB
## Packages: DBI, RPostgres, duckdb

# install.packages("duckdb")
library(DBI)
library(RPostgres)
library(duckdb)

# Establish a connection to the local PostgreSQL server
pg_conn <- dbConnect(RPostgres::Postgres(), dbname = "infomgmt", host = "127.0.0.1",port=5432, user = "postgres", password = "4688")

# Create a DuckDB connection
duckdb_conn <- dbConnect(duckdb::duckdb(), ":memory:") # create an in-memory database

# Fetch data from PostgreSQL and use in DuckDB
instructor <- dbGetQuery(pg_conn, "SELECT * FROM instructor")
dbWriteTable(duckdb_conn, "table_instructor", instructor, overwrite = T)

# Query the data using DuckDB
result <- dbGetQuery(duckdb_conn, "SELECT * FROM table_instructor WHERE salary >= 100000")
print(result)

# Close connections
dbDisconnect(pg_conn)
dbDisconnect(duckdb_conn)
