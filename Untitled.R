library(dplyr)
library(reticulate)

# Set Python environment
use_python("/opt/homebrew/bin/python3", required = TRUE)

# Connect to the database
oracledb <- import("oracledb")

pd <- import("pandas")

username = 'mtg'

password = 'MedKF-TG'

dsn = 'soracle:1521/dwh'

connection <- oracledb$connect(user = username, password = password, dsn = dsn)

# Fetch data for subcategory valid
query <- "SELECT * FROM MTG.mtg_users" 
# "SELECT * FROM MTG.MTG_MEDIA_LIST_TEST"

df_ml <- pd$read_sql_query(query, connection)

df_ml <- as.data.frame(df_ml)




