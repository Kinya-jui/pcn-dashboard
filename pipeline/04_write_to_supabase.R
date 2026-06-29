# =============================================================================
# 04_write_to_supabase.R
# PURPOSE: Write pcn_tool2_ready.csv to Supabase PostgreSQL database
#          Replaces failed KoboToolbox media API upload
# =============================================================================
library(DBI)
library(RPostgres)
library(readr)

SUPABASE_HOST <- Sys.getenv("SUPABASE_HOST")
SUPABASE_USER <- Sys.getenv("SUPABASE_USER")
SUPABASE_PASS <- Sys.getenv("SUPABASE_PASS")

cat("Connecting to Supabase...\n")
con <- dbConnect(
  RPostgres::Postgres(),
  host     = SUPABASE_HOST,
  port     = 5432,
  dbname   = "postgres",
  user     = SUPABASE_USER,
  password = SUPABASE_PASS
)
cat("  Connected.\n")

# Write pcn_lookup
pcn_data <- read_csv("data/pcn_lookup.csv", show_col_types = FALSE)
dbWriteTable(con, "pcn_lookup", pcn_data, overwrite = TRUE, row.names = FALSE)
cat(sprintf("  pcn_lookup: %d rows written\n", nrow(pcn_data)))

# Write full facility data too
facility_data <- read_csv("data/consolidated_facility_data.csv", show_col_types = FALSE)
dbWriteTable(con, "facility_data", facility_data, overwrite = TRUE, row.names = FALSE)
cat(sprintf("  facility_data: %d rows written\n", nrow(facility_data)))

# Write PCN aggregates
pcn_ready <- read_csv("data/pcn_tool2_ready.csv", show_col_types = FALSE)
dbWriteTable(con, "pcn_tool2_ready", pcn_ready, overwrite = TRUE, row.names = FALSE)
cat(sprintf("  pcn_tool2_ready: %d rows written\n", nrow(pcn_ready)))

dbDisconnect(con)
cat("Done — Supabase updated.\n")