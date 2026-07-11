# =============================================================================
# 04_write_to_supabase.R
# PURPOSE: Write all pipeline outputs to Supabase PostgreSQL database
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
  port     = 6543,
  dbname   = "postgres",
  user     = SUPABASE_USER,
  password = SUPABASE_PASS,
  sslmode  = "require"
)
cat("  Connected.\n")

# --- 1. PCN Lookup ---
pcn_data <- read_csv("data/pcn_lookup.csv", show_col_types = FALSE)
dbWriteTable(con, "pcn_lookup", pcn_data, overwrite = TRUE, row.names = FALSE)
cat(sprintf("  pcn_lookup: %d rows written\n", nrow(pcn_data)))

# --- 2. Raw facility data ---
facility_data <- read_csv("data/consolidated_facility_data.csv", show_col_types = FALSE)
dbWriteTable(con, "facility_data", facility_data, overwrite = TRUE, row.names = FALSE)
cat(sprintf("  facility_data: %d rows written\n", nrow(facility_data)))

# --- 3. PCN aggregated indicators ---
pcn_ready <- read_csv("data/pcn_tool2_ready.csv", show_col_types = FALSE)
dbWriteTable(con, "pcn_tool2_ready", pcn_ready, overwrite = TRUE, row.names = FALSE)
cat(sprintf("  pcn_tool2_ready: %d rows written\n", nrow(pcn_ready)))

# --- 4. PCN Establishment data (reads from Google Sheets) ---
# To update: edit the Google Sheet — pipeline picks it up automatically that night
ESTABLISHMENT_SHEET_URL <- "https://docs.google.com/spreadsheets/d/YOUR_SHEET_ID/export?format=csv"

tryCatch({
  pcn_est <- read_csv(ESTABLISHMENT_SHEET_URL, show_col_types = FALSE)
  dbWriteTable(con, "pcn_establishment", pcn_est, overwrite = TRUE, row.names = FALSE)
  cat(sprintf("  pcn_establishment: %d rows written from Google Sheets\n", nrow(pcn_est)))
}, error = function(e) {
  cat("  pcn_establishment: Google Sheets read failed —", conditionMessage(e), "\n")
  cat("  Keeping existing Supabase data unchanged\n")
})

dbDisconnect(con)
cat("Done — Supabase updated.\n")
