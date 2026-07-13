# ============================================================
# 06_sync_establishment_from_gsheet.R
# Syncs the PCN establishment register from Google Sheets
# into the Supabase `pcn_establishment` table.
#
# Runs as the last step of the nightly GitHub Actions pipeline.
# Requires env vars (already set as GitHub secrets for step 04):
#   SUPABASE_HOST, SUPABASE_USER, SUPABASE_PASS
# Plus one new secret:
#   PCN_SHEET_CSV_URL — the sheet's CSV export URL, i.e.
#   https://docs.google.com/spreadsheets/d/<SHEET_ID>/export?format=csv&gid=<GID>
#   (Sheet must be shared as "Anyone with the link — Viewer")
# ============================================================

suppressPackageStartupMessages({
  library(DBI)
  library(RPostgres)
})

cat("=== 06: Sync pcn_establishment from Google Sheets ===\n")

# ── 1. Fetch the sheet ──────────────────────────────────────
sheet_url <- Sys.getenv("PCN_SHEET_CSV_URL")
if (sheet_url == "") stop("PCN_SHEET_CSV_URL env var is not set.")

df <- tryCatch(
  read.csv(sheet_url, check.names = FALSE, stringsAsFactors = FALSE,
           fileEncoding = "UTF-8"),
  error = function(e) stop("Failed to read Google Sheet: ", conditionMessage(e))
)
cat("Fetched", nrow(df), "rows,", ncol(df), "columns from Google Sheets\n")

# ── 2. Validate BEFORE touching the database ────────────────
# If anything here fails, the existing Supabase table is left untouched.
required_cols <- c("County", "Subcounty", "PCN", "pcn_location",
                   "supporting_partner", "establishment_status")

missing <- setdiff(required_cols, names(df))
if (length(missing) > 0)
  stop("Sheet is missing required column(s): ", paste(missing, collapse = ", "),
       "\nFound columns: ", paste(names(df), collapse = ", "))

# Keep only the expected columns, in canonical order (drops any helper
# columns someone adds to the sheet, e.g. notes or checkboxes)
df <- df[, required_cols]

# Basic hygiene
df$PCN    <- trimws(df$PCN)
df$County <- trimws(df$County)
df        <- df[!is.na(df$PCN) & df$PCN != "", ]

if (nrow(df) < 100)
  stop("Sanity check failed: only ", nrow(df),
       " rows after cleaning (expected ~300+). Refusing to overwrite table.")

# ── 3. Coordinate sanity report (warn, don't block) ─────────
coords <- strsplit(gsub("\\s+", "", df$pcn_location), ",")
first_val  <- function(x) if (length(x) >= 1) x[1] else NA_character_
second_val <- function(x) if (length(x) >= 2) x[2] else NA_character_
v1 <- suppressWarnings(as.numeric(vapply(coords, first_val,  character(1))))
v2 <- suppressWarnings(as.numeric(vapply(coords, second_val, character(1))))

in_kenya <- (v1 >= -5.5 & v1 <= 5.5 & v2 >= 33 & v2 <= 42.5) |
            (v2 >= -5.5 & v2 <= 5.5 & v1 >= 33 & v1 <= 42.5)
bad <- which(!is.na(df$pcn_location) & df$pcn_location != "" &
             (is.na(in_kenya) | !in_kenya))
if (length(bad) > 0) {
  cat("WARNING:", length(bad), "row(s) have coordinates outside Kenya's",
      "bounding box (these will be dropped by the dashboard):\n")
  print(df[bad, c("County", "PCN", "pcn_location")], row.names = FALSE)
}

# Duplicate-coordinate report (copy-paste artifacts)
loc_counts <- table(df$pcn_location[!is.na(df$pcn_location) & df$pcn_location != ""])
dups <- names(loc_counts[loc_counts > 1])
if (length(dups) > 0) {
  cat("NOTE:", length(dups), "coordinate value(s) are shared by multiple PCNs:\n")
  for (d in head(dups, 10))
    cat("  ", d, "->", paste(df$PCN[df$pcn_location == d], collapse = " | "), "\n")
}

# ── 4. Write to Supabase ────────────────────────────────────
con <- dbConnect(
  RPostgres::Postgres(),
  host     = Sys.getenv("SUPABASE_HOST"),
  port     = 6543,
  dbname   = "postgres",
  user     = Sys.getenv("SUPABASE_USER"),
  password = Sys.getenv("SUPABASE_PASS"),
  sslmode  = "require"
)
on.exit(try(dbDisconnect(con), silent = TRUE), add = TRUE)

dbWriteTable(con, "pcn_establishment", df, overwrite = TRUE)

# Verify the write
n_written <- dbGetQuery(con, 'SELECT COUNT(*) AS n FROM pcn_establishment')$n
cat("Wrote", n_written, "rows to pcn_establishment\n")
if (n_written != nrow(df)) stop("Row count mismatch after write!")

cat("=== 06: Done ===\n")
