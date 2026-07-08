# =============================================================================
# 00_pull_tool2_from_kobo.R
# PURPOSE: Pull PCN monitoring submissions from KoboToolbox Tool 2
#          and write raw data + computed dashboard indicators to Supabase
# =============================================================================
library(httr2)
library(jsonlite)
library(dplyr)
library(readr)
library(DBI)
library(RPostgres)

KOBO_TOKEN    <- Sys.getenv("KOBO_TOKEN")
KOBO_TOOL2_UID <- Sys.getenv("KOBO_TOOL2_UID")
KOBO_BASE     <- "https://eu.kobotoolbox.org/api/v2/assets"

cat("Fetching Tool 2 (PCN Monitoring) submissions from KoboToolbox...\n")

# --- Fetch all Tool 2 submissions ---
fetch_kobo_all <- function(asset_uid, token) {
  all_results <- list()
  url <- paste0(KOBO_BASE, "/", asset_uid, "/data/")
  repeat {
    resp <- request(url) |>
      req_url_query(format = "json", limit = 30000) |>
      req_headers(Authorization = paste("Token", token)) |>
      req_perform()
    body    <- resp_body_json(resp, simplifyVector = FALSE)
    results <- body$results
    if (length(results) == 0) break
    flat <- lapply(results, function(sub) {
      flat <- list()
      for (key in names(sub)) {
        val <- sub[[key]]
        flat[[key]] <- if (is.null(val)) NA_character_
                       else if (is.list(val)) paste(unlist(val), collapse="; ")
                       else as.character(val)
      }
      as.data.frame(flat, stringsAsFactors=FALSE, check.names=FALSE)
    })
    all_results <- c(all_results, flat)
    if (is.null(body$`next`) || is.na(body$`next`)) break
    url <- body$`next`
  }
  bind_rows(all_results)
}

raw_tool2 <- fetch_kobo_all(KOBO_TOOL2_UID, KOBO_TOKEN)
cat(sprintf("  Fetched %d Tool 2 submissions\n", nrow(raw_tool2)))

# Clean column names
names(raw_tool2) <- gsub("^[^/]+/", "", names(raw_tool2))
names(raw_tool2) <- make.unique(names(raw_tool2), sep="_")

# --- Write to Supabase ---
con <- dbConnect(
  RPostgres::Postgres(),
  host     = Sys.getenv("SUPABASE_HOST"),
  port     = 6543,
  dbname   = "postgres",
  user     = Sys.getenv("SUPABASE_USER"),
  password = Sys.getenv("SUPABASE_PASS"),
  sslmode  = "require"
)

dbWriteTable(con, "tool2_submissions", raw_tool2, overwrite=TRUE, row.names=FALSE)
cat(sprintf("  Written to Supabase: tool2_submissions (%d rows)\n", nrow(raw_tool2)))
dbDisconnect(con)
cat("Done.\n")
