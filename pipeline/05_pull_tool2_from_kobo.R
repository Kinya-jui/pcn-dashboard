writeLines('
# =============================================================================
# 05_pull_tool2_from_kobo.R
# PURPOSE: Pull PCN + County monitoring submissions from KoboToolbox Tool 2
#          and write to Supabase as tool2_submissions table.
# =============================================================================
library(httr2)
library(jsonlite)
library(dplyr)
library(DBI)
library(RPostgres)

KOBO_TOKEN     <- Sys.getenv("KOBO_TOKEN")
KOBO_TOOL2_UID <- Sys.getenv("KOBO_TOOL2_UID")
KOBO_BASE      <- "https://eu.kobotoolbox.org/api/v2/assets"

cat("Fetching Tool 2 submissions from KoboToolbox...\\n")
if (!nzchar(KOBO_TOKEN) || !nzchar(KOBO_TOOL2_UID))
  stop("Missing KOBO_TOKEN or KOBO_TOOL2_UID")

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
      row <- list()
      for (key in names(sub)) {
        val <- sub[[key]]
        row[[key]] <- if (is.null(val)) NA_character_
                      else if (is.list(val)) paste(unlist(val), collapse = "; ")
                      else as.character(val)
      }
      as.data.frame(row, stringsAsFactors = FALSE, check.names = FALSE)
    })
    all_results <- c(all_results, flat)
    if (is.null(body[["next"]]) || is.na(body[["next"]])) break
    url <- body[["next"]]
  }
  bind_rows(all_results)
}

raw <- fetch_kobo_all(KOBO_TOOL2_UID, KOBO_TOKEN)
cat(sprintf("  Fetched %d submissions, %d raw columns\\n", nrow(raw), ncol(raw)))

if (nrow(raw) == 0) { cat("  No submissions — skipping\\n"); quit(save="no", status=0) }

# --- Drop columns that cause PostgreSQL duplicate name errors after truncation ---
drop_exact <- c(
  # county_managementsystems — keep _score, drop raw question
  "county_tool/county_managementsystems/county_quality_improvement_coordination_mechanism",
  "county_tool/county_managementsystems/county_support_supervision_implementation_mechanism",
  "county_tool/county_managementsystems/county_health_department_ipc_committee",
  "county_tool/county_managementsystems/qoc_weight",
  # county_partnerships — keep _score, drop raw integer
  "county_tool/county_partnerships/num_of_biannually_multisectoral_stakeholder_forums",
  "county_tool/county_partnerships/num_of_research_studies_on_pcn_implementation",
  # pcn multisectoral — drop one of each genuine pair
  "pcn_tool/multi_sectoral_partnerships/no_multisectoral_actions_identified",
  "pcn_tool/multi_sectoral_partnerships/innovations_learning/no_target_innovations"
)

drop_pattern <- paste(
  "red_note", "orange_note", "darkgreen_note",
  "_note$", "_note\\\\s+$",
  "_weight$", "_weight\\\\s+$",
  "logo", "intro_note",
  sep = "|"
)

keep1 <- !names(raw) %in% drop_exact
keep2 <- !grepl(drop_pattern, names(raw), ignore.case = TRUE)
raw   <- raw[, keep1 & keep2, drop = FALSE]
cat(sprintf("  After dropping display/clash columns: %d remain\\n", ncol(raw)))

# --- Clean column names for PostgreSQL ---
names(raw) <- gsub("/", "__", names(raw))
names(raw) <- gsub("[^a-zA-Z0-9_]", "_", names(raw))
names(raw) <- substr(names(raw), 1, 63)

# Safety net
dupes <- names(raw)[duplicated(names(raw))]
if (length(dupes) > 0) {
  cat("WARNING — still clashing after clean, deduplicating:\\n")
  cat(paste0("  ", dupes), sep="\\n")
  names(raw) <- make.unique(names(raw), sep="_")
}

# --- Write to Supabase ---
cat("Connecting to Supabase...\\n")
con <- dbConnect(
  RPostgres::Postgres(),
  host     = Sys.getenv("SUPABASE_HOST"),
  port     = 6543,
  dbname   = "postgres",
  user     = Sys.getenv("SUPABASE_USER"),
  password = Sys.getenv("SUPABASE_PASS"),
  sslmode  = "require"
)
dbWriteTable(con, "tool2_submissions", raw, overwrite = TRUE, row.names = FALSE)
count <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM tool2_submissions")$n
cat(sprintf("  Written: tool2_submissions — %d rows, %d columns\\n", count, ncol(raw)))
dbDisconnect(con)
cat("Done.\\n")
', "pipeline/05_pull_tool2_from_kobo.R")

cat("Script saved.\n")
