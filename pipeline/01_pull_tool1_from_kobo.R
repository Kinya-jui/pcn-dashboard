# =============================================================================
# 01_pull_tool1_from_kobo.R
# PURPOSE: Pull raw facility assessment data from KoboToolbox Tool 1 (pcn_assessment_tool)
#          and save locally as consolidated_facility_data.csv
# =============================================================================

library(httr2)
library(jsonlite)
library(dplyr)
library(readr)

# --- Credentials (set these in .Renviron or GitHub Secrets) ---
KOBO_TOKEN        <- Sys.getenv("KOBO_TOKEN")
TOOL1_ASSET_UID   <- Sys.getenv("KOBO_TOOL1_UID")   # Facility Assessment Tool
KOBO_BASE         <- "https://eu.kobotoolbox.org/api/v2/assets"

#DEBUG
cat("TOKEN length:", nchar(KOBO_TOKEN), "\n")
cat("TOOL1 UID:", TOOL1_ASSET_UID, "\n")
cat("API URL will be:", paste0(KOBO_BASE, "/", TOOL1_ASSET_UID, "/data/"), "\n")

# --- Fetch all submissions from KoboToolbox ---
fetch_kobo_all <- function(asset_uid, token) {
  all_results <- list()
  url <- paste0(KOBO_BASE, "/", asset_uid, "/data/")
  
  repeat {
    resp <- request(url) |>
      req_url_query(format = "json", limit = 30000) |>
      req_headers(Authorization = paste("Token", token)) |>
      req_perform()
    
    body    <- resp_body_json(resp, simplifyVector = TRUE)
    results <- body$results
    
    if (length(results) == 0) break
    all_results <- c(all_results, list(as.data.frame(results)))
    
    if (is.null(body$`next`) || is.na(body$`next`)) break
    url <- body$`next`
  }
  
  bind_rows(all_results)
}

cat("Fetching Tool 1 (Facility Assessment) data from KoboToolbox...\n")
raw_data <- fetch_kobo_all(TOOL1_ASSET_UID, KOBO_TOKEN)
cat(sprintf("  Fetched %d facility submissions\n", nrow(raw_data)))

# --- Rename KoboToolbox internal names to match the R analysis script ---
# KoboToolbox prefixes grouped questions with group name (e.g. "group_a/q1")
# Strip group prefixes if present
names(raw_data) <- gsub("^[^/]+/", "", names(raw_data))

# The analysis script expects: county, subcounty, q1, q3, q4, q5, q6, q8, q8_a,
# q9_a..q9_e, q11, q12, q13_a..q13_c, q14_a, q15, q16, q17, q18..q42_11
# These should already match if your Tool 1 question names match the analysis script columns.
# Fix duplicate column names by making them unique
names(raw_data) <- make.unique(names(raw_data), sep = "_")

# Flatten any nested list/matrix columns to simple strings
for (col in names(raw_data)) {
  if (is.list(raw_data[[col]])) {
    raw_data[[col]] <- sapply(raw_data[[col]], function(x) {
      if (is.null(x) || length(x) == 0) NA_character_
      else paste(unlist(x), collapse = "; ")
    })
  }
}
# Save to disk for the analysis script to consume
output_path <- "data/consolidated_facility_data.csv"
dir.create("data", showWarnings = FALSE)
write_csv(raw_data, output_path)
cat(sprintf("  Saved to %s\n", output_path))
