# =============================================================================
# 01_pull_tool1_from_kobo.R
# =============================================================================
library(httr2)
library(jsonlite)
library(dplyr)
library(readr)

KOBO_TOKEN      <- Sys.getenv("KOBO_TOKEN")
TOOL1_ASSET_UID <- Sys.getenv("KOBO_TOOL1_UID")
KOBO_BASE       <- "https://eu.kobotoolbox.org/api/v2/assets"

cat("TOKEN length:", nchar(KOBO_TOKEN), "\n")
cat("TOOL1 UID: ***\n")
cat("API URL will be:", paste0(KOBO_BASE, "/", TOOL1_ASSET_UID, "/data/"), "\n")

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
    
    # Flatten each submission individually
    flat_list <- lapply(results, function(submission) {
      flat <- list()
      for (key in names(submission)) {
        val <- submission[[key]]
        if (is.null(val)) {
          flat[[key]] <- NA_character_
        } else if (is.list(val) || is.data.frame(val)) {
          # Collapse nested structures to string
          flat[[key]] <- tryCatch(
            paste(unlist(val), collapse = "; "),
            error = function(e) NA_character_
          )
        } else {
          flat[[key]] <- as.character(val)
        }
      }
      as.data.frame(flat, stringsAsFactors = FALSE, check.names = FALSE)
    })
    
    all_results <- c(all_results, flat_list)
    
    if (is.null(body$`next`) || is.na(body$`next`)) break
    url <- body$`next`
  }
  
  bind_rows(all_results)
}

cat("Fetching Tool 1 (Facility Assessment) data from KoboToolbox...\n")
raw_data <- fetch_kobo_all(TOOL1_ASSET_UID, KOBO_TOKEN)
cat(sprintf("  Fetched %d facility submissions\n", nrow(raw_data)))

# Strip group prefixes and deduplicate column names
names(raw_data) <- gsub("^[^/]+/", "", names(raw_data))
names(raw_data) <- make.unique(names(raw_data), sep = "_")

# Save
output_path <- "data/consolidated_facility_data.csv"
dir.create("data", showWarnings = FALSE)
write_csv(raw_data, output_path)
cat(sprintf("  Saved to %s\n", output_path))
