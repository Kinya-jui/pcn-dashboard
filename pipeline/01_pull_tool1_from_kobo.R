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

# DEBUG
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

# Strip group prefixes
names(raw_data) <- gsub("^[^/]+/", "", names(raw_data))

# Make column names unique
names(raw_data) <- make.unique(names(raw_data), sep = "_")

# Flatten list columns safely - column by column
for (col in names(raw_data)) {
  if (is.list(raw_data[[col]])) {
    flat <- vector("character", nrow(raw_data))
    for (i in seq_len(nrow(raw_data))) {
      val <- raw_data[[col]][[i]]
      if (is.null(val) || length(val) == 0 || all(is.na(val))) {
        flat[i] <- NA_character_
      } else {
        flat[i] <- paste(unlist(val), collapse = "; ")
      }
    }
    raw_data[[col]] <- flat
  }
}

# Save
output_path <- "data/consolidated_facility_data.csv"
dir.create("data", showWarnings = FALSE)
write_csv(raw_data, output_path)
cat(sprintf("  Saved to %s\n", output_path))
