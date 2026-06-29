# =============================================================================
# 04_upload_pulldata_to_kobo.R
# PURPOSE: Automatically upload pcn_lookup.csv to KoboToolbox Tool 2
#          via the Media API so the pulldata() function in the XLSForm
#          always has fresh data after each nightly pipeline run.
# =============================================================================

library(httr2)
library(readr)

KOBO_TOKEN     <- Sys.getenv("KOBO_TOKEN")
KOBO_TOOL2_UID <- Sys.getenv("KOBO_TOOL2_UID")
KOBO_BASE      <- "https://eu.kobotoolbox.org/api/v2/assets"
LOOKUP_FILE    <- "data/pcn_lookup.csv"
LOOKUP_NAME    <- "pcn_lookup.csv"   # must match exactly what's in pulldata() formula

cat("Uploading pull data CSV to KoboToolbox Tool 2...\n")

if (!file.exists(LOOKUP_FILE)) stop("pcn_lookup.csv not found. Run 03_generate_pulldata_csv.R first.")

# =============================================================================
# STEP 1: Check if the file already exists on KoboToolbox
#         If yes → delete it first (KoboToolbox doesn't allow overwrite)
# =============================================================================
media_url <- paste0(KOBO_BASE, "/", KOBO_TOOL2_UID, "/files/")

existing <- tryCatch({
  resp <- request(media_url) |>
    req_headers(Authorization = paste("Token", KOBO_TOKEN)) |>
    req_perform()
  resp_body_json(resp, simplifyVector = TRUE)$results
}, error = function(e) {
  cat("  Could not check existing files:", conditionMessage(e), "\n")
  NULL
})

# Find and delete existing pcn_lookup.csv if present
if (!is.null(existing) && length(existing) > 0) {
  existing_df <- as.data.frame(existing)
  if ("metadata" %in% names(existing_df)) {
    # Extract filename from metadata
    for (i in seq_len(nrow(existing_df))) {
      meta <- existing_df$metadata[[i]]
      fname <- if (is.list(meta)) meta$filename else meta
      if (!is.null(fname) && fname == LOOKUP_NAME) {
        delete_url <- existing_df$url[i]
        cat(sprintf("  Deleting existing %s...\n", LOOKUP_NAME))
        tryCatch({
          request(delete_url) |>
            req_headers(Authorization = paste("Token", KOBO_TOKEN)) |>
            req_method("DELETE") |>
            req_perform()
          cat("  Deleted.\n")
        }, error = function(e) {
          cat("  Delete failed:", conditionMessage(e), "\n")
        })
        break
      }
    }
  }
}

# =============================================================================
# STEP 2: Upload the new pcn_lookup.csv
# =============================================================================
cat(sprintf("  Uploading %s...\n", LOOKUP_NAME))

upload_resp <- tryCatch({
  request(media_url) |>
    req_headers(Authorization = paste("Token", KOBO_TOKEN)) |>
    req_body_multipart(
      content      = curl::form_file(LOOKUP_FILE, type = "text/csv"),
      file_type    = "form_media"
    ) |>
    req_perform()
}, error = function(e) {
  cat("  Upload failed:", conditionMessage(e), "\n")
  NULL
})

if (!is.null(upload_resp)) {
  status <- resp_status(upload_resp)
  if (status %in% c(200, 201)) {
    cat(sprintf("  Successfully uploaded %s (HTTP %d)\n", LOOKUP_NAME, status))
    cat("  KoboCollect will use this file for pulldata() lookups immediately.\n")
  } else {
    cat(sprintf("  Upload returned HTTP %d\n", status))
    cat("  Response:", resp_body_string(upload_resp), "\n")
  }
}

cat("\nDone.\n")
