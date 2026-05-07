# =============================================================================
# 03_push_to_tool2.R
# PURPOSE: POST the aggregated PCN-level indicators to KoboToolbox Tool 2
#          (pcn_tool) so the Shiny dashboard can pull them via the REST API.
#
# KoboToolbox accepts submissions via POST to:
#   /api/v2/assets/{ASSET_UID}/data/
# Each submission is one JSON object where keys = question names in the XLSForm.
# =============================================================================

library(httr2)
library(jsonlite)
library(dplyr)
library(readr)
library(purrr)

KOBO_TOKEN      <- Sys.getenv("KOBO_TOKEN")
TOOL2_ASSET_UID <- Sys.getenv("KOBO_TOOL2_UID")    # PCN Monitoring Tool
KOBO_BASE       <- "https://kf.kobotoolbox.org/api/v2/assets"

# --- Load analysed data ---
pcn_data <- read_csv("data/pcn_aggregated_indicators.csv", show_col_types = FALSE)
cat(sprintf("Preparing to push %d PCN records to Tool 2...\n", nrow(pcn_data)))

# =============================================================================
# FIELD MAPPING: aggregated column → Tool 2 XLSForm question name
# These names must EXACTLY match the 'name' column in pcn_tool.xlsx survey sheet
# =============================================================================
field_map <- c(
  # Identity
  "counties"                          = "county",
  "facility"                          = "pcn_name",      # PCN selection field in Tool 2

  # Capacity Readiness
  "facilities_22pharma_avail"         = "facilities_22pharma_avail",
  "facilities_23nonpharma_avail"      = "facilities_23nonpharma_avail",
  "blood_availability_hospitals"      = "blood_availability_hospitals",
  "stockout_22pharma_7days_month"     = "stockout_22pharma_7days_month",
  "stockout_23nonpharma_7days_months" = "stockout_23nonpharma_7days_months",
  "hospitals_comp_lab_services"       = "hospitals_comp_lab_services",
  "spokes_basic_lab_services"         = "spokes_basic_lab_services",
  "allbasic_tracer_equipments"        = "allbasic_tracer_equipments",

  # Health Care Financing
  "clients_access_cash"               = "clients_access_cash",
  "clients_access_shif"               = "clients_access_shif",
  "hfs_empaneled_sha"                 = "hfs_empaneled_sha",
  "claims_reimbursed_hfs"             = "claims_reimbursed_hfs",
  "fif_collected_rollback"            = "fif_collected_rollback",
  "people_waived_userfees"            = "people_waived_userfees",
  "userfees_total_waived"             = "userfees_total_waived",

  # Infrastructure
  "hfs_accessible_road"               = "hfs_accessible_road",
  "hfs_wash_facilities"               = "hfs_wash_facilities",
  "hfs_tracer_infra_keph"             = "hfs_tracer_infra_keph",
  "hfs_reliable_power"                = "hfs_reliable_power",
  "hfs_staff_accommodation"           = "hfs_staff_accommodation",

  # HMIS
  "hfs_reliable_internet"             = "hfs_reliable_internet",
  "hfs_opd_tools_pcn"                 = "hfs_opd_tools_pcn",
  "hfs_integrated_emr"                = "hfs_integrated_emr",

  # HRH
  "hrhpop"                            = "hrhpop",
  "doctors_pop"                       = "doctors_pop",
  "nurses_pop"                        = "nurses_pop",
  "co_pop"                            = "co_pop",
  "chps_trained_basic"                = "chps_trained_basic",
  "hcws_sensitized_phc_pcn"           = "hcws_sensitized_phc_pcn",
  "hcws_skills_training_2yrs"         = "hcws_skills_training_2yrs",
  "absenteesm_phc_facilities"         = "absenteesm_phc_facilities",

  # Quality of Care
  "hospitals_qit_functional"          = "hospitals_qit_functional",
  "spokes_wit_functional"             = "spokes_wit_functional",
  "ipc_items_availability"            = "ipc_items_availability",
  "clinical_guidelines_adherence"     = "clinical_guidelines_adherence",
  "facilities_mpdsr_conducted"        = "facilities_mpdsr_conducted",
  "fresh_stillbirth_rate"             = "fresh_stillbirth_rate",
  "maternal_deaths_rate"              = "maternal_deaths_rate",
  "maternal_deaths_audited"           = "maternal_deaths_audited",
  "neonatal_deaths_rate"              = "neonatal_deaths_rate",
  "neonatal_deaths_audited"           = "neonatal_deaths_audited",
  "tb_treatment_success"              = "tb_treatment_success",

  # Social Accountability
  "facilities_client_survey"          = "facilities_client_survey",
  "facilities_functional_grms"        = "facilities_functional_grms"
)

# =============================================================================
# BUILD SUBMISSION PAYLOAD
# Convert each PCN row to a named list matching Tool 2 field names
# =============================================================================
build_submission <- function(row) {
  payload <- list()

  # Add pipeline-sourced fields
  for (tool2_field in names(field_map)) {
    source_col <- field_map[[tool2_field]]
    if (source_col %in% names(row)) {
      val <- row[[source_col]]
      # Skip NA values — KoboToolbox treats missing fields as not answered
      if (!is.na(val) && !is.null(val)) {
        payload[[tool2_field]] <- as.character(val)
      }
    }
  }

  # Mark as auto-submitted
  payload[["interviewername"]]   <- "AUTOMATED_PIPELINE"
  payload[["date.visit"]]        <- as.character(Sys.Date())
  payload[["tool_selection"]]    <- "pcn_tool"

  payload
}

# =============================================================================
# POST TO KOBO
# Uses the KoboToolbox v2 bulk submission endpoint
# =============================================================================
post_submission <- function(payload, asset_uid, token) {
  tryCatch({
    resp <- request(paste0(KOBO_BASE, "/", asset_uid, "/data/")) |>
      req_headers(
        Authorization  = paste("Token", token),
        `Content-Type` = "application/json"
      ) |>
      req_body_json(list(submission = payload)) |>
      req_perform()

    status <- resp_status(resp)
    if (status %in% c(200, 201)) {
      return(list(ok = TRUE, status = status))
    } else {
      return(list(ok = FALSE, status = status, body = resp_body_string(resp)))
    }
  }, error = function(e) {
    list(ok = FALSE, status = NA, error = conditionMessage(e))
  })
}

# =============================================================================
# SUBMIT ALL PCN ROWS
# =============================================================================
results <- map(seq_len(nrow(pcn_data)), function(i) {
  row     <- pcn_data[i, ]
  pcn_id  <- paste0(row$county, " / ", row$subcounty, " / ", row$pcn_name)
  payload <- build_submission(row)
  result  <- post_submission(payload, TOOL2_ASSET_UID, KOBO_TOKEN)

  if (result$ok) {
    cat(sprintf("  [OK]   %s\n", pcn_id))
  } else {
    cat(sprintf("  [FAIL] %s — HTTP %s\n", pcn_id, result$status))
    if (!is.null(result$body)) cat("         ", result$body, "\n")
  }
  result
})

n_ok   <- sum(map_lgl(results, "ok"))
n_fail <- length(results) - n_ok
cat(sprintf("\nDone: %d succeeded, %d failed\n", n_ok, n_fail))

if (n_fail > 0) {
  cat("Check logs above for failed submissions.\n")
  quit(status = 1)
}
