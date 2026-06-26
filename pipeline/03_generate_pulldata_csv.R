# =============================================================================
# 03_generate_pulldata_csv.R
# PURPOSE: Generate the KoboToolbox "pull data" CSV lookup file.
#
# HOW PULL DATA WORKS IN KOBOTOOLBOX:
#   1. This script creates a CSV with one row per PCN
#   2. The CSV is uploaded to Tool 2 as a "pull data" file (Media tab)
#   3. In the XLSForm, each auto-filled question uses:
#        calculation = pulldata('pcn_lookup', 'tool2_field', 'pcn_key', ${facility})
#   4. When the enumerator selects a PCN → KoboToolbox looks up that PCN
#      in the CSV → pre-fills all mapped fields automatically
#
# KEY COLUMN: 'pcn_key' must EXACTLY match the choice names in the
#             'pcn' choices list of Tool 2's XLSForm.
# =============================================================================

library(dplyr)
library(readr)
library(stringr)

cat("Generating KoboToolbox pull data CSV...\n")

# Load the analysis output (already renamed to Tool 2 field names)
pcn_data <- read_csv("data/pcn_tool2_ready.csv", show_col_types = FALSE)

# =============================================================================
# TOOL 2 FIELD COLUMNS TO INCLUDE IN LOOKUP
# These are the exact question names from pcn_tool.xlsx that will be auto-filled
# =============================================================================
AUTOFILL_FIELDS <- c(
  # Support Supervision
  "no_hfs_support",
  # Health Care Financing
  "hfs_empaneled_sha",
  "claims_reimbursed_hfs",
  "clients_access_shif",
  "clients_access_cash",
  "fif_collected_rollback",
  "people_waived_userfees",
  "userfees_total_waived",
  # Capacity Readiness
  "facilities_22pharma_avail",
  "facilities_23nonpharma_avail",
  "blood_availability_hospitals",
  "stockout_22pharma_7days_month",
  "stockout_23nonpharma_7days_months",
  "hospitals_comp_lab_services",
  "spokes_basic_lab_services",
  "allbasic_tracer_equipments",
  # Infrastructure
  "hfs_accessible_road",
  "hfs_wash_facilities",
  "hfs_tracer_infra_keph",
  "hfs_reliable_power",
  # HMIS
  "hfs_reliable_internet",
  "hfs_opd_tools_pcn",
  "hfs_integrated_emr",
  # HRH
  "hcws_skills_training_2yrs",
  # Quality of Care
  "hospitals_qit_functional",
  "spokes_wit_functional",
  "ipc_items_availability",
  "clinical_guidelines_adherence",
  "absenteesm_phc_facilities",
  # Social Accountability
  "facilities_client_survey"
)

# =============================================================================
# BUILD THE LOOKUP CSV
# The 'pcn_key' column is what KoboToolbox matches against ${facility}
# It must match EXACTLY the choice 'name' values in your Tool 2 XLSForm
# =============================================================================

# Create pcn_key by normalising pcn_name to match Tool 2 choice names
# KoboToolbox choice names are typically lowercase with underscores
make_pcn_key <- function(x) {
  x %>%
    tolower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("^_|_$", "")
}

# Select only columns that exist in the data
available_fields <- intersect(AUTOFILL_FIELDS, names(pcn_data))
missing_fields   <- setdiff(AUTOFILL_FIELDS, names(pcn_data))

if (length(missing_fields) > 0) {
  cat("  NOTE: These fields are not in the analysis output (will be NA):\n")
  cat(paste0("    ", missing_fields, collapse = "\n"), "\n")
}

pulldata_csv <- pcn_data %>%
  select(county, subcounty, all_of(available_fields)) %>%
  mutate(
    # pcn_key: must match Tool 2 XLSForm choice names for the 'facility' question
    pcn_key     = make_pcn_key(subcounty),
    # Keep human-readable labels for reference
    county_key  = make_pcn_key(county),
    subcounty_key = make_pcn_key(subcounty)
  ) %>%
  # Add missing fields as NA columns
  bind_cols(
    setNames(
      lapply(missing_fields, function(f) rep(NA_real_, nrow(.))),
      missing_fields
    )
  ) %>%
  # Round all numerics to 1 decimal place
  mutate(across(where(is.numeric), ~ round(.x, 1))) %>%
  # Put key column first
  # TO:
select(any_of(c("pcn_key", "pcn_label", "county_key", "subcounty_key", 
                 "county", "subcounty")), everything())

cat(sprintf("  Built lookup table: %d PCNs × %d fields\n",
            nrow(pulldata_csv), length(AUTOFILL_FIELDS)))

# Save
write_csv(pulldata_csv, "data/pcn_lookup.csv")
cat("  Saved → data/pcn_lookup.csv\n")
cat("\n  NEXT STEP: Upload data/pcn_lookup.csv to Tool 2 on KoboToolbox:\n")
cat("    KoboToolbox → your PCN Monitoring form → Settings → Media → Add file\n")
cat("    Filename must be exactly: pcn_lookup.csv\n")

# =============================================================================
# PRINT THE XLSForm CALCULATION FORMULAS
# Copy these into the 'calculation' column of your Tool 2 XLSForm survey sheet
# for each question that should be auto-filled
# =============================================================================
cat("\n  XLSForm pulldata() formulas to add to Tool 2:\n")
cat("  (Add these in the 'calculation' column, set 'type' to 'calculate')\n\n")

for (field in available_fields) {
  formula <- sprintf(
    "  %-45s = pulldata('pcn_lookup', '%s', 'pcn_key', ${facility})",
    field, field
  )
  cat(formula, "\n")
}
