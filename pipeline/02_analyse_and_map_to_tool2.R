# =============================================================================
# 02_analyse_and_map_to_tool2.R
# PURPOSE: Run all indicator computations on Tool 1 facility data,
#          aggregate to PCN/subcounty level, and rename every column
#          to the EXACT Tool 2 question name using the confirmed mapping.
#
# MAPPING SOURCE: analysis_result_to_tool_2_codes.xlsx
# =============================================================================

library(dplyr)
library(readr)
library(tidyr)
library(stringr)

cat("Reading facility data...\n")
pcn_data <- read_csv("data/consolidated_facility_data.csv", show_col_types = FALSE)
cat("=== ALL COLUMN NAMES ===\n")
cat(paste(seq_along(names(pcn_data)), names(pcn_data), sep = ": ", collapse = "\n"), "\n")
stop("DEBUG STOP - check column names above")

cat(sprintf("  %d facility rows, %d counties\n",
            nrow(pcn_data), n_distinct(pcn_data$county, na.rm = TRUE)))

# --- YES/NO column conversion ---
yes_no_cols <- c("q1","q3","q14_a","q18","q19","q20","q204",
                 "q204_1","q204_2","q204_3","q204_4","q204_5",
                 "q204_6","q204_7","q204_8","q204_9","q204_10","q204_11",
                 "q25","q28","q29","q30","q31","q32","q34",
                 "q35","q37","q38","q39","q40","q41")
yes_no_cols <- intersect(yes_no_cols, names(pcn_data))
pcn_data[yes_no_cols] <- lapply(pcn_data[yes_no_cols], function(x) {
  ifelse(tolower(trimws(as.character(x))) == "yes", 1,
         ifelse(tolower(trimws(as.character(x))) == "no", 0, NA))
})

# =============================================================================
# STEP 1: Compute all analysis outputs at PCN level
#         Column names here match the "Tool 1 Analysis Output" column exactly
# =============================================================================

cat("Computing PCN-level aggregates...\n")

pct  <- function(x) round(mean(as.numeric(x), na.rm = TRUE) * 100, 1)
tot  <- function(x) sum(as.numeric(x), na.rm = TRUE)
avg  <- function(x) round(mean(as.numeric(x), na.rm = TRUE), 1)
n_yes <- function(x) sum(x == 1, na.rm = TRUE)

# Helper: IPC average across three IPC items (q36_1, q36_2, q36_3)
ipc_avg <- function(df) {
  cols <- intersect(c("q36_1","q36_2","q36_3"), names(df))
  if (length(cols) == 0) return(NA_real_)
  round(rowMeans(df[cols] %>% mutate(across(everything(), as.numeric)),
                 na.rm = TRUE) %>% mean(na.rm = TRUE) * 100, 1)
}

pcn_results <- pcn_data %>%
  group_by(county, subcounty, pcn_name) %>%
  summarise(
    total_facilities = n(),

    # ── 0. Support Supervision ───────────────────────────────────────────────
    # Tool 1 output: have_support_supervision  → Tool 2: no_hfs_support
    # This is a COUNT (number of facilities with support supervision)
    have_support_supervision              = n_yes(q1),

    # ── 1. SHA Empanelment ───────────────────────────────────────────────────
    # Tool 1: percent_offering / percent_offering_sha_empanelment → hfs_empaneled_sha
    percent_offering                      = round(n_yes(q3) / n() * 100),
    percent_offering_sha_empanelment      = round(n_yes(q3) / n() * 100),

    # ── 2. SHA Reimbursement ─────────────────────────────────────────────────
    # Tool 1: Proportion_of_SHA_reimbursement → claims_reimbursed_hfs
    Proportion_of_SHA_reimbursement       = avg(q5),

    # ── 3. SHIF Access ───────────────────────────────────────────────────────
    # Tool 1: Proportion_of_clients_accessing_Health_Services_using_SHIF → clients_access_shif
    Proportion_of_clients_accessing_Health_Services_using_SHIF = avg(q4),

    # ── 4. User Fee Waivers (count) ──────────────────────────────────────────
    # Tool 1: Total_clients_waived → people_waived_userfees
    Total_clients_waived                  = tot(q6),

    # ── 5. Essential Medicines Availability ──────────────────────────────────
    # Tool 1: essential_medicines_availability_on_day_of_visit → facilities_22pharma_avail
    essential_medicines_availability_on_day_of_visit = avg(
      rowSums(across(any_of(paste0("q22_", 1:23)), as.numeric), na.rm = TRUE) /
        23 * 100
    ),

    # ── 6. Non-pharma Availability ───────────────────────────────────────────
    # Tool 1: non_pharms_availability_on_day_of_visit → facilities_23nonpharma_avail
    non_pharms_availability_on_day_of_visit = avg(
      rowSums(across(any_of(paste0("q24_", 1:23)), as.numeric), na.rm = TRUE) /
        23 * 100
    ),

    # ── 7. Blood Availability ────────────────────────────────────────────────
    # Tool 1: Availability_of_the_whole_blood_and_blood_components → blood_availability_hospitals
    # Not directly in Tool 1 facility assessment — set NA, filled manually in Tool 2
    Availability_of_the_whole_blood_and_blood_components = NA_real_,

    # ── 8. Essential Medicines Stock-out ─────────────────────────────────────
    # Tool 1: essential_medicines_stock_out → stockout_22pharma_7days_month
    essential_medicines_stock_out         = avg(
      rowSums(across(any_of(paste0("q23_", 1:23)), as.numeric), na.rm = TRUE) /
        23 * 100
    ),

    # ── 9. Non-pharma Stock-out ──────────────────────────────────────────────
    # Tool 1: non_pharms_stock_out → stockout_23nonpharma_7days_months
    non_pharms_stock_out                  = avg(
      rowSums(across(any_of(paste0("q25_", 1:23)), as.numeric), na.rm = TRUE) /
        23 * 100
    ),

    # ── 10. Comprehensive Lab (hospitals) ────────────────────────────────────
    # Tool 1: percent_offering_lab → hospitals_comp_lab_services
    percent_offering_lab                  = round(n_yes(q204) / n() * 100),

    # ── 11. Basic Lab (spokes/PHC) ───────────────────────────────────────────
    # Tool 1: phc_percent_offering_lab → spokes_basic_lab_services
    # Using same q204 as proxy for PHC-level basic lab
    phc_percent_offering_lab              = round(n_yes(q204) / n() * 100),

    # ── 12. Basic Tracer Equipment ───────────────────────────────────────────
    # Tool 1: basic_equipment_availability → allbasic_tracer_equipments
    basic_equipment_availability          = avg(
      rowSums(across(any_of(paste0("q21_", 1:13)), as.numeric), na.rm = TRUE) /
        13 * 100
    ),

    # ── 13. Clients Paying Cash ──────────────────────────────────────────────
    # Tool 1: Proportions_paid_through_cash → clients_access_cash
    Proportions_paid_through_cash         = avg(
      if ("q43_3" %in% names(cur_data())) q43_3 else NA_real_
    ),

    # ── 14. FIF Rollback ─────────────────────────────────────────────────────
    # Tool 1: Proportion_of_FIF_collected_rolled_back_to_the_facilities_within_PCN
    #       → fif_collected_rollback
    Proportion_of_FIF_collected_rolled_back_to_the_facilities_within_PCN = avg(q8),

    # ── 15. People Waived (proportion) ───────────────────────────────────────
    # Tool 1: Proportion_of_people_waived_for_user_fees_in_HFs_within_the_PCN
    #       → people_waived_userfees
    Proportion_of_people_waived_for_user_fees_in_HFs_within_the_PCN = avg(q6),

    # ── 16. Total Amount Waived ──────────────────────────────────────────────
    # Tool 1: Total_amount_user_fee_waived_in_PCN → userfees_total_waived
    Total_amount_user_fee_waived_in_PCN   = tot(q8_a),

    # ── 17. Road Network ─────────────────────────────────────────────────────
    # Tool 1: percentage_having_reliable_road_network → hfs_accessible_road
    percentage_having_reliable_road_network = round(n_yes(q28) / n() * 100),

    # ── 18. WASH ─────────────────────────────────────────────────────────────
    # Tool 1: WASH_Average_Percentage → hfs_wash_facilities
    WASH_Average_Percentage               = round(n_yes(q35) / n() * 100),

    # ── 19. Infrastructure Items ─────────────────────────────────────────────
    # Tool 1: infrastructure_items_availability → hfs_tracer_infra_keph
    infrastructure_items_availability     = avg(
      rowSums(across(any_of(paste0("q33_", 1:23)), as.numeric), na.rm = TRUE) /
        23 * 100
    ),

    # ── 20. Reliable Power ───────────────────────────────────────────────────
    # Tool 1: percentage_having_reliable_power → hfs_reliable_power
    percentage_having_reliable_power      = round(n_yes(q32) / n() * 100),

    # ── 21. Reliable Internet ────────────────────────────────────────────────
    # Tool 1: percentage_having_reliable_internet → hfs_reliable_internet
    percentage_having_reliable_internet   = round(n_yes(q30) / n() * 100),

    # ── 22. OPD Reporting Tools ──────────────────────────────────────────────
    # Tool 1: reportingtools_availability → hfs_opd_tools_pcn
    reportingtools_availability           = avg(
      rowSums(across(any_of(paste0("q42_", 1:6)), as.numeric), na.rm = TRUE) /
        6 * 100
    ),

    # ── 23. Functional EMR ───────────────────────────────────────────────────
    # Tool 1: percentage_having_functional_EMR → hfs_integrated_emr
    percentage_having_functional_EMR      = round(n_yes(q31) / n() * 100),

    # ── 24. HCW Skills Training ──────────────────────────────────────────────
    # Tool 1: Health_care_workers_undergone_skills_competency_course → hcws_skills_training_2yrs
    Health_care_workers_undergone_skills_competency_course = avg(q11),

    # ── 25. QIT ──────────────────────────────────────────────────────────────
    # Tool 1: qit_team_percentage → hospitals_qit_functional
    qit_team_percentage                   = round(n_yes(q39) / n() * 100),

    # ── 26. WIT ──────────────────────────────────────────────────────────────
    # Tool 1: wit_team_percentage → spokes_wit_functional
    wit_team_percentage                   = round(n_yes(q40) / n() * 100),

    # ── 27. IPC Average ──────────────────────────────────────────────────────
    # Tool 1: IPC_avg → ipc_items_availability
    IPC_avg                               = round(
      rowMeans(
        across(any_of(c("q36_1","q36_2","q36_3")), as.numeric),
        na.rm = TRUE
      ) %>% mean(na.rm = TRUE) * 100
    ),

    # ── 28. Clinical Guidelines Adherence ────────────────────────────────────
    # Tool 1: Clinical_adherence → clinical_guidelines_adherence
    Clinical_adherence                    = round(n_yes(q25) / n() * 100),

    # ── 29. Staff Absenteeism ────────────────────────────────────────────────
    # Tool 1: Proportion_of_staff_absent → absenteesm_phc_facilities
    # q12 = proportion present → absenteeism = 100 - present
    Proportion_of_staff_absent            = round(100 - avg(q12)),

    # ── 30. Client Satisfaction Survey ───────────────────────────────────────
    # Tool 1: percent_done_client_satisfaction_survey → facilities_client_survey
    percent_done_client_satisfaction_survey = round(n_yes(q18) / n() * 100),

    .groups = "drop"
  )

cat(sprintf("  Computed %d PCN rows\n", nrow(pcn_results)))

# =============================================================================
# STEP 2: Rename Tool 1 columns → Tool 2 field names
#         Based EXACTLY on analysis_result_to_tool_2_codes.xlsx
# =============================================================================

# Exact mapping from the xlsx file (Tool1_output → Tool2_field)
TOOL1_TO_TOOL2 <- c(
  "have_support_supervision"                                                   = "no_hfs_support",
  "percent_offering"                                                           = "hfs_empaneled_sha",
  "Proportion_of_SHA_reimbursement"                                            = "claims_reimbursed_hfs",
  "Proportion_of_clients_accessing_Health_Services_using_SHIF"                 = "clients_access_shif",
  "Total_clients_waived"                                                       = "people_waived_userfees",
  "essential_medicines_availability_on_day_of_visit"                          = "facilities_22pharma_avail",
  "non_pharms_availability_on_day_of_visit"                                   = "facilities_23nonpharma_avail",
  "Availability_of_the_whole_blood_and_blood_components"                      = "blood_availability_hospitals",
  "essential_medicines_stock_out"                                              = "stockout_22pharma_7days_month",
  "non_pharms_stock_out"                                                      = "stockout_23nonpharma_7days_months",
  "percent_offering_lab"                                                       = "hospitals_comp_lab_services",
  "phc_percent_offering_lab"                                                   = "spokes_basic_lab_services",
  "basic_equipment_availability"                                               = "allbasic_tracer_equipments",
  "Proportions_paid_through_cash"                                              = "clients_access_cash",
  "percent_offering_sha_empanelment"                                           = "hfs_empaneled_sha",
  "Proportion_of_FIF_collected_rolled_back_to_the_facilities_within_PCN"      = "fif_collected_rollback",
  "Proportion_of_people_waived_for_user_fees_in_HFs_within_the_PCN"           = "people_waived_userfees",
  "Total_amount_user_fee_waived_in_PCN"                                        = "userfees_total_waived",
  "percentage_having_reliable_road_network"                                    = "hfs_accessible_road",
  "WASH_Average_Percentage"                                                    = "hfs_wash_facilities",
  "infrastructure_items_availability"                                          = "hfs_tracer_infra_keph",
  "percentage_having_reliable_power"                                           = "hfs_reliable_power",
  "percentage_having_reliable_internet"                                        = "hfs_reliable_internet",
  "reportingtools_availability"                                                = "hfs_opd_tools_pcn",
  "percentage_having_functional_EMR"                                           = "hfs_integrated_emr",
  "Health_care_workers_undergone_skills_competency_course"                     = "hcws_skills_training_2yrs",
  "qit_team_percentage"                                                        = "hospitals_qit_functional",
  "wit_team_percentage"                                                        = "spokes_wit_functional",
  "IPC_avg"                                                                    = "ipc_items_availability",
  "Clinical_adherence"                                                         = "clinical_guidelines_adherence",
  "Proportion_of_staff_absent"                                                 = "absenteesm_phc_facilities",
  "percent_done_client_satisfaction_survey"                                    = "facilities_client_survey"
)

# Apply renaming — only rename columns that exist
rename_vec <- TOOL1_TO_TOOL2[names(TOOL1_TO_TOOL2) %in% names(pcn_results)]
pcn_tool2_ready <- pcn_results %>%
  rename(!!!rename_vec)

# Clamp all percentage columns to 0–100
pcn_tool2_ready <- pcn_tool2_ready %>%
  mutate(across(where(is.numeric), ~ pmin(pmax(.x, 0, na.rm = TRUE), 100)))

# Drop duplicate columns (hfs_empaneled_sha appeared twice in mapping)
pcn_tool2_ready <- pcn_tool2_ready[!duplicated(names(pcn_tool2_ready))]

cat(sprintf("  Final output: %d PCN rows, %d columns\n",
            nrow(pcn_tool2_ready), ncol(pcn_tool2_ready)))
cat("  Tool 2 field columns present:\n")
tool2_cols <- intersect(names(pcn_tool2_ready), unname(TOOL1_TO_TOOL2))
cat(paste0("    ", tool2_cols, collapse = "\n"), "\n")

# Save
dir.create("data", showWarnings = FALSE)
write_csv(pcn_tool2_ready, "data/pcn_tool2_ready.csv")
cat("  Saved → data/pcn_tool2_ready.csv\n")
