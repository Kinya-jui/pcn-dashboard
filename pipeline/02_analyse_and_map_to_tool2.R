# =============================================================================
# 02_analyse_and_map_to_tool2.R
# PURPOSE: Run all indicator computations on Tool 1 facility data,
#          aggregate to PCN/subcounty level, and rename every column
#          to the EXACT Tool 2 question name using the confirmed mapping.
#
# MAPPING SOURCE: analysis_result_to_tool_2_codes.xlsx
# COLUMN NAMES: Updated to match actual Kobo export variable names
#               (confirmed from debug log of consolidated_facility_data.csv)
# =============================================================================

library(dplyr)
library(readr)
library(tidyr)
library(stringr)

cat("Reading facility data...\n")
pcn_data <- read_csv("data/consolidated_facility_data.csv", show_col_types = FALSE)
# Normalise column names — map known variants to expected names
col_aliases <- c(
  "tracer_stock_out_nonpharma" = "tracer_stock_out_nonpharma",
  "tracer_stockout_nonpharma"  = "tracer_stock_out_nonpharma",
  "stockout_nonpharma"         = "tracer_stock_out_nonpharma",
  "tracer_nphe"                = "tracer_nphe",
  "tracer_nophe"               = "tracer_nphe",
  "non_pharma"                 = "tracer_nphe"
)
for (alias in names(col_aliases)) {
  target <- col_aliases[[alias]]
  if (alias %in% names(pcn_data) && !target %in% names(pcn_data)) {
    pcn_data <- pcn_data %>% rename(!!target := !!alias)
  }
}
cat(sprintf("  %d facility rows, %d counties\n",
            nrow(pcn_data), n_distinct(pcn_data$county, na.rm = TRUE)))

# --- Helper functions ---
tot   <- function(x) sum(as.numeric(x), na.rm = TRUE)
avg   <- function(x) round(mean(as.numeric(x), na.rm = TRUE), 1)

# n_yes: works on YES/NO text columns
n_yes <- function(x) sum(tolower(trimws(as.character(x))) == "yes", na.rm = TRUE)

# n_yes_num: works on already-numeric 1/0 columns
n_yes_num <- function(x) sum(as.numeric(x) == 1, na.rm = TRUE)

# =============================================================================
# STEP 1: Compute all analysis outputs at subcounty (PCN) level
#         NOTE: Tool 1 has no dedicated pcn_name column — grouping by subcounty
#               which maps 1:1 to a PCN in the PCN structure
# =============================================================================

cat("Computing PCN-level aggregates...\n")

pcn_results <- pcn_data %>%
  group_by(county, subcounty) %>%
  summarise(
    total_facilities = n(),

    # ── 0. Support Supervision ───────────────────────────────────────────────
    # Tool 1 output: have_support_supervision  → Tool 2: no_hfs_support
    # COUNT of facilities that received support supervision
    have_support_supervision              = n_yes(support_supervision),

    # ── 1. SHA Empanelment ───────────────────────────────────────────────────
    # Tool 1: percent_offering_sha_empanelment → Tool 2: hfs_empaneled_sha
    percent_offering                      = round(n_yes(sha_empanelment) / n() * 100),
    percent_offering_sha_empanelment      = round(n_yes(sha_empanelment) / n() * 100),

    # ── 2. SHA Reimbursement ─────────────────────────────────────────────────
    # Tool 1: Proportion_of_SHA_reimbursement → Tool 2: claims_reimbursed_hfs
    Proportion_of_SHA_reimbursement       = avg(sha_reimbursed),

    # ── 3. SHIF Access ───────────────────────────────────────────────────────
    # Tool 1: Proportion_of_clients_accessing_Health_Services_using_SHIF → Tool 2: clients_access_shif
    Proportion_of_clients_accessing_Health_Services_using_SHIF = avg(shif_clients),

    # ── 4. User Fee Waivers (count) ──────────────────────────────────────────
    # Tool 1: Total_clients_waived → Tool 2: people_waived_userfees
    Total_clients_waived                  = tot(clients_waived),

    # ── 5. Essential Medicines Availability ──────────────────────────────────
    # Tool 1: essential_medicines_availability_on_day_of_visit → Tool 2: facilities_22pharma_avail
    # tracer_meds column holds the composite score directly
    essential_medicines_availability_on_day_of_visit = avg(tracer_meds),

    # ── 6. Non-pharma Availability ───────────────────────────────────────────
    # Tool 1: non_pharms_availability_on_day_of_visit → Tool 2: facilities_23nonpharma_avail
    # tracer_nphe column holds the composite score directly
    non_pharms_availability_on_day_of_visit = if ("tracer_nphe" %in% names(pcn_data))
                                            avg(tracer_nphe)
                                          else if ("tracer_nophe" %in% names(pcn_data))
                                            avg(tracer_nophe)
                                          else NA_real_,

    # ── 7. Blood Availability ────────────────────────────────────────────────
    # Tool 1: Availability_of_the_whole_blood_and_blood_components → Tool 2: blood_availability_hospitals
    # Not captured in Tool 1 facility assessment — filled manually in Tool 2
    Availability_of_the_whole_blood_and_blood_components = NA_real_,

    # ── 8. Essential Medicines Stock-out ─────────────────────────────────────
    # Tool 1: essential_medicines_stock_out → Tool 2: stockout_22pharma_7days_month
    essential_medicines_stock_out         = avg(tracer_stock_out_meds),

    # ── 9. Non-pharma Stock-out ──────────────────────────────────────────────
    # Tool 1: non_pharms_stock_out → Tool 2: stockout_23nonpharma_7days_months
    non_pharms_stock_out                  = avg(tracer_stock_out_nonpharma),

    # ── 10. Comprehensive Lab (hospitals) ────────────────────────────────────
    # Tool 1: percent_offering_lab → Tool 2: hospitals_comp_lab_services
    # lab_present = YES/NO whether lab services are available
    percent_offering_lab                  = round(n_yes(lab_present) / n() * 100),

    # ── 11. Basic Lab (spokes/PHC) ───────────────────────────────────────────
    # Tool 1: phc_percent_offering_lab → Tool 2: spokes_basic_lab_services
    # Using same lab_present as proxy for PHC-level basic lab
    phc_percent_offering_lab              = round(n_yes(lab_present) / n() * 100),

    # ── 12. Basic Tracer Equipment ───────────────────────────────────────────
    # Tool 1: basic_equipment_availability → Tool 2: allbasic_tracer_equipments
    # equipment_available holds the composite equipment score
    basic_equipment_availability          = avg(equipment_available),

    # ── 13. Clients Paying Cash ──────────────────────────────────────────────
    # Tool 1: Proportions_paid_through_cash → Tool 2: clients_access_cash
    # Not directly captured in Tool 1 — set NA, filled manually in Tool 2
    Proportions_paid_through_cash         = NA_real_,

    # ── 14. FIF Rollback ─────────────────────────────────────────────────────
    # Tool 1: Proportion_of_FIF_collected_rolled_back_to_the_facilities_within_PCN
    #       → Tool 2: fif_collected_rollback
    # prop_fif = proportion of FIF rolled back
    Proportion_of_FIF_collected_rolled_back_to_the_facilities_within_PCN = avg(prop_fif),

    # ── 15. People Waived (proportion) ───────────────────────────────────────
    # Tool 1: Proportion_of_people_waived_for_user_fees_in_HFs_within_the_PCN
    #       → Tool 2: people_waived_userfees
    Proportion_of_people_waived_for_user_fees_in_HFs_within_the_PCN = avg(clients_waived),

    # ── 16. Total Amount Waived ──────────────────────────────────────────────
    # Tool 1: Total_amount_user_fee_waived_in_PCN → Tool 2: userfees_total_waived
    # user_fee = total amount of user fees waived
    Total_amount_user_fee_waived_in_PCN   = tot(user_fee),

    # ── 17. Road Network ─────────────────────────────────────────────────────
    # Tool 1: percentage_having_reliable_road_network → Tool 2: hfs_accessible_road
    # No dedicated road column found — using conduct_ward_rounds as proxy
    # (facilities doing ward rounds implies road access)
    percentage_having_reliable_road_network = round(n_yes(conduct_ward_rounds) / n() * 100),

    # ── 18. WASH ─────────────────────────────────────────────────────────────
    # Tool 1: WASH_Average_Percentage → Tool 2: hfs_wash_facilities
    # Average across 4 WASH components
    WASH_Average_Percentage               = round(
      rowMeans(
        pcn_data[pcn_data$subcounty == subcounty[1],
                 intersect(c("toilet_available","handwashing_place",
                             "handwashing_place_features","lpc_and_hygiene_items"),
                           names(pcn_data))] %>%
          mutate(across(everything(), ~ as.numeric(tolower(trimws(.)) == "yes"))),
        na.rm = TRUE
      ) %>% mean(na.rm = TRUE) * 100
    ),

    # ── 19. Infrastructure Items ─────────────────────────────────────────────
    # Tool 1: infrastructure_items_availability → Tool 2: hfs_tracer_infra_keph
    # infrastructure_items = composite infrastructure score
    infrastructure_items_availability     = avg(infrastructure_items),

    # ── 20. Reliable Power ───────────────────────────────────────────────────
    # Tool 1: percentage_having_reliable_power → Tool 2: hfs_reliable_power
    percentage_having_reliable_power      = round(n_yes(power) / n() * 100),

    # ── 21. Reliable Internet ────────────────────────────────────────────────
    # Tool 1: percentage_having_reliable_internet → Tool 2: hfs_reliable_internet
    percentage_having_reliable_internet   = round(n_yes(internet) / n() * 100),

    # ── 22. OPD Reporting Tools ──────────────────────────────────────────────
    # Tool 1: reportingtools_availability → Tool 2: hfs_opd_tools_pcn
    # opd_reporting_tools = composite reporting tools score
    reportingtools_availability           = avg(opd_reporting_tools),

    # ── 23. Functional EMR ───────────────────────────────────────────────────
    # Tool 1: percentage_having_functional_EMR → Tool 2: hfs_integrated_emr
    # Not directly captured in Tool 1 — set NA, filled manually in Tool 2
    percentage_having_functional_EMR      = NA_real_,

    # ── 24. HCW Skills Training ──────────────────────────────────────────────
    # Tool 1: Health_care_workers_undergone_skills_competency_course → Tool 2: hcws_skills_training_2yrs
    # competency_building = number/proportion of HCWs trained
    Health_care_workers_undergone_skills_competency_course = avg(competency_building),

    # ── 25. QIT ──────────────────────────────────────────────────────────────
    # Tool 1: qit_team_percentage → Tool 2: hospitals_qit_functional
    # qit_team = YES/NO whether QIT team is functional
    qit_team_percentage                   = round(n_yes(qit_team) / n() * 100),

    # ── 26. WIT ──────────────────────────────────────────────────────────────
    # Tool 1: wit_team_percentage → Tool 2: spokes_wit_functional
    # wit_team = YES/NO whether WIT team is functional
    wit_team_percentage                   = round(n_yes(wit_team) / n() * 100),

    # ── 27. IPC Average ──────────────────────────────────────────────────────
    # Tool 1: IPC_avg → Tool 2: ipc_items_availability
    # lpc_and_hygiene_items used as IPC proxy from WASH section
    IPC_avg                               = round(
      mean(as.numeric(tolower(trimws(as.character(lpc_and_hygiene_items))) == "yes"),
           na.rm = TRUE) * 100
    ),

    # ── 28. Clinical Guidelines Adherence ────────────────────────────────────
    # Tool 1: Clinical_adherence → Tool 2: clinical_guidelines_adherence
    # clinical_guidelines_adherence = direct score from Tool 1
    Clinical_adherence                    = avg(clinical_guidelines_adherence),

    # ── 29. Staff Absenteeism ────────────────────────────────────────────────
    # Tool 1: Proportion_of_staff_absent → Tool 2: absenteesm_phc_facilities
    # absenteeism = direct absenteeism score; present = staff present count
    # Use absenteeism directly if available, else derive as 100 - avg(present/expected*100)
    Proportion_of_staff_absent            = if ("absenteeism" %in% names(pcn_data))
                                              avg(absenteeism)
                                            else
                                              round(100 - avg(present / expected * 100)),

    # ── 30. Client Satisfaction Survey ───────────────────────────────────────
    # Tool 1: percent_done_client_satisfaction_survey → Tool 2: facilities_client_survey
    # client_satisfaction = score or YES/NO for satisfaction survey done
    percent_done_client_satisfaction_survey = avg(client_satisfaction),

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
