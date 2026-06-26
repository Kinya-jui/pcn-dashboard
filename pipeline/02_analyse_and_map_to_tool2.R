# =============================================================================
# 02_analyse_and_map_to_tool2.R
# PURPOSE: Run all indicator computations on Tool 1 facility data,
#          aggregate to PCN level, and rename every column
#          to the EXACT Tool 2 question name using the confirmed mapping.
#
# MAPPING SOURCE: analysis_result_to_tool_2_codes.xlsx
# COLUMN NAMES: Verified against pcn_assessment_tool.xlsx (XLSForm survey sheet)
# =============================================================================

library(dplyr)
library(readr)
library(tidyr)
library(stringr)

cat("Reading facility data...\n")
pcn_data <- read_csv("data/consolidated_facility_data.csv", show_col_types = FALSE)
cat(sprintf("  %d facility rows, %d counties\n",
            nrow(pcn_data), n_distinct(pcn_data$county, na.rm = TRUE)))

# --- Helper functions ---
tot   <- function(x) sum(as.numeric(x), na.rm = TRUE)
avg   <- function(x) round(mean(as.numeric(x), na.rm = TRUE), 1)

# n_yes: works on YES/NO text columns
n_yes <- function(x) sum(tolower(trimws(as.character(x))) == "yes", na.rm = TRUE)

# count_selected: for select_multiple columns stored as space-separated strings,
# counts how many options were selected per row then averages across facilities
count_selected <- function(x) {
  lengths <- sapply(strsplit(trimws(as.character(x)), " "), function(v) {
    v <- v[v != "" & !is.na(v)]
    length(v)
  })
  round(mean(lengths, na.rm = TRUE), 1)
}

# prop_selected: for select_multiple, what % of max_n options were selected on average
prop_selected <- function(x, max_n) {
  lengths <- sapply(strsplit(trimws(as.character(x)), " "), function(v) {
    v <- v[v != "" & !is.na(v) & v != "NA"]
    length(v)
  })
  round(mean(lengths / max_n * 100, na.rm = TRUE), 1)
}

# =============================================================================
# STEP 1: Compute all analysis outputs at PCN level
#         selected_pcn is the PCN field confirmed from XLSForm
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
    # sha_prop = calculated proportion field; sha_reimbursed = raw amount
    Proportion_of_SHA_reimbursement       = avg(sha_prop),

    # ── 3. SHIF Access ───────────────────────────────────────────────────────
    # Tool 1: Proportion_of_clients_accessing_Health_Services_using_SHIF → Tool 2: clients_access_shif
    # shif_prop = calculated proportion of clients using SHIF
    Proportion_of_clients_accessing_Health_Services_using_SHIF = avg(shif_prop),

    # ── 4. User Fee Waivers (count) ──────────────────────────────────────────
    # Tool 1: Total_clients_waived → Tool 2: people_waived_userfees
    Total_clients_waived                  = tot(clients_waived),

    # ── 5. Essential Medicines Availability ──────────────────────────────────
    # Tool 1: essential_medicines_availability_on_day_of_visit → Tool 2: facilities_22pharma_avail
    # tracer_meds = select_multiple with 23 tracer medicines options
    essential_medicines_availability_on_day_of_visit = prop_selected(tracer_meds, 23),

    # ── 6. Non-pharma Availability ───────────────────────────────────────────
    # Tool 1: non_pharms_availability_on_day_of_visit → Tool 2: facilities_23nonpharma_avail
    # tracer_nphc = select_multiple with 23 non-pharma tracer items (XLSForm name: tracer_nphc)
    non_pharms_availability_on_day_of_visit = prop_selected(tracer_nphc, 23),

    # ── 7. Blood Availability ────────────────────────────────────────────────
    # Tool 1: Availability_of_the_whole_blood_and_blood_components → Tool 2: blood_availability_hospitals
    # Not captured in Tool 1 — filled manually in Tool 2
    Availability_of_the_whole_blood_and_blood_components = NA_real_,

    # ── 8. Essential Medicines Stock-out ─────────────────────────────────────
    # Tool 1: essential_medicines_stock_out → Tool 2: stockout_22pharma_7days_month
    # tracer_stock_out_meds = select_multiple stock-out items (23 options)
    essential_medicines_stock_out         = prop_selected(tracer_stock_out_meds, 23),

    # ── 9. Non-pharma Stock-out ──────────────────────────────────────────────
    # Tool 1: non_pharms_stock_out → Tool 2: stockout_23nonpharma_7days_months
    # tracer_stock_out_nonpharms = select_multiple (XLSForm name: tracer_stock_out_nonpharms)
    non_pharms_stock_out                  = prop_selected(tracer_stock_out_nonpharms, 23),

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
    # equipment_available = select_multiple with basic equipment options
    basic_equipment_availability          = prop_selected(equipment_available, 13),

    # ── 13. Clients Paying Cash ──────────────────────────────────────────────
    # Tool 1: Proportions_paid_through_cash → Tool 2: clients_access_cash
    # paid_cash_prop = calculated proportion paying cash
  Proportions_paid_through_cash = if ("paid_cash" %in% names(pcn_data))
  round(sum(as.numeric(paid_cash), na.rm = TRUE) /
        sum(as.numeric(total_attended), na.rm = TRUE) * 100, 1)
else NA_real_,

    # ── 14. FIF Rollback ─────────────────────────────────────────────────────
    # Tool 1: Proportion_of_FIF_collected_rolled_back_to_the_facilities_within_PCN
    #       → Tool 2: fif_collected_rollback
    # prop_fif = calculated proportion of FIF rolled back
    Proportion_of_FIF_collected_rolled_back_to_the_facilities_within_PCN = avg(prop_fif),

    # ── 15. People Waived (proportion) ───────────────────────────────────────
    # Tool 1: Proportion_of_people_waived_for_user_fees_in_HFs_within_the_PCN
    #       → Tool 2: people_waived_userfees
    # waiver_prop = calculated proportion waived
    Proportion_of_people_waived_for_user_fees_in_HFs_within_the_PCN = avg(waiver_prop),

    # ── 16. Total Amount Waived ──────────────────────────────────────────────
    # Tool 1: Total_amount_user_fee_waived_in_PCN → Tool 2: userfees_total_waived
    # user_fee = total amount of user fees waived per facility
    Total_amount_user_fee_waived_in_PCN   = tot(user_fee),

    # ── 17. Road Network ─────────────────────────────────────────────────────
    # Tool 1: percentage_having_reliable_road_network → Tool 2: hfs_accessible_road
    # roadinternet = YES/NO reliable road network (XLSForm name: roadinternet)
    percentage_having_reliable_road_network = round(n_yes(roadinternet) / n() * 100),

    # ── 18. WASH ─────────────────────────────────────────────────────────────
    # Tool 1: WASH_Average_Percentage → Tool 2: hfs_wash_facilities
    # Average of toilet_available + handwashing_place (both YES/NO)
    WASH_Average_Percentage               = round(
      (n_yes(toilet_available) + n_yes(handwashing_place)) / (2 * n()) * 100
    ),

    # ── 19. Infrastructure Items ─────────────────────────────────────────────
    # Tool 1: infrastructure_items_availability → Tool 2: hfs_tracer_infra_keph
    # infrastructure_items = select_multiple tracer infrastructure items
    infrastructure_items_availability     = prop_selected(infrastructure_items, 10),

    # ── 20. Reliable Power ───────────────────────────────────────────────────
    # Tool 1: percentage_having_reliable_power → Tool 2: hfs_reliable_power
    # power = YES/NO reliable power source
    percentage_having_reliable_power      = round(n_yes(power) / n() * 100),

    # ── 21. Reliable Internet ────────────────────────────────────────────────
    # Tool 1: percentage_having_reliable_internet → Tool 2: hfs_reliable_internet
    # internet = YES/NO reliable internet connection
    percentage_having_reliable_internet   = round(n_yes(internet) / n() * 100),

    # ── 22. OPD Reporting Tools ──────────────────────────────────────────────
    # Tool 1: reportingtools_availability → Tool 2: hfs_opd_tools_pcn
    # opd_reporting_tools = select_multiple OPD reporting tools
    reportingtools_availability           = prop_selected(opd_reporting_tools, 6),

    # ── 23. Functional EMR ───────────────────────────────────────────────────
    # Tool 1: percentage_having_functional_EMR → Tool 2: hfs_integrated_emr
    # emrpower = YES/NO integrated functional EMR (XLSForm name: emrpower)
    percentage_having_functional_EMR      = round(n_yes(emrpower) / n() * 100),

    # ── 24. HCW Skills Training ──────────────────────────────────────────────
    # Tool 1: Health_care_workers_undergone_skills_competency_course → Tool 2: hcws_skills_training_2yrs
    # competency_building = count of HCWs trained; competency_building_prop = proportion
    Health_care_workers_undergone_skills_competency_course = avg(competency_building_prop),

    # ── 25. QIT ──────────────────────────────────────────────────────────────
    # Tool 1: qit_team_percentage → Tool 2: hospitals_qit_functional
    # improvement_teams = select_one with options including qit/wit
    # checking if "qit" appears in the improvement_teams value
    qit_team_percentage                   = round(
      sum(grepl("qit", tolower(as.character(improvement_teams)), fixed = TRUE), na.rm = TRUE) /
        n() * 100
    ),

    # ── 26. WIT ──────────────────────────────────────────────────────────────
    # Tool 1: wit_team_percentage → Tool 2: spokes_wit_functional
    wit_team_percentage                   = round(
      sum(grepl("wit", tolower(as.character(improvement_teams)), fixed = TRUE), na.rm = TRUE) /
        n() * 100
    ),

    # ── 27. IPC Average ──────────────────────────────────────────────────────
    # Tool 1: IPC_avg → Tool 2: ipc_items_availability
    # ipc_and_hygiene_items = select_multiple IPC items in OPD
    IPC_avg                               = prop_selected(ipc_and_hygiene_items, 5),

    # ── 28. Clinical Guidelines Adherence ────────────────────────────────────
    # Tool 1: Clinical_adherence → Tool 2: clinical_guidelines_adherence
    # clinical_guidelines_adherence = select_multiple minimum clinical guidelines
    Clinical_adherence                    = prop_selected(clinical_guidelines_adherence, 5),

    # ── 29. Staff Absenteeism ────────────────────────────────────────────────
    # Tool 1: Proportion_of_staff_absent → Tool 2: absenteesm_phc_facilities
    # absenteesm = calculated field (note spelling matches XLSForm: absenteesm)
    Proportion_of_staff_absent            = avg(absenteesm),

    # ── 30. Client Satisfaction Survey ───────────────────────────────────────
    # Tool 1: percent_done_client_satisfaction_survey → Tool 2: facilities_client_survey
    # client_satisfaction = YES/NO has facility ever conducted satisfaction survey
    percent_done_client_satisfaction_survey = round(n_yes(client_satisfaction) / n() * 100),

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
