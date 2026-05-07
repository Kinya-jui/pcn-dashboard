# =============================================================================
# 02_analyse_facility_data.R
# PURPOSE: Run all indicator computations from pcn_assessment6.R on the
#          pulled facility data, aggregate to PCN/subcounty level, and
#          produce a clean data frame matching Tool 2 field names.
# =============================================================================

library(dplyr)
library(readr)
library(tidyr)
library(stringr)

cat("Reading facility data...\n")
pcn_data <- read_csv("data/consolidated_facility_data.csv", show_col_types = FALSE)
cat(sprintf("  %d facility rows across %d counties\n",
            nrow(pcn_data), n_distinct(pcn_data$county, na.rm = TRUE)))

# --- YES/NO column conversion (same as original script) ---
yes_no_cols <- c("q1", "q3", "q14_a", "q18", "q19", "q20", "q204",
                 "q204_1","q204_2","q204_3","q204_4","q204_5",
                 "q204_6","q204_7","q204_8","q204_9","q204_10","q204_11",
                 "q25","q28","q29","q30","q31","q32","q34",
                 "q35","q37","q38","q39","q40","q41")

# Only convert columns that actually exist in the data
yes_no_cols <- intersect(yes_no_cols, names(pcn_data))
pcn_data[yes_no_cols] <- lapply(pcn_data[yes_no_cols], function(x) {
  ifelse(tolower(trimws(as.character(x))) == "yes", 1,
         ifelse(tolower(trimws(as.character(x))) == "no", 0, NA))
})

# =============================================================================
# AGGREGATE TO PCN LEVEL
# Each row in the output = one PCN (grouped by county + subcounty + pcn_name)
# These aggregated values are what get posted to Tool 2.
# =============================================================================

pct <- function(x) round(mean(x, na.rm = TRUE) * 100, 1)
tot <- function(x) sum(as.numeric(x), na.rm = TRUE)
avg <- function(x) round(mean(as.numeric(x), na.rm = TRUE), 1)
n_yes <- function(x) sum(x == 1, na.rm = TRUE)

pcn_aggregated <- pcn_data %>%
  group_by(county, subcounty, pcn_name) %>%
  summarise(
    total_facilities = n(),

    # ── Health Care Financing ──────────────────────────────────────────────
    # clients_access_cash: % facilities where clients pay cash
    clients_access_cash               = pct(q3 == 0),   # NOT empaneled ≈ paying cash
    # clients_access_shif: % using SHIF (q4)
    clients_access_shif               = avg(q4),
    # SHA empanelment (q3 = SHA empaneled yes/no)
    hfs_empaneled_sha                 = round(n_yes(q3) / n() * 100),
    # Claims reimbursed (q5: proportion reimbursed)
    claims_reimbursed_hfs             = avg(q5),
    # FIF rollback (q8)
    fif_collected_rollback            = avg(q8),
    # People waived (q6: number waived per facility → sum across PCN)
    people_waived_userfees            = tot(q6),
    # Total amount waived (q8_a)
    userfees_total_waived             = tot(q8_a),

    # ── HRH ───────────────────────────────────────────────────────────────
    hrhpop                            = tot(q9_a),
    doctors_pop                       = tot(q9_b),
    nurses_pop                        = tot(q9_c),
    co_pop                            = tot(q9_d),
    hcws_sensitized_phc_pcn           = round(tot(q9_e) / tot(q9_a) * 100),
    hcws_skills_training_2yrs         = avg(q11),
    # Provider availability / absenteeism (q12 = % staff present)
    absenteesm_phc_facilities         = avg(q12),
    # CHPs trained on basic modules: no direct equivalent in facility tool
    # → proxied from q9 (sensitisation %)
    chps_trained_basic                = avg(q9),

    # ── Infrastructure ────────────────────────────────────────────────────
    hfs_accessible_road               = round(n_yes(q28) / n() * 100),
    hfs_wash_facilities               = round(n_yes(q35) / n() * 100),
    # Infrastructure items from q33 checklist (23 items)
    hfs_tracer_infra_keph             = round(
      rowMeans(across(any_of(paste0("q33_", 1:23)), as.numeric),
               na.rm = TRUE) * 100
    ) %>% mean(na.rm = TRUE) %>% round(),
    hfs_reliable_power                = round(n_yes(q32) / n() * 100),
    # Staff accommodation: not directly in facility tool; set to NA for Tool 2 entry
    hfs_staff_accommodation           = NA_real_,
    # Ambulance: from q38
    # pcn_ambulance and ambulance_request are PCN-level so they stay in Tool 2

    # ── HMIS / Digital Health ─────────────────────────────────────────────
    hfs_reliable_internet             = round(n_yes(q30) / n() * 100),
    hfs_opd_tools_pcn                 = avg(
      rowSums(across(any_of(paste0("q42_", 1:6)), as.numeric), na.rm = TRUE) /
      6 * 100
    ),
    hfs_integrated_emr                = round(n_yes(q31) / n() * 100),
    # CHUs reporting monthly: not in facility tool; remains manual in Tool 2

    # ── Capacity Readiness ────────────────────────────────────────────────
    facilities_22pharma_avail         = avg(
      rowSums(across(any_of(paste0("q22_", 1:23)), as.numeric), na.rm = TRUE) /
      23 * 100
    ),
    facilities_23nonpharma_avail      = avg(
      rowSums(across(any_of(paste0("q24_", 1:23)), as.numeric), na.rm = TRUE) /
      23 * 100
    ),
    stockout_22pharma_7days_month     = avg(
      rowSums(across(any_of(paste0("q23_", 1:23)), as.numeric), na.rm = TRUE) /
      23 * 100
    ),
    stockout_23nonpharma_7days_months = avg(
      rowSums(across(any_of(paste0("q25_", 1:23)), as.numeric), na.rm = TRUE) /
      23 * 100
    ),
    # Lab services
    hospitals_comp_lab_services       = round(n_yes(q204) / n() * 100),
    spokes_basic_lab_services         = round(n_yes(q204) / n() * 100),  # proxy
    allbasic_tracer_equipments        = avg(
      rowSums(across(any_of(paste0("q21_", 1:13)), as.numeric), na.rm = TRUE) /
      13 * 100
    ),
    blood_availability_hospitals      = NA_real_,  # not in facility tool

    # ── Quality of Care ───────────────────────────────────────────────────
    hospitals_qit_functional          = round(n_yes(q39) / n() * 100),
    spokes_wit_functional             = round(n_yes(q40) / n() * 100),
    ipc_items_availability            = avg(
      (as.numeric(pcn_data$q36_1) + as.numeric(pcn_data$q36_2) +
       as.numeric(pcn_data$q36_3)) / 3 * 100
    ),
    clinical_guidelines_adherence     = round(n_yes(q25) / n() * 100),
    facilities_mpdsr_conducted        = NA_real_,
    fresh_stillbirth_rate             = NA_real_,
    maternal_deaths_rate              = NA_real_,
    maternal_deaths_audited           = NA_real_,
    neonatal_deaths_rate              = NA_real_,
    neonatal_deaths_audited           = NA_real_,
    tb_treatment_success              = NA_real_,

    # ── Social Accountability ─────────────────────────────────────────────
    facilities_client_survey          = round(n_yes(q18) / n() * 100),
    facilities_functional_grms        = round(n_yes(q19) / n() * 100),

    .groups = "drop"
  ) %>%
  # Clamp all percentage columns to 0–100
  mutate(across(where(is.numeric), ~ pmin(pmax(.x, 0, na.rm = TRUE), 100)))

cat(sprintf("  Aggregated to %d PCN-level rows\n", nrow(pcn_aggregated)))

# Save intermediate result
write_csv(pcn_aggregated, "data/pcn_aggregated_indicators.csv")
cat("  Saved → data/pcn_aggregated_indicators.csv\n")
