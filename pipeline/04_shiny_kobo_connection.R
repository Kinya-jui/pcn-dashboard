# =============================================================================
# 04_shiny_kobo_connection.R
# PURPOSE: Drop-in replacement for sections 04–06 of app.R
#          Replace the static read_csv() calls with live KoboToolbox API fetches.
#
# HOW TO USE:
#   1. Add a .Renviron file in your Shiny project root containing:
#        KOBO_TOKEN=your_api_token_here
#        KOBO_TOOL2_UID=your_tool2_asset_uid_here
#   2. Replace sections 04, 05, 06 of your app.R with this file's contents.
#   3. In server(), replace all references to data_pcn  with data_pcn_live()
#                                           data_county with data_county_live()
# =============================================================================

library(httr2)
library(jsonlite)
library(dplyr)
library(readr)

# ============================================================
# 04. KOBO API CREDENTIALS
# ============================================================
KOBO_TOKEN      <- Sys.getenv("KOBO_TOKEN")
KOBO_TOOL2_UID  <- Sys.getenv("KOBO_TOOL2_UID")
KOBO_BASE       <- "https://kf.kobotoolbox.org/api/v2/assets"

# Validate credentials on startup
if (nchar(KOBO_TOKEN) == 0)     stop("KOBO_TOKEN not set in .Renviron")
if (nchar(KOBO_TOOL2_UID) == 0) stop("KOBO_TOOL2_UID not set in .Renviron")

# ============================================================
# 05. FETCH FUNCTION
# Handles pagination — KoboToolbox caps at 30,000 per page
# ============================================================
fetch_kobo_all <- function(asset_uid, token) {
  all_results <- list()
  url <- paste0(KOBO_BASE, "/", asset_uid, "/data/")

  repeat {
    resp <- tryCatch(
      request(url) |>
        req_url_query(format = "json", limit = 30000) |>
        req_headers(Authorization = paste("Token", token)) |>
        req_timeout(60) |>
        req_perform(),
      error = function(e) {
        message("KoboToolbox API error: ", conditionMessage(e))
        return(NULL)
      }
    )

    if (is.null(resp)) break

    body    <- resp_body_json(resp, simplifyVector = TRUE)
    results <- body$results

    if (length(results) == 0) break
    all_results <- c(all_results, list(as.data.frame(results)))
    if (is.null(body[["next"]]) || is.na(body[["next"]])) break
    url <- body[["next"]]
  }

  if (length(all_results) == 0) return(NULL)
  bind_rows(all_results)
}

# ============================================================
# 06. COLUMN NAME MAPPING
# KoboToolbox returns XLSForm 'name' values as column headers.
# Map them to the display names the dashboard indicator system expects.
# ============================================================
KOBO_TO_DASHBOARD <- c(
  # Identity
  "counties"                                   = "County",
  "facility"                                   = "PCN",
  # Governance
  "functional_phc_chc"                         = "Proportion of functional Community Health Committees in the PCN",
  "proportion_hfs_support_supervision"         = "Proportion of Health facilities that have received supportive supervision in the PCN",
  "functional_phc_committee"                   = "Functional PCN Management Committee",
  "disp_functional_mdt"                        = "Functionality of MDTs",
  "pcn_governance_score"                       = "Governance Score",
  # Population Health Needs
  "population_profiling_count"                 = "Number of population profiling assessments conducted",
  "proportion_pop_health_needs"                = "Proportion of population health needs that have been addressed",
  "wellness_activities_pcn"                    = "Number of wellness activities conducted within the PCN",
  "population_health_needs_score"              = "Population Needs Score",
  # Capacity Readiness
  "facilities_22pharma_avail"                  = "Proportion of facilities in the PCN that had all 22 tracer pharmaceuticals at assessment",
  "facilities_23nonpharma_avail"               = "Proportion with all 23 tracer non-pharmaceuticals",
  "blood_availability_hospitals"               = "Availability of whole blood components",
  "stockout_22pharma_7days_month"              = "Percentage of Health facilities with stock out on any of the 22 tracer pharmaceuticals for 7 consecutive days in a month",
  "stockout_23nonpharma_7days_months"          = "Percentage of Health facilities with stock out on any of the 22 tracer non-pharmaceuticals for 7 consecutive days in a month",
  "hospitals_comp_lab_services"                = "Proportion of hospitals with comprehensive lab services within the PCN",
  "spokes_basic_lab_services"                  = "Proportion of spokes with basic lab services within the PCN",
  "allbasic_tracer_equipments"                 = "Proportion of Facilities within the PCN with all basic  tracer equipment available and functional",
  "capacity_readiness_score"                   = "Capacity Readiness Score",
  # Health Care Financing
  "proportion_clients_shif"                    = "Proportion of clients using SHIF",
  "proportion_facilities_empaneled"            = "Proportion of facilities empaneled by SHA",
  "proportion_facilities_claims_reimbursed"    = "Proportion submitting SHA claims",
  "proportion_fif_collected_rollback"          = "Proportion of FIF collected rolled back to the facilities within PCN",
  "people_waived_userfees"                     = "Number of people waived for user fees in Hospitals within the PCN",
  "userfees_total_waived"                      = "Total amount of user fees  waived in Health care Facilities within the PCN",
  "health_financing_score"                     = "Healthcare Financing Score",
  # Infrastructure
  "hfs_accessible_road"                        = "Proportion of health facilities with accessible road network",
  "hfs_wash_facilities"                        = "Proportion of  facilities with the appropriate WASH facilities",
  "hfs_tracer_infra_keph"                      = "Proportion of  facilities with the tracer list of infrastructure as per KEPH standards",
  "hfs_reliable_power"                         = "Proportion of facilties with a reliable power source",
  "calc_pcn_ambulance"                         = "PCN access to adequate ambulance services",
  "calc_ambulance_request"                     = "Ambulance Request Score",
  "health_infrastructure_score"                = "Health Infrastructure Score",
  # HMIS
  "hfs_reliable_internet"                      = "Proprtion of facilties with  reliable internet connection",
  "hfs_opd_tools_pcn"                          = "Proportion of facilities in the PCN with the key OPD reporting tools (6)",
  "performance_review_meetings"                = "No of performance and data quality review meetings held quarterly within the PCN",
  "hfs_integrated_emr"                         = "Proportion of facilities in a PCN with an integrated functional EMR",
  "chus_reporting_monthly"                     = "Proportion of CHUs within the PCN reporting monthly",
  "hmis_score"                                 = "HMIS Score",
  # HRH
  "hrh_density"                                = "Core HRH density",
  "doctor_pop_ratio"                           = "Doctor to population ratio",
  "clinical_officer_pop_ratio"                 = "Clinical officer to  population ratio",
  "nurse_pop_ratio"                            = "Nurse to population ratio",
  "cha_cho_pop_ratio"                          = "CHA/CHO  to population ratio",
  "chps_trained_basic"                         = "Proportion of CHPs trained on basic modules",
  "hcws_sensitized_phc_pcn"                    = "Health care workers sensitized on PHC /PCN ",
  "county_skill_mechanism"                     = "Skills improvement mechanism",
  "hcws_skills_training_2yrs"                  = "Proportion of health workers who have undergone a skills/ competency buliding course within the last 2 years",
  "hrh_score"                                  = "HRH Score",
  # Service Delivery
  "mdt_outreaches"                             = "Number of outreaches conducted  by the MDT",
  "inreaches_pcn"                              = "Number of in-reaches conducted within the PCN",
  "service_delivery_score"                     = "Service Delivery Score",
  # Quality of Care – Management Systems
  "hospitals_qit_functional"                   = "Proportion of hospitals with functional facility quality improvement teams (QIT)",
  "spokes_wit_functional"                      = "Proportion of spokes with functional facility work improvement teams (WIT)",
  "ipc_items_availability"                     = "Average availability of selected IPC items  *(items defined below)",
  "pcn_qoc_score"                              = "QoC Management Systems Score",
  # Quality of Care – PHC Core Systems
  "clinical_guidelines_adherence"              = "Adherence to clinical guidelines for Primary health care facilities",
  "absenteesm_phc_facilities"                  = "Provider Availability (absenteeism) for Primary health care facilities",
  # Quality of Care – Outcomes
  "facilities_mpdsr_conducted"                 = "Proportion of facilities conducting MPDSR",
  "fresh_stillbirth_rate"                      = "Fresh Stillbirth rate per 1,000 births in health facilities",
  "maternal_deaths_rate"                       = "Number of maternal deaths reported in Health facilities per 100,000 live births",
  "maternal_deaths_audited"                    = "Proportion of maternal deaths Audited",
  "neonatal_deaths_rate"                       = "Number of neonatal deaths per 1,000 live births",
  "neonatal_deaths_audited"                    = "Proportion of neonatal deaths audited ",
  "tb_treatment_success"                       = "TB treatment success",
  # Social Accountability
  "facilities_client_survey"                   = "Proportion of facilities which have conducted a client satisfaction survey",
  "mdt_engagements_community"                  = "No. of MDT engagements with the community",
  "facilities_functional_grms"                 = "No. of health facilities with functional GRMs",
  "social_accountability_score"                = "Social Accountability Score",
  # Innovations
  "calc_num_of_phc_innovations_implemented"    = "Number of PHC related innovations/ best practice implemented/adapted",
  "innovations_score"                          = "Innovations and Learning Score",
  # Establishment status
  "establishment_status"                       = "establishment_status",
  "supporting_partner"                         = "supporting_partner",
  "pcn_location"                               = "pcn_location",
  "Subcounty"                                  = "Subcounty"
)

# ============================================================
# 07. CLEAN & RENAME FUNCTION
# Applied after fetching from KoboToolbox
# ============================================================
clean_kobo_data <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(df)

  # Strip group prefixes (e.g. "pcn_tool/governance/func_pcn_chc" → "func_pcn_chc")
  names(df) <- gsub("^.*?/([^/]+)$", "\\1", names(df))

  # Rename using the mapping (only rename columns that exist)
  for (kobo_name in names(KOBO_TO_DASHBOARD)) {
    display_name <- KOBO_TO_DASHBOARD[[kobo_name]]
    if (kobo_name %in% names(df) && !display_name %in% names(df)) {
      names(df)[names(df) == kobo_name] <- display_name
    }
  }

  # Ensure County / Subcounty / PCN are character
  for (col in c("County", "Subcounty", "PCN")) {
    if (col %in% names(df)) df[[col]] <- as.character(df[[col]])
  }

  # Coerce all indicator columns to numeric
  indicator_cols <- setdiff(names(df), c("County", "Subcounty", "PCN",
                                          "establishment_status", "supporting_partner",
                                          "pcn_location", "_id", "_uuid",
                                          "_submission_time", "_submitted_by"))
  df[indicator_cols] <- lapply(df[indicator_cols], function(x) suppressWarnings(as.numeric(x)))

  # Add normalised join keys
  df %>%
    filter(!is.na(County), County != "") %>%
    mutate(
      County_raw      = County,
      County_clean    = normalize_name(County),
      Subcounty_raw   = if ("Subcounty" %in% names(.)) Subcounty else NA_character_,
      Subcounty_clean = if ("Subcounty" %in% names(.)) normalize_name(Subcounty) else NA_character_
    )
}

# ============================================================
# 08. REACTIVE DATA SOURCES (place these INSIDE server())
# ============================================================
# Replace your static data_pcn / data_county with these reactives.
# The timer re-fetches every 5 minutes (300,000 ms).
#
# USAGE IN SERVER:
#
#   kobo_timer <- reactiveTimer(300000)
#
#   data_pcn_live <- reactive({
#     kobo_timer()
#     raw <- fetch_kobo_all(KOBO_TOOL2_UID, KOBO_TOKEN)
#     clean_kobo_data(raw)
#   })
#
#   data_county_live <- reactive({
#     kobo_timer()
#     raw    <- fetch_kobo_all(KOBO_TOOL2_UID, KOBO_TOKEN)
#     cleaned <- clean_kobo_data(raw)
#     # County-level tool submissions have tool_selection == "county_tool"
#     cleaned %>% filter(tool_selection == "county_tool")
#   })
#
# Then everywhere you currently use data_pcn  → use data_pcn_live()
#                                 data_county → use data_county_live()
#
# NOTE: indicator_cols and county_indicator_cols must also become reactive:
#
#   indicator_cols <- reactive({
#     names(data_pcn_live())[sapply(data_pcn_live(), is.numeric)]
#   })
#
#   county_indicator_cols <- reactive({
#     names(data_county_live())[sapply(data_county_live(), is.numeric)]
#   })
# ============================================================
