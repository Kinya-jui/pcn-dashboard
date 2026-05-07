# ============================================================
# app.R — Kenya PCN Monitoring Dashboard
# Author: Quintine | Version: Stable (Manual Tabs)
# ============================================================


# ============================================================
# 01. LOAD LIBRARIES
# ============================================================
library(shiny)
library(bs4Dash)
library(dplyr)
library(readr)
library(sf)
library(ggplot2)
library(plotly)
library(DT)
library(ggspatial)
library(units)
library(stringr)
library(showtext)
library(tidyr)
library(rlang)
library(ggtext)
library(stringi)
library(stringdist)
library(litedown)
library(ggiraph)
library(ggrepel)
library(shinydashboard)
library(httr2)
library(jsonlite)

# ============================================================
# 02. GLOBAL HELPER FUNCTIONS
# ============================================================

# Standard ggplot2 map theme for all choropleth maps
map_theme <- function(title_size = 20) {
  theme_void() +
    theme(
      plot.background    = element_rect(fill = "white", color = NA),
      panel.background   = element_rect(fill = "white", color = NA),
      plot.title         = element_text(size = title_size, face = "bold"),
      plot.title.position = "plot",
      legend.title       = element_blank(),
      legend.text        = element_text(size = 18),
      legend.key.height  = unit(1.9, "cm"),
      legend.key.width   = unit(0.9, "cm"),
      plot.margin        = margin(18, 18, 18, 18)
    )
}

# Normalizes names for fuzzy joining (strips punctuation, lowercases)
normalize_name <- function(x) {
  x %>%
    tolower() %>%
    stringr::str_replace_all("[^a-z0-9]", "") %>%
    trimws()
}

# Safe null-coalescing operator
`%||%` <- function(x, y) if (!is.null(x)) x else y

# Fuzzy string match — returns closest column name from choices
match_to_closest <- function(x, choices) {
  idx <- amatch(x, choices, maxDist = 20)
  if (is.na(idx)) return(NA_character_)
  choices[idx]
}

# Geometry cleaning — fixes invalid polygons
fix_geom <- function(x) {
  x <- st_make_valid(x)
  x <- st_collection_extract(x, "POLYGON", warn = FALSE)
  st_cast(x, "MULTIPOLYGON", warn = FALSE)
}

# Geometry simplification — reduces file size while keeping shape
smooth_geom <- function(x) {
  x <- st_make_valid(x)
  if (requireNamespace("rmapshaper", quietly = TRUE))
    x <- rmapshaper::ms_simplify(x, keep = 0.1, keep_shapes = TRUE)
  else
    x <- st_simplify(x, dTolerance = 0.001, preserveTopology = TRUE)
  st_cast(x, "MULTIPOLYGON", warn = FALSE)
}


# ============================================================
# 03. REGISTER FONTS
# ============================================================
#font_add(family = "Arial",           regular = "C:/Windows/Fonts/arial.ttf")
#font_add(family = "Georgia",         regular = "C:/Windows/Fonts/georgia.ttf")
#font_add(family = "Times New Roman", regular = "C:/Windows/Fonts/times.ttf")
#font_add(family = "Calibri",         regular = "C:/Windows/Fonts/calibri.ttf")
#showtext_auto()


# ============================================================
# 04. Shapefile Paths
# ============================================================

county_shp_path <- "kenya_shapefiles/ken_admbnda_adm1_iebc_20191031.shp"
subc_shp_path   <- "kenya_shapefiles/ken_admbnda_adm2_iebc_20191031.shp"
lakes_shp_path  <- "kenya_shapefiles/KEN_Lakes.shp"

# ============================================================
# 04. KOBO API CREDENTIALS
# ============================================================
KOBO_TOKEN    <- Sys.getenv("6a74367c364625414eeb8a4f2222c7ca793c97d0")     # store in .Renviron, NOT hardcoded
KOBO_PCN_UID  <- Sys.getenv("aWoWkvQC6WVnzdNyTCXQ4n")   # Asset UID for PCN/county tool (Tool 2)
KOBO_BASE     <- "https://kf.kobotoolbox.org/api/v2/assets"


# Find where your app.R is
getwd()

# Create the .Renviron file right there
writeLines(
  c(
    "KOBO_TOKEN=6a74367c364625414eeb8a4f2222c7ca793c97d0",
    "KOBO_PCN_UID=aWoWkvQC6WVnzdNyTCXQ4n"
  ),
  con = file.path(getwd(), ".Renviron")
)


Sys.getenv("KOBO_TOKEN")    # should show your token
Sys.getenv("KOBO_PCN_UID")  # should show your UID
# ============================================================
# 05. FETCH FUNCTION (replaces read_csv)
# ============================================================
fetch_kobo <- function(asset_uid) {
  req <- httr2::request(KOBO_BASE) |>
    httr2::req_url_path_append(asset_uid, "data") |>
    httr2::req_url_query(format = "json") |>
    httr2::req_headers(Authorization = paste("Token", KOBO_TOKEN))
  
  resp <- httr2::req_perform(req)
  df   <- httr2::resp_body_json(resp, simplifyVector = TRUE)$results
  as.data.frame(df)
}

# ============================================================
# 06. REACTIVE DATA (replaces static read_csv)
#     Polls KoboToolbox every 5 minutes
# ============================================================
# These go inside server(), replacing the static data_pcn / data_county objects

kobo_timer <- reactiveTimer(300000)   # 5 minutes = 300,000 ms

data_pcn_live <- reactive({
  kobo_timer()
  df <- fetch_kobo(KOBO_PCN_UID)
  # --- same cleaning as your original section 05 ---
  df <- df %>%
    filter(!is.na(County), County != "", !is.na(Subcounty), !is.na(PCN)) %>%
    mutate(
      County         = as.character(County),
      Subcounty      = as.character(Subcounty),
      PCN            = as.character(PCN),
      County_raw     = County,
      County_clean   = normalize_name(County),
      Subcounty_raw  = Subcounty,
      Subcounty_clean= normalize_name(Subcounty)
    )
  names(df) <- trimws(gsub("\\s+", " ", names(df)))
  df
})

# Normalize column names
names(data_pcn) <- gsub("\\s+", " ", names(data_pcn))
names(data_pcn) <- trimws(names(data_pcn))

# Ensure key fields are character type
data_pcn$County    <- as.character(data_pcn$County)
data_pcn$Subcounty <- as.character(data_pcn$Subcounty)
data_pcn$PCN       <- as.character(data_pcn$PCN)

# Add normalized name columns for fuzzy joining
data_pcn <- data_pcn %>%
  mutate(
    County_raw      = County,
    County_clean    = normalize_name(County),
    Subcounty_raw   = Subcounty,
    Subcounty_clean = normalize_name(Subcounty)
  )

# Extract numeric indicator column names
indicator_cols <- names(data_pcn)[sapply(data_pcn, is.numeric)]


# ============================================================
# 06. LOAD & CLEAN COUNTY-LEVEL DATA
# ============================================================
data_county <- read_csv(county_csv_path, show_col_types = FALSE) %>%
  mutate(
    County       = trimws(County),
    County_clean = normalize_name(County)
  )

# Safe encoding fix for column names (handles NBSP and special chars)
names(data_county) <- names(data_county) %>%
  stringi::stri_enc_toutf8() %>%
  stringi::stri_replace_all_fixed("\u00A0", " ") %>%
  stringi::stri_replace_all_regex("\\s+", " ") %>%
  stringi::stri_trim_both() %>%
  stringi::stri_trans_general("Latin-ASCII") %>%
  stringr::str_replace_all("[.]", "") %>%
  stringr::str_trim()

# Extract numeric indicator column names
county_indicator_cols <- names(data_county)[sapply(data_county, is.numeric)]


# ============================================================
# 07. LOAD SHAPEFILES & BUILD SPATIAL OBJECTS
# ============================================================

# --- County shapefile ---
county_shp <- st_read(county_shp_path, quiet = TRUE) %>%
  st_transform(4326)

if (!"County" %in% names(county_shp) && "ADM1_EN" %in% names(county_shp))
  county_shp$County <- trimws(county_shp$ADM1_EN)

county_shp$County <- as.character(county_shp$County)
county_shp <- smooth_geom(fix_geom(county_shp)) %>%
  mutate(
    County_raw   = County,
    County_clean = normalize_name(County)
  )

# --- Subcounty shapefile ---
subc_shp <- st_read(subc_shp_path, quiet = TRUE) %>%
  st_transform(4326)

if (!"Subcounty" %in% names(subc_shp) && "ADM2_EN" %in% names(subc_shp))
  subc_shp$Subcounty <- trimws(subc_shp$ADM2_EN)
if (!"County" %in% names(subc_shp) && "ADM1_EN" %in% names(subc_shp))
  subc_shp$County <- trimws(subc_shp$ADM1_EN)

subc_shp$County    <- as.character(subc_shp$County)
subc_shp$Subcounty <- as.character(subc_shp$Subcounty)
subc_shp <- smooth_geom(fix_geom(subc_shp)) %>%
  mutate(
    Subcounty_raw   = Subcounty,
    Subcounty_clean = normalize_name(Subcounty),
    County_clean    = normalize_name(County)
  )

# --- Lakes shapefile (optional) ---
lakes_shp <- tryCatch({
  if (!file.exists(lakes_shp_path)) {
    message("Lakes shapefile NOT found at: ", lakes_shp_path)
    return(NULL)
  }
  shp <- st_read(lakes_shp_path, quiet = TRUE)
  if (is.na(st_crs(shp))) st_crs(shp) <- 4326
  shp <- st_transform(shp, 4326)
  fix_geom(shp)
}, error = function(e) {
  message("Failed to load lakes shapefile: ", e$message)
  NULL
})

# --- County label centroids ---
county_centroids <- suppressWarnings(st_centroid(county_shp))
cent_coords <- data.frame(st_coordinates(county_centroids), County = county_shp$County)


# ============================================================
# 08. PCN GEO POINTS (for map dots)
# ============================================================
pcn_points <- data_pcn %>%
  select(PCN, County, Subcounty, pcn_location) %>%
  filter(!is.na(pcn_location), pcn_location != "") %>%
  mutate(
    coord_clean = stringr::str_replace_all(pcn_location, "\\s+", ""),
    lon         = as.numeric(stringr::str_extract(coord_clean, "^[^,]+")),
    lat         = as.numeric(stringr::str_extract(coord_clean, "(?<=,).*")),
    # Auto-fix reversed lat/lon
    lon_fixed   = ifelse(abs(lon) > 90, lon, lat),
    lat_fixed   = ifelse(abs(lon) > 90, lat, lon)
  ) %>%
  filter(!is.na(lat_fixed), !is.na(lon_fixed)) %>%
  st_as_sf(coords = c("lon_fixed", "lat_fixed"), crs = 4326, remove = FALSE) %>%
  mutate(
    County_clean    = normalize_name(County),
    Subcounty_clean = normalize_name(Subcounty)
  )


# ============================================================
# 09. PCN INDICATOR GROUPS (14 THEMATIC TABS)
# ============================================================
pcn_indicators <- list(
  "Overview" = indicator_cols,
  
  "Governance" = c(
    "Proportion of functional Community Health Committees in the PCN",
    "Proportion of Health facilities that have received supportive supervision in the PCN",
    "Functional PCN Management Committee",
    "Functionality of MDTs",
    "Governance Score"
  ),
  "Population Health Needs" = c(
    "Number of population profiling assessments conducted",
    "Proportion of population health needs that have been addressed",
    "Number of wellness activities conducted within the PCN",
    "Population Needs Score"
  ),
  "Capacity Readiness" = c(
    "Proportion of facilities in the PCN that had all 22 tracer pharmaceuticals at assessment",
    "Proportion with all 23 tracer non-pharmaceuticals",
    "Availability of whole blood components",
    "Percentage of Health facilities with stock out on any of the 22 tracer pharmaceuticals for 7 consecutive days in a month",
    "Percentage of Health facilities with stock out on any of the 22 tracer non-pharmaceuticals for 7 consecutive days in a month",
    "Proportion of hospitals with comprehensive lab services within the PCN",
    "Proportion of spokes with basic lab services within the PCN",
    "Proportion of Facilities within the PCN with all basic  tracer equipment available and functional",
    "Capacity Readiness Score"
  ),
  "Health Care Financing" = c(
    "Proportion of clients using SHIF",
    "Proportion of facilities empaneled by SHA",
    "Proportion submitting SHA claims",
    "Proportion of claims reimbursed to HFs within the PCN",
    "Proportion of FIF collected rolled back to the facilities within PCN",
    "Number of people waived for user fees in Hospitals within the PCN",
    "Total amount of user fees  waived in Health care Facilities within the PCN",
    "Healthcare Financing Score"
  ),
  "Health Infrastructure" = c(
    "Proportion of health facilities with accessible road network",
    "Proportion of  facilities with the appropriate WASH facilities",
    "Proportion of  facilities with the tracer list of infrastructure as per KEPH standards",
    "Proportion of facilties with a reliable power source",
    "PCN access to adequate ambulance services",
    "Ambulance Request Score",
    "Health Infrastructure Score"
  ),
  "HMIS / Digital Health" = c(
    "Proprtion of facilties with  reliable internet connection",
    "Proportion of facilities in the PCN with the key OPD reporting tools (6)",
    "No of performance and data quality review meetings held quarterly within the PCN",
    "Proportion of facilities with ICT infrastructure ",
    "Proportion of facilities in a PCN with an integrated functional EMR",
    "Proportion of CHUs within the PCN reporting monthly",
    "HMIS Score"
  ),
  "HRH" = c(
    "Core HRH density",
    "Doctor to population ratio",
    "Clinical officer to  population ratio",
    "Nurse to population ratio",
    "CHA/CHO  to population ratio",
    "Proportion of CHPs trained on basic modules",
    "Health care workers sensitized on PHC /PCN ",
    "Skills improvement mechanism",
    "Proportion of health workers who have undergone a skills/ competency buliding course within the last 2 years",
    "HRH Score"
  ),
  "Service Delivery" = c(
    "Number of outreaches conducted  by the MDT",
    "Number of in-reaches conducted within the PCN",
    "Service Delivery Score"
  ),
  "Quality of Care – Management Systems" = c(
    "Proportion of hospitals with functional facility quality improvement teams (QIT)",
    "Proportion of spokes with functional facility work improvement teams (WIT)",
    "Average availability of selected IPC items  *(items defined below)",
    "QoC Management Systems Score"
  ),
  "Quality of Care – PHC Core Systems" = c(
    "Adherence to clinical guidelines for Primary health care facilities",
    "Provider Availability (absenteeism) for Primary health care facilities",
    "QoC PHC Core Systems Score"
  ),
  "Quality of Care – Outcomes" = c(
    "Proportion of facilities conducting MPDSR",
    "Fresh Stillbirth rate per 1,000 births in health facilities",
    "Number of maternal deaths reported in Health facilities per 100,000 live births",
    "Proportion of maternal deaths Audited",
    "Number of neonatal deaths per 1,000 live births",
    "Proportion of neonatal deaths audited ",
    "TB treatment success",
    "QoC Outcomes Score"
  ),
  "Social Accountability" = c(
    "Proportion of facilities which have conducted a client satisfaction survey",
    "No. of MDT engagements with the community",
    "No. of health facilities with functional GRMs",
    "Social Accountability Score"
  ),
  "Innovations and Learning" = c(
    "Number of PHC related innovations/ best practice implemented/adapted",
    "Innovations and Learning Score"
  )
)

# Fuzzy-match PCN indicator names to actual column names
pcn_indicators <- lapply(pcn_indicators, function(vec) {
  matched <- sapply(vec, match_to_closest, choices = names(data_pcn))
  ifelse(is.na(matched), vec, matched)
})


# ============================================================
# 10. COUNTY INDICATOR GROUPS (10 THEMATIC TABS)
# ============================================================
county_indicators <- list(
  "Overview" = county_indicator_cols,
  
  "Governance & Leadership for PCNs" = c(
    "Proportion of functional PHC advisory Committees in the Place",
    "Proportion of PCNs Established",
    "Proportion of PCNs Gazetted",
    "Availability of a Functional PCN management committee",
    "Proportion of Hospital management boards appointed/gazetted",
    "Proportion of Health Facilities (level 2&3) with Health Facility Management Committee Appointed/Gazetted",
    "Availability of a functional PHC TWG Score",
    "Proportion of PCNs with an operational budget for the MDT activities",
    "Performance Review Score",
    "CHMT Support Supervision Score",
    "Governance Score",
    "Governance Weighted Score"
  ),
  "Human Resource for Health" = c(
    "Does the County have a mechanism to enhance health workers skills",
    "HRH Score", "HRH Weighted Score"
  ),
  "Health Product Technologies" = c(
    "Proportion of county health budget allocated to drugs and supplies",
    "Proportion of county HPT budget allocated to levels 2&3",
    "HPT Score", "HPT Weighted Score"
  ),
  "Service Delivery" = c(
    "PCNs with functional referral mechanisms",
    "Service Delivery Systems Score",
    "Service Delivery Systems Weighted Score"
  ),
  "Health Care Financing" = c(
    "Proportion of households registered on SHA within the County",
    "Healthcare Financing Score",
    "Healthcare Financing Weighted Score"
  ),
  "HMIS / Digital Health" = c(
    "Proportion of SMART PCNs in the County",
    "HMIS Score", "HMIS Weighted Score"
  ),
  "Quality of Care – Management Systems" = c(
    "Mechanism to Coordinate Quality Improvement Score",
    "Mechanism for Implementation of Support Supervision in Health Facilities Score",
    "Presence of an Infection Prevention Control (IPC) committee Score",
    "QoC Management Systems Score",
    "QoC Management Systems Weighted Score"
  ),
  "Multisectoral Partnerships and Coordination" = c(
    "Number of bi-annual multisectoral stakeholder forums Score",
    "Proportion of MOUs and partnership agreements aligned to PHC signed",
    "Research studies done on PCN implementation Score",
    "Multisectoral Partnerships and Coordination Score",
    "Multisectoral Partnerships and Coordination Weighted Score"
  ),
  "Innovations and Learning" = c(
    "Number of knowledge management and learning forums conducted Score",
    "No. of research studies done on PCN implementation Score",
    "Innovations and Learning Score",
    "Innovations and Learning Weighted Score"
  ),
  "Overall Performance" = c("Total County Score (Total Weighted Score)")
)

# Fuzzy-match county indicator names to actual column names
county_indicators <- lapply(county_indicators, function(vec) {
  matched <- sapply(vec, match_to_closest, choices = names(data_county))
  ifelse(is.na(matched), vec, matched)
})


# ============================================================
# 11. UI COLOUR PALETTE — Tab accent colours
# ============================================================
tab_colors <- list(
  overview       = "#1a73e8", governance  = "#d93025",
  pophealth      = "#e67e22", capacity    = "#f39c12",
  financing      = "#16a085", infrastructure = "#8e44ad",
  hmis           = "#2c3e50", hrh         = "#00a86b",
  service        = "#27ae60", qoc_mgmt    = "#c0392b",
  qoc_phc        = "#2980b9", qoc_outcomes = "#9b59b6",
  social         = "#e84393", innovation  = "#f1c40f"
)


# ============================================================
# 12. CSS — Floating controls, glossy boxes, layout overrides
# ============================================================
floating_css <- "
/* Anchor chart/map containers for floating child elements */
.chart-container, .map-container {
  position: relative !important;
  min-height: 520px !important;
}

/* Gear / settings toggle button (top-right of each panel) */
.control-button, .toggle-top-right {
  position: absolute !important;
  top: 60px !important;
  right: 12px !important;
  z-index: 9999 !important;
  background: rgba(255,255,255,0.95) !important;
  border-radius: 6px !important;
  border: 1px solid rgba(0,0,0,0.08) !important;
  padding: 6px 8px !important;
  cursor: pointer !important;
}

/* Frosted-glass settings popup */
.chart-settings-popup, .map-settings-popup {
  position: absolute !important;
  top: 48px !important;
  right: 12px !important;
  width: 200px !important;
  padding: 6px !important;
  border-radius: 8px !important;
  background: rgba(255,255,255,0.18) !important;
  backdrop-filter: blur(5px) saturate(120%) !important;
  -webkit-backdrop-filter: blur(6px) saturate(120%) !important;
  border: 1px solid rgba(255,255,255,0.22) !important;
  box-shadow: 0 8px 24px rgba(0,0,0,0.10) !important;
  z-index: 9998 !important;
  animation: slideDown 0.22s ease-out !important;
}

.small-help { font-size: 11px; color: #444; margin-top:6px; }
.box-body   { position: relative !important; }

@keyframes slideDown {
  from { opacity: 0; transform: translateY(-6px); }
  to   { opacity: 1; transform: translateY(0); }
}
"


# ============================================================
# 13. DASHBOARD UI  (shown after login)
# ============================================================
dashboard_ui <- bs4DashPage(
  title = "Kenya PCN Dashboard",
  
  # ── Header ──────────────────────────────────────────────
  header = bs4DashNavbar(
    title  = tags$span(
      "Kenya PCN Monitoring Dashboard",
      style = "color:#00eaff; font-weight:700; font-size:16px; text-shadow: 0 0 6px rgba(0,234,255,0.55);"
    ),
    skin   = "light",
    status = "primary",
    rightUi = tags$li(
      class = "nav-item dropdown",
      tags$a(
        class = "nav-link dropdown-toggle", href = "#",
        `data-toggle` = "dropdown",
        icon("sign-out-alt", style = "color:#FFFF00;"),
        span("Log Out", style = "color:#FFFF00; font-weight:600;")
      ),
      tags$div(
        class = "dropdown-menu dropdown-menu-right",
        actionButton("logout", "Log out",
                     icon  = icon("sign-out-alt"),
                     class = "dropdown-item text-danger")
      )
    )
  ),
  
  # ── Sidebar ─────────────────────────────────────────────
  sidebar = bs4DashSidebar(
    skin = "light", collapsed = FALSE,
    bs4SidebarMenu(
      id = "sidebar_menu",
      bs4SidebarMenuItem("PCN Establishment", tabName = "pcn_establishment", icon = icon("sitemap")),
      bs4SidebarMenuItem("PCN Monitoring",    tabName = "pcn_monitoring",    icon = icon("chart-bar")),
      bs4SidebarMenuItem("County Monitoring", tabName = "county_monitoring", icon = icon("landmark")),
      bs4SidebarMenuItem("Data Table",        tabName = "datatable",         icon = icon("table"))
    )
  ),
  
  # ── Body ────────────────────────────────────────────────
  body = bs4DashBody(
    
    tags$head(
      # JS to toggle body class for intro page background
      tags$script(HTML("
        Shiny.addCustomMessageHandler('setBodyClass', function(msg) {
          if (msg.remove) document.body.classList.remove(msg.remove);
          if (msg.add)    document.body.classList.add(msg.add);
        });
        // Reset overflow when dashboard loads
        document.documentElement.style.overflow = 'auto';
        document.body.style.overflow = 'auto';
      ")),
      tags$style(HTML("
        html, body, .wrapper { overflow: auto !important; height: auto !important; }
        .content-wrapper { overflow-y: auto !important; height: auto !important; min-height: 100vh !important; }
      ")),
      # Floating controls CSS
      tags$style(HTML(floating_css)),
      tags$style(HTML("
 .nav-tabs { background:rgba(5,15,30,0.85) !important; border-radius:10px 10px 0 0 !important; padding:4px 6px 0 6px !important; flex-wrap:wrap !important; border-top:1px solid rgba(0,234,255,0.35) !important; border-bottom:none !important; box-shadow:0 -2px 6px rgba(0,234,255,0.12) !important; }
  .nav-tabs .nav-link { background:rgba(5,15,30,0.60) !important; color:rgba(255,255,255,0.95) !important; border:1px solid rgba(0,234,255,0.20) !important; border-bottom:none !important; border-radius:8px 8px 0 0 !important; margin-right:3px !important; margin-bottom:3px !important; font-size:11px !important; font-weight:600 !important; padding:5px 8px !important; opacity:1 !important; white-space:nowrap !important; }
  .nav-tabs .nav-link.active { background:rgba(5,15,30,0.85) !important; color:#00eaff !important; border-color:rgba(0,234,255,0.45) !important; border-bottom:none !important; text-shadow:0 0 6px rgba(0,234,255,0.55) !important; font-weight:700 !important; }
  .nav-tabs .nav-link span { color:inherit !important; opacity:1 !important; visibility:visible !important; }
")),
      tags$style(HTML("
  /* Dummy data toggle button */
  .dummy-toggle-btn {
    position: absolute !important;
    top: 60px !important;
    right: 52px !important;   /* sits left of the gear button */
    z-index: 9999 !important;
    background: rgba(255,255,255,0.95) !important;
    border-radius: 6px !important;
    border: 1px solid rgba(0,0,0,0.08) !important;
    padding: 4px 8px !important;
    cursor: pointer !important;
    font-size: 11px !important;
    font-weight: 600 !important;
    color: #c0392b !important;
    line-height: 1.3 !important;
    text-align: center !important;
    min-width: 38px !important;
  }
  .dummy-toggle-btn.real-only {
    color: #27ae60 !important;
    border-color: #27ae60 !important;
    background: rgba(39,174,96,0.08) !important;
  }
  /* No-data watermark overlay */
  .nodata-watermark {
    position: absolute !important;
    top: 50% !important;
    left: 50% !important;
    transform: translate(-50%, -50%) rotate(-20deg) !important;
    font-size: 38px !important;
    font-weight: 900 !important;
    color: rgba(180,0,0,0.13) !important;
    pointer-events: none !important;
    z-index: 10 !important;
    white-space: nowrap !important;
    letter-spacing: 4px !important;
    text-transform: uppercase !important;
    border: 4px solid rgba(180,0,0,0.10) !important;
    padding: 6px 18px !important;
    border-radius: 8px !important;
    user-select: none !important;
  }
")),
      
      # Tab accent colours + content panel backgrounds
      tags$style(HTML("
  .nav-tabs .nav-link[data-value='overview']      { color:white !important; }
  .nav-tabs .nav-link[data-value='governance']    { color:white !important; }
  .nav-tabs .nav-link[data-value='pophealth']     { color:white !important; }
  .nav-tabs .nav-link[data-value='capacity']      { color:white !important; }
  .nav-tabs .nav-link[data-value='financing']     { color:white !important; }
  .nav-tabs .nav-link[data-value='infrastructure']{ color:white !important; }
  .nav-tabs .nav-link[data-value='hmis']          { color:white !important; }
  .nav-tabs .nav-link[data-value='hrh']           { color:white !important; }
  .nav-tabs .nav-link[data-value='service']       { color:white !important; }
  .nav-tabs .nav-link[data-value='qoc_mgmt']      { color:white !important; }
  .nav-tabs .nav-link[data-value='qoc_phc']       { color:white !important; }
  .nav-tabs .nav-link[data-value='qoc_outcomes']  { color:white !important; }
  .nav-tabs .nav-link[data-value='social']        { color:white !important; }
  .nav-tabs .nav-link[data-value='innovation']    { color:white !important; }

  #overview      { background:rgba(0,63,92,0.07);    padding:15px; border-radius:8px; }
  #governance    { background:rgba(88,80,141,0.07);  padding:15px; border-radius:8px; }
  #pophealth     { background:rgba(188,80,144,0.07); padding:15px; border-radius:8px; }
  #capacity      { background:rgba(255,99,97,0.07);  padding:15px; border-radius:8px; }
  #financing     { background:rgba(255,166,0,0.07);  padding:15px; border-radius:8px; }
  #infrastructure{ background:rgba(47,75,124,0.07);  padding:15px; border-radius:8px; }
  #hmis          { background:rgba(102,81,145,0.07); padding:15px; border-radius:8px; }
  #hrh           { background:rgba(160,81,149,0.07); padding:15px; border-radius:8px; }
  #service       { background:rgba(212,80,135,0.07); padding:15px; border-radius:8px; }
  #qoc_mgmt      { background:rgba(249,93,106,0.07); padding:15px; border-radius:8px; }
  #qoc_phc       { background:rgba(255,124,67,0.07); padding:15px; border-radius:8px; }
  #qoc_outcomes  { background:rgba(255,166,0,0.07);  padding:15px; border-radius:8px; }
  #social        { background:rgba(44,160,44,0.07);  padding:15px; border-radius:8px; }
  #innovation    { background:rgba(23,190,207,0.07); padding:15px; border-radius:8px; }
")),
      
      # Glossy metric box styles + gradient colours
      tags$style(HTML("
        .glossy-box {
          border-radius:16px; padding:20px; color:white;
          position:relative; overflow:hidden;
          box-shadow:0 6px 14px rgba(0,0,0,0.15);
          transition:transform 0.15s ease, box-shadow 0.15s ease;
        }
        .glossy-box:hover { transform:translateY(-3px); box-shadow:0 10px 22px rgba(0,0,0,0.25); }
        .glossy-box {
  box-shadow: 0 6px 14px rgba(0,0,0,0.25) !important;
}
        .bg-blue   { background:linear-gradient(135deg,#1a73e8,#4dabf7); }
        .bg-green  { background:linear-gradient(135deg,#2ecc71,#27ae60); }
        .bg-red    { background:linear-gradient(135deg,#e74c3c,#c0392b); }
        .bg-orange { background:linear-gradient(135deg,#f39c12,#e67e22); }
        .bg-purple { background:linear-gradient(135deg,#8e44ad,#6c5ce7); }
        .glossy-title { font-size:14px; opacity:0.9; }
        .glossy-value { font-size:34px; font-weight:700; }
        .glossy-sub   { font-size:13px; opacity:0.85; }
.glossy-box   { padding:10px !important; min-height:105px !important; }
.glossy-title { font-size:11px !important; }
.glossy-value { font-size:18px !important; }
.glossy-sub   { font-size:10px !important; }
    ")),
      tags$style(HTML("
#pcn_establishment h4,
#pcn_establishment h5 {
  color: #7df9ff !important;
  text-shadow:
    0 0 6px rgba(125,249,255,0.8),
    0 0 12px rgba(0,255,255,0.6),
    0 0 20px rgba(0,255,255,0.4);
  font-weight: 700 !important;
}
")),
      tags$style(HTML("

/* existing CSS above */

/* ===== PCN HEADER ===== */
.glow-pcn-performance,
.glow-pcn-coverage {

  display: block !important;
  width: 100% !important;

  padding: 12px 20px;
  margin: 10px 0;

  border-radius: 12px;

  background: rgba(0, 234, 255, 0.25) !important;

  box-shadow:
    0 0 20px rgba(0,234,255,0.5),
    0 0 40px rgba(0,234,255,0.35);

  position: relative;
  overflow: hidden;
}



.glow-pcn-performance,
.glow-pcn-coverage {
  color: #00eaff !important;

  text-shadow:
    0 0 8px rgba(0,234,255,1),
    0 0 16px rgba(0,234,255,0.9),
    0 0 28px rgba(0,234,255,0.7);

  font-weight: 800 !important;
  letter-spacing: 0.6px;

  position: relative;
  z-index: 2;
}

/* existing CSS continues */

")),
      # Expand button hover behaviour
      tags$style(HTML("
        .hover-expand-zone {
          position:absolute; bottom:0; left:0;
          height:60px; width:100%; cursor:pointer; z-index:998;
        }
        .expand-wrapper {
          position:absolute; bottom:12px; right:12px;
          display:flex; align-items:center;
          opacity:0; transition:opacity 0.25s ease-in-out; z-index:9999;
        }
        .chart-container:hover .expand-wrapper,
        .map-container:hover  .expand-wrapper { opacity:1; }
        .expand-btn {
          width:42px; height:42px; border-radius:50%;
          background:white; border:1px solid #ccc;
          box-shadow:0 2px 8px rgba(0,0,0,0.15);
          display:flex; align-items:center; justify-content:center;
          font-size:20px; cursor:pointer;
        }
        .expand-label {
          background:black; color:white;
          padding:6px 10px; border-radius:6px;
          font-size:12px; margin-right:8px; white-space:nowrap;
        }
      ")),
      
      # Modal overlay behaviour
      tags$style(HTML("
        /* Hide floating controls behind open modal */
        body.modal-open .toggle-top-right,
        body.modal-open .hover-expand-zone,
        body.modal-open .expand-wrapper,
        body.modal-open .chart-settings-popup,
        body.modal-open .map-settings-popup {
          display:none !important; visibility:hidden !important;
          opacity:0 !important; pointer-events:none !important;
        }
        body.modal-open .chart-container,
        body.modal-open .map-container { pointer-events:none !important; }
        body.modal-open .content-wrapper { filter:blur(2px) brightness(0.85); }

        /* Ensure modal sits above everything */
        .modal         { z-index:99999 !important; }
        .modal-backdrop{ z-index:99998 !important; }

        /* Modal gear button */
        .modal-toggle-btn {
          position:absolute !important; top:10px !important;
          right:15px !important; z-index:10000 !important;
        }
        .modal-toggle-btn button { font-size:18px !important; padding:4px 10px !important; }

        /* Prevent plot clipping inside modals */
        .plotly, .shiny-plot-output { overflow:visible !important; }
      ")),
      
      # Card + box title colour overrides
      tags$style(HTML("
        .card                          { border-radius:14px !important; box-shadow:0 6px 18px rgba(0,0,0,0.12) !important; width:100% !important; }
        .card-header                   { font-weight:600 !important; font-size:15px !important; }
        .card .card-header .card-title { color:#0057B7 !important; font-weight:bold !important; }
        .modal-dialog .modal-content h3{ color:#D32F2F !important; font-weight:bold !important; }
      ")),
      
      # Full-width content area (overrides bs4Dash sidebar margin)
      tags$style(HTML("
  .content-wrapper {
    max-width:100% !important;
    width:calc(100% - 250px) !important;
    margin-left:250px !important;
    padding-left:10px !important;
    padding-right:10px !important;
  }
  .container-fluid {
    max-width:100% !important;
    width:100% !important;
    padding-left:10px !important;
    padding-right:10px !important;
  }
  .main-sidebar { position:fixed !important; width:250px !important; }
  .main-header  { margin-left:250px !important; }
  .main-footer  { margin-left:250px !important; }
")),                     
      # Glassmorphism theme
      tags$style(HTML("
/* ============================================================
   BASE THEME — Black glassmorphism
============================================================ */

/* Toggle switch styling */
.dark-mode-checkbox { display: none; }

.dark-mode-label {
  display: block;
  width: 52px; height: 26px;
  background: rgba(255,255,255,0.20);
  border-radius: 13px;
  border: 1px solid rgba(255,255,255,0.30);
  position: relative;
  cursor: pointer;
  transition: background 0.3s ease;
}

.dark-mode-label::after {
  content: '';
  position: absolute;
  top: 3px; left: 3px;
  width: 20px; height: 20px;
  background: white;
  border-radius: 50%;
  transition: transform 0.3s ease, background 0.3s ease;
  box-shadow: 0 2px 6px rgba(0,0,0,0.3);
}

.dark-mode-checkbox:checked + .dark-mode-label {
  background: rgba(0,255,180,0.40);
  border-color: rgba(0,255,180,0.60);
}

.dark-mode-checkbox:checked + .dark-mode-label::after {
  transform: translateX(26px);
  background: #00ffb4;
}

/* ── PAGE BACKGROUND — pure black base ── */
body, .wrapper {
  background: #ffffff !important;
  min-height: 100vh !important;
}

/* ── CONTENT WRAPPER ── */
.content-wrapper {
  background: rgba(255,255,255,0.95) !important;
  backdrop-filter: blur(4px) !important;
}

/* ── SIDEBAR ── */
.main-sidebar, .sidebar {
  background: rgba(20,20,20,0.90) !important;
  backdrop-filter: blur(16px) !important;
  -webkit-backdrop-filter: blur(16px) !important;
  border-right: 1px solid rgba(255,255,255,0.08) !important;
  box-shadow: 4px 0 24px rgba(0,0,0,0.60) !important;
}

.sidebar-menu > li > a,
.nav-sidebar > li > a {
  color: rgba(255,255,255,0.75) !important;
  border-radius: 10px !important;
  margin: 3px 8px !important;
  transition: background 0.25s ease !important;
}

.sidebar-menu > li.active > a,
.nav-sidebar > li.active > a,
.sidebar-menu > li > a:hover,
.nav-sidebar > li > a:hover {
  background: rgba(255,255,255,0.10) !important;
  color: white !important;
}

/* ── NAVBAR ── */
.main-header, .navbar {
  background: rgba(30,60,120,0.92) !important;
  backdrop-filter: blur(16px) !important;
  -webkit-backdrop-filter: blur(16px) !important;
  border-bottom: 1px solid rgba(255,255,255,0.15) !important;
  box-shadow: 0 4px 24px rgba(0,0,0,0.30) !important;
}
.navbar-brand, .main-header .navbar-nav .nav-link,
.main-header .brand-text {
  color: #00eaff !important;
  text-shadow: 0 0 6px rgba(0,234,255,0.55) !important;
}

/* ── CARDS / BOXES ── */
.card, .box, .info-box, .small-box {
  background: rgba(30,30,30,0.80) !important;
  backdrop-filter: blur(16px) !important;
  -webkit-backdrop-filter: blur(16px) !important;
  border: 1px solid rgba(255,255,255,0.10) !important;
  border-radius: 18px !important;
  box-shadow: 0 8px 32px rgba(0,0,0,0.50), inset 0 1px 0 rgba(255,255,255,0.06) !important;
  color: rgba(255,255,255,0.90) !important;
  transition: transform 0.2s ease, box-shadow 0.2s ease !important;
}

.card:hover, .box:hover {
  transform: translateY(-2px) !important;
  box-shadow: 0 14px 40px rgba(0,0,0,0.65) !important;
}

.card-header, .box-header {
  background: rgba(40,40,40,0.70) !important;
  border-bottom: 1px solid rgba(255,255,255,0.08) !important;
  border-radius: 18px 18px 0 0 !important;
}

.card .card-header .card-title, .box-title {
  color: rgba(255,255,255,0.95) !important;
  font-weight: 700 !important;
}

/* Center box titles */
.box-header .box-title {
  width: 100% !important;
  text-align: center !important;
}

/* ── GLOSSY METRIC BOXES ── */
.glossy-box {
  border: 1px solid rgba(255,255,255,0.15) !important;
  border-radius: 16px !important;
  box-shadow: 0 6px 24px rgba(0,0,0,0.50), inset 0 1px 0 rgba(255,255,255,0.12) !important;
  color: white !important;
  transition: transform 0.18s ease, box-shadow 0.18s ease !important;
}

.glossy-box:hover {
  transform: translateY(-4px) !important;
  box-shadow: 0 14px 36px rgba(0,0,0,0.60) !important;
}

.bg-blue   { background: linear-gradient(135deg, rgba(26,115,232,0.75), rgba(77,171,247,0.65))  !important; }
.bg-green  { background: linear-gradient(135deg, rgba(46,204,113,0.75), rgba(39,174,96,0.65))   !important; }
.bg-red    { background: linear-gradient(135deg, rgba(231,76,60,0.75),  rgba(192,57,43,0.65))   !important; }
.bg-orange { background: linear-gradient(135deg, rgba(243,156,18,0.75), rgba(230,126,34,0.65))  !important; }
.bg-purple { background: linear-gradient(135deg, rgba(142,68,173,0.75), rgba(108,92,231,0.65))  !important; }
.bg-deepgreen   { background: linear-gradient(135deg, rgba(0,100,0,0.80),    rgba(0,150,0,0.65))    !important; }
.bg-lightgreen  { background: linear-gradient(135deg, rgba(46,160,67,0.80),  rgba(80,200,90,0.65))  !important; }
.bg-yellowgreen { background: linear-gradient(135deg, rgba(120,180,0,0.80),  rgba(160,210,0,0.65))  !important; }
.bg-yellow      { background: linear-gradient(135deg, rgba(200,180,0,0.80),  rgba(240,210,0,0.65))  !important; }
.bg-amber       { background: linear-gradient(135deg, rgba(210,120,0,0.80),  rgba(240,150,0,0.65))  !important; }
.bg-orangered   { background: linear-gradient(135deg, rgba(200,60,0,0.80),   rgba(230,80,0,0.65))   !important; }
/* ── TABS ── */
.nav-tabs {
  border-bottom: 1px solid rgba(255,255,255,0.08) !important;
  gap: 4px !important;
}

.nav-tabs .nav-link {
  backdrop-filter: blur(6px) !important;
  border-radius: 10px 10px 0 0 !important;
  transition: background 0.2s ease !important;
  opacity: 0.85;
}

.nav-tabs .nav-link.active {
  font-weight: 700 !important;
  border-bottom-color: transparent !important;
  opacity: 1;
}

.tab-content, .tab-pane {
  background: rgba(20,20,20,0.70) !important;
  border: 1px solid rgba(255,255,255,0.08) !important;
  border-radius: 0 12px 12px 12px !important;
  padding: 14px !important;
}

/* ── SELECT INPUTS ── */
.selectize-control .selectize-input,
.form-control, select {
  background: rgba(10,20,45,0.98) !important;
  border: 1px solid rgba(0,234,255,0.40) !important;
  border-radius: 10px !important;
  color: #ffffff !important;
  backdrop-filter: none !important;
  -webkit-backdrop-filter: none !important;
}

.selectize-control .selectize-input input,
.selectize-control .selectize-input .item { color: white !important; }

.selectize-dropdown {
  background: rgba(25,25,25,0.97) !important;
  border: 1px solid rgba(255,255,255,0.15) !important;
  border-radius: 10px !important;
  color: white !important;
  box-shadow: 0 10px 30px rgba(0,0,0,0.70) !important;
}

.selectize-dropdown .option:hover,
.selectize-dropdown .option.active {
  background: rgba(255,255,255,0.10) !important;
  color: white !important;
}

/* ── BUTTONS ── */
.btn, .action-button {
  background: rgba(50,50,50,0.90) !important;
  border: 1px solid rgba(255,255,255,0.15) !important;
  border-radius: 10px !important;
  color: white !important;
  box-shadow: 0 3px 10px rgba(0,0,0,0.40) !important;
  transition: background 0.2s ease, transform 0.15s ease !important;
}

.btn:hover, .action-button:hover {
  background: rgba(70,70,70,0.95) !important;
  transform: translateY(-1px) !important;
  color: white !important;
}

/* ── DATA TABLE ── */
.dataTables_wrapper, table.dataTable {
  background: transparent !important;
  color: rgba(255,255,255,0.85) !important;
}

table.dataTable thead th {
  background: rgba(40,40,40,0.90) !important;
  color: white !important;
  border-bottom: 1px solid rgba(255,255,255,0.15) !important;
}

table.dataTable tbody tr {
  background: rgba(30,30,30,0.60) !important;
  color: rgba(255,255,255,0.85) !important;
  border-bottom: 1px solid rgba(255,255,255,0.05) !important;
}

table.dataTable tbody tr:hover {
  background: rgba(255,255,255,0.08) !important;
}

.dataTables_filter input,
.dataTables_length select {
  background: rgba(40,40,40,0.90) !important;
  border: 1px solid rgba(255,255,255,0.15) !important;
  border-radius: 8px !important;
  color: white !important;
}

/* ── MODAL DIALOGS ── */
.modal-content {
  background: rgba(15,15,15,0.92) !important;
  backdrop-filter: blur(24px) !important;
  border: 1px solid rgba(255,255,255,0.12) !important;
  border-radius: 20px !important;
  box-shadow: 0 24px 80px rgba(0,0,0,0.80) !important;
  color: white !important;
}

.modal-header {
  border-bottom: 1px solid rgba(255,255,255,0.10) !important;
  background: rgba(30,30,30,0.80) !important;
  border-radius: 20px 20px 0 0 !important;
}

.modal-dialog .modal-content h3 { color: #ff6b6b !important; }

/* ── SCROLLBARS ── */
::-webkit-scrollbar       { width: 6px; height: 6px; }
::-webkit-scrollbar-track { background: rgba(255,255,255,0.03); border-radius: 3px; }
::-webkit-scrollbar-thumb { background: rgba(255,255,255,0.15); border-radius: 3px; }
::-webkit-scrollbar-thumb:hover { background: rgba(255,255,255,0.30); }

/* ── PLOTS ── */
.shiny-plot-output, .plotly {
  background: transparent !important;
  border-radius: 12px !important;
  overflow: visible !important;
}

/* ── INPUTS ── */
input[type='number'], input[type='text'], input[type='password'] {
  background: rgba(15,25,50,0.95) !important;
  border: 1px solid rgba(0,234,255,0.35) !important;
  border-radius: 8px !important;
  color: #ffffff !important;
  backdrop-filter: none !important;
}

input[type='number']:focus, input[type='text']:focus {
  border-color: rgba(0,255,180,0.60) !important;
  box-shadow: 0 0 0 3px rgba(0,255,180,0.10) !important;
  outline: none !important;
}

/* ── BASE TEXT ── */
label              { color: rgba(255,255,255,0.80) !important; font-weight: 500 !important; }
h3, h4             { color: #4db8ff !important; font-weight: 700 !important; text-shadow: 0 0 6px rgba(77,184,255,0.85) !important; }
h5                 { color: #4db8ff !important; font-weight: 600 !important; text-shadow: 0 0 6px rgba(77,184,255,0.85) !important; }
p                  { color: rgba(255,255,255,0.75) !important; }
.checkbox label,
.radio label       { color: rgba(255,255,255,0.80) !important; }

/* ── SETTINGS POPUPS ── */
.chart-settings-popup, .map-settings-popup {
  background: rgba(20,20,20,0.92) !important;
  backdrop-filter: blur(16px) !important;
  border: 1px solid rgba(255,255,255,0.12) !important;
  box-shadow: 0 10px 40px rgba(0,0,0,0.60) !important;
  color: white !important;
}

.expand-btn   { background: rgba(40,40,40,0.90) !important; border: 1px solid rgba(255,255,255,0.20) !important; color: white !important; }
.expand-label { background: rgba(0,0,0,0.70) !important; color: white !important; }


/* ============================================================
   DARK MODE — activated when toggle is checked
   All invisible text becomes neon
============================================================ */

body.dark-mode, body.dark-mode .wrapper {
  background: #000000 !important;
}

/* Neon text for all headings */
body.dark-mode h1, body.dark-mode h2,
body.dark-mode h3, body.dark-mode h4,
body.dark-mode h5, body.dark-mode h6 {
  color: #00ffb4 !important;
  text-shadow: 0 0 8px rgba(0,255,180,0.70), 0 0 20px rgba(0,255,180,0.40) !important;
}

/* Neon body text */
body.dark-mode p,
body.dark-mode label,
body.dark-mode span:not(.nav-link span),
body.dark-mode .glossy-title,
body.dark-mode .glossy-sub {
  color: #e0ffe0 !important;
  text-shadow: 0 0 6px rgba(0,255,180,0.30) !important;
}

/* Neon values in metric boxes */
body.dark-mode .glossy-value {
  color: #ffffff !important;
  text-shadow: 0 0 10px rgba(255,255,255,0.80), 0 0 24px rgba(0,255,180,0.50) !important;
}

/* Neon card titles */
body.dark-mode .card .card-header .card-title,
body.dark-mode .box-title {
  color: #00ffb4 !important;
  text-shadow: 0 0 8px rgba(0,255,180,0.60) !important;
}

/* Neon tab labels */
body.dark-mode .nav-tabs .nav-link span {
  text-shadow: 0 0 6px currentColor !important;
  filter: brightness(1.4) !important;
}

/* Brighter inputs in dark mode */
body.dark-mode .selectize-control .selectize-input,
body.dark-mode .form-control,
body.dark-mode select,
body.dark-mode input[type='number'],
body.dark-mode input[type='text'] {
  background: rgba(0,20,10,0.80) !important;
  border-color: rgba(0,255,180,0.40) !important;
  color: #00ffb4 !important;
  text-shadow: 0 0 4px rgba(0,255,180,0.30) !important;
}

/* Neon select options */
body.dark-mode .selectize-dropdown {
  background: rgba(0,10,5,0.97) !important;
  border-color: rgba(0,255,180,0.30) !important;
  color: #00ffb4 !important;
}

body.dark-mode .selectize-dropdown .option:hover,
body.dark-mode .selectize-dropdown .option.active {
  background: rgba(0,255,180,0.15) !important;
  color: #00ffb4 !important;
}

/* Neon buttons */
body.dark-mode .btn,
body.dark-mode .action-button {
  background: rgba(0,30,15,0.85) !important;
  border-color: rgba(0,255,180,0.40) !important;
  color: #00ffb4 !important;
  text-shadow: 0 0 6px rgba(0,255,180,0.50) !important;
  box-shadow: 0 0 12px rgba(0,255,180,0.15) !important;
}

body.dark-mode .btn:hover,
body.dark-mode .action-button:hover {
  background: rgba(0,50,25,0.90) !important;
  box-shadow: 0 0 20px rgba(0,255,180,0.30) !important;
}

/* Neon table text */
body.dark-mode table.dataTable thead th {
  color: #00ffb4 !important;
  text-shadow: 0 0 6px rgba(0,255,180,0.50) !important;
  background: rgba(0,30,15,0.80) !important;
  border-color: rgba(0,255,180,0.20) !important;
}

body.dark-mode table.dataTable tbody tr {
  color: #c0ffe0 !important;
  background: rgba(0,15,8,0.60) !important;
}

body.dark-mode table.dataTable tbody tr:hover {
  background: rgba(0,255,180,0.08) !important;
}

/* Neon data table search/length inputs */
body.dark-mode .dataTables_filter input,
body.dark-mode .dataTables_length select,
body.dark-mode .dataTables_info,
body.dark-mode .dataTables_paginate {
  color: #00ffb4 !important;
  border-color: rgba(0,255,180,0.30) !important;
  background: rgba(0,20,10,0.80) !important;
}

/* Neon cards in dark mode */
body.dark-mode .card,
body.dark-mode .box {
  background: rgba(0,15,8,0.75) !important;
  border-color: rgba(0,255,180,0.12) !important;
  box-shadow: 0 8px 32px rgba(0,0,0,0.70), 0 0 0 1px rgba(0,255,180,0.08) !important;
}

body.dark-mode .card-header,
body.dark-mode .box-header {
  background: rgba(0,25,12,0.80) !important;
  border-color: rgba(0,255,180,0.12) !important;
}

/* Neon settings popups */
body.dark-mode .chart-settings-popup,
body.dark-mode .map-settings-popup {
  background: rgba(0,15,8,0.95) !important;
  border-color: rgba(0,255,180,0.25) !important;
  box-shadow: 0 10px 40px rgba(0,0,0,0.80), 0 0 20px rgba(0,255,180,0.10) !important;
  color: #00ffb4 !important;
}

/* Neon modal */
body.dark-mode .modal-content {
  background: rgba(0,12,6,0.95) !important;
  border-color: rgba(0,255,180,0.20) !important;
  box-shadow: 0 24px 80px rgba(0,0,0,0.90), 0 0 40px rgba(0,255,180,0.10) !important;
  color: #c0ffe0 !important;
}

body.dark-mode .modal-header {
  background: rgba(0,20,10,0.90) !important;
  border-color: rgba(0,255,180,0.15) !important;
}

body.dark-mode .modal-dialog .modal-content h3 {
  color: #00ffb4 !important;
  text-shadow: 0 0 10px rgba(0,255,180,0.70) !important;
}

/* Neon tab content */
body.dark-mode .tab-content,
body.dark-mode .tab-pane {
  background: rgba(0,12,6,0.70) !important;
  border-color: rgba(0,255,180,0.10) !important;
}

/* Neon scrollbar */
body.dark-mode ::-webkit-scrollbar-thumb {
  background: rgba(0,255,180,0.30) !important;
}

body.dark-mode ::-webkit-scrollbar-thumb:hover {
  background: rgba(0,255,180,0.55) !important;
}

/* Neon expand button */
body.dark-mode .expand-btn {
  background: rgba(0,25,12,0.90) !important;
  border-color: rgba(0,255,180,0.35) !important;
  color: #00ffb4 !important;
  box-shadow: 0 0 10px rgba(0,255,180,0.20) !important;
}

body.dark-mode .expand-label {
  background: rgba(0,0,0,0.80) !important;
  color: #00ffb4 !important;
  border: 1px solid rgba(0,255,180,0.25) !important;
}

/* Sidebar neon in dark mode */
body.dark-mode .main-sidebar,
body.dark-mode .sidebar {
  background: rgba(0,10,5,0.95) !important;
  border-color: rgba(0,255,180,0.10) !important;
}

body.dark-mode .sidebar-menu > li > a,
body.dark-mode .nav-sidebar > li > a {
  color: rgba(0,255,180,0.80) !important;
}

body.dark-mode .sidebar-menu > li.active > a,
body.dark-mode .nav-sidebar > li.active > a,
body.dark-mode .sidebar-menu > li > a:hover,
body.dark-mode .nav-sidebar > li > a:hover {
  background: rgba(0,255,180,0.12) !important;
  color: #00ffb4 !important;
  text-shadow: 0 0 8px rgba(0,255,180,0.50) !important;
}

/* Header neon */
body.dark-mode .main-header,
body.dark-mode .navbar {
  background: rgba(0,8,4,0.95) !important;
  border-color: rgba(0,255,180,0.10) !important;
  box-shadow: 0 4px 24px rgba(0,0,0,0.80), 0 0 20px rgba(0,255,180,0.05) !important;
}
/* ============================================================
   DARK MODE TOGGLE — Always visible in navbar
============================================================ */

/* Wrapper that holds the icon + switch */
.custom-toggle-wrapper {
  display: flex !important;
  align-items: center !important;
  gap: 6px !important;
}

/* The sliding pill track */
.dark-mode-label {
  display: block !important;
  width: 56px !important;
  height: 28px !important;
  background: rgba(255,255,255,0.25) !important;
  border-radius: 14px !important;
  border: 2px solid rgba(255,255,255,0.60) !important;
  position: relative !important;
  cursor: pointer !important;
  transition: background 0.3s ease, border-color 0.3s ease !important;
  box-shadow: 0 0 10px rgba(255,255,255,0.20),
              inset 0 1px 3px rgba(0,0,0,0.30) !important;
}

/* The sliding circle */
.dark-mode-label::after {
  content: '' !important;
  position: absolute !important;
  top: 3px !important;
  left: 3px !important;
  width: 18px !important;
  height: 18px !important;
  background: #ffffff !important;
  border-radius: 50% !important;
  transition: transform 0.3s ease, background 0.3s ease !important;
  box-shadow: 0 2px 6px rgba(0,0,0,0.50) !important;
}

/* When toggled ON — neon green active state */
.dark-mode-checkbox:checked + .dark-mode-label {
  background: rgba(0,255,180,0.50) !important;
  border-color: #00ffb4 !important;
  box-shadow: 0 0 14px rgba(0,255,180,0.60),
              inset 0 1px 3px rgba(0,0,0,0.20) !important;
}

.dark-mode-checkbox:checked + .dark-mode-label::after {
  transform: translateX(28px) !important;
  background: #00ffb4 !important;
  box-shadow: 0 0 8px rgba(0,255,180,0.80) !important;
}

/* Moon/Sun emoji label — always white and visible */
#dark_mode_label {
  color: #ffffff !important;
  font-size: 16px !important;
  font-weight: 700 !important;
  text-shadow: 0 0 8px rgba(255,255,255,0.60) !important;
  opacity: 1 !important;
  visibility: visible !important;
  display: inline-block !important;
  min-width: 20px !important;
}

/* Make sure the nav-item containing the toggle is fully visible */
.nav-item .custom-toggle-wrapper,
.nav-item #dark_mode_label {
  opacity: 1 !important;
  visibility: visible !important;
  pointer-events: auto !important;
}

/* Override any inherited opacity/color from navbar CSS */
.main-header .nav-item span#dark_mode_label {
  color: #ffffff !important;
  opacity: 1 !important;
}

/* Dark mode active state — toggle stays neon visible */
body.dark-mode .dark-mode-label {
  background: rgba(0,255,180,0.40) !important;
  border-color: #00ffb4 !important;
  box-shadow: 0 0 16px rgba(0,255,180,0.70),
              0 0 40px rgba(0,255,180,0.20) !important;
}

body.dark-mode #dark_mode_label {
  color: #00ffb4 !important;
  text-shadow: 0 0 10px rgba(0,255,180,0.80) !important;
}
")),
      tags$style(HTML("
/* ============================================================
   GLOSSY BOX TOOLTIP — shows county list on hover
============================================================ */

.glossy-box-wrapper {
  position: relative !important;
  display: block !important;
}

.glossy-tooltip {
  visibility: hidden;
  opacity: 0;
  position: absolute;
  top: calc(100% + 8px);
  left: 50%;
  transform: translateX(-50%);
  z-index: 99999;
  min-width: 220px;
  max-width: 340px;
  max-height: 280px;          /* ← caps height */
  overflow-y: auto;           /* ← scrolls when list is long */
  background: rgba(10,10,10,0.95);
  backdrop-filter: blur(16px);
  border: 1px solid rgba(255,255,255,0.20);
  border-radius: 12px;
  padding: 12px 14px;
  box-shadow: 0 12px 40px rgba(0,0,0,0.60);
  transition: opacity 0.22s ease, visibility 0.22s ease;
  pointer-events: auto !important;
}

/* Tooltip arrow pointing down */
.glossy-tooltip::after {
  content: '';
  position: absolute;
  top: 100%;
  left: 50%;
  transform: translateX(-50%);
  border: 7px solid transparent;
  border-top-color: rgba(10,10,10,0.95);
}

.glossy-tooltip .tooltip-title {
  font-size: 11px;
  font-weight: 700;
  color: #00ffb4;
  text-transform: uppercase;
  letter-spacing: 0.8px;
  margin-bottom: 8px;
  padding-bottom: 6px;
  border-bottom: 1px solid rgba(255,255,255,0.12);
}

.glossy-tooltip .tooltip-county {
  font-size: 12px;
  color: rgba(255,255,255,0.90);
  padding: 3px 0;
  display: flex;
  align-items: center;
  gap: 6px;
}

.glossy-tooltip .tooltip-county::before {
  content: '';
  display: inline-block;
  width: 6px;
  height: 6px;
  border-radius: 50%;
  background: #00ffb4;
  flex-shrink: 0;
}

.glossy-tooltip .tooltip-empty {
  font-size: 12px;
  color: rgba(255,255,255,0.45);
  font-style: italic;
}
/* Scrollbar inside tooltip */
.glossy-tooltip::-webkit-scrollbar       { width: 4px; }
.glossy-tooltip::-webkit-scrollbar-track { background: rgba(255,255,255,0.05); border-radius: 2px; }
.glossy-tooltip::-webkit-scrollbar-thumb { background: rgba(255,255,255,0.25); border-radius: 2px; }
.glossy-tooltip::-webkit-scrollbar-thumb:hover { background: rgba(0,255,180,0.50); }
")), 
      tags$script(HTML("
// ============================================================
// TOOLTIP MANAGER
// - Shows only on hover over its own box
// - Grace period so mouse can move into tooltip to scroll
// - Auto-fades after 3s of no interaction
// - JS-only, no CSS hover triggers
// ============================================================

function showTooltip(tooltip) {
  tooltip.style.visibility = 'visible';
  tooltip.style.opacity    = '1';
}

function hideTooltip(tooltip) {
  tooltip.style.visibility = 'hidden';
  tooltip.style.opacity    = '0';
}

function attachTooltipListeners() {

  document.querySelectorAll('.glossy-box-wrapper').forEach(function(wrapper) {

    // Skip if already bound
    if (wrapper.dataset.tooltipBound === 'true') return;
    wrapper.dataset.tooltipBound = 'true';

    var tooltip    = wrapper.querySelector('.glossy-tooltip');
    if (!tooltip) return;

    var hideTimer    = null;   // grace period timer
    var fadeTimer    = null;   // auto-fade timer
    var isOverBox    = false;
    var isOverTip    = false;

    // ── Start auto-fade countdown ──
    function startFadeTimer() {
      clearTimeout(fadeTimer);
      fadeTimer = setTimeout(function() {
        // Only fade if mouse is not currently inside box or tooltip
        if (!isOverBox && !isOverTip) {
          hideTooltip(tooltip);
        }
      }, 3000);   // fades after 3 seconds of no interaction
    }

    // ── Reset fade timer on any mouse movement inside tooltip ──
    tooltip.addEventListener('mousemove', function() {
      clearTimeout(fadeTimer);
      startFadeTimer();
    });

    // ── BOX: mouse enters ──
    wrapper.addEventListener('mouseenter', function() {
      isOverBox = true;
      clearTimeout(hideTimer);
      clearTimeout(fadeTimer);
      showTooltip(tooltip);
      startFadeTimer();
    });

    // ── BOX: mouse leaves ──
    wrapper.addEventListener('mouseleave', function() {
      isOverBox = false;
      // Grace period — gives time to move into tooltip
      hideTimer = setTimeout(function() {
        if (!isOverTip) {
          hideTooltip(tooltip);
          clearTimeout(fadeTimer);
        }
      }, 250);
    });

    // ── TOOLTIP: mouse enters ──
    tooltip.addEventListener('mouseenter', function() {
      isOverTip = true;
      clearTimeout(hideTimer);
      clearTimeout(fadeTimer);
      showTooltip(tooltip);
    });

    // ── TOOLTIP: mouse leaves ──
    tooltip.addEventListener('mouseleave', function() {
      isOverTip = false;
      // Start fade timer once they leave the tooltip
      startFadeTimer();
    });

  });
}

// Run on page load
document.addEventListener('DOMContentLoaded', function() {
  attachTooltipListeners();
});

// Re-run after Shiny re-renders any output
// (needed because renderUI recreates DOM elements)
$(document).on('shiny:value', function() {
  setTimeout(attachTooltipListeners, 400);
});
")),
      tags$style(HTML("
  .hoverlayer .hovertext rect {
    fill: rgba(0,0,0,0) !important;
    stroke: rgba(0,0,0,0) !important;
  }
  .hoverlayer .hovertext path {
    fill: rgba(0,0,0,0) !important;
    stroke: rgba(0,0,0,0) !important;
  }
")),
      tags$style(HTML("
/* ============================================================
   PCN ESTABLISHMENT — Futuristic neon theme
============================================================ */

/* Page background — deep space, not pitch black */
.tab-pane#pcn_establishment,
bs4TabItem[data-value='pcn_establishment'] {
  background: transparent !important;
}

/* ── Neon glossy boxes ── */
#pcn_establishment .glossy-box {
  border-radius: 14px !important;
  padding: 12px !important;
  color: white !important;
  position: relative !important;
  overflow: hidden !important;
  border: 1px solid rgba(255,255,255,0.18) !important;
  box-shadow:
    0 0 0 1px rgba(255,255,255,0.08),
    0 4px 24px rgba(0,0,0,0.50),
    inset 0 1px 0 rgba(255,255,255,0.14) !important;
  transition: transform 0.18s ease, box-shadow 0.18s ease !important;
  backdrop-filter: blur(10px) !important;
}

#pcn_establishment .glossy-box:hover {
  transform: translateY(-4px) scale(1.02) !important;
  box-shadow:
    0 0 0 1px rgba(255,255,255,0.15),
    0 12px 36px rgba(0,0,0,0.65),
    0 0 20px rgba(0,234,255,0.15),
    inset 0 1px 0 rgba(255,255,255,0.20) !important;
}

/* Sheen sweep on hover */
#pcn_establishment .glossy-box::before {
  content: '' !important;
  position: absolute !important;
  top: 0 !important; left: -75% !important;
  width: 200% !important; height: 100% !important;
  background: linear-gradient(
    120deg,
    rgba(255,255,255,0.00),
    rgba(255,255,255,0.12),
    rgba(255,255,255,0.00)
  ) !important;
  transform: skewX(-25deg) !important;
  transition: left 0.5s ease !important;
}
#pcn_establishment .glossy-box:hover::before {
  left: 125% !important;
}


/* ── Original colours kept, Style F glow treatment applied ── */

#pcn_establishment .bg-blue {
  background: linear-gradient(140deg,
    rgba(5,15,50,0.88) 0%,
    rgba(26,115,232,0.55) 100%) !important;
  border: 1px solid rgba(77,171,247,0.45) !important;
  box-shadow: 0 0 20px rgba(26,115,232,0.35),
              0 8px 28px rgba(5,15,50,0.70),
              inset 0 1px 0 rgba(77,171,247,0.20) !important;
}

#pcn_establishment .bg-green {
  background: linear-gradient(140deg,
    rgba(5,40,15,0.88) 0%,
    rgba(46,204,113,0.55) 100%) !important;
  border: 1px solid rgba(46,204,113,0.45) !important;
  box-shadow: 0 0 20px rgba(46,204,113,0.35),
              0 8px 28px rgba(5,40,15,0.70),
              inset 0 1px 0 rgba(46,204,113,0.20) !important;
}

#pcn_establishment .bg-orange {
  background: linear-gradient(140deg,
    rgba(50,25,0,0.88) 0%,
    rgba(243,156,18,0.55) 100%) !important;
  border: 1px solid rgba(243,156,18,0.45) !important;
  box-shadow: 0 0 20px rgba(243,156,18,0.35),
              0 8px 28px rgba(50,25,0,0.70),
              inset 0 1px 0 rgba(243,156,18,0.20) !important;
}

#pcn_establishment .bg-red {
  background: linear-gradient(140deg,
    rgba(50,5,5,0.88) 0%,
    rgba(231,76,60,0.55) 100%) !important;
  border: 1px solid rgba(231,76,60,0.45) !important;
  box-shadow: 0 0 20px rgba(231,76,60,0.35),
              0 8px 28px rgba(50,5,5,0.70),
              inset 0 1px 0 rgba(231,76,60,0.20) !important;
}

#pcn_establishment .bg-purple {
  background: linear-gradient(140deg,
    rgba(30,5,50,0.88) 0%,
    rgba(142,68,173,0.55) 100%) !important;
  border: 1px solid rgba(142,68,173,0.45) !important;
  box-shadow: 0 0 20px rgba(142,68,173,0.35),
              0 8px 28px rgba(30,5,50,0.70),
              inset 0 1px 0 rgba(142,68,173,0.20) !important;
}

/* Neon glow on values — colour matches each box */
#pcn_establishment .bg-blue .glossy-value   { color:#4dabf7 !important; text-shadow:0 0 12px rgba(77,171,247,0.80), 0 0 24px rgba(77,171,247,0.35) !important; }
#pcn_establishment .bg-green .glossy-value  { color:#2ecc71 !important; text-shadow:0 0 12px rgba(46,204,113,0.80), 0 0 24px rgba(46,204,113,0.35) !important; }
#pcn_establishment .bg-orange .glossy-value { color:#f39c12 !important; text-shadow:0 0 12px rgba(243,156,18,0.80), 0 0 24px rgba(243,156,18,0.35) !important; }
#pcn_establishment .bg-red .glossy-value    { color:#ff6b6b !important; text-shadow:0 0 12px rgba(231,76,60,0.80),  0 0 24px rgba(231,76,60,0.35)  !important; }
#pcn_establishment .bg-purple .glossy-value { color:#c084fc !important; text-shadow:0 0 12px rgba(142,68,173,0.80), 0 0 24px rgba(142,68,173,0.35) !important; }


#pcn_establishment .bg-deepgreen {
  background: linear-gradient(135deg, #006400, #228B22) !important;
}

#pcn_establishment .bg-lightgreen {
  background: linear-gradient(135deg, #2E8B57, #3CB371) !important;
}

#pcn_establishment .bg-yellowgreen {
  background: linear-gradient(135deg, #9ACD32, #ADFF2F) !important;
}

#pcn_establishment .bg-yellow {
  background: linear-gradient(135deg, #FFD700, #FFEA00) !important;
}

#pcn_establishment .bg-amber {
  background: linear-gradient(135deg, #FFA500, #FF8C00) !important;
}

#pcn_establishment .bg-orangered {
  background: linear-gradient(135deg, #FF4500, #FF6347) !important;
}

#pcn_establishment .bg-red {
  background: linear-gradient(135deg, #B22222, #DC143C) !important;
}
/* Shared label style */
#pcn_establishment .glossy-value {
  font-size: 22px !important;
  font-weight: 800 !important;
}
#pcn_establishment .glossy-title {
  color: rgba(255,255,255,0.65) !important;
  font-size: 10px !important;
  text-transform: uppercase !important;
  letter-spacing: 1px !important;
}

/* ── Cards / boxes on this page ── */
#pcn_establishment .card,
#pcn_establishment .box {
  background: rgba(5,15,30,0.75) !important;
  border: 1px solid rgba(0,234,255,0.15) !important;
  border-radius: 16px !important;
  box-shadow:
    0 0 0 1px rgba(0,234,255,0.08),
    0 8px 32px rgba(0,0,0,0.55),
    inset 0 1px 0 rgba(255,255,255,0.06) !important;
  backdrop-filter: blur(12px) !important;
}

#pcn_establishment .card-header,
#pcn_establishment .box-header {
  background: rgba(0,234,255,0.07) !important;
  border-bottom: 1px solid rgba(0,234,255,0.15) !important;
}

#pcn_establishment .card .card-header .card-title,
#pcn_establishment .box-title {
  color: #00eaff !important;
  text-shadow: 0 0 8px rgba(0,234,255,0.50) !important;
  font-weight: 700 !important;
}

/* ── Select inputs ── */
#pcn_establishment .selectize-control .selectize-input,
#pcn_establishment .form-control,
#pcn_establishment select {
  background: rgba(0,20,40,0.85) !important;
  border: 1px solid rgba(0,234,255,0.30) !important;
  color: #00eaff !important;
  border-radius: 8px !important;
}

/* ── Donut chart wrappers ── */
#pcn_establishment .js-plotly-plot,
#pcn_establishment .plotly {
  background: transparent !important;
}
")),
      tags$style(HTML("
/* ============================================================
   RESTORE LOST AURA
============================================================ */

/* 1. Defined glowing border around every chart/map box */
.box {
  background: rgba(5,15,30,0.80) !important;
  border: 1px solid rgba(0,234,255,0.55) !important;
  border-radius: 16px !important;
  box-shadow:
    0 0 8px rgba(0,234,255,0.40),
    0 0 20px rgba(0,234,255,0.20),
    0 8px 32px rgba(0,0,0,0.60),
    inset 0 1px 0 rgba(255,255,255,0.06) !important;
  backdrop-filter: blur(12px) !important;
  transition: all 0.22s ease !important;
  overflow: visible !important;
}

.box-header {
  background: rgba(0,234,255,0.07) !important;
  border-bottom: 1px solid rgba(0,234,255,0.15) !important;
  border-radius: 16px 16px 0 0 !important;
}

.box-title {
  color: #00eaff !important;
  text-shadow: 0 0 8px rgba(0,234,255,0.50) !important;
  font-weight: 700 !important;
}

/* 2. Playful hover lift on boxes */
.box:hover {
  border: 1px solid rgba(0,234,255,0.90) !important;
  box-shadow:
    0 0 12px rgba(0,234,255,0.70),
    0 0 35px rgba(0,234,255,0.35),
    0 0 60px rgba(0,234,255,0.15),
    0 16px 48px rgba(0,0,0,0.70),
    inset 0 1px 0 rgba(255,255,255,0.10) !important;
  transform: translateY(-3px) scale(1.004) !important;
}

/* 3. Neon cursor glow on ALL input types */
input[type='text']:focus,
input[type='password']:focus,
input[type='number']:focus,
textarea:focus,
.selectize-control.focus .selectize-input,
.selectize-input.focus {
  border-color: rgba(0,234,255,0.70) !important;
  box-shadow:
    0 0 0 3px rgba(0,234,255,0.15),
    0 0 12px rgba(0,234,255,0.25),
    inset 0 0 8px rgba(0,234,255,0.08) !important;
  outline: none !important;
  caret-color: #00eaff !important;
}

/* 4. Content wrapper — darker so glass cards pop */
.content-wrapper {
  background: rgba(8,15,30,0.92) !important;
}

/* 5. Tab content panels — defined dark glass */
.tab-content, .tab-pane {
  background: rgba(5,12,25,0.75) !important;
  border: 1px solid rgba(0,234,255,0.12) !important;
  border-radius: 0 12px 12px 12px !important;
  box-shadow: inset 0 2px 12px rgba(0,0,0,0.30) !important;
}

/* 6. Plotly/ggiraph output containers — subtle glow frame */
.shiny-plot-output,
.girafe,
.js-plotly-plot {
  border-radius: 10px !important;
  box-shadow: 0 0 0 1px rgba(0,234,255,0.10),
              0 4px 20px rgba(0,0,0,0.40) !important;
}
")),
      tags$style(HTML("
/* ============================================================
   FINAL OVERRIDES — labels, headings, navbar, tabs, boxes
============================================================ */

/* Box/card titles — cyan glow */
.box-title,
.card-title,
.card .card-header .card-title,
.card-header .card-title {
  color: #00eaff !important;
  text-shadow: 0 0 8px rgba(0,234,255,0.90) !important;
  font-weight: 700 !important;
}

/* Filter labels — bright blue, tight glow, no mist */
label, .control-label, .form-group label, .box label, .card label,
.shiny-input-container label, .shiny-input-container .control-label {
  color: #00eaff !important;
  text-shadow: 0 0 6px rgba(0,234,255,0.55), 0 0 12px rgba(0,234,255,0.30) !important;
  font-weight: 700 !important;
  font-size: 14px !important;
  letter-spacing: 0.5px !important;
}

/* Navbar title */
.navbar-brand,
.main-header .brand-text,
.main-header .navbar-brand,
.navbar-header .brand-text,
.navbar-header .navbar-brand,
.main-header .logo-xl,
.main-header .logo-mini,
.main-header span.brand-text,
.bs4-navbar .navbar-brand,
.main-header .navbar .navbar-brand {
  color: #00eaff !important;
  text-shadow: 0 0 6px rgba(0,234,255,0.55) !important;
  font-weight: 700 !important;
}

/* Sidebar items */
.sidebar-menu > li > a,
.nav-sidebar > li > a {
  color: rgba(200,240,255,0.80) !important;
  text-shadow: 0 0 5px rgba(0,234,255,0.25) !important;
  font-weight: 600 !important;
}
.sidebar-menu > li.active > a,
.nav-sidebar > li.active > a,
.sidebar-menu > li > a:hover,
.nav-sidebar > li > a:hover {
  color: #00eaff !important;
  text-shadow: 0 0 8px rgba(0,234,255,0.80) !important;
}

/* Tab pill labels — tight single glow, each own colour */
.nav-tabs .nav-link[data-value='overview']       { color:#1a73e8 !important; font-weight:700 !important; text-shadow:0 0 6px #1a73e8 !important; }
.nav-tabs .nav-link[data-value='governance']     { color:#d93025 !important; font-weight:700 !important; text-shadow:0 0 6px #d93025 !important; }
.nav-tabs .nav-link[data-value='pophealth']      { color:#e67e22 !important; font-weight:700 !important; text-shadow:0 0 6px #e67e22 !important; }
.nav-tabs .nav-link[data-value='capacity']       { color:#f39c12 !important; font-weight:700 !important; text-shadow:0 0 6px #f39c12 !important; }
.nav-tabs .nav-link[data-value='financing']      { color:#16a085 !important; font-weight:700 !important; text-shadow:0 0 6px #16a085 !important; }
.nav-tabs .nav-link[data-value='infrastructure'] { color:#8e44ad !important; font-weight:700 !important; text-shadow:0 0 6px #8e44ad !important; }
.nav-tabs .nav-link[data-value='hmis']           { color:#94a3b8 !important; font-weight:700 !important; text-shadow:0 0 6px #94a3b8 !important; }
.nav-tabs .nav-link[data-value='hrh']            { color:#00a86b !important; font-weight:700 !important; text-shadow:0 0 6px #00a86b !important; }
.nav-tabs .nav-link[data-value='service']        { color:#27ae60 !important; font-weight:700 !important; text-shadow:0 0 6px #27ae60 !important; }
.nav-tabs .nav-link[data-value='qoc_mgmt']       { color:#c0392b !important; font-weight:700 !important; text-shadow:0 0 6px #c0392b !important; }
.nav-tabs .nav-link[data-value='qoc_phc']        { color:#2980b9 !important; font-weight:700 !important; text-shadow:0 0 6px #2980b9 !important; }
.nav-tabs .nav-link[data-value='qoc_outcomes']   { color:#9b59b6 !important; font-weight:700 !important; text-shadow:0 0 6px #9b59b6 !important; }
.nav-tabs .nav-link[data-value='social']         { color:#e84393 !important; font-weight:700 !important; text-shadow:0 0 6px #e84393 !important; }
.nav-tabs .nav-link[data-value='innovation']     { color:#f1c40f !important; font-weight:700 !important; text-shadow:0 0 6px #f1c40f !important; }
.nav-tabs .nav-link[data-value='county_overview']      { color:#1a73e8 !important; font-weight:700 !important; text-shadow:0 0 6px #1a73e8 !important; }
.nav-tabs .nav-link[data-value='county_governance']    { color:#d93025 !important; font-weight:700 !important; text-shadow:0 0 6px #d93025 !important; }
.nav-tabs .nav-link[data-value='county_hrh']           { color:#00a86b !important; font-weight:700 !important; text-shadow:0 0 6px #00a86b !important; }
.nav-tabs .nav-link[data-value='county_hpt']           { color:#8e44ad !important; font-weight:700 !important; text-shadow:0 0 6px #8e44ad !important; }
.nav-tabs .nav-link[data-value='county_service']       { color:#27ae60 !important; font-weight:700 !important; text-shadow:0 0 6px #27ae60 !important; }
.nav-tabs .nav-link[data-value='county_financing']     { color:#16a085 !important; font-weight:700 !important; text-shadow:0 0 6px #16a085 !important; }
.nav-tabs .nav-link[data-value='county_hmis']          { color:#5d8aa8 !important; font-weight:700 !important; text-shadow:0 0 6px #5d8aa8 !important; }
.nav-tabs .nav-link[data-value='county_qoc_mgmt']      { color:#c0392b !important; font-weight:700 !important; text-shadow:0 0 6px #c0392b !important; }
.nav-tabs .nav-link[data-value='county_multisectoral'] { color:#6c5ce7 !important; font-weight:700 !important; text-shadow:0 0 6px #6c5ce7 !important; }
.nav-tabs .nav-link[data-value='county_innovation']    { color:#f1c40f !important; font-weight:700 !important; text-shadow:0 0 6px #f1c40f !important; }
.nav-tabs .nav-link.active { filter:brightness(1.5) !important; text-shadow:0 0 8px currentColor !important; }
.nav-tabs .nav-link span   { color:inherit !important; font-weight:700 !important; text-shadow:inherit !important; }

/* Kill mist on all dropdowns and inputs */
.selectize-control .selectize-input,
.selectize-input,
.form-control,
select,
.shiny-input-container .form-control {
  background: rgba(10,20,45,0.98) !important;
  border: 1px solid rgba(0,234,255,0.40) !important;
  color: #ffffff !important;
  backdrop-filter: none !important;
  -webkit-backdrop-filter: none !important;
  box-shadow: 0 0 0 1px rgba(0,234,255,0.15) !important;
}

/* Donut chart containers */
#pcn_establishment .js-plotly-plot,
#pcn_establishment .plotly.html-widget {
  border: 1px solid rgba(0,234,255,0.35) !important;
  border-radius: 10px !important;
  box-shadow: 0 -2px 6px rgba(0,234,255,0.12), 0 2px 6px rgba(0,234,255,0.12) !important;
  background: rgba(5,15,30,0.60) !important;
}
/* Cursor */
input[type='text'], input[type='password'], input[type='number'] { caret-color:#000000 !important; }
input[type='text']:focus, input[type='password']:focus, input[type='number']:focus {
  caret-color: #000000 !important;
  border-color: rgba(0,234,255,0.70) !important;
  box-shadow: 0 0 0 3px rgba(0,234,255,0.20), 0 0 14px rgba(0,234,255,0.30) !important;
  outline: none !important;
}
")),
      tags$style(HTML("
/* ============================================================
   NAVBAR TITLE — force override
============================================================ */
.navbar-header .navbar-brand,
.navbar-header span,
.main-header .navbar-brand,
.main-header .navbar-brand span,
.main-header .logo span,
.main-header .brand-text,
.bs4-navbar .brand-text,
.bs4-navbar span,
.navbar .brand-text,
.content-header h1,
.navbar-light .navbar-brand,
.navbar-light .navbar-brand span {
  color: #00eaff !important;
  text-shadow: 0 0 6px rgba(0,234,255,0.55) !important;
  font-weight: 700 !important;
}
")),
      tags$script(HTML("
  // Receives server message to update DEMO button appearance
Shiny.addCustomMessageHandler('updateDummyBtn', function(msg) {
    var btn = document.getElementById(msg.id);
    if (!btn) return;
    if (msg.state) {
      // dummy is ON — show DEMO in red
      btn.textContent = 'DEMO';
      btn.classList.remove('real-only');
    } else {
      // dummy is OFF — show REAL in green
      btn.textContent = 'REAL';
      btn.classList.add('real-only');
    }
  });
 "))
    ), # end tags$head
    # ── Tab pages ───────────────────────────────────────────
    bs4TabItems(
      
      # ----------------------------------------------------------
      # PAGE: PCN ESTABLISHMENT
      # ----------------------------------------------------------
      bs4TabItem(
        tabName = "pcn_establishment",
        
        # County summary boxes (2 rows × 4 columns)
        tags$div(
          style = "text-align:center; margin:16px 0 8px 0; padding:10px; border-top:1px solid rgba(0,234,255,0.35); border-bottom:1px solid rgba(0,234,255,0.35); border-left:none; border-right:none; background:rgba(5,15,30,0.85); box-shadow:0 -1px 8px rgba(0,234,255,0.20), 0 1px 8px rgba(0,234,255,0.20); border-radius:8px;",
          tags$span("County PCN Status Overview",
                    style = "color:#00eaff; font-weight:900; font-size:22px; letter-spacing:2px; display:block; text-shadow: 0 0 8px rgba(0,234,255,0.6);")
        ),
        fluidRow(
          column(3, uiOutput("county_total_box")),
          column(3, uiOutput("county_operational_box")),
          column(3, uiOutput("county_gazetted_box")),
          column(3, uiOutput("county_ongoing_box"))
        ),
        fluidRow(
          column(3, uiOutput("county_pending_box")),
          column(3, uiOutput("county_notgazetted_box")),
          column(3, uiOutput("county_awaiting_box")),
          column(3, uiOutput("county_partner_box"))
        ),
        
        br(),
        
        # Subcounty boxes (grid) + status bar chart side-by-side
        tags$div(
          style = "text-align:center; margin:16px 0 8px 0; padding:10px; border-top:1px solid rgba(0,234,255,0.35); border-bottom:1px solid rgba(0,234,255,0.35); border-left:none; border-right:none; background:rgba(5,15,30,0.85); box-shadow:0 -1px 8px rgba(0,234,255,0.20), 0 1px 8px rgba(0,234,255,0.20); border-radius:8px;",
          tags$span("Subcounty PCN Status Overview",
                    style = "color:#00eaff; font-weight:900; font-size:22px; letter-spacing:2px; display:block; text-shadow: 0 0 8px rgba(0,234,255,0.6);")
        ),
        fluidRow(
          column(5,
                 div(
                   style = "display:grid; grid-template-columns:repeat(4,1fr); gap:8px;",
                   uiOutput("sub_total_box2"),
                   uiOutput("sub_operational_box"),
                   uiOutput("sub_gazetted_box"),
                   uiOutput("sub_ongoing_box"),
                   uiOutput("sub_pending_box"),
                   uiOutput("sub_notgazetted_box"),
                   uiOutput("sub_awaiting_box"),
                   uiOutput("sub_partner_box")
                 ),
                 br(),
                 tags$span("National Subcounty Coverage",
                           style = "color:#00eaff; text-align:center; margin:4px 0; font-weight:900; font-size:14px; display:block; text-shadow: 0 0 6px rgba(0,234,255,0.55); letter-spacing:1px;"),
                 plotlyOutput("subcounty_donut", height = "280px", width = "100%")
          ),
          column(7,
                 bs4Card(
                   title = "Subcounty Establishment Status", status = "success",
                   solidHeader = TRUE, elevation = 3, width = 12,
                   div(style = "display:flex; justify-content:flex-end; padding:5px;",
                       selectInput("subcounty_filter", label = NULL,
                                   choices = c("All", sort(unique(data_pcn$County))),
                                   selected = "All", width = "160px")
                   ),
                   plotlyOutput("subcounty_perf_chart", height = "550px", width = "100%")
                 )
          )
        ),
        
        br(),
        
        # PCN boxes (grid) + status bar chart side-by-side
        tags$div(
          style = "text-align:center; margin:16px 0 8px 0; padding:10px; border-top:1px solid rgba(0,234,255,0.35); border-bottom:1px solid rgba(0,234,255,0.35); border-left:none; border-right:none; background:rgba(5,15,30,0.85); box-shadow:0 -1px 8px rgba(0,234,255,0.20), 0 1px 8px rgba(0,234,255,0.20); border-radius:8px;",
          tags$span("PCN Performance",
                    style = "color:#00eaff; font-weight:900; font-size:22px; letter-spacing:2px; display:block; text-shadow: 0 0 8px rgba(0,234,255,0.6);")
        ),
        fluidRow(
          column(5,
                 div(
                   style = "display:grid; grid-template-columns:repeat(4,1fr); gap:8px;",
                   uiOutput("pcn_total_box"),
                   uiOutput("pcn_operational_box"),
                   uiOutput("pcn_gazetted_box"),
                   uiOutput("pcn_ongoing_box"),
                   uiOutput("pcn_pending_box"),
                   uiOutput("pcn_notgazetted_box"),
                   uiOutput("pcn_awaiting_box"),
                   uiOutput("pcn_partner_box")
                 ),
                 br(),
                 tags$span("National PCN Coverage",
                           style = "color:#00eaff; text-align:center; margin:4px 0; font-weight:900; font-size:14px; display:block; text-shadow: 0 0 6px rgba(0,234,255,0.55); letter-spacing:1px;"),
                 plotlyOutput("pcn_donut", height = "280px", width = "100%")
          ),
          column(7,
                 bs4Card(
                   title = "PCN Establishment Status", status = "primary",
                   solidHeader = TRUE, elevation = 3, width = 12,
                   div(style = "display:flex; justify-content:flex-end; padding:5px;",
                       selectInput("pcn_filter", label = NULL,
                                   choices = c("All", sort(unique(data_pcn$County))),
                                   selected = "All", width = "160px")
                   ),
                   plotlyOutput("pcn_status_chart", height = "550px", width = "100%")
                 )
          )
        ),
        
        br(),
        
        # Partner support bar chart
        fluidRow(
          column(12,
                 bs4Card(
                   title = "PCNs Supported by Partners (%)", status = "primary",
                   solidHeader = TRUE, elevation = 3, width = 12,
                   
                   # County + Subcounty filters
                   fluidRow(
                     column(3,
                            selectInput(
                              "partner_county_filter",
                              label = "County:",
                              choices  = c("All", sort(unique(data_pcn$County))),
                              selected = "All",
                              width    = "100%"
                            )
                     ),
                     column(3,
                            selectizeInput(
                              "partner_subcounty_filter",
                              label    = "Subcounty:",
                              choices  = "All",
                              selected = "All",
                              width    = "100%"
                            )
                     ),
                     column(3,
                            selectizeInput(
                              "partner_pcn_filter",
                              label = "PCN:",
                              choices = "All",
                              selected = "All",
                              width = "100%"
                            )
                     ),
                     column(3)   # empty spacer
                   ),
                   
                   uiOutput("est_partner_chart_ui")
                 )
          )
        )
      ),
      
      # ----------------------------------------------------------
      # PAGE: PCN MONITORING (14 indicator tabs)
      # ----------------------------------------------------------
      bs4TabItem(
        tabName = "pcn_monitoring",
        fluidRow(
          column(12,
                 tabsetPanel(
                   id = "topic_tabs", type = "tabs", selected = "overview",
                   tabPanel(title = tagList(icon("home",            style = paste0("color:", tab_colors$overview)),       span(" Overview",              style = paste0("color:", tab_colors$overview))),       value = "overview",      uiOutput("tab_overview_ui")),
                   tabPanel(title = tagList(icon("gavel",           style = paste0("color:", tab_colors$governance)),     span(" Governance",            style = paste0("color:", tab_colors$governance))),     value = "governance",    uiOutput("tab_governance_ui")),
                   tabPanel(title = tagList(icon("users",           style = paste0("color:", tab_colors$pophealth)),      span(" Population Health Needs",style = paste0("color:", tab_colors$pophealth))),     value = "pophealth",     uiOutput("tab_pophealth_ui")),
                   tabPanel(title = tagList(icon("boxes",           style = paste0("color:", tab_colors$capacity)),       span(" Capacity Readiness",    style = paste0("color:", tab_colors$capacity))),       value = "capacity",      uiOutput("tab_capacity_ui")),
                   tabPanel(title = tagList(icon("money-bill-wave", style = paste0("color:", tab_colors$financing)),      span(" Health Care Financing", style = paste0("color:", tab_colors$financing))),      value = "financing",     uiOutput("tab_financing_ui")),
                   tabPanel(title = tagList(icon("road",            style = paste0("color:", tab_colors$infrastructure)), span(" Health Infrastructure", style = paste0("color:", tab_colors$infrastructure))), value = "infrastructure",uiOutput("tab_infrastructure_ui")),
                   tabPanel(title = tagList(icon("server",          style = paste0("color:", tab_colors$hmis)),           span(" HMIS / Digital Health", style = paste0("color:", tab_colors$hmis))),           value = "hmis",          uiOutput("tab_hmis_ui")),
                   tabPanel(title = tagList(icon("user-md",         style = paste0("color:", tab_colors$hrh)),            span(" Human Resources",       style = paste0("color:", tab_colors$hrh))),            value = "hrh",           uiOutput("tab_hrh_ui")),
                   tabPanel(title = tagList(icon("truck-medical",   style = paste0("color:", tab_colors$service)),        span(" Service Delivery",      style = paste0("color:", tab_colors$service))),        value = "service",       uiOutput("tab_service_ui")),
                   tabPanel(title = tagList(icon("clipboard-check", style = paste0("color:", tab_colors$qoc_mgmt)),       span(" QoC – Management Systems",style = paste0("color:", tab_colors$qoc_mgmt))),    value = "qoc_mgmt",      uiOutput("tab_qoc_mgmt_ui")),
                   tabPanel(title = tagList(icon("stethoscope",     style = paste0("color:", tab_colors$qoc_phc)),        span(" QoC – PHC Core Systems",style = paste0("color:", tab_colors$qoc_phc))),       value = "qoc_phc",       uiOutput("tab_qoc_phc_ui")),
                   tabPanel(title = tagList(icon("chart-line",      style = paste0("color:", tab_colors$qoc_outcomes)),   span(" QoC – Outcomes",        style = paste0("color:", tab_colors$qoc_outcomes))),   value = "qoc_outcomes",  uiOutput("tab_qoc_outcomes_ui")),
                   tabPanel(title = tagList(icon("hands-helping",   style = paste0("color:", tab_colors$social)),         span(" Social Accountability", style = paste0("color:", tab_colors$social))),         value = "social",        uiOutput("tab_social_ui")),
                   tabPanel(title = tagList(icon("lightbulb",       style = paste0("color:", tab_colors$innovation)),     span(" Innovations & Learning",style = paste0("color:", tab_colors$innovation))),     value = "innovation",    uiOutput("tab_innovation_ui"))
                 )
          )
        )
      ),
      
      # ----------------------------------------------------------
      # PAGE: COUNTY MONITORING (10 indicator tabs)
      # ----------------------------------------------------------
      bs4TabItem(
        tabName = "county_monitoring",
        fluidRow(
          column(12,
                 tabsetPanel(
                   id = "county_topic_tabs", type = "tabs", selected = "county_overview",
                   tabPanel(title = tagList(icon("home",           style = "color:#1a73e8"), span(" Overview",                 style = "color:#1a73e8")), value = "county_overview",      uiOutput("county_tab_overview_ui")),
                   tabPanel(title = tagList(icon("gavel",          style = "color:#d93025"), span(" Governance & Leadership",  style = "color:#d93025")), value = "county_governance",    uiOutput("county_tab_governance_ui")),
                   tabPanel(title = tagList(icon("user-md",        style = "color:#00a86b"), span(" Human Resources",          style = "color:#00a86b")), value = "county_hrh",           uiOutput("county_tab_hrh_ui")),
                   tabPanel(title = tagList(icon("pills",          style = "color:#8e44ad"), span(" Health Products",          style = "color:#8e44ad")), value = "county_hpt",           uiOutput("county_tab_hpt_ui")),
                   tabPanel(title = tagList(icon("truck-medical",  style = "color:#27ae60"), span(" Service Delivery",         style = "color:#27ae60")), value = "county_service",       uiOutput("county_tab_service_ui")),
                   tabPanel(title = tagList(icon("money-bill-wave",style = "color:#16a085"), span(" Health Care Financing",    style = "color:#16a085")), value = "county_financing",     uiOutput("county_tab_financing_ui")),
                   tabPanel(title = tagList(icon("server",         style = "color:#2c3e50"), span(" HMIS / Digital Health",    style = "color:#2c3e50")), value = "county_hmis",          uiOutput("county_tab_hmis_ui")),
                   tabPanel(title = tagList(icon("clipboard-check",style = "color:#c0392b"), span(" QoC – Management Systems", style = "color:#c0392b")), value = "county_qoc_mgmt",      uiOutput("county_tab_qoc_mgmt_ui")),
                   tabPanel(title = tagList(icon("handshake",      style = "color:#6c5ce7"), span(" Multisectoral Partnerships",style = "color:#6c5ce7")),value = "county_multisectoral", uiOutput("county_tab_multisectoral_ui")),
                   tabPanel(title = tagList(icon("lightbulb",      style = "color:#f1c40f"), span(" Innovations & Learning",  style = "color:#f1c40f")), value = "county_innovation",    uiOutput("county_tab_innovation_ui"))
                 )
          )
        )
      ),
      
      # ----------------------------------------------------------
      # PAGE: RAW DATA TABLE
      # ----------------------------------------------------------
      bs4TabItem(
        tabName = "datatable",
        fluidRow(
          column(12,
                 bs4Card(title = "Raw Data Table", width = 12, DTOutput("raw_data"))
          )
        )
      )
      
    ) # end bs4TabItems
  )   # end bs4DashBody
)     # end bs4DashPage


# ============================================================
# 14. INTRO / LOGIN PAGE UI
# ============================================================
intro_ui <- fluidPage(
  tags$head(
    tags$meta(name = "viewport", 
              content = "width=device-width, initial-scale=1.0, maximum-scale=1.0"),
    tags$style(HTML("
      html, body {
        height: 100%;
        margin: 0;
        overflow: hidden;
      }
      body, body.intro-active {
        background-image: url('tech_cover.jpeg') !important;
        background-size: cover !important;
        background-position: center !important;
        background-repeat: no-repeat !important;
        background-attachment: fixed !important;
      }
      .intro-container {
        height: 100vh;
        display: flex;
        align-items: center;
        justify-content: center;
        padding: 20px;
        box-sizing: border-box;
      }
      .intro-card {
        width: 100%;
        max-width: 1200px;
        height: auto;
        max-height: 95vh;
        background: transparent;
        border-radius: 16px;
        display: flex;
        overflow: hidden;
        box-shadow: none;
      }
      .intro-left {
        width: 55%;
        padding: 36px 44px;
        overflow-y: auto;
        box-sizing: border-box;
        display: flex;
        flex-direction: column;
        justify-content: center;
        background: rgba(255,255,255,0.18);
        backdrop-filter: blur(10px);
        -webkit-backdrop-filter: blur(10px);
        border-radius: 16px;
      }
      .intro-right {
        width: 45%;
        background: transparent;
        display: flex;
        align-items: center;
        justify-content: center;
        animation: slideIn 1.2s ease-out;
      }
      .intro-right .shiny-plot-output {
        width: 85% !important;
        height: 85% !important;
        background: transparent !important;
      }
body.intro-active h1,
      body.intro-active .intro-left h1 {
        font-weight: 700 !important;
        color: #00a86b !important;
        font-size: clamp(18px, 2vw, 28px) !important;
        text-shadow: 0 0 12px rgba(0,168,107,0.50), 0 1px 3px rgba(0,0,0,0.20) !important;
      }
      body.intro-active .subtitle {
        color: #003580 !important;
        font-size: clamp(12px, 1.1vw, 15px) !important;
        margin-bottom: 20px !important;
        line-height: 1.6 !important;
        font-weight: 500 !important;
      }
      body.intro-active .login-box label {
        color: #0047AB !important;
        font-weight: 700 !important;
      }
      body.intro-active .login-box input {
        background-color: rgba(255,255,255,0.90) !important;
        color: #000000 !important;
        border-radius: 6px !important;
        border: 1px solid rgba(255,255,255,0.50) !important;
        width: 100% !important;
      }
      body.intro-active .login-btn {
        background: rgba(0,87,183,0.75) !important;
        color: #ffffff !important;
        width: 100% !important;
        border-radius: 8px !important;
        font-weight: 600 !important;
        margin-top: 8px !important;
        border: 1px solid rgba(255,255,255,0.40) !important;
      }
      body.intro-active h1,
      body.intro-active h2,
      body.intro-active h3,
      body.intro-active h4,
      body.intro-active h5,
      body.intro-active h6 {
        color: inherit !important;
        text-shadow: none !important;
      }
      body.intro-active .intro-left h1 {
        color: #00a86b !important;
        text-shadow: 0 0 12px rgba(0,168,107,0.50) !important;
      }
      body.intro-active p {
        color: inherit !important;
        text-shadow: none !important;
      }
      body.intro-active .subtitle {
        color: #003580 !important;
      }
      body.intro-active,
      body.intro-active .wrapper {
        background-image: url('tech_cover.jpeg') !important;
        background-size: cover !important;
        background-position: center !important;
        background-repeat: no-repeat !important;
        background-attachment: fixed !important;
        background-color: transparent !important;
      }
      body.intro-active .intro-left {
        background: rgba(255,255,255,0.18) !important;
        backdrop-filter: blur(10px) !important;
        -webkit-backdrop-filter: blur(10px) !important;
      }
      body.intro-active .card,
      body.intro-active .box {
        background: transparent !important;
        border: none !important;
        box-shadow: none !important;
        backdrop-filter: none !important;
      }
      body.intro-active label {
        color: #0047AB !important;
        font-weight: 700 !important;
      }
      body.intro-active input[type='text'],
      body.intro-active input[type='password'] {
        background: rgba(255,255,255,0.90) !important;
        color: #000000 !important;
        border: 1px solid rgba(255,255,255,0.50) !important;
      }
      .login-box {
        background: rgba(255,255,255,0.15);
        padding: 20px 24px;
        border-radius: 12px;
        border: 1px solid rgba(255,255,255,0.35);
      }
      .login-box label { color: #0047AB; font-weight: 600; }
      .login-box input {
        background-color: rgba(255,255,255,0.25);
        border-radius: 6px;
        border: 1px solid rgba(255,255,255,0.50);
        width: 100%;
        color: #000000;
      }
      .login-btn {
        background: rgba(0,87,183,0.75);
        color: #ffffff;
        width: 100%;
        border-radius: 8px;
        font-weight: 600;
        margin-top: 8px;
        border: 1px solid rgba(255,255,255,0.40);
      }
      @keyframes slideIn {
        from { opacity: 0; transform: translateX(30px); }
        to   { opacity: 1; transform: translateX(0); }
      }
      /* Stack vertically on small screens */
      @media (max-width: 768px) {
        .intro-card    { flex-direction: column; max-height: none; height: auto; }
        .intro-left    { width: 100%; padding: 24px; }
        .intro-right   { display: none; }  /* hide map on mobile */
      }
    "))
  ),
  tags$script(HTML("document.body.classList.add('intro-active');")),
  div(class = "intro-container",
      div(class = "intro-card",
          div(class = "intro-left",
              div(style = "text-align:center; margin-bottom:20px;",
                  tags$img(src = "joint_logo.png",
                           style = "max-width:100%; height:auto; object-fit:contain;")
              ),
              h1("Kenya Primary Care Networks Monitoring Dashboard"),
              p(class = "subtitle",
                "This dashboard provides a national and subnational view of Primary Care Networks (PCNs)
                 functionality across Kenya, supporting evidence-based planning, performance monitoring,
                 and PHC system strengthening."),
              div(class = "login-box",
                  textInput("username", "Username"),
                  passwordInput("password", "Password"),
                  br(),
                  actionButton("login", "Log In", class = "login-btn"),
                  uiOutput("login_error_msg")
              )
          ),
          div(class = "intro-right",
              plotOutput("kenya_intro_map", height = "90%", width = "100%")
          )
      )
  )
)


# ============================================================
# 15. SERVER
# ============================================================
server <- function(input, output, session) {
  
  
  kobo_timer <- reactiveTimer(300000)  # refresh every 5 min
  
  data_pcn_live <- reactive({
    kobo_timer()
    df <- fetch_kobo(KOBO_PCN_UID)
    df %>%
      filter(!is.na(County), County != "", !is.na(Subcounty), !is.na(PCN)) %>%
      mutate(
        County          = as.character(County),
        Subcounty       = as.character(Subcounty),
        PCN             = as.character(PCN),
        County_raw      = County,
        County_clean    = normalize_name(County),
        Subcounty_raw   = Subcounty,
        Subcounty_clean = normalize_name(Subcounty)
      )
  })
  
  # ── 15a. Authentication ─────────────────────────────────
  logged_in <- reactiveVal(FALSE)
  
  # Set default tab on app start
  observe({
    if (logged_in()) {
      updateTabItems(session, "sidebar_menu", selected = "pcn_establishment")
    }
  })
  
  
  observe({
    if (logged_in()) {
      session$sendCustomMessage("setBodyClass",
                                list(add = "", remove = "intro-active"))
    } else {
      session$sendCustomMessage("setBodyClass",
                                list(add = "intro-active", remove = ""))
    }
  })
  
  login_error <- reactiveVal("")
  
  observeEvent(input$login, {
    user <- trimws(input$username)
    pass <- trimws(input$password)
    
    if (toupper(user) == "PCN" && pass == "001") {
      login_error("")
      logged_in(TRUE)
      # Navigate to PCN Establishment tab on login
      updateTabItems(session, "sidebar_menu", selected = "pcn_establishment")
    } else {
      login_error("Invalid username or password.")
    }
  })
  
  output$login_error_msg <- renderUI({
    msg <- login_error()
    if (nzchar(msg)) {
      tags$p(
        msg,
        style = paste0(
          "color:#cc0000; font-weight:600; font-size:13px;",
          "margin-top:10px; margin-bottom:0; text-align:center;"
        )
      )
    }
  })
  
  rightUi = tagList(
    
    # Dark mode toggle button
    tags$li(
      class = "nav-item",
      style = "margin-right:10px; display:flex; align-items:center;",
      tags$span(
        "🌙",
        id    = "dark_mode_label",
        style = "color:white; font-size:14px; margin-right:6px; font-weight:600;"
      ),
      tags$div(
        class = "custom-toggle-wrapper",
        style = "display:flex; align-items:center;",
        tags$input(
          type  = "checkbox",
          id    = "dark_mode_toggle",
          class = "dark-mode-checkbox"
        ),
        tags$label(
          `for` = "dark_mode_toggle",
          class = "dark-mode-label"
        )
      )
    ),
    
    # Existing logout dropdown
    tags$li(
      class = "nav-item dropdown",
      tags$a(
        class = "nav-link dropdown-toggle", href = "#",
        `data-toggle` = "dropdown",
        icon("sign-out-alt", style = "color:#FFFF00;"),
        span("Log Out", style = "color:#FFFF00; font-weight:600;")
      ),
      tags$div(
        class = "dropdown-menu dropdown-menu-right",
        actionButton("logout", "Log out",
                     icon  = icon("sign-out-alt"),
                     class = "dropdown-item text-danger")
      )
    )
  )
  observeEvent(input$logout, {
    logged_in(FALSE)
    updateTextInput(session, "username", value = "")
    updateTextInput(session, "password", value = "")
    session$sendCustomMessage("setBodyClass",
                              list(add = "intro-active", remove = ""))
    # Reset to PCN Establishment tab on next login
    updateTabItems(session, "sidebar_menu", selected = "pcn_establishment")
  })
  
  # Swap between intro and dashboard based on login state
  output$app_ui <- renderUI({
    if (logged_in()) dashboard_ui else intro_ui
  })
  
  # Global: TRUE = show dummy data, FALSE = real data only
  show_dummy <- reactiveVal(TRUE)
  
  #Sub County Perfomance Donut
  output$subcounty_donut <- renderPlotly({ 
    x <- subcounty_status_summary()
    
    donut_data <- data.frame(
      status = c("Operational", "Gazetted", "Ongoing Establishment",
                 "Pending Gazettement", "Established Not Gazetted", "Awaiting Establishment",
                 "Awaiting Partner Support"),
      count  = c(x$Operational, x$Gazetted, x$Ongoing,
                 x$Pending, x$NotGazetted, x$Awaiting, x$PartnerSupport),
      color  = c("#006400", "#2E8B57", "#9ACD32","#FFD700", "#FFA500", "#FF4500", "#B22222")
    )
    
    plot_ly(
      donut_data,
      labels = ~status,
      values = ~count,
      customdata = ~paste0(round(count / sum(count) * 100), "%"),
      type   = "pie",
      hole   = 0.62,
      marker = list(
        colors = ~color,
        line   = list(color = "#1a1a2e", width = 2)
      ),
      textinfo      = "none",
      textposition  = "inside",
      textfont      = list(color = "#ffffff", size = 11),
      hovertemplate = "<b>%{label}</b><br>(%{percent})<extra></extra>",
      hoverlabel = list(                          
        bgcolor   = "rgba(0,0,0,0)",             
        bordercolor = "rgba(0,0,0,0)",           
        font      = list(color = "#ffffff", size = 13)
      )
    ) %>%
      layout(
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        showlegend    = FALSE,
        autosize      = TRUE,
        annotations = list(list(
          text      = "<b>322</b><br>Subcounties",
          x = 0.5, y = 0.5,
          showarrow = FALSE,
          font      = list(size = 10, color = "#ffffff"),
          xanchor   = "center",
          yanchor   = "middle"
        )),
        margin = list(t = 0, b = 0, l = 0, r = 0)
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })
  
  # ── PCN donut chart ──
  output$pcn_donut <- renderPlotly({
    x <- pcn_status_summary()
    
    donut_data <- data.frame(
      status = c("Operational", "Gazetted", "Ongoing Establishment",
                 "Pending Gazettement", "Established Not Gazetted", "Awaiting Establishment",
                 "Awaiting Partner Support"),
      count  = c(x$Operational, x$Gazetted, x$Ongoing,
                 x$Pending, x$NotGazetted, x$Awaiting, x$PartnerSupport),
      color  = c("#006400", "#2E8B57", "#9ACD32","#FFD700", "#FFA500", "#FF4500", "#B22222")
    )
    
    total_pcns <- sum(donut_data$count, na.rm = TRUE)
    
    plot_ly(
      donut_data,
      labels = ~status,
      values = ~count,
      customdata = ~paste0(round(count / sum(count) * 100), "%"),
      type   = "pie",
      hole   = 0.62,
      marker = list(
        colors = ~color,
        line   = list(color = "#1a1a2e", width = 2)
      ),
      textinfo      = "none",
      hovertemplate = "<b>%{label}</b><br>(%{percent})<extra></extra>" 
    ) %>%
      layout(
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        showlegend    = FALSE,
        autosize      = TRUE,
        hoverlabel = list(                         
          bgcolor   = "rgba(0,0,0,0)",             
          bordercolor = "rgba(0,0,0,0)",           
          font      = list(color = "#ffffff", size = 13)
        ),
        annotations = list(list(
          text      = paste0("<b>", total_pcns, "</b><br>PCNs"),
          x = 0.5, y = 0.5,
          showarrow = FALSE,
          font      = list(size = 11, color = "#ffffff"),
          xanchor   = "center",
          yanchor   = "middle"
        )),
        margin = list(t = 0, b = 0, l = 0, r = 0)
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })
  
  # ── 15b. Intro page — Kenya outline map ─────────────────
  output$kenya_intro_map <- renderPlot({
    kenya_lines <- st_cast(county_shp, "MULTILINESTRING")
    ggplot() +
      geom_sf(data = kenya_lines, color = "#1B61E4", linewidth = 1) +
      coord_sf(expand = TRUE) +
      theme_void() +
      theme(
        plot.background  = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA)
      )
  }, bg = "transparent")
  
  
  # ── 15c. Reusable UI component builders ─────────────────
  
  # Glossy gradient metric box
  glossy_box <- function(title, value, subtitle = "", color = "bg-blue",
                         tag = "", counties = NULL,
                         items = NULL, item_type = "County") {
    
    # Use items if provided, otherwise fall back to counties for backward compat
    display_items <- if (!is.null(items)) items else counties
    
    # Dot colour per entity type
    dot_color <- switch(item_type,
                        "County"    = "#00ffb4",
                        "Subcounty" = "#4dabf7",
                        "PCN"       = "#ffa94d",
                        "#ffffff"
    )
    
    # Build tooltip content
    tooltip_content <- if (!is.null(display_items) && length(display_items) > 0) {
      
      item_els <- lapply(display_items, function(nm) {
        div(
          style = "font-size:12px; color:rgba(255,255,255,0.90);
                 padding:3px 0; display:flex; align-items:center; gap:6px;",
          # Coloured dot
          tags$span(
            style = paste0(
              "display:inline-block; width:6px; height:6px;",
              "border-radius:50%; background:", dot_color,
              "; flex-shrink:0;"
            )
          ),
          nm
        )
      })
      
      tagList(
        div(
          style = paste0(
            "font-size:11px; font-weight:700; color:", dot_color, ";",
            "text-transform:uppercase; letter-spacing:0.8px;",
            "margin-bottom:8px; padding-bottom:6px;",
            "border-bottom:1px solid rgba(255,255,255,0.12);"
          ),
          paste0(length(display_items), " ",
                 if (length(display_items) == 1) {
                   item_type
                 } else {
                   switch(item_type,
                          "County"    = "Counties",
                          "Subcounty" = "Subcounties",
                          "PCN"       = "PCNs",
                          paste0(item_type, "s")   # fallback for any future types
                   )
                 })
        ),
        do.call(tagList, item_els)
      )
      
    } else {
      div(
        style = "font-size:12px; color:rgba(255,255,255,0.45); font-style:italic;",
        paste0("No ", item_type, "s in this category")
      )
    }
    
    # Wrapper with tooltip below
    div(
      class = "glossy-box-wrapper",
      
      div(class = "glossy-tooltip", tooltip_content),
      
      div(
        class = paste("glossy-box", color),
        div(
          style = "position:absolute; top:8px; left:12px;
                 font-size:11px; opacity:0.8; font-weight:600;",
          tag
        ),
        br(),
        div(class = "glossy-title", title),
        div(class = "glossy-value", value),
        div(class = "glossy-sub",   subtitle)
      )
    )
  }
  # Simple bs4Card value box (used in legacy overview tab)
  value_box_custom <- function(title, value, subtitle, color = "primary") {
    bs4Card(title = title, status = color, width = 12,
            h2(value, style = "font-weight:700;"),
            p(subtitle,  style = "color:#555; font-size:14px;")
    )
  }
  
  # Return input value or fall back to default (prevents NULLs crashing plots)
  get_input_or_default <- function(input, id, default) {
    val <- input[[id]]
    if (is.null(val) || identical(val, "") || (is.numeric(val) && is.na(val))) return(default)
    val
  }
  
  
  # ── 15d. Establishment status lookup table ───────────────
  # Maps raw data values to clean, human-readable labels
  status_labels <- c(
    "gazetted"                                = "Gazetted",
    "gazetted_and_operational"                = "Gazetted & Operational",
    "gazettement_ongoing"                     = "Gazettement Ongoing",
    "pending_gazettement"                     = "Pending Gazettement",
    "new_sub_county_not_gazetted"             = "New Subcounty (Not Gazetted) but PCN is established",
    "sub_county_not_gazetted_PCN_established" = "Subcounty PCN Established (Not Gazetted)",
    "awaiting_establishment"                  = "Awaiting Establishment",
    "awaiting_partner_support"                = "Awaiting Partner Support"
  )
  
  
  # Updates partner subcounty dropdown when county changes
  observe({
    selected_county <- input$partner_county_filter
    
    subs <- if (is.null(selected_county) || selected_county == "All") {
      sort(unique(data_pcn_live()$Subcounty))
    } else {
      sort(unique(data_pcn_live()$Subcounty[data_pcn_live()$County == selected_county]))
    }
    
    updateSelectizeInput(
      session,
      "partner_subcounty_filter",
      choices  = c("All", subs),
      selected = "All"
    )
  }) 
  
  observe({
    df <- data_pcn_live()
    if (!is.null(input$partner_county_filter) && input$partner_county_filter != "All")
      df <- df %>% filter(County == input$partner_county_filter)
    if (!is.null(input$partner_subcounty_filter) && input$partner_subcounty_filter != "All")
      df <- df %>% filter(Subcounty == input$partner_subcounty_filter)
    pcns <- sort(unique(trimws(df$PCN)))
    updateSelectizeInput(session, "partner_pcn_filter",
                         choices = c("All", pcns), selected = "All")
  })
  # ── 15e. Dummy data helpers (fills missing rows for maps) ─
  
  add_dummy_subcounty_values <- function(df, indicator_col, use_dummy = TRUE) {
    df <- as_tibble(df)
    if (!"Subcounty_clean" %in% names(df))  df$Subcounty_clean <- rep(NA_character_, nrow(df))
    if (!indicator_col %in% names(df))       df[[indicator_col]] <- rep(NA_real_,      nrow(df))
    
    all_subcounties <- subc_shp %>% st_drop_geometry() %>%
      select(Subcounty_clean) %>% distinct()
    
    df_clean <- df %>%
      select(Subcounty_clean, !!rlang::sym(indicator_col)) %>%
      mutate(real_data = !is.na(.data[[indicator_col]]))
    
    missing_subs <- all_subcounties %>%
      filter(!Subcounty_clean %in% df_clean$Subcounty_clean)
    
    if (nrow(missing_subs) > 0) {
      dummy_val <- if (use_dummy) round(runif(nrow(missing_subs), 10, 90), 1) else NA_real_
      df_clean <- bind_rows(
        df_clean,
        missing_subs %>% mutate(!!indicator_col := dummy_val, real_data = FALSE)
      )
    }
    
    nas <- is.na(df_clean[[indicator_col]])
    if (any(nas)) {
      df_clean[[indicator_col]][nas] <- if (use_dummy) round(runif(sum(nas), 10, 90), 1) else NA_real_
      df_clean$real_data[nas] <- FALSE
    }
    df_clean
  }
  
  add_dummy_pcn_values <- function(df, indicator_col, available_pcns = NULL, use_dummy = TRUE) {
    df <- as_tibble(df)
    if (!"PCN"   %in% names(df))       df$PCN          <- rep(NA_character_, nrow(df))
    if (!indicator_col %in% names(df)) df[[indicator_col]] <- rep(NA_real_,  nrow(df))
    
    if (is.null(available_pcns))
      available_pcns <- df %>% select(PCN) %>% distinct()
    else
      available_pcns <- as_tibble(available_pcns) %>% select(PCN) %>% distinct()
    
    df_clean <- df %>% mutate(real_data = !is.na(.data[[indicator_col]]))
    
    missing_pcns <- available_pcns %>% filter(!PCN %in% df_clean$PCN)
    if (nrow(missing_pcns) > 0) {
      dummy_val <- if (use_dummy) round(runif(nrow(missing_pcns), 10, 90), 1) else NA_real_
      df_clean <- bind_rows(
        df_clean,
        missing_pcns %>% mutate(!!indicator_col := dummy_val, real_data = FALSE)
      )
    }
    
    nas <- is.na(df_clean[[indicator_col]])
    if (any(nas)) {
      df_clean[[indicator_col]][nas] <- if (use_dummy) round(runif(sum(nas), 10, 90), 1) else NA_real_
      df_clean$real_data[nas] <- FALSE
    }
    df_clean
  }
  
  
  # ── 15f. Establishment data — core reactive ──────────────
  # Computes all PCN / subcounty / county establishment totals
  establishment_data <- reactive({
    df <- data_pcn_live()
    
    finalized_status <- c("gazettement_ongoing","pending_gazettement","Gazetted",
                          "gazetted_and_operational","new_sub_county_not_gazetted",
                          "sub_county_not_gazetted_PCN_established")
    not_established  <- c("awaiting_partner_support","awaiting_establishment")
    
    count_status <- function(s) sum(df$establishment_status %in% s, na.rm = TRUE)
    
    sub_sum <- df %>% group_by(Subcounty) %>%
      summarise(has_finalized = any(establishment_status %in% finalized_status),
                has_none      = all(establishment_status %in% not_established), .groups = "drop")
    
    cty_sum <- df %>% group_by(County) %>%
      summarise(has_finalized = any(establishment_status %in% finalized_status),
                has_none      = all(establishment_status %in% not_established), .groups = "drop")
    
    total_pcns        <- nrow(df)
    total_subcounties <- n_distinct(df$Subcounty)
    total_counties    <- n_distinct(df$County)
    
    gazetted_n <- count_status(c("gazetted","gazetted_and_operational"))
    pending_n  <- count_status("pending_gazettement")
    awaiting_n <- count_status(c("awaiting_partner_support","awaiting_establishment"))
    
    list(
      totals = list(pcns = total_pcns, subcounties = total_subcounties, counties = total_counties),
      subcounty = list(
        finalized_n   = sum(sub_sum$has_finalized),
        finalized_pct = round(100 * sum(sub_sum$has_finalized) / total_subcounties, 1),
        not_n         = sum(sub_sum$has_none)
      ),
      county = list(
        finalized_n   = sum(cty_sum$has_finalized),
        finalized_pct = round(100 * sum(cty_sum$has_finalized) / total_counties, 1),
        not_n         = sum(cty_sum$has_none)
      ),
      pcn = list(
        gazetted     = gazetted_n, gazetted_pct = round(100 * gazetted_n / total_pcns, 1),
        pending      = pending_n,  pending_pct  = round(100 * pending_n  / total_pcns, 1),
        awaiting_establishment = awaiting_n,
        awaiting_pct = round(100 * awaiting_n  / total_pcns, 1)
      )
    )
  })
  
  
  # ── 15g. County status summary reactive ─────────────────
  # Counts how many counties have each establishment status
  county_status_summary <- reactive({
    data_pcn_live() %>%
      mutate(County = trimws(County)) %>%
      filter(!is.na(County), County != "", tolower(County) != "county") %>%
      group_by(County) %>%
      summarise(
        gazetted    = any(establishment_status %in% c("gazetted","gazetted_and_operational")),
        operational = any(establishment_status == "gazetted_and_operational"),
        ongoing     = any(establishment_status == "gazettement_ongoing"),
        pending     = any(establishment_status == "pending_gazettement"),
        notgazetted = any(establishment_status == "sub_county_not_gazetted_PCN_established"),
        awaiting    = any(establishment_status == "awaiting_establishment"),
        partner     = any(establishment_status == "awaiting_partner_support"),
        .groups = "drop"
      ) %>%
      summarise(
        Gazetted = sum(gazetted), Operational = sum(operational),
        Ongoing  = sum(ongoing),  Pending      = sum(pending),
        NotGazetted = sum(notgazetted), Awaiting = sum(awaiting),
        PartnerSupport = sum(partner),  Total    = n()
      )
  })
  # Returns county names that have a given establishment status
  counties_with_status <- function(status_values, require_all = FALSE) {
    
    df <- data_pcn_live() %>%
      mutate(County = trimws(County)) %>%
      filter(!is.na(County), County != "",
             tolower(County) != "county")
    
    if (require_all) {
      # All PCNs in that county must match (used for "awaiting" logic)
      df %>%
        group_by(County) %>%
        summarise(
          matches = all(establishment_status %in% status_values),
          .groups = "drop"
        ) %>%
        filter(matches) %>%
        pull(County) %>%
        sort()
    } else {
      # At least one PCN in that county matches
      df %>%
        filter(establishment_status %in% status_values) %>%
        distinct(County) %>%
        pull(County) %>%
        sort()
    }
  }
  
  # Returns subcounty names that have a given establishment status
  subcounties_with_status <- function(status_values) {
    data_pcn_live() %>%
      mutate(Subcounty = trimws(Subcounty)) %>%
      filter(!is.na(Subcounty), Subcounty != "",
             establishment_status %in% status_values) %>%
      distinct(Subcounty) %>%
      pull(Subcounty) %>%
      sort()
  }
  
  # Returns PCN names that have a given establishment status
  pcns_with_status <- function(status_values) {
    data_pcn_live() %>%
      mutate(PCN = trimws(PCN)) %>%
      filter(!is.na(PCN), PCN != "",
             establishment_status %in% status_values) %>%
      distinct(PCN) %>%
      pull(PCN) %>%
      sort()
  }
  
  
  # ── 15h. Subcounty status summary reactive ───────────────
  # Counts how many subcounties have each establishment status
  subcounty_status_summary <- reactive({
    data_pcn_live() %>%
      mutate(Subcounty = trimws(Subcounty)) %>%
      group_by(Subcounty) %>%
      summarise(
        gazetted    = any(establishment_status %in% c("gazetted","gazetted_and_operational")),
        operational = any(establishment_status == "gazetted_and_operational"),
        ongoing     = any(establishment_status == "gazettement_ongoing"),
        pending     = any(establishment_status == "pending_gazettement"),
        notgazetted = any(establishment_status == "sub_county_not_gazetted_PCN_established"),
        awaiting    = any(establishment_status == "awaiting_establishment"),
        partner     = any(establishment_status == "awaiting_partner_support"),
        .groups = "drop"
      ) %>%
      summarise(
        Gazetted = sum(gazetted), Operational = sum(operational),
        Ongoing  = sum(ongoing),  Pending      = sum(pending),
        NotGazetted = sum(notgazetted), Awaiting = sum(awaiting),
        PartnerSupport = sum(partner),  Total    = n()
      )
  })
  
  
  # ── 15i. PCN status summary reactive ────────────────────
  # Counts individual PCN rows by each establishment status
  pcn_status_summary <- reactive({
    data_pcn_live() %>%
      mutate(establishment_status = trimws(establishment_status)) %>%
      summarise(
        Gazetted       = sum(establishment_status %in% c("gazetted","gazetted_and_operational"), na.rm = TRUE),
        Operational    = sum(establishment_status == "gazetted_and_operational",                 na.rm = TRUE),
        Ongoing        = sum(establishment_status == "gazettement_ongoing",                      na.rm = TRUE),
        Pending        = sum(establishment_status == "pending_gazettement",                      na.rm = TRUE),
        NotGazetted    = sum(establishment_status %in% c("new_sub_county_not_gazetted",
                                                         "sub_county_not_gazetted_PCN_established"),                         na.rm = TRUE),
        Awaiting       = sum(establishment_status == "awaiting_establishment",                   na.rm = TRUE),
        PartnerSupport = sum(establishment_status == "awaiting_partner_support",                 na.rm = TRUE)
      )
  })
  
  
  # ── 15j. Filtered chart data reactives ──────────────────
  
  # Subcounty establishment bar chart (filtered by county dropdown)
  subcounty_perf <- reactive({
    df <- if (input$subcounty_filter != "All")
      data_pcn_live() %>% filter(County == input$subcounty_filter)
    else data_pcn_live()
    
    df %>%
      mutate(establishment_status = trimws(establishment_status)) %>%
      count(establishment_status) %>%
      mutate(
        percent      = round(100 * n / sum(n), 1),
        status_label = dplyr::coalesce(
          status_labels[establishment_status],
          stringr::str_to_title(stringr::str_replace_all(establishment_status, "_", " "))
        )
      )
  })
  
  # PCN establishment bar chart (filtered by county dropdown)
  pcn_perf_status <- reactive({
    df <- if (input$pcn_filter != "All")
      data_pcn_live() %>% filter(County == input$pcn_filter)
    else data_pcn_live()
    
    df %>%
      mutate(establishment_status = trimws(establishment_status)) %>%
      count(establishment_status) %>%
      mutate(
        percent      = round(100 * n / sum(n), 1),
        status_label = dplyr::coalesce(
          status_labels[establishment_status],
          stringr::str_to_title(stringr::str_replace_all(establishment_status, "_", " "))
        )
      )
  })
  
  
  # ── 15k. County glossy boxes ────────────────────────────
  make_county_box <- function(name, value, color) glossy_box(name, value, "", color, "COUNTIES")
  
  output$county_total_box <- renderUI({
    x <- county_status_summary()
    all_counties <- sort(unique(trimws(data_pcn$County)))
    glossy_box("Total Counties", x$Total, "", "bg-blue",
               counties = all_counties)
  })
  
  output$county_operational_box <- renderUI({
    x <- county_status_summary()
    glossy_box("Operational PCNs", x$Operational, "", "bg-deepgreen",
               counties = counties_with_status("gazetted_and_operational"))
  })
  
  output$county_gazetted_box <- renderUI({
    x <- county_status_summary()
    glossy_box("Gazetted PCNs", x$Gazetted, "", "bg-green",
               counties = counties_with_status(
                 c("gazetted", "gazetted_and_operational")))
  })
  
  output$county_ongoing_box <- renderUI({
    x <- county_status_summary()
    glossy_box("Ongoing Establishment of PCNs", x$Ongoing, "", "bg-yellowgreen",
               counties = counties_with_status("gazettement_ongoing"))
  })
  
  output$county_pending_box <- renderUI({
    x <- county_status_summary()
    glossy_box("Pending Establishment of PCNs", x$Pending, "", "bg-yellow",
               counties = counties_with_status("pending_gazettement"))
  })
  
  output$county_notgazetted_box <- renderUI({
    x <- county_status_summary()
    glossy_box("Ungazetted Subcounty Established PCNs", x$NotGazetted, "", "bg-amber",
               counties = counties_with_status(
                 "sub_county_not_gazetted_PCN_established"))
  })
  
  output$county_awaiting_box <- renderUI({
    x <- county_status_summary()
    glossy_box("Awaiting PCNs Establishment", x$Awaiting, "", "bg-orangered",
               counties = counties_with_status("awaiting_establishment"))
  })
  
  output$county_partner_box <- renderUI({
    x <- county_status_summary()
    glossy_box("Awaiting Partner Support", x$PartnerSupport, "", "bg-red",
               counties = counties_with_status("awaiting_partner_support"))
  }) 
  
  # ── 15l. Subcounty glossy boxes ─────────────────────────
  output$sub_total_box2 <- renderUI({
    x <- subcounty_status_summary()
    all_subs <- sort(unique(trimws(data_pcn$Subcounty)))
    glossy_box("Total Subcounties", x$Total, "", "bg-blue",
               items     = all_subs,
               item_type = "Subcounty")
  })
  
  output$sub_operational_box <- renderUI({
    x <- subcounty_status_summary()
    glossy_box("Operational", x$Operational, "", "bg-deepgreen",
               items     = subcounties_with_status("gazetted_and_operational"),
               item_type = "Subcounty")
  })
  
  output$sub_gazetted_box <- renderUI({
    x <- subcounty_status_summary()
    glossy_box("Gazetted", x$Gazetted, "", "bg-green",
               items     = subcounties_with_status(
                 c("gazetted","gazetted_and_operational")),
               item_type = "Subcounty")
  })
  
  output$sub_ongoing_box <- renderUI({
    x <- subcounty_status_summary()
    glossy_box("Ongoing Establishment", x$Ongoing, "", "bg-yellowgreen",
               items     = subcounties_with_status("gazettement_ongoing"),
               item_type = "Subcounty")
  })
  
  output$sub_pending_box <- renderUI({
    x <- subcounty_status_summary()
    glossy_box("Pending Gazettement", x$Pending, "", "bg-yellow",
               items     = subcounties_with_status("pending_gazettement"),
               item_type = "Subcounty")
  })
  
  output$sub_notgazetted_box <- renderUI({
    x <- subcounty_status_summary()
    glossy_box("Established, Not Gazetted", x$NotGazetted, "", "bg-amber",
               items     = subcounties_with_status(
                 "sub_county_not_gazetted_PCN_established"),
               item_type = "Subcounty")
  })
  
  output$sub_awaiting_box <- renderUI({
    x <- subcounty_status_summary()
    glossy_box("Awaiting Establishment", x$Awaiting, "", "bg-orangered",
               items     = subcounties_with_status("awaiting_establishment"),
               item_type = "Subcounty")
  })
  
  output$sub_partner_box <- renderUI({
    x <- subcounty_status_summary()
    glossy_box("Awaiting Partner Support", x$PartnerSupport, "", "bg-red",
               items     = subcounties_with_status("awaiting_partner_support"),
               item_type = "Subcounty")
  }) 
  
  # ── 15m. PCN glossy boxes ───────────────────────────────
  output$pcn_total_box <- renderUI({
    est      <- establishment_data()
    all_pcns <- sort(unique(trimws(data_pcn$PCN)))
    glossy_box("Total PCNs", est$totals$pcns, "", "bg-blue",
               items     = all_pcns,
               item_type = "PCN")
  })
  
  output$pcn_operational_box <- renderUI({
    x <- pcn_status_summary()
    glossy_box("Gazetted & Operational", x$Operational, "", "bg-deepgreen",
               items     = pcns_with_status("gazetted_and_operational"),
               item_type = "PCN")
  })
  
  output$pcn_gazetted_box <- renderUI({
    x <- pcn_status_summary()
    glossy_box("Gazetted", x$Gazetted, "", "bg-green",
               items     = pcns_with_status(
                 c("gazetted","gazetted_and_operational")),
               item_type = "PCN")
  })
  
  output$pcn_ongoing_box <- renderUI({
    x <- pcn_status_summary()
    glossy_box("Ongoing Establishment", x$Ongoing, "", "bg-yellowgreen",
               items     = pcns_with_status("gazettement_ongoing"),
               item_type = "PCN")
  })
  
  output$pcn_pending_box <- renderUI({
    x <- pcn_status_summary()
    glossy_box("Pending Gazettement", x$Pending, "", "bg-yellow",
               items     = pcns_with_status("pending_gazettement"),
               item_type = "PCN")
  })
  
  output$pcn_notgazetted_box <- renderUI({
    x <- pcn_status_summary()
    glossy_box("Established, Not Gazetted", x$NotGazetted, "", "bg-amber",
               items     = pcns_with_status(
                 c("new_sub_county_not_gazetted",
                   "sub_county_not_gazetted_PCN_established")),
               item_type = "PCN")
  })
  
  output$pcn_awaiting_box <- renderUI({
    x <- pcn_status_summary()
    glossy_box("Awaiting Establishment", x$Awaiting, "", "bg-orangered",
               items     = pcns_with_status("awaiting_establishment"),
               item_type = "PCN")
  })
  
  output$pcn_partner_box <- renderUI({
    x <- pcn_status_summary()
    glossy_box("Awaiting Partner Support", x$PartnerSupport, "", "bg-red",
               items     = pcns_with_status("awaiting_partner_support"),
               item_type = "PCN")
  })  
  # ── 15n. Shared bar-chart theme (used by all 3 establishment charts) ──
  establishment_bar_chart <- function(df, fill_color) {
    ggplot(df, aes(x = percent, y = reorder(status_label, percent))) +
      geom_col(fill = fill_color, width = 0.8) +
      geom_col(fill = "white", alpha = 0.15, width = 0.8) +
      geom_text(aes(label = n),              hjust = -0.3,  size = 4.5, fontface = "bold", color = "black") +
      geom_text(aes(label = paste0(percent,"%")), hjust = 1.15, size = 5,   fontface = "bold", color = "white") +
      scale_x_continuous(limits = c(0, max(df$percent) * 1.2), expand = c(0, 0)) +
      labs(x = NULL, y = NULL) +
      theme_minimal() +
      theme(
        plot.margin        = margin(10, 20, 10, 10),
        panel.grid         = element_blank(),
        axis.text.y        = element_text(size = 11, face = "bold"),
        panel.spacing      = unit(0, "lines"),
        plot.background    = element_rect(fill = "transparent", color = NA),
        panel.background   = element_rect(fill = "transparent", color = NA)
      )
  }
  
  # Style C — Glow bar with left accent line
  # Neon glow bar chart — flat bars with glow effect
  plotly_neon_bar <- function(df, bar_color, glow_color, bg_color) {
    df <- df %>% arrange(percent)
    
    plot_ly(
      df,
      x           = ~percent,
      y           = ~reorder(status_label, percent),
      type        = "bar",
      orientation = "h",
      text        = ~paste0(n, "  |  ", percent, "%"),
      textposition     = "inside",
      insidetextanchor = "middle",
      textfont    = list(color = "white", size = 12, family = "Arial"),
      marker = list(
        color = bar_color,
        line  = list(color = "rgba(0,0,0,0)", width = 0)
      ),
      hovertemplate = "<b>%{y}</b><br>%{text}<extra></extra>",
      showlegend = FALSE
    ) %>%
      layout(
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = bg_color,
        font          = list(color = "white", family = "Arial"),
        shapes = lapply(seq_len(nrow(df)), function(i) {
          list(
            type      = "rect",
            xref      = "x",
            yref      = "y",
            x0        = 0,
            x1        = df$percent[i],
            y0        = i - 1 - 0.28,
            y1        = i - 1 + 0.28,
            fillcolor = glow_color,
            line      = list(color = "rgba(0,0,0,0)", width = 0),
            opacity   = 0.18,
            layer     = "below"
          )
        }),
        xaxis = list(
          showgrid   = FALSE,
          zeroline   = FALSE,
          showline   = FALSE,
          ticksuffix = "%",
          tickfont   = list(color = "rgba(255,255,255,0.50)", size = 11),
          range      = c(0, max(df$percent, na.rm = TRUE) * 1.15)
        ),
        yaxis = list(
          showgrid   = FALSE,
          zeroline   = FALSE,
          showline   = FALSE,
          title      = "",
          tickfont   = list(color = "rgba(255,255,255,0.85)", size = 12),
          automargin = TRUE
        ),
        margin = list(l = 10, r = 30, t = 10, b = 30),
        bargap = 0.35,
        hoverlabel = list(
          bgcolor     = "rgba(0,0,0,0.80)",
          bordercolor = "rgba(255,255,255,0.20)",
          font        = list(color = "white", size = 13)
        )
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  }
  
  # Subcounty establishment status chart
  output$subcounty_perf_chart <- renderPlotly({
    df <- subcounty_perf()
    if (nrow(df) == 0) return(NULL)
    plotly_neon_bar(df,
                    bar_color  = "rgba(0,220,130,0.85)",
                    glow_color = "rgba(0,255,140,1.0)",
                    bg_color   = "rgba(0,15,8,0.75)")
  })
  # PCN establishment status chart
  output$pcn_status_chart <- renderPlotly({
    df <- pcn_perf_status()
    if (nrow(df) == 0) return(NULL)
    plotly_neon_bar(df,
                    bar_color  = "rgba(0,170,255,0.85)",
                    glow_color = "rgba(0,220,255,1.0)",
                    bg_color   = "rgba(0,8,20,0.75)")
  })
  
  # Dynamic height based on number of partners for the partners chart
  output$est_partner_chart_ui <- renderUI({
    df <- data_pcn_live()
    if (!is.null(input$partner_county_filter) && input$partner_county_filter != "All")
      df <- df %>% filter(County == input$partner_county_filter)
    if (!is.null(input$partner_subcounty_filter) && input$partner_subcounty_filter != "All")
      df <- df %>% filter(Subcounty == input$partner_subcounty_filter)
    if (!is.null(input$partner_pcn_filter) && input$partner_pcn_filter != "All")
      df <- df %>% filter(PCN == input$partner_pcn_filter)
    df_p <- df %>%
      mutate(supporting_partner = trimws(supporting_partner)) %>%
      filter(!is.na(supporting_partner), supporting_partner != "") %>%
      mutate(supporting_partner = stringr::str_wrap(supporting_partner, width = 25)) %>%
      distinct(supporting_partner)
    n <- nrow(df_p)
    max_lines <- max(stringr::str_count(df_p$supporting_partner, "\n") + 1, na.rm = TRUE)
    dynamic_height <- max(300, min(1200, n * (80 + (max_lines - 1) * 30)))
    plotlyOutput("est_partner_chart",
                 height = paste0(dynamic_height, "px"), width = "100%")
  })
  # ── 15o. Partner support horizontal bar chart ────────────
  output$est_partner_chart <- renderPlotly({
    
    df <- data_pcn_live()
    
    if (!is.null(input$partner_county_filter) && input$partner_county_filter != "All")
      df <- df %>% filter(County == input$partner_county_filter)
    if (!is.null(input$partner_subcounty_filter) && input$partner_subcounty_filter != "All")
      df <- df %>% filter(Subcounty == input$partner_subcounty_filter)
    if (!is.null(input$partner_pcn_filter) && input$partner_pcn_filter != "All")
      df <- df %>% filter(PCN == input$partner_pcn_filter)
    
    df_partner <- df %>%
      mutate(supporting_partner = trimws(supporting_partner)) %>%
      filter(!is.na(supporting_partner), supporting_partner != "") %>%
      group_by(supporting_partner) %>%
      summarise(n = n(), .groups = "drop") %>%
      mutate(percent = round(100 * n / sum(n), 1)) %>%
      arrange(percent)
    
    if (nrow(df_partner) == 0) {
      return(plot_ly() %>%
               layout(
                 paper_bgcolor = "rgba(0,0,0,0)",
                 plot_bgcolor  = "rgba(0,5,15,0.80)",
                 annotations   = list(list(
                   text      = "No partner data for the selected filters.",
                   x = 0.5, y = 0.5, showarrow = FALSE,
                   font = list(color = "rgba(255,255,255,0.50)", size = 14),
                   xref = "paper", yref = "paper"
                 ))
               ) %>% config(displayModeBar = FALSE))
    }
    
    plot_ly(
      df_partner,
      x           = ~percent,
      y           = ~reorder(supporting_partner, percent),
      type        = "bar",
      orientation = "h",
      text        = ~paste0(n, "  |  ", percent, "%"),
      textposition     = "inside",
      insidetextanchor = "middle",
      textfont    = list(color = "white", size = 12, family = "Arial"),
      marker      = list(
        color = "rgba(0,180,255,0.82)",
        line  = list(color = "rgba(0,234,255,0.40)", width = 1)
      ),
      hovertemplate = "<b>%{y}</b><br>PCNs: %{text}<extra></extra>"
    ) %>%
      layout(
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,5,15,0.80)",
        font          = list(color = "white", family = "Arial"),
        title         = list(
          text = "PCNs Supported by Partners (%)",
          font = list(color = "#00eaff", size = 15, family = "Arial"),
          x    = 0.5
        ),
        xaxis = list(
          showgrid    = FALSE,
          zeroline    = FALSE,
          showline    = FALSE,
          ticksuffix  = "%",
          tickfont    = list(color = "rgba(255,255,255,0.55)", size = 11),
          range       = c(0, max(df_partner$percent) * 1.18)
        ),
        yaxis = list(
          showgrid   = FALSE,
          zeroline   = FALSE,
          showline   = FALSE,
          title      = "",
          tickfont   = list(color = "rgba(255,255,255,0.85)", size = 12),
          automargin = TRUE
        ),
        margin  = list(l = 10, r = 40, t = 50, b = 30),
        bargap  = 0.30,
        hoverlabel = list(
          bgcolor    = "rgba(0,0,0,0.80)",
          bordercolor= "rgba(0,234,255,0.40)",
          font       = list(color = "white", size = 13)
        )
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })
  
  # ── 15p. Settings popup UI builders ─────────────────────
  
  # Gear popup for national + PCN charts
  make_chart_settings_ui <- function(prefix) {
    tagList(
      tags$h5("Chart Controls"),
      numericInput(paste0(prefix, "_nat_col_width"),   "Column Width:",  0.6, 0.2, 0.9),
      numericInput(paste0(prefix, "_nat_label_size"),  "Label Size:",    4,   1,   20),
      selectInput( paste0(prefix, "_nat_color"),       "Bar Color:",     c("steelblue","forestgreen","firebrick","purple","darkorange","darkblue")),
      selectInput( paste0(prefix, "_nat_font_family"), "Font:",          c("Arial","Georgia","Times New Roman","Calibri")),
      numericInput(paste0(prefix, "_nat_title_size"),  "Title Size:",    16,  8,   40),
      br(),
      downloadButton(paste0(prefix, "_download_national_chart"), "Download PNG")
    )
  }
  
  # Gear popup for subcounty charts
  make_subcounty_chart_settings_ui <- function(prefix) {
    tagList(
      tags$h5("Subcounty Controls"),
      numericInput(paste0(prefix, "_sub_top_n"),       "Top N Subcounties:", 20,  5,   100, step = 5),
      numericInput(paste0(prefix, "_sub_col_width"),   "Column Width:",      0.6, 0.2, 0.9, step = 0.05),
      numericInput(paste0(prefix, "_sub_label_size"),  "Label Size:",        6,   1,   20,  step = 0.5),
      numericInput(paste0(prefix, "_sub_title_size"),  "Title Size:",        20,  8,   40),
      selectInput( paste0(prefix, "_sub_color"),       "Bar Color:",         c("steelblue","forestgreen","firebrick","purple","darkorange","darkblue")),
      selectInput( paste0(prefix, "_sub_font_family"), "Font:",              c("Arial","Georgia","Times New Roman","Calibri")),
      br(),
      downloadButton(paste0(prefix, "_download_subcounty_chart"), "Download PNG")
    )
  }
  
  
  # ── 15q. Modal expand helper ─────────────────────────────
  # Opens a full-screen modal containing an expanded version of a chart or map
  show_expanded_modal <- function(output_id, type, title_text = "Expanded view") {
    height <- if (type == "chart") "650px" else "90vh"
    
    showModal(modalDialog(
      size = "xl", easyClose = TRUE, footer = NULL,
      
      # Pure CSS toggle — uses a hidden checkbox + label trick
      # No JavaScript, no Shiny inputs, always works
      tags$style(HTML(sprintf("
        #%s_css_toggle { display: none; }
        #%s_css_toggle:checked ~ #%s_settings_panel { display: block !important; }
        #%s_settings_label {
          position: absolute; top: 10px; right: 15px; z-index: 10001;
          background: rgba(50,50,50,0.92); color: white;
          border: 1px solid rgba(255,255,255,0.25); border-radius: 6px;
          font-size: 18px; padding: 4px 10px; cursor: pointer;
          display: inline-block; user-select: none;
        }
        #%s_settings_label:hover { background: rgba(80,80,80,0.95); }
      ",
                              output_id, output_id, output_id,
                              output_id, output_id
      ))),
      
      tags$h3(title_text),
      
      div(style = "position: relative;",
          
          # Hidden checkbox — this IS the toggle state
          tags$input(
            type = "checkbox",
            id   = paste0(output_id, "_css_toggle")
          ),
          
          # Label acts as the visible gear button
          tags$label(
            `for` = paste0(output_id, "_css_toggle"),
            id    = paste0(output_id, "_settings_label"),
            "⚙"
          ),
          
          # Settings panel — hidden by default, shown when checkbox checked
          div(
            id    = paste0(output_id, "_settings_panel"),
            style = paste0(
              "display:none; position:absolute; top:48px; right:15px;",
              "width:220px; z-index:10000;",
              "background:rgba(20,20,20,0.95);",
              "border:1px solid rgba(255,255,255,0.15);",
              "border-radius:8px; padding:10px;",
              "box-shadow:0 8px 24px rgba(0,0,0,0.5);"
            ),
            # Render settings content directly — no renderUI needed
            if (grepl("subcounty_chart", output_id)) {
              # Extract prefix from output_id for subcounty charts
              local({
                pfx <- gsub("_subcounty_chart_big$", "", output_id)
                make_subcounty_chart_settings_ui(pfx)
              })
            } else {
              local({
                pfx <- gsub("_(nat_chart_big|pcn_chart_big)$", "", output_id)
                make_chart_settings_ui(pfx)
              })
            }
          ),
          
          # The chart itself
          if (type == "chart") plotOutput(output_id, height = height)
          else                 plotly::plotlyOutput(output_id, height = height)
      )
    ))
  }
  # ── 15r. PCN monitoring tab UI builder ───────────────────
  # Generates the full layout for each of the 14 indicator tabs
  make_tab_ui <- function(indicator_choices, prefix) {
    
    # Helper: wraps a chart/map output with the dummy toggle button + watermark
    wrap_with_toggle <- function(output_widget, toggle_id, watermark_id) {
      div(style = "position:relative;",
          output_widget,
          # Dummy toggle button
          actionButton(
            toggle_id, label = "DEMO",
            class  = "dummy-toggle-btn",
            title  = "Toggle dummy/real data"
          ),
          # Watermark (shown/hidden via server)
          uiOutput(watermark_id)
      )
    }
    
    tab_display_names <- c(
      overview="Overview", governance="Governance",
      pophealth="Population Health Needs", capacity="Capacity Readiness",
      financing="Health Care Financing", infrastructure="Health Infrastructure",
      hmis="HMIS / Digital Health", hrh="Human Resources",
      service="Service Delivery", qoc_mgmt="QoC \u2013 Management Systems",
      qoc_phc="QoC \u2013 PHC Core Systems", qoc_outcomes="QoC \u2013 Outcomes",
      social="Social Accountability", innovation="Innovations & Learning"
    )
    display_name <- tab_display_names[[prefix]] %||% prefix
    bs4Card(
      title = paste(display_name, "Dashboard"), status = "primary",
      collapsible = TRUE, width = 12,
      
      fluidRow(column(12,
                      selectInput(paste0(prefix,"_indicator"), "Select Indicator:",
                                  choices = indicator_choices)
      )),
      br(),
      
      # National chart + national map
      fluidRow(
        column(6, box(width = NULL, title = "National County Chart",
                      div(class = "chart-container",
                          div(class = "toggle-top-right",
                              actionButton(paste0(prefix,"_toggle_national"), "⚙")),
                          uiOutput(paste0(prefix,"_nat_settings_ui")),
                          # dummy toggle sits left of gear
                          actionButton(paste0(prefix,"_dummy_nat_chart"), "REAL",
                                       class = "dummy-toggle-btn real-only",
                                       title = "Toggle dummy/real data"),
                          uiOutput(paste0(prefix,"_nat_chart_watermark")),
                          plotOutput(paste0(prefix,"_nat_chart"), height = "480px"),
                          div(class = "hover-expand-zone"),
                          div(class = "expand-wrapper",
                              div(class = "expand-label", "Expand"),
                              div(class = "expand-btn",
                                  onclick = sprintf("Shiny.setInputValue('%s', Math.random());",
                                                    paste0(prefix,"_expand_national_chart")),
                                  HTML("&#x2922;")))
                      )
        )),
        column(6, box(width = NULL, title = "National County Map",
                      div(class = "map-container",
                          div(class = "toggle-top-right",
                              actionButton(paste0(prefix,"_toggle_county_map"), "⚙")),
                          uiOutput(paste0(prefix,"_map_settings_ui")),
                          actionButton(paste0(prefix,"_dummy_nat_map"), "REAL",
                                       class = "dummy-toggle-btn real-only",
                                       title = "Toggle dummy/real data"),
                          uiOutput(paste0(prefix,"_nat_map_watermark")),
                          girafeOutput(paste0(prefix,"_nat_map"), height = "520px"),
                          div(class = "hover-expand-zone"),
                          div(class = "expand-wrapper",
                              div(class = "expand-label", "Expand"),
                              div(class = "expand-btn",
                                  onclick = sprintf("Shiny.setInputValue('%s', Math.random());",
                                                    paste0(prefix,"_expand_national_map")),
                                  HTML("&#x2922;")))
                      )
        ))
      ),
      br(),
      
      # PCN-level filters
      fluidRow(
        column(2, selectInput(paste0(prefix,"_pcn_county"), "County:",
                              choices  = c("Kenya"="Kenya",
                                           setNames(county_shp$County_clean, county_shp$County_raw)),
                              selected = "Kenya")),
        column(2, selectizeInput(paste0(prefix,"_pcn_subcounty"), "Subcounty:",
                                 choices = "All", selected = "All")),
        column(8, selectInput(paste0(prefix,"_pcn_indicator"), "Indicator:",
                              choices = indicator_choices))
      ),
      
      # PCN chart + subcounty map
      fluidRow(
        column(6, box(width = NULL, title = "PCN Performance",
                      div(class = "chart-container",
                          div(class = "toggle-top-right",
                              actionButton(paste0(prefix,"_toggle_pcn_chart"), "⚙")),
                          uiOutput(paste0(prefix,"_pcn_chart_settings_ui")),
                          actionButton(paste0(prefix,"_dummy_pcn_chart"), "REAL",
                                       class = "dummy-toggle-btn real-only",
                                       title = "Toggle dummy/real data"),
                          uiOutput(paste0(prefix,"_pcn_chart_watermark")),
                          plotOutput(paste0(prefix,"_pcn_chart"), height = "380px"),
                          div(class = "hover-expand-zone"),
                          div(class = "expand-wrapper",
                              div(class = "expand-label", "Expand"),
                              div(class = "expand-btn",
                                  onclick = sprintf("Shiny.setInputValue('%s', Math.random());",
                                                    paste0(prefix,"_expand_pcn_chart")),
                                  HTML("&#x2922;")))
                      )
        )),
        column(6, box(width = NULL, title = "Subcounty Map",
                      div(class = "map-container",
                          div(class = "toggle-top-right",
                              actionButton(paste0(prefix,"_toggle_pcn_map"), "⚙")),
                          uiOutput(paste0(prefix,"_pcn_map_settings_ui")),
                          actionButton(paste0(prefix,"_dummy_pcn_map"), "REAL",
                                       class = "dummy-toggle-btn real-only",
                                       title = "Toggle dummy/real data"),
                          uiOutput(paste0(prefix,"_pcn_map_watermark")),
                          girafeOutput(paste0(prefix,"_pcn_map"), height = "80vh"),
                          div(class = "hover-expand-zone"),
                          div(class = "expand-wrapper",
                              div(class = "expand-label", "Expand"),
                              div(class = "expand-btn",
                                  onclick = sprintf("Shiny.setInputValue('%s', Math.random());",
                                                    paste0(prefix,"_expand_pcn_map")),
                                  HTML("&#x2922;")))
                      )
        ))
      ),
      br(),
      
      # Subcounty chart filters
      fluidRow(
        column(2, selectInput(paste0(prefix,"_subchart_county"), "County:",
                              choices = c("Kenya", sort(unique(county_shp$County))), selected = "Kenya")),
        column(2, selectizeInput(paste0(prefix,"_subchart_subcounty"), "Subcounty:",
                                 choices = "All", selected = "All")),
        column(8, selectInput(paste0(prefix,"_subchart_indicator"), "Indicator:",
                              choices = indicator_choices))
      ),
      
      # Subcounty chart
      fluidRow(
        column(12, box(width = NULL, title = "Subcounty Chart",
                       div(class = "chart-container",
                           div(class = "toggle-top-right",
                               actionButton(paste0(prefix,"_toggle_subcounty_chart"), "⚙")),
                           uiOutput(paste0(prefix,"_subcounty_chart_settings_ui")),
                           actionButton(paste0(prefix,"_dummy_sub_chart"), "REAL",
                                        class = "dummy-toggle-btn real-only",
                                        title = "Toggle dummy/real data"),
                           uiOutput(paste0(prefix,"_sub_chart_watermark")),
                           plotOutput(paste0(prefix,"_subcounty_chart2"), height = "420px"),
                           div(class = "hover-expand-zone"),
                           div(class = "expand-wrapper",
                               div(class = "expand-label", "Expand"),
                               div(class = "expand-btn",
                                   onclick = sprintf("Shiny.setInputValue('%s', Math.random());",
                                                     paste0(prefix,"_expand_subcounty_chart")),
                                   HTML("&#x2922;")))
                       )
        ))
      )
    )
  }
  
  
  # ── 15s. County monitoring tab UI builder ────────────────
  # Generates the layout for each of the 10 county indicator tabs (chart + map only)
  make_county_tab_ui <- function(indicator_choices, prefix) {
    county_display_names <- c(
      county_overview="Overview", county_governance="Governance & Leadership",
      county_hrh="Human Resources", county_hpt="Health Products",
      county_service="Service Delivery", county_financing="Health Care Financing",
      county_hmis="HMIS / Digital Health", county_qoc_mgmt="QoC \u2013 Management Systems",
      county_multisectoral="Multisectoral Partnerships", county_innovation="Innovations & Learning"
    )
    display_name <- county_display_names[[prefix]] %||% prefix
    bs4Card(
      title = paste(display_name, "County Dashboard"), status = "primary", width = 12,
      selectInput(paste0(prefix,"_indicator"), "Select Indicator:", choices = indicator_choices),
      br(),
      fluidRow(
        column(6, box(width = NULL, title = "National County Chart",
                      div(class = "chart-container",
                          div(class = "toggle-top-right", actionButton(paste0(prefix,"_toggle_national"), "⚙")),
                          uiOutput(paste0(prefix,"_nat_settings_ui")),
                          plotOutput(paste0(prefix,"_nat_chart"), height = "480px"),
                          div(class = "hover-expand-zone"),
                          div(class = "expand-wrapper",
                              div(class = "expand-label", "Expand"),
                              div(class = "expand-btn",
                                  onclick = sprintf("Shiny.setInputValue('%s', Math.random());", paste0(prefix,"_expand_national_chart")),
                                  HTML("&#x2922;")))
                      ),
                      downloadButton(paste0(prefix,"_download_national_chart"), "Download Chart")
        )),
        column(6, box(width = NULL, title = "National County Map",
                      girafeOutput(paste0(prefix,"_nat_map"), height = "520px")
        ))
      )
    )
  }
  
  
  # ── 15t. Render all 14 PCN monitoring tab UIs ───────────
  output$tab_overview_ui     <- renderUI({ tagList(bs4Card(title="Overview Dashboard",status="primary",collapsible=TRUE,width=12, h3("Overview Summary",style="color:#004c97;"), p("This tab gives a full national overview of PCN indicators."), make_tab_ui(indicator_cols,"overview"))) })
  output$tab_governance_ui   <- renderUI({ make_tab_ui(pcn_indicators[["Governance"]],                         "governance")    })
  output$tab_pophealth_ui    <- renderUI({ make_tab_ui(pcn_indicators[["Population Health Needs"]],            "pophealth")     })
  output$tab_capacity_ui     <- renderUI({ make_tab_ui(pcn_indicators[["Capacity Readiness"]],                 "capacity")      })
  output$tab_financing_ui    <- renderUI({ make_tab_ui(pcn_indicators[["Health Care Financing"]],              "financing")     })
  output$tab_infrastructure_ui <- renderUI({ make_tab_ui(pcn_indicators[["Health Infrastructure"]],            "infrastructure")})
  output$tab_hmis_ui         <- renderUI({ make_tab_ui(pcn_indicators[["HMIS / Digital Health"]],              "hmis")          })
  output$tab_hrh_ui          <- renderUI({ make_tab_ui(pcn_indicators[["HRH"]],                                "hrh")           })
  output$tab_service_ui      <- renderUI({ make_tab_ui(pcn_indicators[["Service Delivery"]],                   "service")       })
  output$tab_qoc_mgmt_ui     <- renderUI({ make_tab_ui(pcn_indicators[["Quality of Care \u2013 Management Systems"]], "qoc_mgmt") })
  output$tab_qoc_phc_ui      <- renderUI({ make_tab_ui(pcn_indicators[["Quality of Care \u2013 PHC Core Systems"]],   "qoc_phc")  })
  output$tab_qoc_outcomes_ui <- renderUI({ make_tab_ui(pcn_indicators[["Quality of Care \u2013 Outcomes"]],   "qoc_outcomes")  })
  output$tab_social_ui       <- renderUI({ make_tab_ui(pcn_indicators[["Social Accountability"]],              "social")        })
  output$tab_innovation_ui   <- renderUI({ make_tab_ui(pcn_indicators[["Innovations and Learning"]],           "innovation")    })
  
  
  # ── 15u. Render all 10 county monitoring tab UIs ────────
  output$county_tab_overview_ui       <- renderUI({ make_county_tab_ui(county_indicators[["Overview"]],                                    "county_overview")       })
  output$county_tab_governance_ui     <- renderUI({ make_county_tab_ui(county_indicators[["Governance & Leadership for PCNs"]],             "county_governance")     })
  output$county_tab_hrh_ui            <- renderUI({ make_county_tab_ui(county_indicators[["Human Resource for Health"]],                    "county_hrh")            })
  output$county_tab_hpt_ui            <- renderUI({ make_county_tab_ui(county_indicators[["Health Product Technologies"]],                  "county_hpt")            })
  output$county_tab_service_ui        <- renderUI({ make_county_tab_ui(county_indicators[["Service Delivery"]],                             "county_service")        })
  output$county_tab_financing_ui      <- renderUI({ make_county_tab_ui(county_indicators[["Health Care Financing"]],                        "county_financing")      })
  output$county_tab_hmis_ui           <- renderUI({ make_county_tab_ui(county_indicators[["HMIS / Digital Health"]],                        "county_hmis")           })
  output$county_tab_qoc_mgmt_ui       <- renderUI({ make_county_tab_ui(county_indicators[["Quality of Care \u2013 Management Systems"]],    "county_qoc_mgmt")       })
  output$county_tab_multisectoral_ui  <- renderUI({ make_county_tab_ui(county_indicators[["Multisectoral Partnerships and Coordination"]], "county_multisectoral")  })
  output$county_tab_innovation_ui     <- renderUI({ make_county_tab_ui(county_indicators[["Innovations and Learning"]],                     "county_innovation")     })
  
  
  # ── 15v. Subcounty dropdown updaters ────────────────────
  pcn_prefixes <- c("overview","governance","pophealth","capacity","financing",
                    "infrastructure","hmis","hrh","service","qoc_mgmt",
                    "qoc_phc","qoc_outcomes","social","innovation")
  
  # PCN-level subcounty filter
  observe({
    lapply(pcn_prefixes, function(prefix) {
      selected <- input[[paste0(prefix,"_pcn_county")]] %||% "Kenya"
      subs <- if (selected == "Kenya") sort(unique(subc_shp$Subcounty))
      else sort(unique(subc_shp$Subcounty[subc_shp$County_clean == selected]))
      updateSelectizeInput(session, paste0(prefix,"_pcn_subcounty"), choices = c("All", subs), selected = "All")
    })
  })
  
  # Subcounty-chart-level subcounty filter
  observe({
    lapply(pcn_prefixes, function(prefix) {
      selected <- input[[paste0(prefix,"_subchart_county")]] %||% "Kenya"
      subs <- if (selected == "Kenya") sort(unique(subc_shp$Subcounty))
      else sort(unique(subc_shp$Subcounty[subc_shp$County == selected]))
      updateSelectizeInput(session, paste0(prefix,"_subchart_subcounty"), choices = c("All", subs), selected = "All")
    })
  })
  
  
  # ── 15w. County monitoring loop ─────────────────────────
  # Generates chart + map outputs for each of the 10 county tabs
  county_prefixes <- c("county_overview","county_governance","county_hrh","county_hpt",
                       "county_service","county_financing","county_hmis",
                       "county_qoc_mgmt","county_multisectoral","county_innovation")
  
  for (p in county_prefixes) {
    local({
      prefix <- p
      
      # Settings popup
      output[[paste0(prefix,"_nat_settings_ui")]] <- renderUI({
        toggle <- input[[paste0(prefix,"_toggle_national")]]
        if (is.null(toggle) || toggle %% 2 == 0) return(NULL)
        div(class = "chart-settings-popup", make_chart_settings_ui(prefix))
      })
      
      # Data reactive
      county_data <- reactive({
        ind <- input[[paste0(prefix,"_indicator")]]; req(ind)
        data_county %>%
          filter(!tolower(County) %in% c("national", "kenya", "total", "national total")) %>%
          group_by(County) %>%
          summarise(Value = mean(.data[[ind]], na.rm = TRUE), .groups = "drop")
      })
      
      # Shared plot builder used by both main and expanded chart
      build_county_nat_chart <- function(df, ind_label, bar_color, bar_width,
                                         label_size, title_size, font_family,
                                         angle = 60, title_prefix = "National") {
        kenya_avg <- suppressWarnings(mean(df$Value, na.rm = TRUE))
        if (is.nan(kenya_avg)) kenya_avg <- NA_real_
        
        df_final <- bind_rows(df, tibble(County = "Kenya", Value = kenya_avg)) %>%
          mutate(County = as.character(County))
        
        sorted_levels <- df_final %>% arrange(desc(Value)) %>% pull(County) %>% unique()
        
        df_final <- df_final %>%
          mutate(County_f = factor(County, levels = sorted_levels),
                 label_text = ifelse(is.na(Value), "", round(Value, 0)))
        
        ggplot(df_final, aes(x = County_f, y = Value)) +
          geom_col(aes(fill = ifelse(County == "Kenya","Kenya","Other")), width = bar_width) +
          scale_fill_manual(values = c("Kenya" = "red","Other" = bar_color), guide = "none") +
          geom_segment(aes(x = 0.5, xend = length(sorted_levels)+0.5, y = 0, yend = 0),
                       linewidth = 0.5, color = "grey") +
          geom_text(aes(label = label_text), vjust = -0.35, size = label_size, family = font_family) +
          labs(title = paste(title_prefix, ":", ind_label), x = NULL, y = "Average (%)") +
          theme_minimal(base_family = font_family) +
          theme(
            legend.position = "none",
            axis.text.y = element_blank(), axis.ticks.y = element_blank(),
            axis.title.y = element_text(size = 18, margin = margin(r = 10)),
            panel.grid = element_blank(),
            panel.background = element_rect(fill = "white", color = NA),
            plot.background  = element_rect(fill = "white", color = NA),
            plot.title   = element_text(size = title_size, face = "bold"),
            axis.text.x  = element_text(size = 14, angle = angle, hjust = 1),
            plot.margin  = margin(10, 30, 20, 30)
          ) +
          scale_y_continuous(limits = c(0,100), expand = expansion(mult = c(0,0.05)))
      }
      
      # Main national chart
      output[[paste0(prefix,"_nat_chart")]] <- renderPlot({
        df <- county_data(); req(df)
        build_county_nat_chart(
          df, ind_label   = input[[paste0(prefix,"_indicator")]],
          bar_color   = get_input_or_default(input, paste0(prefix,"_nat_color"),       "steelblue"),
          bar_width   = get_input_or_default(input, paste0(prefix,"_nat_col_width"),   0.6),
          label_size  = get_input_or_default(input, paste0(prefix,"_nat_label_size"),  4),
          title_size  = get_input_or_default(input, paste0(prefix,"_nat_title_size"),  16),
          font_family = get_input_or_default(input, paste0(prefix,"_nat_font_family"), "Arial")
        )
      })
      
      # Expanded modal chart
      output[[paste0(prefix,"_nat_chart_big")]] <- renderPlot({
        df <- county_data(); req(df)
        build_county_nat_chart(
          df, ind_label   = input[[paste0(prefix,"_indicator")]],
          bar_color   = get_input_or_default(input, paste0(prefix,"_nat_color"),       "steelblue"),
          bar_width   = get_input_or_default(input, paste0(prefix,"_nat_col_width"),   0.6),
          label_size  = get_input_or_default(input, paste0(prefix,"_nat_label_size"),  4) + 2,
          title_size  = get_input_or_default(input, paste0(prefix,"_nat_title_size"),  16) + 6,
          font_family = get_input_or_default(input, paste0(prefix,"_nat_font_family"), "Arial"),
          angle = 45, title_prefix = "Expanded National"
        )
      })
      
      
      # Expand button opens modal
      observeEvent(input[[paste0(prefix,"_expand_national_chart")]], {
        show_expanded_modal(paste0(prefix,"_nat_chart_big"), "chart", "Expanded National Chart")
      })
      
      # Download handler
      output[[paste0(prefix,"_download_national_chart")]] <- downloadHandler(
        filename = function() paste0(prefix,"_county_nat_chart.png"),
        content  = function(file) {
          df <- county_data()
          g <- build_county_nat_chart(
            df, ind_label   = input[[paste0(prefix,"_indicator")]],
            bar_color   = get_input_or_default(input, paste0(prefix,"_nat_color"),       "steelblue"),
            bar_width   = get_input_or_default(input, paste0(prefix,"_nat_col_width"),   0.6),
            label_size  = get_input_or_default(input, paste0(prefix,"_nat_label_size"),  4),
            title_size  = get_input_or_default(input, paste0(prefix,"_nat_title_size"),  16),
            font_family = get_input_or_default(input, paste0(prefix,"_nat_font_family"), "Arial")
          )
          ggsave(file, plot = g, width = 10, height = 8, dpi = 300)
        }
      )
      
      # National choropleth map
      output[[paste0(prefix,"_nat_map")]] <- renderGirafe({
        df <- county_data(); req(df)
        map_df <- county_shp %>% left_join(df, by = "County")
        
        p <- ggplot(map_df) +
          geom_sf_interactive(aes(
            fill    = Value,
            tooltip = paste0("County: ", County, "\nPerformance: ",
                             ifelse(is.na(Value),"NA", paste0(round(Value,1),"%"))),
            data_id = County), color = "black", size = 0.25) +
          geom_sf(data = lakes_shp, fill = "#3EA4F0", color = NA, alpha = 0.85) +
          scale_fill_gradientn(colours = c("#e41a1c","#ff7f00","#ffff33","#3ff40d","#399c35"),
                               limits = c(0,100), na.value = "white") +
          map_theme() +
          labs(title = paste(input[[paste0(prefix,"_indicator")]])) +
          theme(plot.title = element_text(margin = margin(b=25)), plot.margin = margin(20,20,70,20)) +
          geom_sf_text(aes(label=County), size=3.5, color="black", fontface="bold") +
          annotation_scale(location="br", style="ticks", width_hint=0.25, text_cex=1.2) +
          annotation_north_arrow(location="tr", which_north="true", style=north_arrow_orienteering)
        
        girafe(ggobj = p, width_svg = 20, height_svg = 18, options = list(
          opts_hover(css     = "stroke:black;stroke-width:1.5;cursor:pointer;"),
          opts_hover_inv(css = "opacity:0.5;"),
          opts_tooltip(css   = "background-color:black;color:white;padding:8px;border-radius:4px;font-size:12px;"),
          opts_sizing(rescale = TRUE)
        ))
      })
    })
  }
  
  
  # ── 15x. PCN monitoring loop ─────────────────────────────
  # Generates all chart + map outputs for each of the 14 PCN indicator tabs
  for (p in pcn_prefixes) {
    local({
      prefix <- p
      # ── Per-chart dummy data toggles ───────────────────────
      # Each button independently tracks odd/even clicks = ON/OFF
      dummy_nat_chart  <- reactiveVal(FALSE)
      dummy_nat_map    <- reactiveVal(FALSE)
      dummy_pcn_chart  <- reactiveVal(FALSE)
      dummy_pcn_map    <- reactiveVal(FALSE)
      dummy_sub_chart  <- reactiveVal(FALSE)
      
      observeEvent(input[[paste0(prefix,"_dummy_nat_chart")]], {
        dummy_nat_chart(!dummy_nat_chart())
        session$sendCustomMessage("updateDummyBtn",
                                  list(id = paste0(prefix,"_dummy_nat_chart"), state = dummy_nat_chart()))
      })
      observeEvent(input[[paste0(prefix,"_dummy_nat_map")]], {
        dummy_nat_map(!dummy_nat_map())
        session$sendCustomMessage("updateDummyBtn",
                                  list(id = paste0(prefix,"_dummy_nat_map"), state = dummy_nat_map()))
      })
      observeEvent(input[[paste0(prefix,"_dummy_pcn_chart")]], {
        dummy_pcn_chart(!dummy_pcn_chart())
        session$sendCustomMessage("updateDummyBtn",
                                  list(id = paste0(prefix,"_dummy_pcn_chart"), state = dummy_pcn_chart()))
      })
      observeEvent(input[[paste0(prefix,"_dummy_pcn_map")]], {
        dummy_pcn_map(!dummy_pcn_map())
        session$sendCustomMessage("updateDummyBtn",
                                  list(id = paste0(prefix,"_dummy_pcn_map"), state = dummy_pcn_map()))
      })
      observeEvent(input[[paste0(prefix,"_dummy_sub_chart")]], {
        dummy_sub_chart(!dummy_sub_chart())
        session$sendCustomMessage("updateDummyBtn",
                                  list(id = paste0(prefix,"_dummy_sub_chart"), state = dummy_sub_chart()))
      })
      
      # Watermark renderers — show when dummy is OFF
      output[[paste0(prefix,"_nat_chart_watermark")]] <- renderUI({
        if (dummy_nat_chart()) div(class = "nodata-watermark", "DEMO DATA")
      })
      output[[paste0(prefix,"_nat_map_watermark")]] <- renderUI({
        if (dummy_nat_map()) div(class = "nodata-watermark", "DEMO DATA")
      })
      output[[paste0(prefix,"_pcn_chart_watermark")]] <- renderUI({
        if (dummy_pcn_chart()) div(class = "nodata-watermark", "DEMO DATA")
      })
      output[[paste0(prefix,"_pcn_map_watermark")]] <- renderUI({
        if (dummy_pcn_map()) div(class = "nodata-watermark", "DEMO DATA")
      })
      output[[paste0(prefix,"_sub_chart_watermark")]] <- renderUI({
        if (dummy_sub_chart()) div(class = "nodata-watermark", "DEMO DATA")
      })
      # Map settings popup helpers (defined once per prefix)
      make_map_settings_ui <- function() {
        div(tags$h5("Map Controls"),
            numericInput(paste0(prefix,"_cnty_map_label_size"),  "Label Size:", 5, 1, 20),
            radioButtons( paste0(prefix,"_cnty_map_label_color"),"Label Color:", c("Black"="black","White"="white"))
        )
      }
      
      make_pcn_map_settings_ui <- function() {
        div(tags$h5("PCN Map Controls"),
            checkboxInput(paste0(prefix,"_show_pcns"),             "Show PCNs",              value = FALSE),
            checkboxInput(paste0(prefix,"_show_subcounty_labels"), "Show Subcounty Labels",  value = FALSE),
            numericInput( paste0(prefix,"_pcn_map_label_size"),    "Label Size:", 5, 1, 20),
            radioButtons( paste0(prefix,"_pcn_map_label_color"),   "Label Color:", c("Black"="black","White"="white"))
        )
      }
      
      # Floating settings popups
      output[[paste0(prefix,"_map_settings_ui")]] <- renderUI({
        toggle <- input[[paste0(prefix,"_toggle_county_map")]]
        if (is.null(toggle) || toggle %% 2 == 0) return(NULL)
        div(class = "map-settings-popup", make_map_settings_ui())
      })
      
      output[[paste0(prefix,"_nat_settings_ui")]] <- renderUI({
        toggle <- input[[paste0(prefix,"_toggle_national")]]
        if (is.null(toggle) || toggle %% 2 == 0) return(NULL)
        div(class = "chart-settings-popup", tags$h5("National Controls"),
            numericInput(paste0(prefix,"_nat_col_width"),   "Column Width:", 0.6, 0.2, 0.9),
            numericInput(paste0(prefix,"_nat_label_size"),  "Label Size:",   4,   1,   20),
            selectInput( paste0(prefix,"_nat_color"),       "Bar Color:",    c("steelblue","forestgreen","firebrick","purple","darkorange","darkblue")),
            selectInput( paste0(prefix,"_nat_font_family"), "Font:",         c("Arial","Georgia","Times New Roman","Calibri")),
            numericInput(paste0(prefix,"_nat_title_size"),  "Title Size:",   16,  8,   40)
        )
      })
      
      output[[paste0(prefix,"_pcn_chart_settings_ui")]] <- renderUI({
        toggle <- input[[paste0(prefix,"_toggle_pcn_chart")]]
        if (is.null(toggle) || toggle %% 2 == 0) return(NULL)
        div(class = "chart-settings-popup", make_chart_settings_ui(prefix))
      })
      
      output[[paste0(prefix,"_pcn_map_settings_ui")]] <- renderUI({
        toggle <- input[[paste0(prefix,"_toggle_pcn_map")]] %||% 0
        if (toggle %% 2 == 0) return(NULL)
        div(class = "map-settings-popup", make_pcn_map_settings_ui())
      })
      
      output[[paste0(prefix,"_subcounty_chart_settings_ui")]] <- renderUI({
        toggle <- input[[paste0(prefix,"_toggle_subcounty_chart")]]
        if (is.null(toggle) || toggle %% 2 == 0) return(NULL)
        div(class = "chart-settings-popup", make_subcounty_chart_settings_ui(prefix))
      })
      
      # ── National data reactive ──────────────────────────
      nat_data <- reactive({
        ind <- input[[paste0(prefix,"_indicator")]]; req(ind)
        use_dummy_here <- dummy_nat_chart()
        
        ref_map <- county_shp %>% st_drop_geometry() %>% distinct(County) %>%
          mutate(County = trimws(County), County_clean = normalize_name(County)) %>%
          select(County, County_clean)
        
        df_summary <- data_pcn_live() %>%
          mutate(County = trimws(County), County_clean = normalize_name(County)) %>%
          filter(!tolower(County) %in% c("national", "kenya", "total", "national total")) %>%
          group_by(County_clean) %>%
          summarise(Value = mean(.data[[ind]], na.rm = TRUE), .groups = "drop")
        
        df_all <- ref_map %>% left_join(df_summary, by = "County_clean") %>%
          mutate(Value = as.numeric(Value), real_data = !is.na(Value))
        
        if (any(is.na(df_all$Value))) {
          nas <- is.na(df_all$Value)
          df_all$Value[nas]     <- if (use_dummy_here) round(runif(sum(nas), 10, 90), 1) else NA_real_
          df_all$real_data[nas] <- FALSE
        }
        df_all %>% arrange(desc(Value))
      })
      
      # ── National data reactive ──────────────────────────
      nat_data_map <- reactive({
        ind <- input[[paste0(prefix,"_indicator")]]; req(ind)
        use_dummy_here <- dummy_nat_map()
        
        ref_map <- county_shp %>% st_drop_geometry() %>% distinct(County) %>%
          mutate(County = trimws(County), County_clean = normalize_name(County)) %>%
          select(County, County_clean)
        
        df_summary <- data_pcn_live() %>%
          mutate(County = trimws(County), County_clean = normalize_name(County)) %>%
          filter(!tolower(County) %in% c("national", "kenya", "total", "national total")) %>%
          group_by(County_clean) %>%
          summarise(Value = mean(.data[[ind]], na.rm = TRUE), .groups = "drop")
        
        df_all <- ref_map %>% left_join(df_summary, by = "County_clean") %>%
          mutate(Value = as.numeric(Value), real_data = !is.na(Value))
        
        if (any(is.na(df_all$Value))) {
          nas <- is.na(df_all$Value)
          df_all$Value[nas]     <- if (use_dummy_here) round(runif(sum(nas), 10, 90), 1) else NA_real_
          df_all$real_data[nas] <- FALSE
        }
        df_all %>% arrange(desc(Value))
      })
      # ── Shared national bar-chart builder ──────────────
      build_nat_chart <- function(df, ind_label, bar_color, bar_width,
                                  label_size, title_size, font_family,
                                  angle = 90, title_prefix = "National") {
        kenya_avg <- suppressWarnings(mean(df$Value[df$real_data], na.rm = TRUE))
        if (is.nan(kenya_avg)) kenya_avg <- NA_real_
        
        df_final <- bind_rows(df, tibble(County="Kenya",County_clean="kenya",
                                         Value=kenya_avg,real_data=!is.na(kenya_avg))) %>%
          mutate(County = as.character(County))
        
        sorted_levels <- df_final %>% arrange(desc(Value)) %>% pull(County) %>% unique()
        df_final <- df_final %>%
          mutate(County_f   = factor(County, levels = sorted_levels),
                 label_text = ifelse(is.na(Value),"", round(Value,0)))
        
        ggplot(df_final, aes(x = County_f, y = Value)) +
          geom_col(aes(fill = ifelse(County=="Kenya","Kenya","Other")), width = bar_width) +
          scale_fill_manual(values = c("Kenya"="red","Other"=bar_color), guide = "none") +
          geom_segment(aes(x=0.5, xend=length(sorted_levels)+0.5, y=0, yend=0),
                       linewidth=0.5, color="grey") +
          geom_text(aes(label=label_text), vjust=-0.35, size=label_size, family=font_family) +
          labs(title=paste(ind_label), x=NULL, y="Average (%)") +
          theme_minimal(base_family=font_family) +
          theme(
            legend.position="none",
            axis.text.y=element_blank(), axis.ticks.y=element_blank(),
            axis.title.y=element_text(size=18,margin=margin(r=10)),
            panel.grid=element_blank(),
            panel.background=element_rect(fill="white",color=NA),
            plot.background =element_rect(fill="white",color=NA),
            plot.title  =element_text(size=title_size,face="bold"),
            axis.text.x =element_text(size=14,angle=angle,hjust=1),
            plot.margin =margin(10,30,20,30)
          ) +
          scale_y_continuous(limits = c(0, 105), expand = expansion(mult = c(0, 0.02)))
      }
      
      # Main national chart
      output[[paste0(prefix,"_nat_chart")]] <- renderPlot({
        df <- nat_data(); req(df)
        build_nat_chart(df, input[[paste0(prefix,"_indicator")]],
                        bar_color   = input[[paste0(prefix,"_nat_color")]]       %||% "steelblue",
                        bar_width   = input[[paste0(prefix,"_nat_col_width")]]   %||% 0.6,
                        label_size  = input[[paste0(prefix,"_nat_label_size")]]  %||% 3,
                        title_size  = input[[paste0(prefix,"_nat_title_size")]]  %||% 12,
                        font_family = input[[paste0(prefix,"_nat_font_family")]] %||% "Arial"
        )
      })
      
      # Expanded modal chart
      output[[paste0(prefix,"_nat_chart_big")]] <- renderPlot({
        df <- nat_data(); req(df)
        build_nat_chart(df, input[[paste0(prefix,"_indicator")]],
                        bar_color   = input[[paste0(prefix,"_nat_color")]]       %||% "#1a73e8",
                        bar_width   = input[[paste0(prefix,"_nat_col_width")]]   %||% 0.6,
                        label_size  = (input[[paste0(prefix,"_nat_label_size")]] %||% 4) + 2,
                        title_size  = (input[[paste0(prefix,"_nat_title_size")]] %||% 16) + 6,
                        font_family = input[[paste0(prefix,"_nat_font_family")]] %||% "Arial",
                        angle = 45, title_prefix = "Expanded National"
        )
      })
      
      # Modal settings content — rendered into the JS-toggled panel
      output[[paste0(prefix,"_nat_chart_big_modal_settings_ui")]] <- renderUI({
        toggle <- input[[paste0(prefix,"_nat_chart_big_modal_toggle")]] %||% 0
        if (toggle == 0 || toggle %% 2 == 0) return(NULL)
        div(class = "chart-settings-popup", make_chart_settings_ui(prefix))
      })
      
      observeEvent(input[[paste0(prefix,"_expand_national_chart")]], {
        show_expanded_modal(paste0(prefix,"_nat_chart_big"), "chart", "Expanded National Chart")
      })
      
      output[[paste0(prefix,"_download_national_chart")]] <- downloadHandler(
        filename = function() paste0(prefix,"_nat_chart.png"),
        content  = function(file) {
          df <- nat_data()
          g  <- build_nat_chart(df, input[[paste0(prefix,"_indicator")]],
                                bar_color   = input[[paste0(prefix,"_nat_color")]]       %||% "steelblue",
                                bar_width   = input[[paste0(prefix,"_nat_col_width")]]   %||% 0.6,
                                label_size  = input[[paste0(prefix,"_nat_label_size")]]  %||% 4,
                                title_size  = input[[paste0(prefix,"_nat_title_size")]]  %||% 16,
                                font_family = input[[paste0(prefix,"_nat_font_family")]] %||% "Arial"
          )
          ggsave(file, plot=g, width=10, height=8, dpi=300)
        }
      )
      
      # ── National choropleth map ─────────────────────────
      output[[paste0(prefix,"_nat_map")]] <- renderGirafe({
        df <- nat_data_map()
        map_df      <- county_shp %>% left_join(df, by = "County")
        label_size  <- input[[paste0(prefix,"_cnty_map_label_size")]]  %||% 5
        label_color <- input[[paste0(prefix,"_cnty_map_label_color")]] %||% "black"
        
        p <- ggplot(map_df) +
          geom_sf_interactive(aes(
            fill    = Value,
            tooltip = paste0("County: ",County,"\nPerformance: ",
                             ifelse(is.na(Value),"NA",paste0(round(Value,1),"%"))),
            data_id = County), color="black", size=0.25) +
          geom_sf(data=lakes_shp, fill="#3EA4F0", color=NA, alpha=0.8) +
          scale_fill_gradientn(colours=c("#e41a1c","#ff7f00","#ffff33","#3ff40d","#399c35"),
                               limits=c(0,100), na.value="white") +
          map_theme() +
          labs(title=paste("National Map –",input[[paste0(prefix,"_indicator")]])) +
          theme(plot.title=element_text(margin=margin(b=25)), plot.margin=margin(20,20,70,20))
        
        p <- p + geom_sf_text(aes(label=County), size=label_size, color=label_color)
        
        p <- p +
          annotation_scale(location="br",style="ticks",width_hint=0.25,text_cex=1.2,
                           tick_height=0.6,pad_y=unit(0.1,"cm")) +
          annotation_north_arrow(location="tr",which_north="true",style=north_arrow_orienteering)
        
        girafe(ggobj=p, width_svg=20, height_svg=18, options=list(
          opts_hover(css="stroke:black;stroke-width:1.5;cursor:pointer;"),
          opts_hover_inv(css="opacity:0.5;"),
          opts_tooltip(css="background-color:black;color:white;padding:8px;border-radius:4px;font-size:12px;"),
          opts_sizing(rescale=TRUE)
        ))
      })
      
      # ── PCN-level data reactive ─────────────────────────
      pcn_data <- reactive({
        ind  <- input[[paste0(prefix,"_pcn_indicator")]]; req(ind)
        cnty <- input[[paste0(prefix,"_pcn_county")]]
        sub  <- input[[paste0(prefix,"_pcn_subcounty")]]
        
        df <- data_pcn_live()
        if (cnty != "Kenya") df <- df %>% filter(County_clean    == normalize_name(cnty))
        if (sub  != "All")   df <- df %>% filter(Subcounty_clean == normalize_name(sub))
        
        available_pcns <- df %>% select(PCN) %>% distinct()
        
        df_pcn <- df %>% group_by(PCN) %>%
          summarise(Value = suppressWarnings(mean(.data[[ind]], na.rm=TRUE)), .groups="drop")
        
        if (!"Value" %in% names(df_pcn)) df_pcn$Value <- NA_real_
        if (!"PCN"   %in% names(df_pcn)) df_pcn$PCN   <- character(nrow(df_pcn))
        
        df_pcn <- add_dummy_pcn_values(df_pcn, "Value", available_pcns, use_dummy = dummy_pcn_chart())
        
        if (nrow(df) == 0) {
          available_pcns <- if (cnty != "Kenya")
            subc_shp %>% filter(County_clean==cnty) %>% st_drop_geometry() %>%
            distinct(Subcounty_raw) %>% rename(PCN=Subcounty_raw)
          else
            subc_shp %>% st_drop_geometry() %>% distinct(Subcounty_raw) %>% rename(PCN=Subcounty_raw)
          
          df_pcn <- add_dummy_pcn_values(
            data.frame(PCN=available_pcns$PCN, Value=NA, stringsAsFactors=FALSE),
            "Value", available_pcns, use_dummy = dummy_pcn_chart())
        }
        
        if (cnty == "Kenya") df_pcn %>% arrange(desc(Value)) %>% slice(1:15)
        else                 df_pcn %>% arrange(desc(Value))
      })
      
      # ── Shared PCN bar-chart builder ───────────────────
      build_pcn_chart <- function(df_pcn, ind_label, bar_color, bar_width,
                                  label_size, title_size, font_family, wrap_width=6) {
        kenya_avg <- mean(df_pcn$Value, na.rm=TRUE)
        df_plot <- bind_rows(
          df_pcn %>% mutate(is_kenya=FALSE),
          tibble(PCN="Kenya", Value=kenya_avg, is_kenya=TRUE)
        ) %>% mutate(label=round(Value,0), PCN_wrapped=stringr::str_wrap(PCN, width=wrap_width))
        
        ggplot(df_plot, aes(x=reorder(PCN_wrapped,-Value), y=Value)) +
          geom_col(aes(fill=ifelse(is_kenya,"Kenya","Other")), width=bar_width) +
          scale_fill_manual(values=c("Kenya"="red","Other"=bar_color), guide="none") +
          geom_text(aes(label=label), vjust=-0.35, size=label_size, family=font_family) +
          labs(title=paste(ind_label), x=NULL, y="Average (%)") +
          theme(plot.title = element_text(hjust = 0.5)) +
          scale_y_continuous(limits=c(0,105), expand=expansion(mult=c(0,0.02))) +
          theme_minimal(base_family=font_family) +
          theme(
            legend.position="none",
            axis.text.y=element_blank(), axis.ticks.y=element_blank(),
            axis.title.y=element_text(size=18,margin=margin(r=10)),
            panel.grid=element_blank(),
            axis.line.x=element_line(color="grey40",linewidth=0.6),
            panel.background=element_rect(fill="white",color=NA),
            plot.background =element_rect(fill="white",color=NA),
            plot.title =element_text(size=title_size,face="bold"),
            axis.text.x=element_text(size=9,angle=45,hjust=1,vjust=1,margin=margin(t=-12)),
            plot.margin=margin(10,30,5,30)
          )
      }
      
      output[[paste0(prefix,"_pcn_chart")]] <- renderPlot({
        df <- pcn_data(); req(df, nrow(df)>0)
        build_pcn_chart(df, input[[paste0(prefix,"_pcn_indicator")]],
                        bar_color   = input[[paste0(prefix,"_nat_color")]]       %||% "steelblue",
                        bar_width   = input[[paste0(prefix,"_nat_col_width")]]   %||% 0.6,
                        label_size  = input[[paste0(prefix,"_nat_label_size")]]  %||% 4,
                        title_size  = input[[paste0(prefix,"_nat_title_size")]]  %||% 16,
                        font_family = input[[paste0(prefix,"_nat_font_family")]] %||% "Arial"
        )
      })
      
      output[[paste0(prefix,"_pcn_chart_big")]] <- renderPlot({
        df <- pcn_data(); req(df)
        build_pcn_chart(df, input[[paste0(prefix,"_pcn_indicator")]],
                        bar_color   = input[[paste0(prefix,"_nat_color")]]       %||% "steelblue",
                        bar_width   = input[[paste0(prefix,"_nat_col_width")]]   %||% 0.6,
                        label_size  = (input[[paste0(prefix,"_nat_label_size")]] %||% 4) + 2,
                        title_size  = (input[[paste0(prefix,"_nat_title_size")]] %||% 16) + 6,
                        font_family = input[[paste0(prefix,"_nat_font_family")]] %||% "Arial",
                        wrap_width  = 4
        )
      })
      
      observeEvent(input[[paste0(prefix,"_expand_pcn_chart")]], {
        show_expanded_modal(paste0(prefix,"_pcn_chart_big"), "chart", "Expanded PCN Chart")
      })
      
      # ── Subcounty map ───────────────────────────────────
      output[[paste0(prefix,"_pcn_map")]] <- renderGirafe({
        ind  <- input[[paste0(prefix,"_pcn_indicator")]]; req(ind)
        cnty <- input[[paste0(prefix,"_pcn_county")]]
        sub  <- input[[paste0(prefix,"_pcn_subcounty")]]
        
        df <- data_pcn_live()
        if (!is.null(cnty) && cnty != "Kenya") df <- df %>% filter(County_clean    == cnty)
        if (!is.null(sub)  && sub  != "All")   df <- df %>% filter(Subcounty_clean == normalize_name(sub))
        
        shp_filtered <- subc_shp
        if (!is.null(cnty) && cnty != "Kenya") shp_filtered <- shp_filtered %>% filter(County_clean    == cnty)
        if (!is.null(sub)  && sub  != "All")   shp_filtered <- shp_filtered %>% filter(Subcounty_clean == normalize_name(sub))
        
        df_sub <- add_dummy_subcounty_values(
          df %>% group_by(Subcounty_clean) %>%
            summarise(Value=mean(.data[[ind]],na.rm=TRUE),.groups="drop") %>%
            mutate(Value=as.numeric(Value)), "Value", use_dummy = dummy_pcn_map())
        
        map_df <- shp_filtered %>% left_join(df_sub, by="Subcounty_clean")
        
        p <- ggplot(map_df) +
          geom_sf_interactive(aes(
            fill=Value,
            tooltip=paste0("Subcounty: ",Subcounty_raw,"\nPerformance: ",
                           ifelse(is.na(Value),"NA",paste0(round(Value,1),"%"))),
            data_id=Subcounty_clean), color="black", size=0.25) +
          coord_sf(expand=FALSE) +
          scale_fill_gradientn(colours=c("#e41a1c","#ff7f00","#ffff33","#3ff40d","#399c35"),
                               limits=c(0,100), breaks=seq(0,100,20),
                               oob=scales::squish, na.value="#f0f0f0") +
          map_theme() +
          labs(title=paste(ind)) +
          theme(plot.title=element_text(margin=margin(b=25)), plot.margin=margin(20,20,70,20))
        
        if (isTRUE(input[[paste0(prefix,"_show_subcounty_labels")]]))
          p <- p + geom_sf_text(aes(label=Subcounty_raw),
                                size  = input[[paste0(prefix,"_pcn_map_label_size")]]  %||% 5,
                                color = input[[paste0(prefix,"_pcn_map_label_color")]] %||% "black")
        
        p <- p +
          annotation_scale(location="br",style="ticks",width_hint=0.6,
                           text_cex=1.2,tick_height=0.6,line_width=0.8,pad_y=unit(0.1,"cm")) +
          annotation_north_arrow(location="tr",which_north="true",style=north_arrow_orienteering)
        
        if (isTRUE(input[[paste0(prefix,"_show_pcns")]])) {
          pcn_perf <- data_pcn %>% group_by(PCN) %>%
            summarise(Value=mean(.data[[ind]],na.rm=TRUE),.groups="drop")
          pcn_filtered <- pcn_points %>% left_join(pcn_perf, by="PCN")
          if (cnty != "Kenya") pcn_filtered <- pcn_filtered %>% filter(County_clean    == cnty)
          if (sub  != "All")   pcn_filtered <- pcn_filtered %>% filter(Subcounty_clean == normalize_name(sub))
          
          p <- p + geom_sf_interactive(data=pcn_filtered,
                                       aes(tooltip=paste0("PCN: ",PCN,"\nCounty: ",County,"\nSubcounty: ",Subcounty,
                                                          "\nPerformance: ",ifelse(is.na(Value),"NA",paste0(round(Value,1),"%"))),
                                           data_id=PCN),
                                       shape=21, size=4.5, fill="#2C7BE5", color="black", stroke=0.8)
        }
        
        girafe(ggobj=p, width_svg=20, height_svg=18, options=list(
          opts_hover(css="stroke:black;stroke-width:1.5;cursor:pointer;"),
          opts_hover_inv(css="opacity:0.5;"),
          opts_tooltip(css="background-color:black;color:white;padding:8px;border-radius:4px;font-size:12px;"),
          opts_sizing(rescale=TRUE)
        ))
      })
      
      # ── Subcounty chart data reactive ──────────────────
      subcounty_data <- reactive({
        ind   <- input[[paste0(prefix,"_subchart_indicator")]]; req(ind)
        cnty  <- input[[paste0(prefix,"_subchart_county")]]
        sub   <- input[[paste0(prefix,"_subchart_subcounty")]]
        top_n <- input[[paste0(prefix,"_sub_top_n")]] %||% 20
        
        df <- data_pcn_live() %>%
          mutate(County_clean=normalize_name(County), Subcounty_clean=normalize_name(Subcounty))
        if (cnty != "Kenya") df <- df %>% filter(County_clean    == normalize_name(cnty))
        if (sub  != "All")   df <- df %>% filter(Subcounty_clean == normalize_name(sub))
        
        if (nrow(df) == 0)
          return(tibble(Subcounty=character(), Value=numeric(), is_county=logical()))
        
        county_avg <- mean(df[[ind]], na.rm=TRUE)
        if (is.nan(county_avg)) county_avg <- NA_real_
        
        df_top <- df %>% group_by(Subcounty) %>%
          summarise(Value=mean(.data[[ind]],na.rm=TRUE),.groups="drop") %>%
          arrange(desc(Value)) %>% slice_head(n=top_n)
        
        bind_rows(df_top %>% mutate(is_county=FALSE),
                  tibble(Subcounty=cnty, Value=county_avg, is_county=TRUE)) %>%
          arrange(desc(Value))
      })
      
      # ── Shared subcounty chart builder ─────────────────
      build_subcounty_chart <- function(df, ind_label, bar_color, bar_width,
                                        label_size, title_size, font_family,
                                        title_prefix = "Subcounty") {
        df <- df %>% mutate(Subcounty_f=factor(Subcounty, levels=Subcounty),
                            label_text=round(Value,0))
        ggplot(df, aes(x=Subcounty_f, y=Value)) +
          geom_col(aes(fill=ifelse(is_county,"County Avg","Subcounty")), width=bar_width) +
          scale_fill_manual(values=c("County Avg"="red","Subcounty"=bar_color), guide="none") +
          geom_segment(aes(x=0.5, xend=nrow(df)+0.5, y=0, yend=0), linewidth=0.5, color="grey") +
          geom_text(aes(label=label_text), vjust=-0.35, size=label_size, family=font_family) +
          labs(title=paste(ind_label), x=NULL, y="Average (%)") +
          theme(plot.title = element_text(hjust = 0.5)) +
          theme_minimal(base_family=font_family) +
          theme(
            legend.position="none",
            axis.text.y=element_blank(), axis.ticks.y=element_blank(),
            panel.grid=element_blank(),
            panel.background=element_rect(fill="white",color=NA),
            plot.background =element_rect(fill="white",color=NA),
            plot.title =element_text(size=title_size,face="bold",hjust=0.5),
            plot.margin=margin(10,30,5,30),
            axis.text.x=element_text(size=9,angle=45,hjust=1,vjust=1,margin=margin(t=-12))
          ) +
          scale_y_continuous(limits = c(0, 105), expand = expansion(mult = c(0, 0.02)))
      }
      
      output[[paste0(prefix,"_subcounty_chart2")]] <- renderPlot({
        df <- subcounty_data(); req(df)
        build_subcounty_chart(df, input[[paste0(prefix,"_subchart_indicator")]],
                              bar_color   = input[[paste0(prefix,"_sub_color")]]       %||% "steelblue",
                              bar_width   = input[[paste0(prefix,"_sub_col_width")]]   %||% 0.6,
                              label_size  = input[[paste0(prefix,"_sub_label_size")]]  %||% 6,
                              title_size  = input[[paste0(prefix,"_sub_title_size")]]  %||% 20,
                              font_family = input[[paste0(prefix,"_sub_font_family")]] %||% "Arial"
        )
      })
      
      output[[paste0(prefix,"_subcounty_chart_big")]] <- renderPlot({
        df <- subcounty_data(); req(df)
        build_subcounty_chart(df, input[[paste0(prefix,"_subchart_indicator")]],
                              bar_color    = input[[paste0(prefix,"_sub_color")]]       %||% "steelblue",
                              bar_width    = input[[paste0(prefix,"_sub_col_width")]]   %||% 0.6,
                              label_size   = input[[paste0(prefix,"_sub_label_size")]]  %||% 6,
                              title_size   = input[[paste0(prefix,"_sub_title_size")]]  %||% 20,
                              font_family  = input[[paste0(prefix,"_sub_font_family")]] %||% "Arial",
                              title_prefix = "Expanded Subcounty"
        )
      })
      
      observeEvent(input[[paste0(prefix,"_expand_subcounty_chart")]], {
        show_expanded_modal(paste0(prefix,"_subcounty_chart_big"), "chart", "Expanded Subcounty Chart")
      })
      
      output[[paste0(prefix,"_download_subcounty_chart")]] <- downloadHandler(
        filename = function() paste0(prefix,"_subcounty_chart.png"),
        content  = function(file) {
          df <- subcounty_data()
          g  <- build_subcounty_chart(df, input[[paste0(prefix,"_subchart_indicator")]],
                                      bar_color   = input[[paste0(prefix,"_sub_color")]]       %||% "steelblue",
                                      bar_width   = input[[paste0(prefix,"_sub_col_width")]]   %||% 0.6,
                                      label_size  = input[[paste0(prefix,"_sub_label_size")]]  %||% 6,
                                      title_size  = input[[paste0(prefix,"_sub_title_size")]]  %||% 16,
                                      font_family = input[[paste0(prefix,"_sub_font_family")]] %||% "Arial"
          )
          ggsave(file, g, width=10, height=8, dpi=300)
        }
      )
      
    }) # end local
  }   # end PCN loop
  
  
  # ── 15y. Raw data table ──────────────────────────────────
  output$raw_data <- renderDT(datatable(data_pcn_live()))
  
  
} # end server


# ============================================================
# 16. LAUNCH APPLICATION
# ============================================================
ui <- uiOutput("app_ui")
shinyApp(ui, server)