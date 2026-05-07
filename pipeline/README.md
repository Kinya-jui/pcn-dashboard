# PCN Dashboard Data Pipeline

## Architecture

```
KoboToolbox Tool 1                KoboToolbox Tool 2
(Facility Assessment)             (PCN / County Monitoring)
        │                                   │
        │  01_pull_tool1_from_kobo.R        │
        │  ← pulls all submissions          │
        ↓                                   │
consolidated_facility_data.csv              │
        │                                   │
        │  02_analyse_facility_data.R        │
        │  ← aggregates to PCN level         │
        ↓                                   │
pcn_aggregated_indicators.csv               │
        │                                   │
        │  03_push_to_tool2.R               │
        └──────────── POST ────────────────►│
                                            │
                                  Shiny Dashboard
                                  ← polls every 5 min
                                  via REST API
```

**Runs automatically at 01:00 EAT nightly via GitHub Actions.**

---

## Setup

### 1. Repository Structure

```
your-repo/
├── pipeline/
│   ├── 01_pull_tool1_from_kobo.R
│   ├── 02_analyse_facility_data.R
│   ├── 03_push_to_tool2.R
│   └── 04_shiny_kobo_connection.R   ← reference for app.R changes
├── dashboard/
│   └── app.R
├── .github/
│   └── workflows/
│       └── nightly_pipeline.yml
└── README.md
```

### 2. Get Your KoboToolbox Credentials

| What | Where to get it |
|------|----------------|
| **API Token** | KoboToolbox → Account → Security → API Key |
| **Tool 1 Asset UID** | URL of facility assessment form: `.../assets/`**`aXXXXXXXX`**`/` |
| **Tool 2 Asset UID** | URL of PCN monitoring form: `.../assets/`**`aYYYYYYYY`**`/` |

### 3. Add GitHub Secrets

Go to: **GitHub repo → Settings → Secrets and variables → Actions → New repository secret**

Add these three:
- `KOBO_TOKEN` — your KoboToolbox API token
- `KOBO_TOOL1_UID` — asset UID of the Facility Assessment tool
- `KOBO_TOOL2_UID` — asset UID of the PCN Monitoring tool

### 4. Set Up Local `.Renviron` (for running the Shiny app locally)

Create `.Renviron` in your Shiny project root:

```
KOBO_TOKEN=your_api_token_here
KOBO_TOOL2_UID=your_tool2_asset_uid_here
```

Then restart R for it to take effect.

### 5. Update app.R

Replace sections **04–06** of `app.R` with the code from `04_shiny_kobo_connection.R`.

Key changes to make inside `server()`:

```r
# Add inside server() near the top
kobo_timer <- reactiveTimer(300000)  # refresh every 5 minutes

data_pcn_live <- reactive({
  kobo_timer()
  raw <- fetch_kobo_all(KOBO_TOOL2_UID, KOBO_TOKEN)
  clean_kobo_data(raw) %>%
    filter(is.na(tool_selection) | tool_selection == "pcn_tool")
})

data_county_live <- reactive({
  kobo_timer()
  raw <- fetch_kobo_all(KOBO_TOOL2_UID, KOBO_TOKEN)
  clean_kobo_data(raw) %>%
    filter(tool_selection == "county_tool")
})
```

Then do a find-and-replace throughout `server()`:
- `data_pcn` → `data_pcn_live()`
- `data_county` → `data_county_live()`

---

## Field Mapping Reference

The analysis script maps facility-level Tool 1 questions to PCN-level Tool 2 fields:

| Tool 1 Question | Aggregation | Tool 2 Field |
|---|---|---|
| `q3` (SHA empaneled) | % yes per PCN | `hfs_empaneled_sha` |
| `q4` (SHIF use %) | mean | `clients_access_shif` |
| `q5` (claims reimbursed) | mean | `claims_reimbursed_hfs` |
| `q8` (FIF rollback) | mean | `fif_collected_rollback` |
| `q28` (accessible road) | % yes | `hfs_accessible_road` |
| `q30` (internet) | % yes | `hfs_reliable_internet` |
| `q31` (EMR) | % yes | `hfs_integrated_emr` |
| `q32` (power) | % yes | `hfs_reliable_power` |
| `q22_1:q22_23` (pharma checklist) | mean % available | `facilities_22pharma_avail` |
| `q39` (QIT) | % yes | `hospitals_qit_functional` |
| `q40` (WIT) | % yes | `spokes_wit_functional` |
| `q9_a` (total HCW) | sum | `hrhpop` |
| `q12` (staff present) | mean | `absenteesm_phc_facilities` |

---

## Troubleshooting

**Dashboard shows no data after connection:**
- Check KoboToolbox → your form → Submissions to confirm data exists
- Confirm Asset UID matches the form URL exactly
- Test the API manually: `curl -H "Authorization: Token YOUR_TOKEN" https://kf.kobotoolbox.org/api/v2/assets/YOUR_UID/data/?format=json`

**GitHub Action fails at push step:**
- Open the failed run → expand "Push aggregated indicators" step
- Look for HTTP status codes: 400 = bad field names, 401 = bad token, 403 = wrong UID

**Column name mismatch warnings:**
- Download your Tool 2 XLS form from KoboToolbox
- Compare the `name` column in the `survey` sheet against `KOBO_TO_DASHBOARD` in `04_shiny_kobo_connection.R`
- Add any missing mappings
