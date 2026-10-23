# europhyt

EU plant-pest interceptions from TRACES, cleaned into a tidy SQLite database
and explorable in a Shiny app.

**Live app:** <https://01a167d0-a818-d97a-4d91-2939ac495358.share.connect.posit.cloud/>

## Data

The source is DG SANTE's public
[TRACES plant interceptions dashboard](https://dashboard.tech.ec.europa.eu/qs_digit_dashboard_mt/public/single/?appid=537e71fa-93ba-4ad7-85f7-6a00fa611e79&sheet=b7eeb89e-8326-403b-9f23-ef88944b54ad),
which holds record-level non-compliance notifications on plants, plant
products and wood packaging imported into the EU and Switzerland, from
February 2021 onwards. It has no download or API, so the pipeline reads the
dashboard's underlying Qlik data model directly.

Older EUROPHYT data (2005–2021) exists only as PDF/xlsx summaries on the
[EUROPHYT interceptions page](https://food.ec.europa.eu/plants/plant-health-and-biosecurity/europhyt/interceptions_en);
the [DG SANTE datalake API](https://developer.datalake.sante.service.ec.europa.eu/)
has no plant-health interception data.

The database keeps only **interceptions of pests identified to species level**
(or below, e.g. pathovars and viruses). Taxonomy comes from a local EPPO
database (EPPO preferred names + GBIF classification), joined on EPPO code.

## Usage

```r
# requires Chrome (via chromote) and the EPPO database at the path in R/02_build_db.R
Rscript R/update.R    # snapshot the dashboard (if newer) and rebuild data/europhyt.db
shiny::runApp()       # explore
```

| Script | Does |
|---|---|
| `R/update.R` | runs the two steps below |
| `R/01_extract_traces.R` | saves every dashboard table as CSV in `data/raw/traces/<extraction date>/`; skipped when that date is already downloaded |
| `R/02_build_db.R` | tidies the newest snapshot into `data/europhyt.db` |
| `R/qlik.R` | minimal Qlik Engine client run inside headless Chrome |
| `app.R` | Shiny explorer |

`data/` is not in git; `R/update.R` recreates it.

## Database (`data/europhyt.db`)

| Table | One row per |
|---|---|
| `findings` | intercepted pest on one commodity or wood-packaging line |
| `consignments` | CHED-PP certificate with at least one such interception |
| `finding_commodities` | plant species on a finding's commodity line (TRACES packs several into one cell) |
| `taxa` | EPPO code with preferred name and GBIF taxonomy |
| `countries`, `refusal_reasons`, `product_types` | lookup tables |
| `snapshot` | source and extraction date |

Join `findings` to `consignments` on `ched_id`, to `finding_commodities` on
`finding_id`, and to `taxa` on `pest_eppo = eppocode`. Logical columns are
stored as 0/1 (SQLite has no boolean type). TRACES placeholders (`/`, `-1`)
are `NA`.

## App

Search by species name or EPPO code (pest, commodity or both; several terms
separated by commas), filter by year, origin, intercepting member state and
item type, and view the table (with CSV/Excel export), monthly trend, top
pests/origins/commodities, and a per-pest summary.

To redeploy to Posit Connect Cloud:

```r
rsconnect::deployApp(appFiles = c("app.R", "data/europhyt.db"),
                     appName = "eu-plant-pest-interceptions",
                     account = "danielflow", server = "connect.posit.cloud")
```

## Caveats

- The extraction depends on the dashboard's internal data model and may break
  if DG SANTE changes it; each snapshot is kept so past builds stay reproducible.
- 2021 is incomplete (data starts 5 February 2021). Consignments without a
  decision yet are dated by their declaration date in the app.
- The meaning of the `(A)` qualifier on some TRACES commodity codes is not
  documented; it is kept in `finding_commodities.qualifier`.
