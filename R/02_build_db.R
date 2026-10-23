# Tidy a TRACES snapshot (R/01_extract_traces.R) and write data/europhyt.db.
#
# Tables (one observational unit each):
# Only interceptions of pests identified to species level are kept; the full
# dataset stays in the raw snapshot.
#
#   consignments    one row per CHED-PP certificate with such an interception
#   findings        one row per intercepted pest on one commodity /
#                   wood-packaging line of a consignment
#   finding_commodities
#                   one row per plant species on a finding's commodity line
#                   (TRACES packs several species into one "A|B|C" cell)
#   countries       TRACES country id -> ISO code, name, EU membership
#   refusal_reasons reason code -> label
#   product_types   product type code -> label
#   taxa            EPPO code -> preferred name + GBIF taxonomy, for every
#                   pest and commodity code used in findings
#   snapshot        provenance of this build

library(tidyverse)
library(DBI)

snapshot_dir <- max(list.files("data/raw/traces", pattern = "^\\d{4}-\\d{2}-\\d{2}$", full.names = TRUE))
eppo_db <- "C:/Users/dafl/OneDrive - Folkehelseinstituttet/R/EPPO DB/EPPO_DB.db"
out_db <- "data/europhyt.db"

read_raw <- function(table) {
  read_csv(file.path(snapshot_dir, paste0(table, ".csv")),
           col_types = cols(.default = col_character()))
}

# TRACES uses "/" for not applicable and -1 for unknown ids
na_placeholder <- function(x) if_else(x %in% c("/", "-1", ""), NA_character_, x)
parse_num <- function(x) parse_number(x, locale = locale(decimal_mark = ","))
parse_dmy <- function(x) as.Date(x, format = "%d/%m/%Y")
yes_no <- function(x) if_else(x %in% c("Y", "yes"), TRUE, if_else(x %in% c("N", "no"), FALSE, NA))


# lookups --------------------------------------------------------------------

countries <- bind_rows(
  read_raw("D_NCD_OriginCountry") |>
    select(country_id = NCD_COM_ORIG_CNTRY_ID, iso2 = NCD_OrigCountryISOCode,
           name = NCD_OrigCountryName, eu = NCD_OrigCountry_EU_NonEU),
  read_raw("D_WP_NCD_OriginCountry") |>
    select(country_id = NCD_WP_ORIG_CNTRY_ID, iso2 = NCD_WP_OrigCountryISOCode,
           name = NCD_WP_OrigCountryName, eu = NCD_WP_OrigCountry_EU_NonEU)
) |>
  mutate(across(everything(), na_placeholder)) |>
  filter(!is.na(country_id)) |>
  distinct() |>
  mutate(eu = eu == "EU")
stopifnot(!anyDuplicated(countries$country_id))

refusal_reasons <- read_raw("D_NCD_REFUSAL_REASONS_T") |>
  transmute(reason_code = NCD_RR_REASN_ID, reason = NCD_RR_REASN_T)

product_types <- read_raw("L_NCD_COM_PRDCT_TYPE_DECOMP_T") |>
  transmute(product_type_code = na_placeholder(NCD_COM_PRDCT_TYPE_ID),
            product_type = na_placeholder(NCD_COM_ProductType)) |>
  filter(!is.na(product_type_code))

wood_packaging_types <- read_raw("L_NCD_WP_COM_DECOMP") |>
  transmute(wp_type_id = na_placeholder(NCD_RR_WOOD_PKGNG_COM_ID_L),
            wood_packaging_type = na_placeholder(NCD_RR_WOOD_PKGNG_NOMEN_DESCR_L_T)) |>
  filter(!is.na(wp_type_id))

country_iso <- \(id) countries$iso2[match(id, countries$country_id)]


# consignments ---------------------------------------------------------------

consignments <- read_raw("F_CHEDPP") |>
  transmute(
    ched_id = as.integer(NCD_RR_ID),
    ched_ref = NCD_CERT_REF,
    member_state = str_split_i(ched_ref, fixed("."), 2),
    status = Status,
    declaration_date = parse_dmy(DeclarationDate),
    decision_date = parse_dmy(DecisionDate)
  ) |>
  arrange(ched_id)
stopifnot(!anyDuplicated(consignments$ched_id))


# findings -------------------------------------------------------------------

findings <- read_raw("F_NON_COMPLC_DET") |>
  mutate(across(everything(), na_placeholder)) |>
  transmute(
    ched_id = as.integer(NCD_RR_ID),
    line_key = NCD_OCCURRENCE_KEY,
    item = str_to_lower(NCD_TYPE),
    inspection_date = parse_dmy(NCD_InspectionDate),
    control_point = PlaceInsp_ID,
    reason_code = NCD_RR_REASN_ID,
    harmful_organism = yes_no(NCD_RR_HarmfulOrganism),
    pest_eppo = NCD_RR_SPE_EPPO_CODE,
    pest_name = NCD_RR_SPE_SCIENTFC_NAME,
    extent = NCD_RR_EXTENT_ID,
    pest_not_identified_reason = NCD_RR_NON_EU_HARMFL_ORGSM_ID,
    pest_not_species_reason = NCD_RR_NON_SPE_HARMFL_ORGSM_ID,
    origin_iso2 = country_iso(NCD_COM_ORIG_CNTRY_ID_L),
    dispatch_iso2 = country_iso(NCD_COM_CNTRY_DISPATCH_ID),
    commodity_eppo = NCD_COM_SPE_EPPO_CODE_L,
    commodity_name = NCD_RR_Species,
    cn_code = NCD_RR_COM_NOMEN_CODE_L,
    commodity_group = str_remove(NCD_RR_COM_COM_GRP_ID_L, "^PLANT_PRODUCT#") |> na_placeholder(),
    product_type_code = NCD_COM_PRDCT_TYPE_ID_L,
    n_packages = parse_num(NCD_COM_NBR_PKG),
    quantity = parse_num(NCD_COM_QTY),
    net_weight_kg = parse_num(NCD_COM_NET_WEIGHT),
    gross_weight_kg = parse_num(NCD_COM_TOT_GROSS_WEIGHT),
    wp_origin_iso2 = country_iso(NCD_WP_ORIG_CNTRY_ID_L),
    wp_type_id = NCD_RR_WOOD_PKGNG_COM_ID_L,
    wp_cn_code = NCD_RR_WOOD_PKGNG_NOMEN_CODE_L,
    wp_eppo = NCD_RR_WOOD_PKGNG_SPE_EPPO_CODE_L,
    wp_name = NCD_RR_WOOD_PKGNG_Species,
    wp_n_packages = parse_num(NCD_WOOD_PKGNG_NBR_PKG),
    wp_net_weight_kg = parse_num(NCD_WP_NET_WEIGHT),
    monitoring_post_entry = yes_no(NCD_Monitorinsppostentry),
    monitoring_with_symptoms = yes_no(NCD_Monitorwithsymptomsorsuspicion),
    monitoring_without_symptoms = yes_no(NCD_Monitorwithoutsymptomsorsuspicion),
    comment = NCD_RR_COMNT
  ) |>
  left_join(wood_packaging_types, by = "wp_type_id") |>
  select(-wp_type_id) |>
  relocate(wood_packaging_type, .after = wp_cn_code) |>
  arrange(ched_id, line_key, reason_code, pest_eppo) |>
  mutate(finding_id = row_number(), .before = 1)

stopifnot(all(na.omit(findings$reason_code) %in% refusal_reasons$reason_code))

# keep only interceptions of a pest identified to species (or below, e.g.
# pathovars and viruses); EPPO codes starting with "1" are genus or higher
findings <- findings |>
  filter(harmful_organism, !is.na(pest_eppo), !str_starts(pest_eppo, "1")) |>
  select(-harmful_organism, -pest_not_species_reason) |>
  mutate(finding_id = row_number())
consignments <- semi_join(consignments, findings, by = "ched_id")
orphans <- sum(!findings$ched_id %in% consignments$ched_id)
if (orphans) warning(orphans, " finding(s) have no consignment in F_CHEDPP")

# split "CODE1|CODE2 (A)" species lists into one row per species; the "(A)"
# qualifier is kept as published (it mostly marks plants for planting)
finding_commodities <- findings |>
  select(finding_id, commodity_eppo, commodity_name) |>
  filter(!is.na(commodity_eppo)) |>
  mutate(n_codes = str_count(commodity_eppo, fixed("|")),
         n_names = str_count(commodity_name, fixed("|")))
stopifnot(with(finding_commodities, all(n_codes == n_names, na.rm = TRUE)))
finding_commodities <- finding_commodities |>
  separate_longer_delim(c(commodity_eppo, commodity_name), delim = "|") |>
  transmute(
    finding_id,
    eppocode = str_remove(commodity_eppo, " \\(.*\\)$"),
    commodity_name = str_remove(commodity_name, " \\(.*\\)$"),
    qualifier = str_extract(commodity_eppo, "(?<=\\()[^)]+(?=\\)$)")
  )
findings <- select(findings, -commodity_eppo, -commodity_name)


# taxa: EPPO preferred names + GBIF taxonomy, joined on EPPO code ------------

codes <- unique(na.omit(c(findings$pest_eppo, finding_commodities$eppocode, findings$wp_eppo)))

eppo <- dbConnect(RSQLite::SQLite(), eppo_db, flags = RSQLite::SQLITE_RO)
eppo_names <- tbl(eppo, "EPPO") |>
  filter(eppocode %in% codes, codelang == "la", preferred == 1) |>
  select(eppocode, eppo_name = fullname, eppo_type = libtype, eppo_status = status) |>
  collect() |>
  distinct(eppocode, .keep_all = TRUE)
gbif <- tbl(eppo, "EPPO_GBIF_TAX") |>
  filter(original_sciname %in% !!eppo_names$eppo_name) |>
  select(original_sciname, synonym, confidence, rank, kingdom, phylum, class,
         order, family, genus, species) |>
  collect() |>
  arrange(original_sciname, synonym, desc(confidence)) |>
  distinct(original_sciname, .keep_all = TRUE) |>
  select(-synonym, -confidence)
dbDisconnect(eppo)

taxa <- tibble(eppocode = codes) |>
  left_join(eppo_names, by = "eppocode") |>
  left_join(gbif, by = c("eppo_name" = "original_sciname")) |>
  rename(gbif_rank = rank) |>
  arrange(eppocode)


# write ----------------------------------------------------------------------

snapshot <- read_raw("LAST_LOAD_TNT") |>
  transmute(source = "TRACES plant interceptions dashboard (DG SANTE, Qlik app 537e71fa)",
            extracted_to = parse_dmy(EXTRACTED_TO_DATE),
            built_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

db_tables <- list(
  consignments = consignments, findings = findings,
  finding_commodities = finding_commodities, countries = countries,
  refusal_reasons = refusal_reasons, product_types = product_types,
  taxa = taxa, snapshot = snapshot
)

if (file.exists(out_db)) file.remove(out_db)
con <- dbConnect(RSQLite::SQLite(), out_db, extended_types = TRUE)
iwalk(db_tables, \(df, name) dbWriteTable(con, name, df))
dbExecute(con, "CREATE INDEX idx_findings_ched ON findings(ched_id)")
dbExecute(con, "CREATE INDEX idx_findings_pest ON findings(pest_eppo)")
dbExecute(con, "CREATE INDEX idx_findings_origin ON findings(origin_iso2)")
dbExecute(con, "CREATE INDEX idx_commodities_finding ON finding_commodities(finding_id)")
dbExecute(con, "CREATE INDEX idx_commodities_eppo ON finding_commodities(eppocode)")
dbDisconnect(con)

message("Wrote ", out_db, ": ",
        paste0(names(db_tables), " (", map_int(db_tables, nrow), ")", collapse = ", "))
