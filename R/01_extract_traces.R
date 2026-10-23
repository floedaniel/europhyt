# Snapshot every table of the public TRACES plant-interceptions dashboard
# into data/raw/traces/<extracted-to-date>/ as untouched CSVs.
# Skipped when the dashboard's current extraction date is already downloaded.

library(readr)
library(purrr)
source("R/qlik.R")

con <- qlik_connect()
tables <- qlik_tables(con)

ll <- tables[tables$table == "LAST_LOAD_TNT", ]
last_load <- qlik_table_data(con, ll$fields[[1]], ll$is_key[[1]], ll$n_non_null[[1]], ll$n_rows)
extracted_to <- as.Date(last_load$EXTRACTED_TO_DATE, format = "%d/%m/%Y")
stopifnot(!is.na(extracted_to))
out_dir <- file.path("data", "raw", "traces", format(extracted_to))

if (all(file.exists(file.path(out_dir, paste0(tables$table, ".csv"))))) {
  message("Snapshot ", out_dir, " already exists; the dashboard has no newer data")
} else {
  tmp_dir <- paste0(out_dir, "_partial")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  pwalk(tables, function(table, n_rows, fields, is_key, n_non_null) {
    df <- qlik_table_data(con, fields, is_key, n_non_null, n_rows)
    message(table, ": ", nrow(df), " / ", n_rows, " rows")
    if (nrow(df) != n_rows) warning(table, ": hypercube returned ", nrow(df), " rows, table has ", n_rows)
    write_csv(df, file.path(tmp_dir, paste0(table, ".csv")), na = "")
  })
  # only a complete download becomes a snapshot
  unlink(out_dir, recursive = TRUE)
  file.rename(tmp_dir, out_dir)
  message("Snapshot written to ", out_dir)
}

con$browser$close()
