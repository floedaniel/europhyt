# Update data/europhyt.db from the TRACES plant-interceptions dashboard.
#
#   Rscript R/update.R      (from the project root)
#
# 1. snapshots the dashboard into data/raw/traces/<extracted-to-date>/,
#    skipped when that date is already downloaded
# 2. rebuilds data/europhyt.db from the newest snapshot

source("R/01_extract_traces.R", local = new.env())
source("R/02_build_db.R", local = new.env())
