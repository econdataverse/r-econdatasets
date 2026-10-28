library(tidyverse)
library(wbids)
library(arrow)

dir.create("data/wbids", recursive = TRUE, showWarnings = FALSE)

series <- ids_list_series()
write_parquet(series, "data/wbids/series.parquet")

counterparts <- ids_list_counterparts()
write_parquet(counterparts, "data/wbids/counterparts.parquet")

entities <- wbids::ids_list_entities()
write_parquet(entities, "data/wbids/entities.parquet")

# Source: https://datacatalog.worldbank.org/search/dataset/0038015

base_url <- "https://datacatalogfiles.worldbank.org/ddh-published/0038015/"

files <- tribble(
  ~file_name, ~file_url,
  "Bulk Download File - Debtor Countries: A to D, Counterpart-Area: All (XLSX)",
  paste0(base_url, "DR0092201/A_D.xlsx"),
  "Bulk Download File - Debtor Countries: E to K, Counterpart-Area: All (XLSX)",
  paste0(base_url, "DR0092202/E_K.xlsx"),
  "Bulk Download File - Debtor Countries: L to M, Counterpart-Area: All (XLSX)",
  paste0(base_url, "DR0092203/L_M.xlsx"),
  "Bulk Download File - Debtor Countries: N to Q, Counterpart-Area: All (XLSX)",
  paste0(base_url, "DR0093225/N_Q.xlsx"),
  "Bulk Download File - Debtor Countries: R to U, Counterpart-Area: All (XLSX)",
  paste0(base_url, "DR0092204/R_U.xlsx"),
  "Bulk Download File - Debtor Countries: V to Z, Counterpart-Area: All (XLSX)",
  paste0(base_url, "DR0092205/V_Z.xlsx")
) |>
  mutate(last_updated_date = as.Date("2025-12-05"))

for (j in seq_len(nrow(files))) {
  bulk <- ids_bulk(files$file_url[j], warn_size = FALSE, timeout = 200)
  write_parquet(bulk, paste0("data-raw/wbids/wbids_bulk_", j, ".parquet"))
  message(j, " done!")
}

bulk_files <- list.files(
  "data-raw/wbids",
  full.names = TRUE,
  pattern = ".parquet"
)

debt_statistics <- bulk_files |>
  map_df(read_parquet)

write_parquet(debt_statistics, "data/wbids/debt_statistics.parquet")
