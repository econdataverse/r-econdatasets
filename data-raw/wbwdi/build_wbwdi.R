library(tidyverse)
library(wbwdi)
library(arrow)
library(furrr)

plan(multisession, workers = availableCores() - 1)

dir.create("data/wbwdi", recursive = TRUE, showWarnings = FALSE)
dir.create("data-raw/wbwdi/series", recursive = TRUE, showWarnings = FALSE)

entities <- wdi_get_entities()
write_parquet(entities, "data/wbwdi/entities.parquet")

# Download and store indicators
indicators <- wdi_get_indicators()

download_indicator <- function(indicator, total, monthly = FALSE) {
  file_path <- paste0("data-raw/wbwdi/series/", indicator, ".parquet")

  if (file.exists(file_path)) {
    return(list(
      indicator = indicator,
      status = "skipped",
      error_message = NA_character_
    ))
  }

  tryCatch(
    {
      ind <- wdi_get("all", indicator) |>
        dplyr::rename(series_id = "indicator_id")
      write_parquet(ind, file_path)
      list(
        indicator = indicator,
        status = "success",
        error_message = NA_character_
      )
    },
    error = function(e) {
      list(
        indicator = indicator,
        status = "error",
        error_message = conditionMessage(e)
      )
    }
  )
}

# Download and store WDI Annual Data
results <- future_map(
  indicators$indicator_id,
  \(x) download_indicator(x, nrow(indicators)),
  .options = furrr_options(seed = TRUE),
  .progress = TRUE
)

results_df <- bind_rows(results)

message("\n=== Download Summary ===")
message("Total indicators: ", nrow(results_df))
message("Successful: ", sum(results_df$status == "success"))
message("Skipped (already existed): ", sum(results_df$status == "skipped"))
message("Errors: ", sum(results_df$status == "error"))

errors_df <- results_df |> filter(status == "error")
if (nrow(errors_df) > 0) {
  message("\n=== Errors ===")
  print(errors_df)
}

errors_df |> count(error_message)

# Remove indicators that did not exist
indicators <- indicators |>
  filter(!indicator_id %in% errors_df$indicator)

topics <- indicators |>
  select(series_id = indicator_id, topics) |>
  unnest(topics)
write_parquet(topics, "data/wbwdi/topics.parquet")

series <- indicators |>
  select(-topics) |>
  rename(series_id = indicator_id, series_name = indicator_name)

write_parquet(series, "data/wbwdi/series.parquet")

# Create table with annual & quertly indicators
files_annual <- list.files(
  "data-raw/wbwdi/series",
  pattern = ".parquet",
  full.names = TRUE
)

indicators_quarterly <- files_annual |>
  keep(~ "quarter" %in% names(read_parquet(.x, n_rows = 0))) |>
  map_df(read_parquet)

indicators_quarterly |>
  drop_na() |>
  write_parquet("data/wbwdi/indicators_quarterly.parquet")

indicators_annual <- files_annual |>
  discard(~ "quarter" %in% names(read_parquet(.x, n_rows = 0))) |>
  map_df(read_parquet)

indicators_annual |>
  drop_na() |>
  write_parquet("data/wbwdi/indicators_annual.parquet")

# Download monthly data --------------------------------------------------
# TODO: update the above logic to also try fetching monthly data
