library(tidyverse)
library(imfweo)
library(arrow)

# Legacy releases (2007 to 2025 Spring) can no longer be downloaded via imfweo
# (https://github.com/Teal-Insights/r-imfweo/issues/22), so we start from the
# vintages already published on Hugging Face and only append newer ones.

dir.create("data/imfweo", recursive = TRUE, showWarnings = FALSE)

hf_url <- "https://huggingface.co/datasets/econdataverse/imfweo/resolve/main"

economic_outlook_hf <- read_parquet(
  paste0(hf_url, "/economic_outlook.parquet")
)

# Spring releases come before Fall releases within a year
vintage_key <- function(year, release) {
  2L * year + dplyr::if_else(release == "Fall", 1L, 0L)
}

latest_key <- economic_outlook_hf |>
  distinct(publication_year, publication_release) |>
  mutate(key = vintage_key(publication_year, publication_release)) |>
  pull(key) |>
  max()

# Only consider vintages released after the latest one on Hugging Face, so
# that we never retry legacy releases
publications <- weo_list_publications() |>
  mutate(key = vintage_key(year, release)) |>
  filter(key > latest_key) |>
  arrange(key)

cat(
  "Latest vintage on Hugging Face:",
  latest_key %/% 2L,
  if_else(latest_key %% 2L == 1L, "Fall", "Spring"),
  "\n"
)
cat("New vintages to download:", nrow(publications), "\n")

error_log <- list()
for (j in seq_len(nrow(publications))) {
  year <- publications$year[j]
  release <- publications$release[j]

  cat("Processing", j, "-", year, release, "...\n")

  tryCatch(
    {
      weo_raw <- weo_get(
        year = year,
        release = release
      ) |>
        mutate(
          publication_year = .env$year,
          publication_release = .env$release
        )

      write_parquet(
        weo_raw,
        paste0("data-raw/imfweo/weo_", release, "_", year, ".parquet")
      )

      cat(year, release, "downloaded successfully!\n")
    },
    error = function(e) {
      error_log[[length(error_log) + 1]] <<- data.frame(
        j = j,
        year = year,
        release = release,
        error_message = conditionMessage(e),
        stringsAsFactors = FALSE
      )

      cat("❌ Error at", year, release, ":", conditionMessage(e), "\n")
    }
  )
}

if (length(error_log) > 0) {
  error_table <- bind_rows(error_log)
  write_csv(error_table, "data-raw/imfweo/weo_error_log.csv")
} else {
  cat("✅ All downloads completed successfully!\n")
}

new_files <- paste0(
  "data-raw/imfweo/weo_",
  publications$release,
  "_",
  publications$year,
  ".parquet"
)
new_files <- new_files[file.exists(new_files)]

economic_outlook_new <- new_files |>
  map_df(read_parquet) |>
  mutate(
    entity_id = as.character(entity_id),
    year = as.integer(year),
    value = as.numeric(value),
    publication_year = as.integer(publication_year)
  ) |>
  select(all_of(names(economic_outlook_hf)))

economic_outlook <- bind_rows(economic_outlook_hf, economic_outlook_new) |>
  distinct()

entities <- economic_outlook |>
  distinct(entity_id, entity_name, publication_year, publication_release)

series <- economic_outlook |>
  distinct(series_id, series_name, units, publication_year, publication_release)

write_parquet(economic_outlook, "data/imfweo/economic_outlook.parquet")
write_parquet(entities, "data/imfweo/entities.parquet")
write_parquet(series, "data/imfweo/series.parquet")
