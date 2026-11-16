# Changelog

## econdatasets (development version)

## econdatasets 0.1.1

CRAN release: 2026-11-15

### Documentation

- New article “How Accurate Are IMF Growth Forecasts?” analysing
  forecast errors across World Economic Outlook vintages using the
  `imfweo` dataset ([@fritzoc](https://github.com/fritzoc),
  [\#8](https://github.com/econdataverse/r-econdatasets/issues/8)).
- New article “Who Still Lends to Developing Countries?” on net debt
  flows by creditor type using the `wbids` dataset
  ([\#10](https://github.com/econdataverse/r-econdatasets/issues/10)).

### Datasets

- New `wbwdi` dataset with World Development Indicators entities and
  series.
- `wbids` dataset refreshed from the December 2025 World Bank
  International Debt Statistics bulk files.
- `imfweo` dataset now keeps legacy vintages (2007 to Spring 2025) and
  only appends newer World Economic Outlook releases.

### Other changes

- The package moved to the `econdataverse` GitHub organization. The
  website is now at <https://econdataverse.github.io/r-econdatasets/>
  and issues should be filed at
  <https://github.com/econdataverse/r-econdatasets/issues>.
- Expanded unit tests to cover error handling when the Hugging Face API
  returns invalid responses
  ([\#4](https://github.com/econdataverse/r-econdatasets/issues/4)).

## econdatasets 0.1.0

CRAN release: 2025-11-21

- Initial CRAN submission with
  [`ed_get()`](https://econdataverse.github.io/r-econdatasets/reference/ed_get.md),
  [`ed_get_tables()`](https://econdataverse.github.io/r-econdatasets/reference/ed_get_tables.md),
  and
  [`ed_get_datasets()`](https://econdataverse.github.io/r-econdatasets/reference/ed_get_datasets.md).
