library(testthat)

test_that("ed_get works with valid dataset and table", {
  test_df <- data.frame(
    counterpart_id = 1:3,
    counterpart_name = c("A", "B", "C"),
    value = c(10, 20, 30)
  )

  local_mocked_bindings(
    read_parquet = function(file_url, col_select = NULL) {
      if (!is.null(col_select)) {
        return(test_df[, col_select, drop = FALSE])
      }
      test_df
    },
    .package = "arrow"
  )

  result <- ed_get("wbids", "counterparts", quiet = TRUE)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 3)
})

test_that("ed_get works with column selection", {
  test_df <- data.frame(
    counterpart_id = 1:3,
    counterpart_name = c("A", "B", "C"),
    value = c(10, 20, 30)
  )

  local_mocked_bindings(
    read_parquet = function(file_url, col_select = NULL) {
      if (!is.null(col_select)) {
        return(test_df[, col_select, drop = FALSE])
      }
      test_df
    },
    .package = "arrow"
  )

  result <- ed_get(
    "wbids",
    "counterparts",
    columns = c("counterpart_id", "counterpart_name"),
    quiet = TRUE
  )

  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 2)
  expect_true(all(c("counterpart_id", "counterpart_name") %in% names(result)))
})

test_that("ed_get shows messages when quiet = FALSE", {
  test_df <- data.frame(x = 1:3)

  local_mocked_bindings(
    read_parquet = function(file_url, col_select = NULL) {
      test_df
    },
    .package = "arrow"
  )

  expect_message(
    ed_get("wbids", "counterparts", quiet = FALSE),
    "Reading dataset from"
  )

  expect_message(
    ed_get("wbids", "counterparts", quiet = FALSE),
    "Successfully loaded"
  )
})

test_that("ed_get handles errors gracefully", {
  local_mocked_bindings(
    read_parquet = function(file_url, col_select = NULL) {
      stop("Connection failed")
    },
    .package = "arrow"
  )

  expect_message(
    ed_get("wbids", "counterparts", quiet = TRUE),
    "Failed to read dataset"
  )
})

test_that("ed_get constructs correct URL", {
  captured_url <- NULL

  local_mocked_bindings(
    read_parquet = function(file_url, col_select = NULL) {
      captured_url <<- file_url
      data.frame(x = 1)
    },
    .package = "arrow"
  )

  ed_get("wbids", "counterparts", quiet = TRUE)

  expect_equal(
    captured_url,
    paste0(
      "https://huggingface.co/datasets/econdataverse/wbids/",
      "resolve/main/counterparts.parquet"
    )
  )
})

# ============================================================================
# Tests for ed_get_datasets()
# ============================================================================

test_that("ed_get_datasets returns valid data frame", {
  # Mock the HTTP response
  mock_response <- list(
    id = c("econdataverse/wbids", "econdataverse/imfifs"),
    downloads = c(1000, 500),
    lastModified = c("2024-01-01T00:00:00.000Z", "2024-01-02T00:00:00.000Z"),
    private = c(FALSE, FALSE),
    gated = c(FALSE, FALSE)
  )

  mock_json_string <- jsonlite::toJSON(mock_response, auto_unbox = FALSE)

  local_mocked_bindings(
    request = function(url) {
      structure(list(url = url), class = "httr2_request")
    },
    req_perform = function(req) {
      structure(
        list(
          body = charToRaw(as.character(mock_json_string))
        ),
        class = "httr2_response"
      )
    },
    resp_body_string = function(resp) {
      rawToChar(resp$body)
    },
    .package = "httr2"
  )

  result <- ed_get_datasets(quiet = TRUE)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_true(all(
    c(
      "dataset",
      "total_downloads",
      "last_modified",
      "is_private",
      "is_gated"
    ) %in%
      names(result)
  ))
  expect_equal(result$dataset, c("econdataverse/wbids", "econdataverse/imfifs"))
})

test_that("ed_get_datasets shows messages when quiet = FALSE", {
  mock_response <- list(
    id = c("econdataverse/test"),
    downloads = c(100),
    lastModified = c("2024-01-01T00:00:00.000Z"),
    private = c(FALSE),
    gated = c(FALSE)
  )

  mock_json_string <- jsonlite::toJSON(mock_response, auto_unbox = FALSE)

  local_mocked_bindings(
    request = function(url) {
      structure(list(url = url), class = "httr2_request")
    },
    req_perform = function(req) {
      structure(
        list(body = charToRaw(as.character(mock_json_string))),
        class = "httr2_response"
      )
    },
    resp_body_string = function(resp) {
      rawToChar(resp$body)
    },
    .package = "httr2"
  )

  expect_message(
    ed_get_datasets(quiet = FALSE),
    "Fetching dataset list from"
  )

  expect_message(
    ed_get_datasets(quiet = FALSE),
    "Found 1 datasets"
  )
})

test_that("ed_get_datasets handles HTTP errors", {
  local_mocked_bindings(
    request = function(url) {
      structure(list(url = url), class = "httr2_request")
    },
    req_perform = function(req) {
      stop("Network error")
    },
    .package = "httr2"
  )

  expect_message(
    ed_get_datasets(quiet = TRUE),
    "Failed to retrieve dataset list"
  )
})

# ============================================================================
# Tests for ed_get_tables()
# ============================================================================

test_that("ed_get_tables returns valid data frame with parquet files", {
  mock_response <- list(
    tree = data.frame(
      path = c("counterparts.parquet", "indicators.parquet", "readme.md"),
      type = c("file", "file", "file"),
      size = c(1024, 2048, 512),
      stringsAsFactors = FALSE
    )
  )

  mock_json_string <- jsonlite::toJSON(mock_response, auto_unbox = FALSE)

  local_mocked_bindings(
    request = function(url) {
      structure(list(url = url), class = "httr2_request")
    },
    req_perform = function(req) {
      structure(
        list(body = charToRaw(as.character(mock_json_string))),
        class = "httr2_response"
      )
    },
    resp_body_string = function(resp) {
      rawToChar(resp$body)
    },
    .package = "httr2"
  )

  result <- ed_get_tables("wbids", quiet = TRUE)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2) # Only parquet files
  expect_true(all(
    c("table", "filename", "path", "size", "url") %in% names(result)
  ))
  expect_equal(result$table, c("counterparts", "indicators"))
  expect_true(all(grepl("resolve/main", result$url)))
})

test_that("ed_get_tables handles data frame response (no tree element)", {
  mock_response <- data.frame(
    path = c("data.parquet"),
    type = c("file"),
    size = c(1024),
    stringsAsFactors = FALSE
  )

  mock_json_string <- jsonlite::toJSON(mock_response, auto_unbox = FALSE)

  local_mocked_bindings(
    request = function(url) {
      structure(list(url = url), class = "httr2_request")
    },
    req_perform = function(req) {
      structure(
        list(body = charToRaw(as.character(mock_json_string))),
        class = "httr2_response"
      )
    },
    resp_body_string = function(resp) {
      rawToChar(resp$body)
    },
    .package = "httr2"
  )

  result <- ed_get_tables("wbids", quiet = TRUE)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
})

test_that("ed_get_tables returns empty data frame when no parquet files", {
  mock_response <- list(
    tree = data.frame(
      path = c("readme.md", "data.csv"),
      type = c("file", "file"),
      size = c(512, 1024),
      stringsAsFactors = FALSE
    )
  )

  mock_json_string <- jsonlite::toJSON(mock_response, auto_unbox = FALSE)

  local_mocked_bindings(
    request = function(url) {
      structure(list(url = url), class = "httr2_request")
    },
    req_perform = function(req) {
      structure(
        list(body = charToRaw(as.character(mock_json_string))),
        class = "httr2_response"
      )
    },
    resp_body_string = function(resp) {
      rawToChar(resp$body)
    },
    .package = "httr2"
  )

  expect_message(
    result <- ed_get_tables("wbids", quiet = FALSE),
    "No Parquet files found"
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), 5)
})

test_that("ed_get_tables shows messages when quiet = FALSE", {
  mock_response <- list(
    tree = data.frame(
      path = c("data.parquet"),
      type = c("file"),
      size = c(1024),
      stringsAsFactors = FALSE
    )
  )

  mock_json_string <- jsonlite::toJSON(mock_response, auto_unbox = FALSE)

  local_mocked_bindings(
    request = function(url) {
      structure(list(url = url), class = "httr2_request")
    },
    req_perform = function(req) {
      structure(
        list(body = charToRaw(as.character(mock_json_string))),
        class = "httr2_response"
      )
    },
    resp_body_string = function(resp) {
      rawToChar(resp$body)
    },
    .package = "httr2"
  )

  expect_message(
    ed_get_tables("wbids", quiet = FALSE),
    "Fetching file tree from"
  )

  expect_message(
    ed_get_tables("wbids", quiet = FALSE),
    "Found 1 Parquet table"
  )
})

test_that("ed_get_tables handles HTTP errors", {
  local_mocked_bindings(
    request = function(url) {
      structure(list(url = url), class = "httr2_request")
    },
    req_perform = function(req) {
      stop("Network error")
    },
    .package = "httr2"
  )

  expect_message(
    ed_get_tables("wbids", quiet = TRUE),
    "Failed to retrieve file tree"
  )
})

test_that("ed_get_tables handles unexpected API response structure", {
  mock_response <- list(
    unexpected_field = "unexpected_value"
  )

  mock_json_string <- jsonlite::toJSON(mock_response, auto_unbox = FALSE)

  local_mocked_bindings(
    request = function(url) {
      structure(list(url = url), class = "httr2_request")
    },
    req_perform = function(req) {
      structure(
        list(body = charToRaw(as.character(mock_json_string))),
        class = "httr2_response"
      )
    },
    resp_body_string = function(resp) {
      rawToChar(resp$body)
    },
    .package = "httr2"
  )

  expect_message(
    ed_get_tables("wbids", quiet = TRUE),
    "Unexpected API response structure"
  )
})

test_that("ed_get_tables handles case-insensitive parquet extension", {
  mock_response <- data.frame(
    path = c("data.PARQUET", "file.Parquet"),
    type = c("file", "file"),
    size = c(1024, 2048),
    stringsAsFactors = FALSE
  )

  mock_json_string <- jsonlite::toJSON(mock_response, auto_unbox = FALSE)

  local_mocked_bindings(
    request = function(url) {
      structure(list(url = url), class = "httr2_request")
    },
    req_perform = function(req) {
      structure(
        list(body = charToRaw(as.character(mock_json_string))),
        class = "httr2_response"
      )
    },
    resp_body_string = function(resp) {
      rawToChar(resp$body)
    },
    .package = "httr2"
  )

  result <- ed_get_tables("wbids", quiet = TRUE)

  expect_equal(nrow(result), 2)
  expect_equal(result$table, c("data", "file"))
})

test_that("ed_get_tables displays error details when JSON parsing fails", {
  local_mocked_bindings(
    req_perform = function(...) {
      structure(list(), class = "httr2_response")
    },
    .package = "httr2"
  )

  local_mocked_bindings(
    resp_body_string = function(...) {
      "invalid json"
    },
    .package = "httr2"
  )

  # Capture all CLI output
  messages <- capture.output(
    result <- ed_get_tables("test-dataset", quiet = FALSE),
    type = "message"
  )

  # Check that both danger and info alerts were shown

  expect_true(any(grepl("Failed to parse API response", messages)))
  expect_true(any(grepl("Error:", messages)))
  expect_null(result)
})

test_that("ed_get_tables handles HTTP request errors gracefully", {
  local_mocked_bindings(
    req_perform = function(...) {
      stop("Connection refused")
    },
    .package = "httr2"
  )

  expect_message(
    result <- ed_get_tables("test-dataset", quiet = FALSE),
    "Failed to retrieve file tree"
  )

  expect_null(result)
})

test_that("ed_get_datasets handles HTTP request errors gracefully", {
  local_mocked_bindings(
    req_perform = function(...) {
      stop("Connection refused")
    },
    .package = "httr2"
  )

  expect_message(
    result <- ed_get_datasets(quiet = FALSE),
    "Failed to retrieve dataset list"
  )

  expect_null(result)
})

test_that("ed_get_datasets displays error details when HTTP request fails", {
  local_mocked_bindings(
    req_perform = function(...) {
      stop("timeout after 10 seconds")
    },
    .package = "httr2"
  )

  messages <- capture.output(
    result <- ed_get_datasets(quiet = FALSE),
    type = "message"
  )

  expect_true(any(grepl("Failed to retrieve dataset list", messages)))
  expect_true(any(grepl("Error:.*timeout", messages)))
  expect_null(result)
})

test_that("ed_get_datasets handles response body read errors gracefully", {
  local_mocked_bindings(
    req_perform = function(...) {
      structure(list(), class = "httr2_response")
    },
    .package = "httr2"
  )

  local_mocked_bindings(
    resp_body_string = function(...) {
      stop("Failed to decode response body")
    },
    .package = "httr2"
  )

  expect_message(
    result <- ed_get_datasets(quiet = FALSE),
    "Failed to read response body"
  )

  expect_null(result)
})

test_that("ed_get_datasets displays error details when body read fails", {
  local_mocked_bindings(
    req_perform = function(...) {
      structure(list(), class = "httr2_response")
    },
    .package = "httr2"
  )

  local_mocked_bindings(
    resp_body_string = function(...) {
      stop("encoding error in response")
    },
    .package = "httr2"
  )

  messages <- capture.output(
    result <- ed_get_datasets(quiet = FALSE),
    type = "message"
  )

  expect_true(any(grepl("Failed to read response body", messages)))
  expect_true(any(grepl("Error:.*encoding", messages)))
  expect_null(result)
})

test_that("ed_get_datasets displays error details when JSON parsing fails", {
  local_mocked_bindings(
    req_perform = function(...) {
      structure(list(), class = "httr2_response")
    },
    .package = "httr2"
  )

  local_mocked_bindings(
    resp_body_string = function(...) {
      "invalid json: missing quotes"
    },
    .package = "httr2"
  )

  messages <- capture.output(
    result <- ed_get_datasets(quiet = FALSE),
    type = "message"
  )

  expect_true(any(grepl("Failed to parse JSON response", messages)))
  expect_true(any(grepl("Error:", messages)))
})
