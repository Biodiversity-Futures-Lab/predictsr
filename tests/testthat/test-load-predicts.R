# Build file paths for a given vector of years
predicts_test_files <- function(years) {
  stopifnot(length(years) >= 1)
  stub <- paste0("predicts_", paste(sort(years), collapse = "_"))
  data_path <- file.path(tempdir(), paste0(stub, ".rds"))
  meta_path <- paste0(data_path, ".aux.json")
  list(data = data_path, meta = meta_path)
}

# Assert metadata structure + specific SHA + years
expect_predicts_metadata <- function(meta_file, expected_years, expected_sha) {
  meta <- jsonlite::read_json(meta_file)

  expect_equal(
    names(meta), 
    c(
      "years", "timestamp", "n_rows", "n_cols", "columns", "sha256",
      "pkg_version"
    )
  )
  expect_equal(sort(unlist(meta$years)), sort(expected_years))
  expect_equal(meta$sha256, expected_sha)
  invisible(meta)
}

# Basic dataframe checks
expect_predicts_df <- function(df) {
  expect_true(inherits(df, "data.frame"))
  expect_true(nrow(df) >= 0)
  invisible(df)
}

test_that("Can load all the PREDICTS data (2016 and 2022)", {
  skip_if_offline("data.nhm.ac.uk")
  files <- predicts_test_files(c(2016, 2022))

  predicts <- LoadPredictsData(files$data, extract = c(2016, 2022))
  expect_predicts_df(predicts)
  CheckPREDICTSData(predicts)

  # Dataset dimensions
  expect_equal(nrow(predicts), 4318808)
  expect_equal(length(levels(predicts$Taxon)), 29585)
  expect_equal(length(levels(predicts$Family)), 1652)

  # Files exist
  expect_true(file.exists(files$data) && file.exists(files$meta))

  # Metadata (SHA fixed; should not change)
  expect_predicts_metadata(
    files$meta,
    c(2016, 2022),
    "94e30c1504b8fdf3a8bcd77d539a1176ae1546ce52f0c70ea3d68b88e36008c8"
  )

  unlink(c(files$data, files$meta))
})

test_that("Can read in the 2016 PREDICTS database extract", {
  skip_if_offline("data.nhm.ac.uk")
  files <- predicts_test_files(2016)

  predicts <- LoadPredictsData(files$data, extract = 2016)
  expect_predicts_df(predicts)
  CheckPREDICTSData(predicts)

  expect_equal(nrow(predicts), 3278056)
  expect_equal(length(levels(predicts$Taxon)), 29584)
  expect_equal(length(levels(predicts$Family)), 1652)

  expect_true(file.exists(files$data) && file.exists(files$meta))

  expect_predicts_metadata(
    files$meta,
    2016,
    "5db89507a0f4056a3cada1580d498325255a79e474db979932b9d8ef195c0670"
  )

  unlink(c(files$data, files$meta))
})

test_that("Can read in the 2022 PREDICTS database extract", {
  skip_if_offline("data.nhm.ac.uk")
  files <- predicts_test_files(2022)

  predicts <- LoadPredictsData(files$data, extract = 2022)
  expect_predicts_df(predicts)
  CheckPREDICTSData(predicts, full = FALSE)

  expect_equal(nrow(predicts), 1040752)
  expect_equal(length(unique(predicts$Taxon)), 6915)
  expect_equal(length(unique(predicts$SSBS)), 9544)
  expect_equal(length(unique(predicts$Family)), 892)
  expect_equal(length(unique(predicts$Source_ID)), 115)

  expect_true(file.exists(files$data) && file.exists(files$meta))

  expect_predicts_metadata(
    files$meta,
    2022,
    "873f842cb2beaaec61d87f6b8e38cf5c6563eb391a60dae9ca2363ea34593cfe"
  )

  unlink(c(files$data, files$meta))
})

test_that("Can load an empty data frame if no data are available", {
  files <- predicts_test_files(c(2016, 2022))  # years irrelevant; mock returns empty

  with_mocked_bindings(
    {
      predicts <- LoadPredictsData(files$data)
    },
    .RequestDataPortal = function(...) {
      list(status = "working", result = NULL, message = "no data available")
    },
    .CheckDownloadResponse = function(...) {
      list(status = "failed", message = "no data available")
    },
    .RequestRDSDataFrame = function(...) {
      data.frame()
    }
  )

  expect_predicts_df(predicts)
  expect_equal(nrow(predicts), 0)
  expect_equal(ncol(predicts), 0)

  unlink(c(files$data, files$meta))
})
