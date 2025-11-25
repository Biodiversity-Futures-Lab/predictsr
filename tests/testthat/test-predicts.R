test_that("Can read in all the PREDICTS data (2016 and 2022)", {
  skip_if_offline("data.nhm.ac.uk")
  predicts <- GetPredictsData(extract = c(2016, 2022))
  expect_true(inherits(predicts, "data.frame"))
  CheckPREDICTSData(predicts)

  # check the basic dataset dimensions
  expect_equal(nrow(predicts), 4318808)
  expect_equal(length(levels(predicts$Taxon)), 29585)
  expect_equal(length(levels(predicts$Family)), 1652)
})

test_that("Can read in the 2016 PREDICTS database extract", {
  skip_if_offline("data.nhm.ac.uk")
  predicts <- GetPredictsData(extract = 2016)
  expect_true(inherits(predicts, "data.frame"))
  CheckPREDICTSData(predicts)

  expect_equal(nrow(predicts), 3278056)
  expect_equal(length(levels(predicts$Taxon)), 29584)
  expect_equal(length(levels(predicts$Family)), 1652)
})

test_that("Can read in the 2022 PREDICTS database extract", {
  skip_if_offline("data.nhm.ac.uk")
  predicts <- GetPredictsData(extract = 2022)
  expect_true(inherits(predicts, "data.frame"))
  CheckPREDICTSData(predicts, full = FALSE)

  # Check sum summary counts over the groups
  expect_equal(nrow(predicts), 1040752)
  expect_equal(length(unique(predicts$Taxon)), 6915)
  expect_equal(length(unique(predicts$SSBS)), 9544)
  expect_equal(length(unique(predicts$Family)), 892)
  expect_equal(length(unique(predicts$Source_ID)), 115)
})

test_that("Can return an empty data frame if no data are available", {
  with_mocked_bindings(
    predicts <- GetPredictsData(),
    .RequestDataPortal = function(...) {
      return(
        list(status = "working", result = NULL, message = "no data available")
      )
    },
    .CheckDownloadResponse = function(...) {
      return(list(status = "failed", message = "no data available"))
    }
  )

  # check that the result is a data frame with no rows or columns
  expect_true(inherits(predicts, "data.frame"))
  expect_equal(nrow(predicts), 0)
  expect_equal(ncol(predicts), 0)
})

test_that("Fails with incorrect inputs", {
  # weird years
  expect_error(GetPredictsData(extract = NA))
  expect_error(GetPredictsData(extract = 123))
  expect_error(GetPredictsData(extract = NULL))
  expect_error(GetPredictsData(extract = 2015))
})
