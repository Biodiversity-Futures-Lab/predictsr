test_that("Can read in the site-level summaries", {
  skip_if_offline("data.nhm.ac.uk")
  sls <- GetSitelevelSummaries()

  # check that DF with expected attributes
  expect_true(inherits(sls, "data.frame"))
  CheckSitelevelData(sls)
})

test_that("Returns empty data frame when no site-level summaries are available", {
  with_mocked_bindings(
    sls <- GetSitelevelSummaries(),
    .RequestDataPortal = function(...) {
      return(
        list(status = "working", result = NULL, message = "no data available")
      )
    },
    .CheckDownloadResponse = function(...) {
      return(list(status = "failed", message = "no data available"))
    }
  )
  # check that we get an empty data frame
  expect_true(inherits(sls, "data.frame"))
  expect_equal(nrow(sls), 0)
  expect_equal(ncol(sls), 0)
})

test_that("Breaks when we have incorrect extracts", {
  expect_error(GetSitelevelSummaries(extract = NA))
  expect_error(GetSitelevelSummaries(extract = 123))
  expect_error(GetSitelevelSummaries(extract = NULL))
  expect_error(GetSitelevelSummaries(extract = 2015))
})
