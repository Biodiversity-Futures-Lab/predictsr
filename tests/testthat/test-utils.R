test_that("Input is not a list triggers error", {
  expect_error(
    .RequestDataPortal("notalist"),
    "Request_body_json should be a list"
  )
})

test_that("Initial request fails returns failed status", {
  with_mocked_bindings(
    result <- .RequestDataPortal(list()),

    request = function(x) {
      return(list(
        status = "failed",
        message = "request failed"
      ))
    },
    req_body_json = function(x, ...) x,
    req_user_agent = function(x, ...) x,
    req_retry = function(x, ...) x,
    req_perform = function(x, ...) {
      stop("request failed")
    }
  )

  # check that the result is a list with status and message
  expect_equal(result$status, "failed")
  expect_equal(result$message, "request failed")
})

test_that("Download request fails appropriately", {
  with_mocked_bindings(
    result <- .CheckDownloadResponse(
      list(
        result = list("download_url" = "https://data.nhm.ac.uk/download")
      )
    ),

    request = function(x, ...) {
      return(list(
        status = "working",
        message = "request in progress",
        download_url = "https://data.nhm.ac.uk/download"
      ))
    },
    req_body_json = function(x, ...) return(x),
    req_user_agent = function(x, ...) return(x),
    req_retry = function(x, ...) return(x),
    req_throttle = function(x, ...) return(x),
    req_perform = function(x, ...) stop("status failed")
  )

  # check that the result is a list with status and message
  expect_equal(result$status, "failed")
  expect_equal(result$message, "status failed")
})


test_that(".RequestRDSDataFrame returns an empty dataframe if incomplete", {
  # will fail before any API calls
  status_json <- "https://data.nhm.ac.uk"
  expect_error(
    .RequestRDSDataFrame(status_json),
    regexp = "status_json should be a list with a 'status' entry"
  )

  # does not need API access - will return before any API calls
  status_json <- list(status = "working")
  df <- .RequestRDSDataFrame(status_json)

  expect_true(inherits(df, "data.frame"))
  expect_equal(nrow(df), 0)
  expect_equal(ncol(df), 0)
})
