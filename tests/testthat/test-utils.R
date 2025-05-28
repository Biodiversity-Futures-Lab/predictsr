test_that(".RequestDataPortal fails if we don't provide a list", {
  # Will fail before any API calls
  request_body <- "https://data.nhm.ac.uk"
  expect_error(.RequestDataPortal(request_body))
})

test_that(".RequestRDSDataFrame fails if incomplete", {
  # Does not need API access - will fail before any API calls
  status_json <- list(status = "working")
  expect_error(.RequestRDSDataFrame(status_json))
})
