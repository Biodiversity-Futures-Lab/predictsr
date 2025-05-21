test_that(".RequestDataPortal fails if we don't provide a list", {
  request_body <- "https://data.nhm.ac.uk"
  expect_error(.RequestDataPortal(request_body))
})

test_that(".RequestRDSDataFrame fails if incomplete", {
  status_json <- list(status = "working")
  expect_error(.RequestRDSDataFrame(status_json))
})
