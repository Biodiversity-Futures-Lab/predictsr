test_that("constructs correct URL for string resource_id", {
  result <- .get_url_string("res456")
  expect_s3_class(result, "glue")
  expect_equal(
    as.character(result),
    "https://data.nhm.ac.uk/dataset/4e3a9108-3e25-43d8-9f58-31b40fe438d6/resource/res456/download"
  )
})

test_that("constructs correct URL for numeric resource_id", {
  result <- .get_url_string(789)
  expect_equal(
    as.character(result),
    "https://data.nhm.ac.uk/dataset/4e3a9108-3e25-43d8-9f58-31b40fe438d6/resource/789/download"
  )
})

test_that("vector input returns vector of URLs", {
  ids <- c("a", "b", "c")
  result <- .get_url_string(ids)
  expected <- c(
    "https://data.nhm.ac.uk/dataset/4e3a9108-3e25-43d8-9f58-31b40fe438d6/resource/a/download",
    "https://data.nhm.ac.uk/dataset/4e3a9108-3e25-43d8-9f58-31b40fe438d6/resource/b/download",
    "https://data.nhm.ac.uk/dataset/4e3a9108-3e25-43d8-9f58-31b40fe438d6/resource/c/download"
  )
  expect_equal(as.character(result), expected)
})

test_that("handles NA resource_id by inserting 'NA' string", {
  result <- .get_url_string(NA)
  expect_equal(
    as.character(result),
    "https://data.nhm.ac.uk/dataset/4e3a9108-3e25-43d8-9f58-31b40fe438d6/resource/NA/download"
  )
})

test_that("zero-length input returns zero-length output", {
  result <- .get_url_string(character(0))
  expect_equal(length(result), 0)
})

test_that("error is thrown when resource_id argument is missing", {
  expect_error(.get_url_string(), "argument \"resource_id\" is missing")
})
