test_that("constructs correct URL for string resource_id", {
  result <- .GetURLString("a", "res456")
  expect_s3_class(result, "glue")
  expect_equal(
    as.character(result),
    "https://data.nhm.ac.uk/dataset/a/resource/res456/download"
  )
})

test_that("constructs correct URL for numeric resource_id", {
  result <- .GetURLString(package_id = "a", resource_id = 789)
  expect_equal(
    as.character(result),
    "https://data.nhm.ac.uk/dataset/a/resource/789/download"
  )
})

test_that("vector input returns vector of URLs", {
  ids <- c("a", "b", "c")
  result <- .GetURLString(ids, ids)
  expected <- c(
    "https://data.nhm.ac.uk/dataset/a/resource/a/download",
    "https://data.nhm.ac.uk/dataset/b/resource/b/download",
    "https://data.nhm.ac.uk/dataset/c/resource/c/download"
  )
  expect_equal(as.character(result), expected)
})

test_that("handles NA resource_id by inserting 'NA' string", {
  result <- .GetURLString(NA, NA)
  expect_equal(
    as.character(result),
    "https://data.nhm.ac.uk/dataset/NA/resource/NA/download"
  )
})

test_that("zero-length input returns zero-length output", {
  result <- .GetURLString(character(0), character(0))
  expect_equal(length(result), 0)
})

test_that("error is thrown when resource_id argument is missing", {
  expect_error(.GetURLString(), "argument \"package_id\" is missing")
})
