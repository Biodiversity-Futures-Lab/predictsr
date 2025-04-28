CheckPREDICTSData <- function(predicts, ...) {
  expect_equal(nrow(predicts), 3278056)
  expect_equal(ncol(predicts), 67)

  # do the names match up to what we expect
  names_predicts <- c(
    "Source_ID", "Reference", "Study_number", "Study_name", "SS",
    "Diversity_metric", "Diversity_metric_unit", "Diversity_metric_type",
    "Diversity_metric_is_effort_sensitive",
    "Diversity_metric_is_suitable_for_Chao", "Sampling_method",
    "Sampling_effort_unit", "Study_common_taxon", "Rank_of_study_common_taxon",
    "Site_number", "Site_name", "Block", "SSS", "SSB", "SSBS",
    "Sample_start_earliest", "Sample_end_latest", "Sample_midpoint",
    "Sample_date_resolution", "Max_linear_extent_metres",
    "Habitat_patch_area_square_metres", "Sampling_effort",
    "Rescaled_sampling_effort", "Habitat_as_described", "Predominant_land_use",
    "Source_for_predominant_land_use", "Use_intensity",
    "Km_to_nearest_edge_of_habitat", "Years_since_fragmentation_or_conversion",
    "Transect_details", "Coordinates_method", "Longitude", "Latitude",
    "Country_distance_metres", "Country", "UN_subregion", "UN_region",
    "Ecoregion_distance_metres", "Ecoregion", "Biome", "Realm", "Hotspot",
    "Wilderness_area", "Taxon_number", "Taxon_name_entered", "Indication",
    "Parsed_name", "Taxon", "COL_ID", "Name_status", "Rank", "Kingdom",
    "Phylum", "Class", "Order", "Family", "Genus", "Species",
    "Best_guess_binomial", "Higher_taxon", "Measurement",
    "Effort_corrected_measurement"
  )
  expect_equal(names(predicts), names_predicts)

  # check that some of the important factors match expectations
  expect_equal(length(levels(predicts$SS)), 993)
  expect_equal(length(levels(predicts$SSS)), 50032)
  expect_equal(length(levels(predicts$SSB)), 6098)
  expect_equal(length(levels(predicts$SSBS)), 50032)

  expect_equal(
    levels(predicts$Diversity_metric_type),
    c("Abundance", "Occurrence", "Species richness")
  )

  expect_equal(length(levels(predicts$Taxon)), 29584)
  expect_equal(length(levels(predicts$Family)), 1652)

  expect_equal(length(levels(predicts$Predominant_land_use)), 10)
  expect_equal(length(levels(predicts$Use_intensity)), 4)
}

test_that("Can read in the PREDICTS database extract", {
  predicts <- GetPredictsData()
  expect_true(inherits(predicts, "data.frame"))
  CheckPREDICTSData(predicts)
})

test_that("Can read in the PREDICTS database extract into a tibble", {
  predicts <- GetPredictsData("tibble")

  # check that it's a tibble
  expect_true(tibble::is_tibble(predicts))
  CheckPREDICTSData(predicts)
})

test_that("fails with incorrect format", {
  # check string expectations match up
  expect_error(
    GetPredictsData("df"),
    regexp = paste(
      "Argument fmt not recognised - please supply either 'data.frame'",
      "or 'tibble'"
    )
  )
  expect_error(
    GetPredictsData("Tibble"),
    regexp = paste(
      "Argument fmt not recognised - please supply either 'data.frame'",
      "or 'tibble'"
    )
  )

  # can't work for a vector
  expect_error(
    GetPredictsData(fmt = c("data.frame", "tibble")),
    regexp = "Input fmt is not a length-1 character"
  )

  # non-character formats
  expect_error(GetPredictsData(fmt = NA))
  expect_error(GetPredictsData(fmt = 123))
  expect_error(GetPredictsData(fmt = NULL))
})
