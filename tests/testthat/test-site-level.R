check_sitelevel_data <- function(sls) {
  expect_equal(nrow(sls), 26194)
  expect_equal(ncol(sls), 50)

  expected_names <- c(
    "Source_ID", "Reference", "Study_number", "Study_name", "SS",
    "Diversity_metric", "Diversity_metric_unit", "Diversity_metric_type",
    "Diversity_metric_is_effort_sensitive",
    "Diversity_metric_is_suitable_for_Chao", "Sampling_method",
    "Sampling_effort_unit", "Study_common_taxon",
    "Rank_of_study_common_taxon", "Site_number", "Site_name", "Block", "SSS",
    "SSB", "SSBS", "Sample_start_earliest", "Sample_end_latest",
    "Sample_midpoint", "Sample_date_resolution", "Max_linear_extent_metres",
    "Habitat_patch_area_square_metres", "Sampling_effort",
    "Rescaled_sampling_effort", "Habitat_as_described",
    "Predominant_land_use", "Source_for_predominant_land_use",
    "Use_intensity", "Km_to_nearest_edge_of_habitat",
    "Years_since_fragmentation_or_conversion", "Transect_details",
    "Coordinates_method", "Longitude", "Latitude", "Country_distance_metres",
    "Country", "UN_subregion", "UN_region", "Ecoregion_distance_metres",
    "Ecoregion", "Biome", "Realm", "Hotspot", "Wilderness_area", "N_samples",
    "Higher_taxa"
  )
  expect_equal(names(sls), expected_names)

  # source ID's are what is quoted on the website
  expect_equal(length(unique(sls$Source_ID)), 480)

  # check that the study counts are expected
  expect_equal(length(unique(sls$SS)), 666)
  expect_equal(length(levels(sls$SS)), 993)

  # check that the sites are expected
  expect_equal(length(unique(sls$SSBS)), 26194)
  expect_equal(length(levels(sls$SSBS)), 53008)

  # we expect 94 countries from the webpage
  expect_equal(length(unique(sls$Country)), 94)

  # some factor checks
  expect_setequal(
    levels(sls$Diversity_metric_type),
    c("Abundance", "Occurrence", "Species richness")
  )
  expect_setequal(
    levels(sls$Use_intensity),
    c("Minimal use", "Light use", "Intense use", "Cannot decide")
  )
  expect_setequal(
    levels(sls$Diversity_metric),
    c(
      "abundance",
      "species richness",
      "effort-corrected abundance",
      "density",
      "group abundance",
      "occurrence",
      "percent cover",
      "sign relative abundance",
      "occurrence frequency",
      "sign density",
      "biomass",
      "biovolume",
      "relative abundance",
      "effort-corrected sign abundance",
      "sign abundance"
    )
  )
}

test_that("can read in the site-level summaries", {
  sls <- get_sitelevel_summaries()

  # check that DF with expected attributes
  expect_true(inherits(sls, "data.frame"))
  check_sitelevel_data(sls)
})

test_that("can read in the site-level summaries as a tibble", {
  sls <- get_sitelevel_summaries(fmt = "tibble")

  # check that DF with expected attributes
  expect_true(tibble::is_tibble(sls))
  check_sitelevel_data(sls)
})

test_that("breaks when fmt is not data.frame or tibble", {
  # check string expectations match up
  expect_error(
    get_sitelevel_summaries("df"),
    regexp = paste(
      "Argument fmt not recognised - please supply either 'data.frame'",
      "or 'tibble'"
    )
  )
  expect_error(
    get_sitelevel_summaries("Tibble"),
    regexp = paste(
      "Argument fmt not recognised - please supply either 'data.frame'",
      "or 'tibble'"
    )
  )

  # check non-character fmt arguments
  expect_error(get_sitelevel_summaries(fmt = NA))
  expect_error(get_sitelevel_summaries(fmt = 123))
  expect_error(get_sitelevel_summaries(fmt = NULL))
})
