CheckPREDICTSData <- function(predicts, full = TRUE) {
  # all extracts should have 67 columns
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

  expect_equal(
    levels(predicts$Diversity_metric_type),
    c("Abundance", "Occurrence", "Species richness")
  )

  # check land use factors
  expect_equal(length(levels(predicts$Predominant_land_use)), 10)
  expect_equal(length(levels(predicts$Use_intensity)), 4)

  # check that the site-level factors match expectations across the data (these
  # are to be the same for the 2016, and 2016+2022 data)
  if (full) {
    expect_equal(length(levels(predicts$SS)), 993)
    expect_equal(length(levels(predicts$SSS)), 50032)
    expect_equal(length(levels(predicts$SSB)), 6098)
    expect_equal(length(levels(predicts$SSBS)), 50032)
  }
}

CheckSitelevelData <- function(sls) {
  expect_equal(nrow(sls), 35738)
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
  expect_equal(length(unique(sls$Source_ID)), 595)

  # check that the study counts are expected
  expect_equal(length(unique(sls$SS)), 817)
  expect_equal(length(levels(sls$SS)), 993)

  # check that the sites are expected
  expect_equal(length(unique(sls$SSBS)), 35738)
  expect_equal(length(levels(sls$SSBS)), 53008)

  # we expect 94 countries from the webpage
  expect_equal(length(unique(sls$Country)), 101)

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


