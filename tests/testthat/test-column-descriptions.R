test_that("We can get the column descriptions as desired", {
  # Basic checks for the case that we have a data.frame output
  columns <- GetColumnDescriptions() |> dplyr::arrange(Column)
  expect_equal(nrow(columns), 69)
  expect_equal(ncol(columns), 8)
  expect_equal(
    names(columns),
    c("Column", "Applies_to", "Site_extract", "Diversity_extract", "Type",
      "Value_guaranteed_to_be_non_empty", "Notes", "Validation")
  )
  names <- c(
    "Best_guess_binomial", "Biome", "Block", "COL_ID", "Class",
    "Coordinates_method", "Country", "Country_distance_metres",
    "Diversity_metric", "Diversity_metric_is_effort_sensitive",
    "Diversity_metric_is_suitable_for_Chao", "Diversity_metric_type",
    "Diversity_metric_unit", "Ecoregion", "Ecoregion_distance_metres",
    "Effort_corrected_measurement", "Family", "Genus", "Habitat_as_described",
    "Habitat_patch_area_square_metres", "Higher_taxa", "Higher_taxon",
    "Hotspot", "Indication", "Kingdom", "Km_to_nearest_edge_of_habitat",
    "Latitude", "Longitude", "Max_linear_extent_metres", "Measurement",
    "N_samples", "Name_status", "Order", "Parsed_name", "Phylum",
    "Predominant_land_use", "Rank", "Rank_of_study_common_taxon", "Realm",
    "Reference", "Rescaled_sampling_effort", "SS", "SSB", "SSBS", "SSS",
    "Sample_date_resolution", "Sample_end_latest", "Sample_midpoint",
    "Sample_start_earliest", "Sampling_effort", "Sampling_effort_unit",
    "Sampling_method", "Site_name", "Site_number", "Source_ID",
    "Source_for_predominant_land_use", "Species", "Study_common_taxon",
    "Study_name", "Study_number", "Taxon", "Taxon_name_entered", "Taxon_number",
    "Transect_details", "UN_region", "UN_subregion", "Use_intensity",
    "Wilderness_area", "Years_since_fragmentation_or_conversion"
  )
  expect_equal(columns$Column, names)

  # Now check against the 2016 data
  resource_id <- "5f167ad3-0e38-49f3-8a9b-df69f6ef670b"
  url_string <- .GetURLString(package_id_2016, resource_id)
  columns_2016 <- read.csv(url_string, header = TRUE) |> dplyr::arrange(Column)

  # So we don't overrun the API
  Sys.sleep(1)

  # Is there some congruency between the old and new data?
  expect_equal(columns$Column, columns_2016$Column)
  expect_equal(columns$Applies_to, columns_2016$Applies_to)
  expect_equal(columns$Site_extract, columns_2016$Site_extract_)
  expect_equal(columns$Type, columns_2016$Type)
  expect_equal(columns$Value_guaranteed_to_be_non_empty, columns_2016$Validation)

  # Now check that we can get a tibble as required
  columns <- GetColumnDescriptions(fmt = "tibble") |> dplyr::arrange(Column)
  expect_true(tibble::is_tibble(columns))
  expect_equal(nrow(columns), 69)
  expect_equal(ncol(columns), 8)
  expect_equal(
    names(columns),
    c("Column", "Applies_to", "Site_extract", "Diversity_extract", "Type",
      "Value_guaranteed_to_be_non_empty", "Notes", "Validation")
  )
})
