#' Load (or download) PREDICTS data to a user-specified RDS file.
#'
#' @description
#' Implements a simple file-based cache. You supply a target filename
#' (e.g. "data/predicts_2016_2022.rds"). The function will:
#' 1. Look for that RDS file and the companion metadata file
#'    "<filename>.aux.json" (e.g. "data/predicts_2016_2022.rds.aux.json").
#' 2. If both exist, verify the file hash, minimal structure, and requested
#'    years.
#' 3. If validation passes return the loaded object.
#' 4. Otherwise download fresh data via `GetPredictsData(extract)`, overwrite
#'    the RDS, write a new `.aux.json`, and return the dataframe.
#'
#' @param file_predicts Character path to the desired PREDICTS database RDS
#' file (must end with ".rds").
#' @param extract Integer vector of release years to fetch. Defaults to
#' `c(2016, 2022)`.
#' @param force_refresh Logical; if TRUE always re-download and overwrite
#' existing files.
#' @returns A dataframe containing the requested PREDICTS extract.
#'
#' @export
LoadPredictsData <- function(file_predicts,
                             extract = c(2016, 2022),
                             force_refresh = FALSE) {
  if (!is.character(file_predicts) || length(file_predicts) != 1L) {
    stop("'file' must be a single character string")
  }
  if (!endsWith(tolower(file_predicts), ".rds")) {
    stop("'file' must end with '.rds'")
  }
  if (!length(extract) || !all(extract %in% c(2016, 2022))) {
    stop("'extract' should be 2016 and/or 2022")
  }
  extract <- sort(extract)

  logger::log_info("Pulling in extracts for {extract}")
  parent_dir <- dirname(file_predicts)
  if (!dir.exists(parent_dir)) {
    ok <- dir.create(parent_dir, recursive = TRUE, showWarnings = FALSE)
    if (!ok && !dir.exists(parent_dir)) {
      stop("Failed to create parent directory '", parent_dir,
           "'. Check path or permissions.")
    }
  }

  aux_file_predicts <- glue::glue("{file_predicts}.aux.json")
  have_cache <- file.exists(file_predicts) && file.exists(aux_file_predicts)

  if (have_cache && !force_refresh) {
    logger::log_info("Attempting to load cached PREDICTS data from: {file_predicts}")
    cached <- .ReadPredictsFileCache(file_predicts, aux_file_predicts, extract)

    if (cached$valid) {
      logger::log_info("Cached data valid; returning loaded dataframe")
      return(cached$data)
    } else {
      logger::log_info("Cached data invalid; will re-download and overwrite")
    }
  } else if (force_refresh && have_cache) {
    logger::log_info("Refresh requested; ignoring existing cache at {file_predicts}")
  } else if (!have_cache) {
    logger::log_info("No existing valid cache at {file_predicts}; will download")
  }

  logger::log_info("Downloading fresh PREDICTS data (extract={paste(extract, collapse=',')})")
  df <- GetPredictsData(extract = extract)

  .WritePredictsFileCache(df, file_predicts, aux_file_predicts, extract)
  return(df)
}

#' Writes the PREDICTS dataframe to disk.
#'
#' @description
#' Given a PREDICTS database extract, loaded as an R dataframe, save it to disk
#' and write the aux JSON file (*.aux.json), which stores metadata for the
#' object. This includes the release years, the timestamp of when it was saved,
#' som dimensions, and the SHA-256 hash of the dataframe (computed with
#' 'digest').
#'
#' @param df Dataframe to be written to disk
#' @param file_predicts Character, path to the desired file - should be an RDS
#' file.
#' @param aux_file_predicts Character, path to where the auxiliary file should
#' be saved to disk.
#' @param extract Numeric vector of release years to be saved.
#' @returns TRUE invisibly.
.WritePredictsFileCache <- function(df, 
                                    file_predicts, 
                                    aux_file_predicts, 
                                    extract) {
  saveRDS(df, file_predicts)

  sha <- tryCatch(
    digest::digest(df, algo = "sha256"),
    error = function(e) NA_character_
  )

  aux <- list(
    years = extract,
    timestamp = format(Sys.time(), tz = "UTC", usetz = TRUE),
    n_rows = nrow(df),
    n_cols = ncol(df),
    columns = names(df),
    sha256 = sha,
    pkg_version = as.character(utils::packageVersion("predictsr"))
  )

  jsonlite::write_json(
    aux, 
    aux_file_predicts, 
    pretty = TRUE, 
    auto_unbox = TRUE
  )
  logger::log_info(
    "Wrote data file '{basename(file_predicts)}' and aux metadata '{basename(aux_file_predicts)}'"
  )
  return(invisible(TRUE))
}

#' Read the PREDICTS file cache.
#'
#' This internal helper function returns a list with 3 elements: 'valid', a
#' boolean indicating if the cache is valid; 'data', a dataframe containing the
#' cached PREDICTS data; and 'aux', the auxiliary data loaded from the cache.
#'
#' @param file_predicts Character, the path to the saved PREDICTS database
#' extract (as an RDS file).
#' @param aux_file_predicts Character, the path to the saved PREDICTS database
#' auxiliary metadata, saved as a JSON file.
#' @param requested_years Numeric vector, the extract years to be saved.
#' @returns List, a named list of three elements: 'valid', a boolean indicating if
#' the cache is valid; 'data', a dataframe containing the cached PREDICTS data;
#' and 'aux', the auxiliary data loaded from the cache.
.ReadPredictsFileCache <- function(file_predicts, 
                                   aux_file_predicts, 
                                   requested_years) {
  # Load the auxiliary file - any errors return NULL
  aux <- tryCatch(
    jsonlite::read_json(aux_file_predicts),
    error = function(e) return(NULL)
  )
  if (is.null(aux)) {
    logger::log_warn("Metadata file '{aux_file_predicts}' couldn't be read")
    return(list(valid = FALSE, data = NULL, aux = NULL))
  }

  # Check that the contents of the metadata are what we want
  names_aux <- c(
    "years", "timestamp", "n_rows", "n_cols", "columns", "sha256",
    "pkg_version"
  )
  if (!setequal(names_aux, names(aux))) {
    missing <- setdiff(names_aux, names(aux))
    logger::log_warn(
      "Aux file corrupted, missing: {paste(missing, collapse = ', ')} entry"
    )
    return(list(valid = FALSE, data = NULL, aux = NULL))
  }

  if (!all(
    (sort(unlist(aux$years)) - sort(requested_years)) <= 1e-8
  )) {
    logger::log_warn(
      "Metadata years ({paste(aux$years, collapse=',')}) differ from requested years ({paste(requested_years, collapse=',')})."
    )
    return(list(valid = FALSE, data = NULL, aux = aux))
  }

  df <- tryCatch(
    readRDS(file_predicts),
    error = function(e) NULL
  )
  if (is.null(df)) {
    logger::log_warn("Failed to read cached RDS at '{file_predicts}'.")
    return(list(valid = FALSE, data = NULL, aux = aux))
  }

  # Check if the dataframe is actually what we want
  if (!.IsValidPredictsData(df)) {
    logger::log_warn("Cached dataframe failed structural validation.")
    return(list(valid = FALSE, data = NULL, aux = aux))
  }

  # And finally check that the hashes line up
  df_hash <- tryCatch(
    digest::digest(df, algo = "sha256"),
    error = function(e) NA_character_
  )
  if (is.na(df_hash) || is.null(aux$sha256) || df_hash != aux$sha256) {
    logger::log_warn(
      "Hash mismatch for cached file; expected {aux$sha256}, got {df_hash}"
    )
    return(list(valid = FALSE, data = NULL, aux = aux))
  }

  return(list(valid = TRUE, data = df, aux = aux))
}

#' Check if a PREDICTS extract is valid.
#'
#' @description
#' A small set of basic checks to ensure that a PREDICTS extract is valid.
#' These include checking the object is a dataframe, checking all the columns
#' are valid, and checking that we have a nonzero row count.
#'
#' @param df Dataframe, containing the PREDICTS extract.
#' @returns Boolean, `TRUE` if the dataframe is valid, `FALSE` if not.
.IsValidPredictsData <- function(df) {
  if (!is.data.frame(df)) {
    return(FALSE)
  }

  if (nrow(df) == 0) {
    logger::log_info("Zero-row dataframe")
    return(FALSE)
  }

  # Check a subset of the columns are present
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
  if (!setequal(names_predicts, names(df))) {
    missing <- setdiff(names_predicts, names(df))
    logger::log_info("Missing columns: {paste(missing, collapse = ', ')}")
    return(FALSE)
  }

  return(TRUE)
}
