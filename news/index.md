# Changelog

## predictsr (development version)

- Fixed error causing failures via the NHM data portal.
- Fixed temp files not being cleaned up on failed downloads
  ([\#38](https://github.com/Biodiversity-Futures-Lab/predictsr/issues/38)).
- Fixed incorrect comparison of integer years
  ([\#39](https://github.com/Biodiversity-Futures-Lab/predictsr/issues/39)).
- Fixed SHA validation failing for NAs
  ([\#40](https://github.com/Biodiversity-Futures-Lab/predictsr/issues/40)).

## predictsr 0.2.0

CRAN release: 2025-11-28

- Introduced `LoadPredictsData` function to save database locally via
  SHA-based invalidation.
- Refactored package structure to spread functions and tests better
  across multiple files.

## predictsr 0.1.1

CRAN release: 2025-07-23

- Refactored underlying code to now return empty dataframes when offline
  or resources are unavailable
  ([\#34](https://github.com/Biodiversity-Futures-Lab/predictsr/issues/34)).
- Expanded README to better handle offline/missing resources
  ([\#35](https://github.com/Biodiversity-Futures-Lab/predictsr/issues/35)).
- Changed copyright holder to NHM.
- Updated documentation to clearly state rules on copyright holding of
  the data.

## predictsr 0.1.0

CRAN release: 2025-07-10

- Initial CRAN submission.
