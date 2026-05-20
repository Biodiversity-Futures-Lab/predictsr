# predictsr 0.2.1

* Fixed API error causing failures via the NHM data portal (on the data portal side).
* air format package and include Github actions check.
* Fixed temp files not being cleaned up on failed downloads (#38).
* Fixed incorrect comparison of integer years (#39).
* Fixed SHA validation failing for NAs (#40).

# predictsr 0.2.0

* Introduced `LoadPredictsData` function to save database locally via SHA-based invalidation.
* Refactored package structure to spread functions and tests better across multiple files.

# predictsr 0.1.1

* Refactored underlying code to now return empty dataframes when offline or resources are unavailable (#34).
* Expanded README to better handle offline/missing resources (#35).
* Changed copyright holder to NHM.
* Updated documentation to clearly state rules on copyright holding of the data.

# predictsr 0.1.0

* Initial CRAN submission.
