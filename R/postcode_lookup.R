#' Lookup table with easting and northing coordinates for each UK postcode.
#'
#' A dataset containing the easting and northing coordinates of UK postcodes.
#'
#' @format A data frame with 1725632 rows and 3 variables:
#' \describe{
#'   \item{postcode}{uk postcode, with no spaces and capitalised}
#'   \item{eastings}{easting coordinate}
#'   \item{northings}{northing coordinate}
#'   ...
#' }
#' @source \url{https://osdatahub.os.uk/downloads/open/CodePointOpen}
"postcode_lookup"
