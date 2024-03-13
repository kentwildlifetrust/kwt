#' Eastings and Northings to OS Grid Ref
#' This function converts eastings and northings to an OS grid ref of a specified length.
#' @param easting The eastings of the point to be converted.
#' @param northing The northings of the point to be converted.
#' @param resolution The resolution of the grid reference to be produced.
#' @keywords Grid references
#' @export
#' @examples
#' df <- data.frame(easting = c(385760, 532765, 479732), northing = c(219790, 122444, 347045))
#' gridrefs <- en2os(df$easting, df$northing, 6)

en2os <- function(easting, northing, resolution){

  endf <- base::data.frame(easting, northing) %>%
    dplyr::mutate(number_code = base::paste0(base::substr(easting, 1, 1),
                                             base::substr(northing, 1, 1))) %>%
    dplyr::inner_join(os_lettercode_lookup, by = "number_code") %>%
    dplyr::mutate(easting_numbers = base::substr(easting, 2, (resolution/2 + 1)),
                  northing_numbers = base::substr(northing, 2, (resolution/2 + 1)))

  output <- base::paste0(endf$squarecode, endf$eastingnumbers, endf$northingnumbers)
  output

}


