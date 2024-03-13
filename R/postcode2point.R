#' Convert postcode to sf point
#'
#' @param df input data frame, containing postcode values
#' @param postcode_col the name of the column containing the postcode
#'
#' @return an sf data frame, containing a formatted postcodes column alongside the original
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' postcodes_df <- data.frame(postcode = c("AB101AB", "AB10 1AF", "ab10 1ag"))
#' postcodes_sf <- postcode2point(postcodes_df, "postcode")
postcode2point <- function(df, postcode_col){
  df_sf <- df %>%
    dplyr::mutate(postcode_formatted = base::toupper(stringr::str_replace_all(!!dplyr::sym(postcode_col), " ", ""))) %>%
    dplyr::left_join(kwt::postcode_lookup, by = c("postcode_formatted" = "postcode")) %>%
    dplyr::filter(!is.na(.data$eastings)) %>%
    sf::st_as_sf(coords = c("eastings", "northings"), crs = 27700)
  return(df_sf)
}
