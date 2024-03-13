#' Function for creating a polygon from a grid reference, with the size of the polygon dependant on the precision of the grid reference
#'
#' This function creates an sf polygon object, .
#' @param df The data frame, must include a column containing grid references
#' @param grid_ref_col The name of the column containing the grid references
#' @export
#' @examples
#' df <- data.frame(gridref = c("SO857197", "TQ327224", "SK797470"))
#' coords <- os2point(df, grid_ref_col = "gridref")

os2point <- function(df, grid_ref_col = "grid_ref"){
  df <- df %>%
    dplyr::rename(grid_ref = grid_ref_col) %>%
    dplyr::mutate(grid_ref =  gsub(" ", "", grid_ref, fixed = TRUE)) %>%
    dplyr::filter(grepl("^[a-zA-Z]{2}[0-9]{4,12}$", grid_ref) | grepl("^[a-zA-Z]{2}[0-9]{2}[a-zA-Z]{1}$", grid_ref_col))

  coords <- os2en(df$grid_ref)

  df$Easting <- coords$Easting
  df$Northing <- coords$Northing

  df <- df %>%
    dplyr::mutate(
      Easting = coords$Easting,
      Northing = coords$Northing,
      geometry = paste0("POINT (",
                        Easting,
                        " ",
                        Northing,
                        ")")) %>%
    sf::st_as_sf(crs = 27700, wkt = "geometry")

  df
}


