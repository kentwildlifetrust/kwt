#' Function for creating a polygon from a grid reference, with the size of the polygon dependent on the precision of the grid reference
#'
#' This function creates an sf polygon object, .
#' @param df The data frame, must include a column containing grid references
#' @param grid_ref_col The name of the column containing the grid references
#' @export
#' @examples
#' df <- data.frame(gridref = c("SO85719789", "TQ327224", "SK7974", "TF73G"))
#' polys <- os2poly(df, grid_ref_col = "gridref")

os2poly <- function(df, grid_ref_col = "grid_ref"){
  df <- df %>%
    dplyr::rename(grid_ref = grid_ref_col) %>%
    dplyr::mutate(grid_ref =  gsub(" ", "", grid_ref, fixed = TRUE)) %>%
    dplyr::filter(grepl("^[a-zA-Z]{2}[0-9]{4,12}$", grid_ref) | grepl("^[a-zA-Z]{2}[0-9]{2}[a-zA-Z]{1}$", grid_ref))

  coords <- os2en(df$grid_ref)

  df$SWE = coords$Easting
  df$SWN = coords$Northing

  df <- df %>%
    dplyr::mutate(l = as.numeric(nchar(grid_ref)),
                  SEE =  dplyr::case_when(l == 5 ~ SWE + 2000,
                                          l == 6 ~ SWE + 1000,
                                          l == 8 ~ SWE + 100,
                                          l == 10 ~ SWE + 10,
                                          l == 12 ~ SWE + 1,
                                          TRUE ~ SWE),
                  SEN = SWN,
                  NWN =  dplyr::case_when(l == 5 ~ SWN + 2000,
                                          l == 6 ~ SWN + 1000,
                                          l == 8 ~ SWN + 100,
                                          l == 10 ~ SWN + 10,
                                          l == 12 ~ SWN + 1,
                                          TRUE ~ SWN),
                  NWE = SWE,
                  NEE =  dplyr::case_when(l == 5 ~ SWE + 2000,
                                          l == 6 ~ SWE + 1000,
                                          l == 8 ~ SWE + 100,
                                          l == 10 ~ SWE + 10,
                                          l == 12 ~ SWE + 1,
                                          TRUE ~ SWE),
                  NEN =  dplyr::case_when(l == 5 ~ SWN + 2000,
                                          l == 6 ~ SWN + 1000,
                                          l == 8 ~ SWN + 100,
                                          l == 10 ~ SWN + 10,
                                          l == 12 ~ SWN + 1,
                                          TRUE ~ SWN),
                  geometry = paste0("POLYGON ((",
                                    as.integer(SWE), " ", as.integer(SWN), ", ",
                                    as.integer(NWE), " ", as.integer(NWN), ", ",
                                    as.integer(NEE), " ", as.integer(NEN), ", ",
                                    as.integer(SEE), " ", as.integer(SEN), ", ",
                                    as.integer(SWE), " ", as.integer(SWN),
                                    "))")
    ) %>%
    dplyr::select(-SWE, -SWN, -l, -SEE, -SEN, -NWN, -NWE, -NEE, NEN) %>%
    sf::st_as_sf(crs = 27700, wkt = "geometry")
  df

}
