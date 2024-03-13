#' OS Grid Ref to Eastings and Northings
#'
#' This function creates a data frame containing the easting and northing coordinates for the SW corner of the grid reference.
#' @param gridreference The grid reference to be converted. Can be any resolution.
#' @keywords Grid references
#' @export
#' @examples
#' df <- data.frame(gridref = c("SO857197", "TQ327224", "SK797470"))
#' coords <- os2en(df$gridref)

os2en <- function(gridreference){
  coord_df <- base::data.frame(gridreference) %>%
    dplyr::mutate(gridref = base::gsub(" ", "", base::as.character(gridreference)),
                  square_code = base::toupper(base::substr(.data$gridref, 1, 2)),
                  gridref_numbers = base::substr(.data$gridref, 3, 20),
                  half_length = base::nchar(.data$gridref_numbers)/2,
                  easting_end = dplyr::case_when(.data$half_length == 1.5 ~ paste0(substr(.data$gridref_numbers, 1, 1),
                                                                          dplyr::case_when(substr(.data$gridref_numbers, 3, 3) %in% c("A", "B", "C", "D", "E") ~ "0",
                                                                                           substr(.data$gridref_numbers, 3, 3) %in% c("F", "G", "H", "I", "J") ~ "2",
                                                                                           substr(.data$gridref_numbers, 3, 3) %in% c("K", "L", "M", "N", "P") ~ "4",
                                                                                           substr(.data$gridref_numbers, 3, 3) %in% c("Q", "R", "S", "T", "U") ~ "6",
                                                                                           substr(.data$gridref_numbers, 3, 3) %in% c("V", "W", "X", "Y", "Z") ~ "8",)),
                                                 .data$half_length != 1.5 ~ substr(.data$gridref_numbers, 1, .data$half_length)),
                  northing_end = dplyr::case_when(.data$half_length == 1.5 ~ paste0(substr(.data$gridref_numbers, 2, 2),
                                                                           dplyr::case_when(substr(.data$gridref_numbers, 3, 3) %in% c("A", "F", "K", "Q", "V") ~ "0",
                                                                                            substr(.data$gridref_numbers, 3, 3) %in% c("B", "G", "L", "R", "W") ~ "2",
                                                                                            substr(.data$gridref_numbers, 3, 3) %in% c("C", "H", "M", "S", "X") ~ "4",
                                                                                            substr(.data$gridref_numbers, 3, 3) %in% c("D", "I", "N", "T", "Y") ~ "6",
                                                                                            substr(.data$gridref_numbers, 3, 3) %in% c("E", "J", "P", "U", "Z") ~ "8")),
                                                  .data$half_length != 1.5 ~ substr(.data$gridref_numbers, .data$half_length + 1, 10)),
                  zeros = "0000") %>%
    dplyr::left_join(os_lettercode_lookup, by = "square_code") %>%
    dplyr::mutate(final_easting = as.numeric(substr(paste0(.data$square_easting, .data$easting_end, .data$zeros), 1, 6)),
                  final_northing = as.numeric(substr(paste0(.data$square_northing, .data$northing_end, .data$zeros), 1, 6))) %>%
    dplyr::select(Easting = .data$final_easting, Northing = .data$final_northing)

  return(coord_df)
}


