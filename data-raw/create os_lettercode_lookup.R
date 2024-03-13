gridref_lookup <- data.frame(square_code = c("SV", "SW", "SX", "SY", "SZ", "TV",
                                             "SQ", "SR", "SS", "ST", "SU", "TQ", "TR",
                                             "SM", "SN", "SO", "SP", "TL", "TM",
                                             "SG", "SH", "SJ", "SK", "TF", "TG",
                                             "SB", "SC", "SD", "SE", "TA",
                                             "NW", "NX", "NY", "NZ", "OV",
                                             "NQ", "NR", "NS", "NT", "NU", "OQ"),
                             square_easting = c(0:5, 0:6, 1:6, 1:6, 1:5, 1:5, 0:5),
                             square_northing = c(rep(0, 6), rep(1, 7), rep(2, 6), rep(3, 6), rep(4, 5), rep(5, 5), rep(6, 6))) %>%
  dplyr::mutate(number_code = paste0(square_easting, square_northing))
