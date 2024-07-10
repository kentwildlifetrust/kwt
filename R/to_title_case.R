#' Convert a string from snake_case to a pretty title case
#' @description Takes a character vector and converts it to a character vector of nice looking titles,
#'     taking into account common acronyms and abbreviations.
#' @param name A string in snake_case
#'
#' @return A character vector of names in title case
#' @export
#'
#' @examples
#' to_title_case("my_column_name")
#' # "My Column Name"
#'
#' to_title_case(c("kwtid", "img_url", "geom"))
#' #  "KWTID"     "Image URL" "Geometry"
#'
to_title_case <- function(name){
  sapply(name, function(name){
    stopifnot(is.character(name))
    vec <- strsplit(name, "_") %>%
      unlist() %>%
      stringr::str_to_title() %>%
      dplyr::recode(
        "Id" = "ID",
        "Url" = "URL",
        "Kwtid" = "KWTID",
        "Img" = "Image",
        "Geom" = "Geometry",
        "Bng" = "BNG"
      ) %>%
      paste0(collapse = " ")
    return(vec)
  }) %>%
    unname()
}
