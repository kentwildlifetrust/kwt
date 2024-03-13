#' Run a query and return the result as a data frame or sf
#'
#' @param x a dplyr tbl object or a character string containing a SQL query.
#' @param conn a connection object. Only required if x is a character string.
#' @param geom_col the name of the geometry column in the query result. If NULL, the result will be returned as a data frame.
#' @param crs the coordinate reference system of the geometry column to be returned in the result. Defaults to 4326.
#'
#' @return a data frame or sf object
#' @export
#'
#'
run_query <- function(x, geom_col = NULL, crs = 4326, conn = db){
  if ("character" %in% class(x)) {
    x <- dplyr::tbl(conn, x)
  }

  if (!is.null(geom_col)) {
    result <- x %>%
      dplyr::mutate("{geom_col}" := dbplyr::sql(paste0("ST_AsText(ST_Transform(", geom_col, ", ", crs, "))"))) %>%
      dplyr::collect() %>%
      dplyr::mutate("{geom_col}" := sf::st_as_sfc(!!rlang::sym(geom_col))) %>%
      sf::st_as_sf()
    sf::st_crs(result) <- crs
  } else {
    result <- x %>%
      dplyr::collect()
  }

  return(result)
}


# query <- glue::glue_sql("SELECT column_name FROM information_schema.columns WHERE table_schema = {tbl$table_schema} AND table_name = {tbl$table_name} AND data_type = 'USER-DEFINED' AND udt_name = 'geometry'",
#                         .con = conn)
# geom_col <- DBI::dbGetQuery(conn, query)
