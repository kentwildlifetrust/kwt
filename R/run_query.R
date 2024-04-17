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
run_query <- function(x = NULL, ref = NULL, conn = db){
  if (is.null(x) & is.null(ref)) {
    stop("Must have at least one of x and ref as non-null values")
  }

  if (is.null(x)) {
    #get the whole table if ref argument was provided but x was not
    x <- dplyr::tbl(conn, RPostgres::Id(ref$table_schema, ref$table_name))
  } else  if ("character" %in% class(x)) {
    #if x was provided as a character sting, assume it was an sql query
    x <- dplyr::tbl(conn, x)
  }

  if (is.null(ref)) {
    warning("Geometry not decoded as no ref")
  } else if (ref$type == "matview") {
    warning("Geometry not decoded as ref is a materialized view")
  } else {
    #find the geometry col
    if ("geometry" %in% ref$atts$udt_name) {
      if (sum("geometry" %in% ref$atts$udt_name) > 1) {
        warning("Multiple geometry columns. The first one will be used")
      }

      geom_col <- ref$atts %>%
        dplyr::filter(udt_name == "geometry") %>%
        dplyr::slice(1) %>%
        dplyr::pull(column_name)

      #find the crs
      query <- glue::glue_sql("SELECT DISTINCT ST_SRID({`geom_col`}) AS srid FROM {`ref$table_schema`}.{`ref$table_name`};",
                              .con = conn)
      crs <- DBI::dbGetQuery(conn, query)

      #get the result as an sf object with correct crs
      result <- x %>%
        dplyr::mutate("{geom_col}" := dbplyr::sql(paste0("ST_AsText(ST_Transform(", geom_col, ", ", crs$srid, "))"))) %>%
        dplyr::collect() %>%
        dplyr::mutate("{geom_col}" := sf::st_as_sfc(!!rlang::sym(geom_col))) %>%
        sf::st_as_sf()
      sf::st_crs(result) <- crs$srid
      return(result)
    }
  }

  #get the result as a data frame
  result <- x %>%
    dplyr::collect()

  return(result)
}


# query <- glue::glue_sql("SELECT column_name FROM information_schema.columns WHERE table_schema = {tbl$table_schema} AND table_name = {tbl$table_name} AND data_type = 'USER-DEFINED' AND udt_name = 'geometry'",
#                         .con = conn)
# geom_col <- DBI::dbGetQuery(conn, query)
