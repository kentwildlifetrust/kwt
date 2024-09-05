#' Run a query and return the result as a data frame or sf
#'
#' @param x a dplyr tbl object or a character string containing a SQL query.
#' @param ref a list containing the table schema and name.
#' @param conn a connection object. Only required if x is a character string.
#'
#' @return a data frame or sf object
#' @export
#'
#'
run_lazy <- function(lazy_tbl){
  if (!"tbl_lazy" %in% class(lazy_tbl)) {
    stop("lazy_tbl must be an lazy table created by kwt::get_lazy")
  }

  #get the sql query from the lazy_tbl
  sql_string <- dbplyr::sql_render(lazy_tbl)

  #get schema and table names
  schema_table_name <- stringr::str_extract(sql_string, "(?<=FROM\\s)([^\\s]+)")
  schema_table_split <- strsplit(schema_table_name, "\\.")[[1]]
  schema_name <- ifelse(length(schema_table_split) > 1, schema_table_split[1], NA) %>%
    stringr::str_replace_all('"', "")
  table_name <- schema_table_split[length(schema_table_split)] %>%
    stringr::str_replace_all('"', "")

  #get the database connection
  conn <- lazy_tbl$src$con

  #get the columns, assuming table or view
  atts <- "SELECT table_schema AS schema_name,
                   table_name,
                   column_name,
                   ordinal_position,
                   is_nullable,
                   data_type,
                   udt_name
           FROM information_schema.columns WHERE
           table_schema = {schema_name} AND table_name = {table_name};" %>%
    glue::glue_sql(.con = conn) %>%
    DBI::dbGetQuery(conn, .)

  #if the columns not found, try materialized view
  if (nrow(atts) == 0) {
    atts <- "SELECT
                  n.nspname AS schema_name,
                  c.relname AS table_name,
                  a.attname AS column_name,
                  a.attnum AS ordinal_position,
                  NULL::boolean AS is_nullable,
                  pg_catalog.format_type(a.atttypid, a.atttypmod) AS data_type,
                  a.atttypid::regtype AS udt_name
              FROM
                  pg_catalog.pg_attribute a
              JOIN
                  pg_catalog.pg_class c ON a.attrelid = c.oid
              JOIN
                  pg_catalog.pg_namespace n ON c.relnamespace = n.oid
              WHERE
                  c.relkind = 'm'  -- 'm' stands for materialized view
                  AND a.attnum > 0
                  AND NOT a.attisdropped
                  AND n.nspname = {schema_name}
                  AND c.relname = {table_name};" %>%
      glue::glue_sql(.con = conn) %>%
      DBI::dbGetQuery(conn, .)
  }

  if (nrow(atts) == 0) {
    stop("Table attributes not found")
  }

  if ("geometry" %in% atts$udt_name) {
    if (sum("geometry" %in% atts$udt_name) > 1) {
      warning("Multiple geometry columns. The first one will be used")
    }

    geom_col <- atts %>%
      dplyr::filter(udt_name == "geometry") %>%
      dplyr::slice(1) %>%
      dplyr::pull(column_name)

    #find the crs
    query <- glue::glue_sql("SELECT DISTINCT ST_SRID({`geom_col`}) AS srid FROM {`schema_name`}.{`table_name`};",
                            .con = conn)
    crs <- DBI::dbGetQuery(conn, query)$srid
    crs <- crs[!is.na(crs)]

    if (length(crs) > 1) {
      stop("Multiple crs found. Make sure all your geometries have the same crs and none are empty.")
    }

    #get the result as an sf object with correct crs
    result <- lazy_tbl %>%
      dplyr::filter(!is.na(!!rlang::sym(geom_col))) %>%
      dplyr::mutate("{geom_col}" := dbplyr::sql(paste0("ST_AsText(ST_Transform(", geom_col, ", ", crs[1], "))"))) %>%
      dplyr::collect() %>%
      dplyr::mutate("{geom_col}" := sf::st_as_sfc(!!rlang::sym(geom_col))) %>%
      sf::st_as_sf()
    sf::st_crs(result) <- crs
    return(result)
  }

  #get the result as a data frame
  result <- lazy_tbl %>%
    dplyr::collect()

  return(result)
}


# query <- glue::glue_sql("SELECT column_name FROM information_schema.columns WHERE table_schema = {tbl$table_schema} AND table_name = {tbl$table_name} AND data_type = 'USER-DEFINED' AND udt_name = 'geometry'",
#                         .con = conn)
# geom_col <- DBI::dbGetQuery(conn, query)
