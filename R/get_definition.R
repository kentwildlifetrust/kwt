#' Get the Definition of a View or Materialised View
#'
#' @param ref a table reference in the format provided by list_tables()
#' @param conn a connection object
#'
#' @return a character string containing the definition of the view or materialised view in SQL
#' @export
#'
#'
get_definition <- function(ref, conn = db){
  if (!(ref$type %in% c("view", "matview"))) {
    stop("Error: table is not a view or materialised view")
  }
  query <- glue::glue_sql("SELECT pg_get_viewdef({paste0(ref$table_schema, '.', ref$table_name)})",
                          .con = conn)
  definition <- DBI::dbGetQuery(conn, query)
  return(definition$pg_get_viewdef)
}
