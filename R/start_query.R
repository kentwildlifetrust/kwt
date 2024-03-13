#' Create a query from a table reference
#'
#' @details This function generates a dplyr lazy tbl object from a table reference in the format provided by list_tables().
#' This is useful for creating an sql query with dplyr syntax.
#'
#' @param ref a table reference in the format provided by list_tables()
#' @param conn a connection object
#'
#' @return a dplyr tbl object
#' @export
#'
#'
start_query <- function(ref, conn = db){
  return(dplyr::tbl(conn, dbplyr::in_schema(ref$table_schema, ref$table_name)))
}
