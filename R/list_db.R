#' List all schemas, tables and views in a database
#'
#' @param conn a connection object
#'
#' @return a list of schemas. Each schema is a list of refs for tables, views and materialised views in that schema. Each ref is a list containing three values:
#'
#' * table_schema: the schema the table/view is in
#' * table_name: the name of the table/view
#' * type: the type of the table/view. Either "table", "view" or "matview"
#'
#' @export
#'
#'

list_db <- function(conn = db){
  query <- "SELECT table_schema, table_name FROM information_schema.tables WHERE table_schema NOT IN ('pg_catalog', 'information_schema') ORDER BY table_schema, table_name;"
  # Execute the query
  info_schema <- DBI::dbGetQuery(conn, query)

  #get view definitions
  views <- DBI::dbGetQuery(conn, "select schemaname, viewname from pg_views where schemaname NOT IN ('pg_catalog', 'information_schema');") %>%
    dplyr::mutate(type = "view")
  mat_views <- DBI::dbGetQuery(conn, "select schemaname, matviewname from pg_matviews where schemaname NOT IN ('pg_catalog', 'information_schema');") %>%
    dplyr::mutate(type = "matview")

  #join
  info_schema <- info_schema %>%
    dplyr::full_join(views, dplyr::join_by(table_schema == schemaname, table_name == viewname)) %>%
    dplyr::full_join(mat_views, dplyr::join_by(table_schema == schemaname, table_name == matviewname)) %>%
    dplyr::mutate(type = dplyr::coalesce(type.x, type.y)) %>%
    dplyr::mutate(type = ifelse(is.na(type), "table", type)) %>%
    dplyr::select(table_schema,
                  table_name,
                  type)

  list_structure <- split(info_schema, info_schema$table_schema)

  # Apply the function to each subset and combine into a single list
  result_list <- lapply(list_structure, transform_to_list)
  return(result_list)
}

transform_to_list <- function(sub_df) {
  list <- lapply(1:nrow(sub_df), function(i) return(as.list(sub_df[i, !is.na(sub_df[i,])])))
  names(list) <- sub_df$table_name
  return(list)
}
