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

  #get tables
  query <- "SELECT table_schema, table_name FROM information_schema.tables WHERE table_schema NOT IN ('pg_catalog', 'information_schema') ORDER BY table_schema, table_name;"
  tables <- DBI::dbGetQuery(conn, query)

  #get view definitions
  views <- DBI::dbGetQuery(conn, "select schemaname, viewname from pg_views where schemaname NOT IN ('pg_catalog', 'information_schema');") %>%
    dplyr::mutate(type = "view")
  mat_views <- DBI::dbGetQuery(conn, "select schemaname, matviewname from pg_matviews where schemaname NOT IN ('pg_catalog', 'information_schema');") %>%
    dplyr::mutate(type = "matview")

  #join
  tables <- tables %>%
    dplyr::full_join(views, dplyr::join_by(table_schema == schemaname, table_name == viewname)) %>%
    dplyr::full_join(mat_views, dplyr::join_by(table_schema == schemaname, table_name == matviewname)) %>%
    dplyr::mutate(type = dplyr::coalesce(type.x, type.y)) %>%
    dplyr::mutate(type = ifelse(is.na(type), "table", type)) %>%
    dplyr::select(table_schema,
                  table_name,
                  type)

  list_structure <- split(tables, tables$table_schema)

  # Apply the function to each subset and combine into a single list
  db_list <- lapply(list_structure, transform_to_list)

  #get columns for tables & views
  query <- "SELECT table_schema, table_name, column_name, ordinal_position, is_nullable, data_type, udt_name FROM information_schema.columns WHERE table_schema NOT IN ('pg_catalog', 'information_schema') ORDER BY table_schema, table_name;"
  columns_1 <- DBI::dbGetQuery(conn, query)

  #get columns for materialised views
  #not sure how to get is_nullable or if this is even relevant for mat_views
  query <- "SELECT
                n.nspname AS table_schema,
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
            ORDER BY
                n.nspname,
                c.relname,
                a.attnum;"
  columns_2 <- DBI::dbGetQuery(conn, query)

  columns <- rbind(columns_1, columns_2)

  columns <- split(columns, columns$table_schema, drop = T) %>%
    lapply(function(x){
      split(x, x$table_name, drop = T) %>%
        lapply(function(y) {
          dplyr::select(y, -table_schema, -table_name) %>%
            dplyr::arrange(ordinal_position)
        }) %>%
        return()
    })

  for (schema in names(columns)){
    for (table in names(columns[[schema]])) {
      db_list[[schema]][[table]]$atts <- columns[[schema]][[table]]
    }
  }

  return(db_list)
}

transform_to_list <- function(sub_df) {
  list <- lapply(1:nrow(sub_df), function(i) return(as.list(sub_df[i, !is.na(sub_df[i,])])))
  names(list) <- sub_df$table_name
  return(list)
}
