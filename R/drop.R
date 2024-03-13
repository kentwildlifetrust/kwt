drop <- function(tbl, conn = db){
  if (tbl$type == "view") {
    query <- glue::glue_sql("DROP VIEW {`tbl$table_schema`}.{`tbl$table_name`}",
                            .con = conn)
  } else if (tbl$type == "matview") {
    query <- glue::glue_sql("DROP MATERIALIZED VIEW {`tbl$table_schema`}.{`tbl$table_name`}",
                            .con = conn)
  } else if (tbl$type == "table") {
    query <- glue::glue_sql("DROP TABLE {`tbl$table_schema`}.{`tbl$table_name`}",
                            .con = conn)
  } else {
    stop("Table type not defined")
  }
  DBI::dbExecute(conn, query)
  message(glue::glue("Table {tbl$table_schema}.{tbl$table_name} dropped"))
}
