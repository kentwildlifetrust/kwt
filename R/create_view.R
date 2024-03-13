create_view <- function(x, schema_name, view_name, matview = F, conn = db){
  #check if view already exists
  query <- glue::glue_sql("SELECT EXISTS (SELECT 1 FROM information_schema.tables WHERE table_schema = {schema_name} AND table_name = {view_name})",
                          .con = conn)
  tbl_or_view_exits <- DBI::dbGetQuery(conn, query)

  query <- glue::glue_sql("select  EXISTS (SELECT 1 FROM pg_matviews WHERE schemaname = {schema_name} AND matviewname = {view_name})",
                          .con = conn)
  matview_exits <- DBI::dbGetQuery(conn, query)

  if (tbl_or_view_exits$exists %in% 1 || matview_exits$exists %in% 1) {
    stop(paste0("Error: ", schema_name, ".", view_name, " already exists"))
  }

  #check if x is a character or a tbl
  if ("tbl" %in% class(x)) {
    x <- dbplyr::sql_render(x)
  }

  #make the view
  if (matview) {
    query <- glue::glue("CREATE MATERIALIZED VIEW {`schema_name`}.{`view_name`} AS \r\n {x}")
  } else {
    query <- glue::glue("CREATE VIEW {`schema_name`}.{`view_name`} AS \r\n {x}")
  }

  DBI::dbExecute(conn, query)
  message(paste0(
    if (matview) {
      "Materialised view"
    } else {
      "View"
    },
    schema_name, ".", view_name, " created"
  ))
}

