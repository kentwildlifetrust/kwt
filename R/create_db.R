# create_db <- function(conn = db, db_name = "my_db"){
#
#   #get script from SQL folder in package directory
#   script <- system.file("SQL/create_db.sql", package = "kwt") %>%
#     readLines() %>%
#     paste(collapse = "\n")
#
#   vals <- list(
#     admin_user = Sys.getenv("admin_user"),
#     admin_password = Sys.getenv("admin_password"),
#     db_name = db_name
#   )
#
#   query <- glue::glue_data_sql(vals, script, .con = conn)
#   dbExecute(conn, query)
# }
