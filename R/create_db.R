#' create_db
#'
#' @param conn A DBI database connection connecting to the postgres database with the admin login
#' @param db_name The name of the database to be created
#' @param app_role The name of the user to be created with the database
#'
#' @return The details of the database and user created
#' @export
#'
#' @examples
#' conn <- pool::dbPool(drv = RPostgres::Postgres(), host = "xxxxxxx", port = 5432, dbname = "postgres", user = "xxxxx", password = "xxxxxxx", sslmode = "prefer")
#' create_db(conn, "test_db", "test_user")

create_db <- function(conn, db_name, app_role){
  # Create database based on temnplate
  DBI::dbExecute(conn, glue::glue_sql("CREATE DATABASE {DBI::dbQuoteIdentifier(conn,db_name)} TEMPLATE kwt_database_template;", .con=conn))
  # Write the app_role to the logins table and fetch the password
  password<-DBI::dbGetQuery(conn, glue::glue_sql("WITH updated_row AS (
                                                  UPDATE admin.logins
                                                  SET username = {DBI::dbQuoteString(conn,app_role)}
                                                  WHERE kwtid = (
                                                    SELECT MIN(kwtid)
                                                    FROM admin.logins
                                                    WHERE (username IS NULL OR username = '')
                                                  )
                                                  RETURNING password
                                                )
                                                SELECT * FROM updated_row;", .con=conn))
  # Create the user with app_role and fetched password
  DBI::dbExecute(conn, glue::glue_sql("CREATE USER {DBI::dbQuoteIdentifier(conn,app_role)} WITH LOGIN NOSUPERUSER NOCREATEDB NOCREATEROLE INHERIT NOREPLICATION CONNECTION LIMIT -1 PASSWORD {password};", .con=conn))
  # Print to console
  print(glue::glue("Database {db_name} created and user {app_role} created with password {password}"))
}

