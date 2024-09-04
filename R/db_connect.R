#' Connect to a database
#' @description Create a connection object to a PostgreSQL database using an ODBC connection. For details about setting up an ODBC connection, see [this guide](https://kentwildlifetrust.github.io/kwt/articles/odbc_database_connection.html).
#' @param connection_name The name of the connection to connect to. This is the name of the connection as it appears in Connections pane in RStudio.
#'
#' @return A list containing an ODBC connection object to a PostgreSQL database, as well as a nested list of the schemas, tables and views in the database.
#' @export
#'
#' @examples
#' \dontrun{
#' db <- db_connect("shared")
#' }
db_connect <- function(connection_name){
  cat("Connecting\n")
  stopifnot(is.character(connection_name))

  tryCatch({
    conn <- DBI::dbConnect(odbc::odbc(), dsn = connection_name, timeout = 10)
  }, error = function(e){
    if (e$message == "ODBC failed with error IM002 from \\\n             \033[32m[Microsoft][ODBC Driver Manager]\033[39m.")
      stop("Connection name not found. Please check the connection name.")
    else if (e$message == "ODBC failed with error 08001 from \\\n             .")
      stop("Connection failed. Please check the username & password using the 'ODBC Data Sources 64-bit' desktop application. You can ask Euan in the Digital Development Team for a reminder of your credentials.")
    else
      stop(e$message)
  })

  database_name <- DBI::dbGetQuery(conn, "SELECT current_database()")$current_database
  cat("Connected to", database_name, "\n")

  cat("Finding contents\n")

  db_list <- list_db(conn)

  cat("Done\n")

  return(
    list(
      conn = conn,
      schemas = db_list
    )
  )
}
