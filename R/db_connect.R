#' Connect to a database
#' @description Create a connection object to a PostgreSQL database using an ODBC connection. For details about setting up an ODBC connection, see [this guide](https://kentwildlifetrust.github.io/kwt/articles/odbc_database_connection.html).
#' @param connection_name The name of the connection to connect to. This is the name of the connection as it appears in Connections pane in RStudio.
#'
#' @return A listed_connection, which is a list containing an ODBC connection object to a PostgreSQL database, as well as a nested list of references to schemas, tables and views in the database.
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
    if (grepl("IM002", e$message))
      stop("Connection name not found. Please check the connection name.")
    else if (grepl("08001", e$message))
      stop("Connection failed. Please check the username & password using the 'ODBC Data Sources 64-bit' desktop application. If you need a reminder of your credentials, please get in touch with Euan in the Digital Development Team.")
    else
      stop(e$message)
  })

  database_name <- DBI::dbGetQuery(conn, "SELECT current_database()")$current_database
  cat("Connected to", database_name, "\n")

  cat("Finding contents\n")

  db_list <- list_db(conn)

  cat("Done\n")

  result <- list(
    conn = conn
  ) %>%
    c(db_list)

  class(result) <- "listed_connection"
  return(result)
}
