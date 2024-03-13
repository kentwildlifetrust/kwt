#' kwt_db_connect
#'
#' @param db name of the internal database within postgres, shared is the standard
#' @param username postgres username, default autoreads kwt_user from global .renviron file, see usethis::edit_r_environ() for details
#' @param password postgres password, default autoreads kwt_password from global .renviron file, see usethis::edit_r_environ() for details
#'
#' @export
#'
#' @examples
#' conn <- kwt_db_connect()
kwt_db_connect <- function(db = "shared",
                           username = Sys.getenv("kwt_user"),
                           password = Sys.getenv("kwt_password")) {
  DBI::dbConnect(drv = RPostgres::Postgres(),
                       host = Sys.getenv("kwt_host"),
                       port = 5432,
                       dbname = db,
                       user = username,
                       password = password,
                       sslmode = 'prefer')

  }
