#' Establishing a database connection to the Wilder Carbon Registry PostGIS database
#'
#' @param username postgres username, default autoreads wc_user from global .renviron file, see usethis::edit_r_environ() for details
#' @param password postgres password, default autoreads wc_user from global .renviron file, see usethis::edit_r_environ() for details
#'
#' @export
#'
#' @examples
#' conn <- wc_db_connect()
wc_db_connect <- function(username = Sys.getenv("wc_user"),
                          password = Sys.getenv("wc_password")) {
  RPostgres::dbConnect(drv = RPostgres::Postgres(),
                       host = Sys.getenv("wc_host"),
                       port = 5432,
                       dbname = "citus",
                       user = username,
                       password = password)

}
