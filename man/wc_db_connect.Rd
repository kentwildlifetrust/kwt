% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wc_db_connect.R
\name{wc_db_connect}
\alias{wc_db_connect}
\title{Establishing a database connection to the Wilder Carbon Registry PostGIS database}
\usage{
wc_db_connect(
  username = Sys.getenv("wc_user"),
  password = Sys.getenv("wc_password")
)
}
\arguments{
\item{username}{postgres username, default autoreads wc_user from global .renviron file, see usethis::edit_r_environ() for details}

\item{password}{postgres password, default autoreads wc_user from global .renviron file, see usethis::edit_r_environ() for details}
}
\description{
Establishing a database connection to the Wilder Carbon Registry PostGIS database
}
\examples{
conn <- wc_db_connect()
}
