% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db_connect.R
\name{db_connect}
\alias{db_connect}
\title{Connect to a database}
\usage{
db_connect(connection_name)
}
\arguments{
\item{connection_name}{The name of the connection to connect to. This is the name of the connection as it appears in Connections pane in RStudio.}
}
\value{
A listed_connection, which is a list containing an ODBC connection object to a PostgreSQL database, as well as a nested list of references to schemas, tables and views in the database.
}
\description{
Create a connection object to a PostgreSQL database using an ODBC connection. For details about setting up an ODBC connection, see \href{https://kentwildlifetrust.github.io/kwt/articles/odbc_database_connection.html}{this guide}.
}
\examples{
\dontrun{
db <- db_connect("shared")
}
}
