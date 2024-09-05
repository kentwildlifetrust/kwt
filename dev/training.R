devtools::install_github("kentwildlifetrust/kwt")

db <- db_connect("shared")

tbl <- get_lazy(db$testing$kent_county)

sf <- run_lazy(tbl)

plot(sf$geom)

df <- get_lazy(db$testing$apports)
df <- run_lazy(df)


DBI::dbWriteTable(db$conn, DBI::Id(schema = "testing", table = "apports_2"), df)

sf::st_write(sf, db$conn, DBI::Id(schema = "testing", table = "kent_county_2"))


sf <- sf::st_read(db, query = "SELECT * FROM habitats.ceh_hedgerows LIMIT 1000;")
sf::st_write(sf, "C:/Users/euan.mckenzie/Downloads/ceh_hedgerows_sample.gpkg")
