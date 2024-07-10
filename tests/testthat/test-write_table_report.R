test_that("write table report works with geometry", {
  db <- DBI::dbConnect(odbc::odbc(), dsn = "shared", timeout = 10)
  shared <- list_db(db)
  filepath <- tempfile(fileext = ".html")
  write_table_report(ref = shared$farmer_clusters$farmer_cluster_areas,
                     filepath = filepath,
                     conn = db)
  expect_true(file.exists(filepath))
})
test_that("write table report works with no geometry", {
  db <- DBI::dbConnect(odbc::odbc(), dsn = "shared", timeout = 10)
  shared <- list_db(db)
  filepath <- tempfile(fileext = ".html")
  write_table_report(ref = shared$electoral_data$kent_westminster_2024_candidates,
                     filepath = filepath,
                     conn = db)
  expect_true(file.exists(filepath))
})
