db <- DBI::dbConnect(odbc::odbc(), dsn = "shared", timeout = 10)
shared <- list_db(db)

test_that("non-spatial table ref works", {
  result <- run_query(ref = shared$cog$acronyms, conn = db)
  expect_equal(class(result)[3], "data.frame")
})
test_that("spatial table ref works", {
  result <- run_query(ref = shared$kwt_reserves$kwt_reserves, conn = db)
  expect_equal(class(result)[1], "sf")
})
test_that("non-spatial view ref works", {
  result <- run_query(ref = shared$testing$view, conn = db)
  expect_equal(class(result)[3], "data.frame")
})
test_that("spatial view ref works", {
  result <- run_query(ref = shared$cog$acquisitions_disposals_all, conn = db)
  expect_equal(class(result)[1], "sf")
})
