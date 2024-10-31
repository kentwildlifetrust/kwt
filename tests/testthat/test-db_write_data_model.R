test_that("db_to_json_data_model works for 1 schema", {
  #read wilder carbon database
  wc <- DBI::dbConnect(odbc::odbc(), dsn = "Wilder Carbon", timeout = 10)
  filepath <- tempfile(fileext = ".json")
  db_to_json_data_model(wc, filepath, schemas = "carbon_plus_carbon_lookups")
  expect_true(file.exists(filepath))

  if (interactive()) {
    model <- read_json_data_model(filepath)
    model <- model_relational_data(model)
  }
  DBI::dbDisconnect(wc)
})

test_that("db_to_json_data_model works for 2 schemas", {
  #read wilder carbon database
  wc <- DBI::dbConnect(odbc::odbc(), dsn = "Wilder Carbon", timeout = 10)
  filepath <- tempfile(fileext = ".json")
  db_to_json_data_model(wc, filepath, schemas = c("carbon_plus_bng_lookups", "carbon_plus_carbon_lookups"))
  expect_true(file.exists(filepath))

  if (interactive()) {
    model <- read_json_data_model(filepath)
    model <- model_relational_data(model)
  }
  DBI::dbDisconnect(wc)
})


test_that("db_to_json_data_model works for all schemas", {
  #read wilder carbon database
  pp <- DBI::dbConnect(odbc::odbc(), dsn = "Planning", timeout = 10)
  filepath <- tempfile(fileext = ".json")
  db_to_json_data_model(pp, filepath)
  expect_true(file.exists(filepath))

  if (interactive()) {
    model <- read_json_data_model(filepath)
    model <- model_relational_data(model)
  }
  DBI::dbDisconnect(pp)
})
