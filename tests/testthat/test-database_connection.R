context("test-database_connection")

test_that("Can connect to database",{
  con = AZASRS_DATABASE_CONNECTION()
  expect_true(length(dplyr::src_tbls(con)) > 1)
})

test_that("Database location is production and not local", {
  expect_equal(AZASRS_DATABASE_LOCATION, "P:\\IMD\\2018 Database Project\\Database\\asrs_database.db")
})

test_that("Checking data against directory is in the right location", {
  expect_equal(AZASRS_TEST_DATA_DIRECTORY, "P:/IMD/2018 Database Project/Application Data/etl_check_data/")
})
