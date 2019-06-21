context("test-database_connection")

test_that("Can connect to database",{
  con = AZASRS_DATABASE_CONNECTION()
  expect_true(length(dplyr::src_tbls(con)) > 1)
})
