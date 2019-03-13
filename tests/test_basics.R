context('Test functions for the basic connections and tables')

test_that("Can connect to database",{
  con = AZASRS_DATABASE_CONNECTION()
  expect_true(length(dplyr::src_tbls(con)) > 1)
})


test_that("",{

})


test_that("",{

})
