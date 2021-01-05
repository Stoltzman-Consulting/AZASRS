context("test-build_get_url")

test_that("build_get_url() returns character string of length 1", {
  url_to_use <- build_get_url("pm_fund_info")

  expect_equal(class(url_to_use), "character")

  expect_equal(length(url_to_use), 1)

  #testing that no input into function produces an error
  expect_error(build_get_url(), "argument \"data_request\" is missing, with no default")

  #check that removing MCR_DATA_RETRIEVAL_TOKEN throws error
  Sys.setenv(AZASRS_DATA_RETRIEVAL_TOKEN = '')
  expect_error(build_get_url("pm_fund_info"), "AZASRS_DATA_RETRIEVAL_TOKEN does not exist, check .Renviron file")

  #check that removing MCR_BASE_URL throws error, (bringing back token)
  Sys.setenv(AZASRS_DATA_RETRIEVAL_TOKEN = "TOKENHERE")
  Sys.setenv(AZASRS_BASE_URL = '')
  expect_error(build_get_url("pm_fund_info"), "AZASRS_BASE_URL does not exist, check .Renviron file")

})
