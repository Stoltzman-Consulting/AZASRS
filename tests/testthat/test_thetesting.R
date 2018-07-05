context("Testing the Testing")

test_that("One equals one", {
  expect_equal(1,1)
})

test_that("a equals b fails", {
  expect_equal('a','b')
})
