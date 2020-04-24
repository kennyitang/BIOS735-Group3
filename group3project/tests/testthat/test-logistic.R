test_that("Input needs to be a number", {
  expect_error(logistic("b"), "invalid argument")
})

test_that("Input needs to be a numeric vector", {
  expect_warning(logistic(factor(c(1,2,3))))
})

test_that("Same output as plogis",{
  t = seq(-2, 2, by=0.25)
  expect_equal(logistic(t), plogis(t))
})

