test_that("Actual and predicted have the same dimension", {
  a1 = matrix(c(1,1,1,2),2,2)
  p1 = c(0,1,1)
  expect_error(calc_acc(a1, p1), "actual and predicted do not have the same dimension")
})

test_that("NA in one vector",{
  a2 = c(1,2,3)
  p2 = c(1,NA,3)
  expect_equal(is.na(calc_acc(a2, p2)), TRUE)
})

test_that("NaN in one vector",{
  a3 = c(1,2,3)
  p3 = c(1,NaN,3)
  expect_equal(is.na(calc_acc(a3, p3)), TRUE)
})

test_that("Na in one vector",{
  a4 = c(1,NA,3)
  p4 = c(1,NA,3)
  expect_equal(is.na(calc_acc(a4, p4)), TRUE)
})
