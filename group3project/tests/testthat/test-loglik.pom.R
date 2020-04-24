set.seed(2)
y = sample(1:5, 100, replace=TRUE, prob = rep(0.2, 5))
X = data.frame(x1 = sample(c("a", "b", "c"), 100, replace=TRUE), 
               x2 = rnorm(100, 0, 1))

test_that("Categorical variables in X need to be dummy coded", {
  expect_error(loglik.pom(y, X, rep(0.1, 6)))
})

X = model.matrix(~x1+x2,data=X)[,-1]

test_that("Dimensions of param and X need to conform", {
  expect_error(loglik.pom(y,X, rep(0.1,3)), "conform")
})

test_that("y is not an ordered factor response", {
  y = sample(c("a", "b", "c"), 100, replace = TRUE)
  expect_error(loglik.pom(y, X, rep(0.1, 7)))
})


