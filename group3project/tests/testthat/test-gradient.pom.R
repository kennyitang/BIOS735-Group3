set.seed(2)
y = sample(1:5, 100, replace=TRUE, prob = rep(0.2, 5))
X = data.frame(x1 = sample(c("a", "b", "c"), 100, replace=TRUE), 
               x2 = rnorm(100, 0, 1))

test_that("Categorical variables in X need to be dummy coded", {
  expect_error(gradient.pom(rep(0.1, 6),y, X))
})

X = model.matrix(~x1+x2,data=X)[,-1]

test_that("Dimensions of param and X need to conform", {
  expect_error(gradient.pom(rep(0.1,3), y,X), "non-conformable")
})

test_that("y is not an ordered factor response", {
  y = sample(c("a", "b", "c"), 100, replace = TRUE)
  expect_error(gradient.pom(rep(0.1, 7), y, X))
})


