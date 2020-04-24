set.seed(2)
y = sample(1:5, 100, replace=TRUE, prob = rep(0.2, 5))
X = data.frame(x1 = sample(c("a", "b", "c"), 100, replace=TRUE), 
               x2 = sample(c(TRUE, FALSE), 100, replace=TRUE),
               x3 = rnorm(100, 0, 1))

test_that("No intercept", {
  expect_false(sum(recode.factor(~x1+x2+x3,data=X)[,1]==rep(1, dim(X)[1]))==dim(X)[1])
})
