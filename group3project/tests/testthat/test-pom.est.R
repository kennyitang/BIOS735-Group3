set.seed(2)
dat = data.frame(y = sample(1:5, 100, replace=TRUE, prob = rep(0.2, 5)),
                 x1 = sample(c("a", "b", "c"), 100, replace=TRUE), 
                 x2 = rnorm(100, 0, 200),
                 x3 = c(rnorm(50, 3, 0.005), rnorm(50, -3, 0.5)))


test_that("actual and predicted have the same dimension", {
  expect_error(pom.est(y~x1, data=dat), "Dependent variable must be an ordered factor.")
})

dat$y = factor(dat$y, ordered=TRUE)

test_that("X not scaled", {
  expect_error(pom.est(y ~ x1 + x2, data=dat), "Gradient function might be wrong")
})

dat$x2 = scale(dat$x2)
dat$x3 = scale(dat$x3)

test_that("Estimates close to that produced by polr",{
  fit1 = pom.est(y ~ x1 + x2, data=dat)
  beta1 = fit$Estimates
  fit2 = MASS::polr(y~x1 + x2, data=dat)
  beta2 = as.numeric(c(fit2$zeta, fit2$coefficients))
  expect_equal(beta1, beta2, tolerance=10^-4)
})

