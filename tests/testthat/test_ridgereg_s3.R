suppressWarnings(context("ridgereg"))

test_that("ridgereg coefficients are similar to lm.ridge", {
  data(mtcars)
  lambda <- 0.1
  ridge_model <- ridgereg(mpg ~ ., data = mtcars, lambda = lambda)
  ridge_mass <- MASS::lm.ridge(mpg ~ ., data = mtcars, lambda = lambda)
  coeff_ridgereg <- as.numeric(ridge_model$coefficients)
  coeff_lmridge <- as.numeric(ridge_mass$coef)
  expect_equal(coeff_ridgereg[-1], coeff_lmridge, tolerance = 1e-1)
})
