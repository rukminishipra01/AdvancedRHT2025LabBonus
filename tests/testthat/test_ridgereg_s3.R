#context("ridgereg")

data("iris")

test_that("ridge rejects errounous input", {
  expect_error(ridgereg(Petal.Length~Sepdsal.Width+Sepal.Length, data=iris, lambda=0))
  expect_error(ridgereg(Petal.Length~Sepdsal.Width+Sepal.Length, data=irfsfdis, lambda=0))
  expect_error(ridgereg(Petal.Length~Sepdsal.Width+Sepal.Length, data=irfsfdis))
})

test_that("class is correct", {
  ridgereg_mod <- ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda=0)

  expect_s3_class(ridgereg_mod, "ridgereg")
})


test_that("coefficients are similar to those of lm.ridge()", {
  tolerance <- 0.01
  ridgereg_mod <- ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda=0)
  lm_ridgereg_mod <- MASS::lm.ridge(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda=0)
  expect_equal(ridgereg_mod$coefficients[-1], lm_ridgereg_mod$coef, tolerance=tolerance)

  ridgereg_mod <- ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda=0.5)
  lm_ridgereg_mod <- MASS::lm.ridge(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda=0.5)
  expect_equal(ridgereg_mod$coefficients[-1], lm_ridgereg_mod$coef, tolerance=tolerance)


  ridgereg_mod <- ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda=1)
  lm_ridgereg_mod <- MASS::lm.ridge(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda=1)
  expect_equal(ridgereg_mod$coefficients[-1], lm_ridgereg_mod$coef, tolerance=tolerance)


  ridgereg_mod <- ridgereg(Petal.Length~Species, data=iris, lambda=0)
  lm_ridgereg_mod <- MASS::lm.ridge(Petal.Length~Species, data=iris, lambda=0)
  expect_equal(ridgereg_mod$coefficients[-1], lm_ridgereg_mod$coef, tolerance=tolerance)

  ridgereg_mod <- ridgereg(Petal.Length~Species, data=iris, lambda=0.5)
  lm_ridgereg_mod <- MASS::lm.ridge(Petal.Length~Species, data=iris, lambda=0.5)
  expect_equal(ridgereg_mod$coefficients[-1], lm_ridgereg_mod$coef, tolerance=tolerance)

  ridgereg_mod <- ridgereg(Petal.Length~Species, data=iris, lambda=1)
  lm_ridgereg_mod <- MASS::lm.ridge(Petal.Length~Species, data=iris, lambda=1)
  expect_equal(ridgereg_mod$coefficients[-1], lm_ridgereg_mod$coef, tolerance=tolerance)
})


