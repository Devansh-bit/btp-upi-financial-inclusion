library(testthat)
library(data.table)
source("../../src/R/lib/descriptives_fns.R")

test_that("cv", {
  dt3 <- data.table(state_id = c("a", "b", "c"), period = "2020Q1",
                    in_main_sample = TRUE, x = c(1, 2, 3))
  expect_equal(sigma_cv(dt3, "x")$cv, 0.5)
})

test_that("beta", {
  x0 <- c(1, 2, 4, 8, 16)
  growth <- 0.1 - 0.02 * log(x0)
  x1 <- exp(log(x0) + growth * 7.5)
  dts <- rbind(
    data.table(state_id = letters[1:5], period = "2018Q4", in_main_sample = TRUE, x = x0),
    data.table(state_id = letters[1:5], period = "2026Q2", in_main_sample = TRUE, x = x1))
  expect_equal(unname(coef(beta_convergence(dts, "x"))[2]), -0.02, tolerance = 1e-9)
})
