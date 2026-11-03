library(testthat)
library(data.table)
source("../../src/R/lib/part1_fns.R")

test_that("decompose terms sum to 1", {
  d <- data.table(state_id = letters[1:4], period = "2020Q1", count = c(10, 40, 90, 160),
                  users = c(5, 10, 15, 20), population = c(100, 100, 100, 100))
  r <- decompose(d, "2020Q1", letters[1:4])
  expect_equal(r$reach + r$intensity, 1)
  expect_equal(r$n, 4L)
  expect_equal(r$var_y, var(log(d$count / 100)))
  expect_equal(r$var_r, var(log(d$users / 100)))
  expect_equal(r$var_i, var(log(d$count / d$users)))
})

test_that("decompose_series gives one row per period", {
  d <- data.table(state_id = rep(letters[1:3], 2), period = rep(c("2020Q1", "2020Q2"), each = 3),
                  count = c(10, 40, 90, 20, 30, 70), users = c(5, 8, 12, 6, 7, 9),
                  population = 100)
  s <- decompose_series(d, letters[1:3])
  expect_equal(s$period, c("2020Q1", "2020Q2"))
  expect_equal(s$reach + s$intensity, c(1, 1))
})

test_that("top_k", {
  d <- data.table(state_id = letters[1:4], period = "2020Q1", count = c(50, 30, 10, 10),
                  amount = c(50, 30, 10, 10), users = c(50, 30, 10, 10),
                  population = c(1, 1, 1, 7))
  t <- top_k(d, "2020Q1", k = 2)
  expect_equal(t[variable == "count"]$share, 80)
  expect_equal(t[variable == "population"]$share, 20)
  expect_setequal(t$variable, c("count", "amount", "users", "population"))
})

test_that("n_for_share", {
  expect_equal(n_for_share(c(50, 30, 10, 10)), 1L)
  expect_equal(n_for_share(c(40, 30, 20, 10)), 2L)
})

test_that("spearman_ties", {
  expect_equal(spearman(c(1, 2, 2, 3), c(1, 2, 3, 4)), cor(c(1, 2.5, 2.5, 4), 1:4))
})

test_that("annual_rate", {
  expect_equal(annual_rate(100, 121, 2), 10)
  expect_equal(years_between("2018Q4", "2026Q2"), 7.5)
})

test_that("spread", {
  d <- data.table(state_id = letters[1:5], period = "2020Q1", x = c(1, 2, 3, 4, 100))
  s <- spread(d, "x", letters[1:4])
  expect_equal(s$med, 2.5)
  expect_equal(s$q1, unname(quantile(1:4, 0.25)))
  expect_equal(s$q3, unname(quantile(1:4, 0.75)))
})

mk <- function(periods, counts) {
  data.table(state_id = rep(c("a", "b"), each = length(periods)), period = rep(periods, 2),
             count = rep(counts, 2), amount = rep(counts * 10, 2), users = 5, population = 100,
             count_p2p = rep(counts / 2, 2), count_retail = rep(counts / 4, 2),
             count_utility = rep(counts / 4, 2))
}

test_that("add_measures", {
  d <- add_measures(mk("2020Q1", 10))[1]
  expect_equal(d$count_pc, 0.1)
  expect_equal(d$amount_pc, 1)
  expect_equal(d$users_per_1000, 50)
  expect_equal(d$intensity, 2)
  expect_equal(d$atv, 10)
  expect_equal(d$share_p2p, 0.5)
  expect_equal(d$share_retail, 0.25)
})

test_that("national qoq", {
  n <- national(mk(c("2020Q1", "2020Q2"), c(10, 15)))
  expect_equal(n$count, c(20, 30))
  expect_equal(n$qoq_count, c(NA, 50))
  expect_equal(n$qoq_amount, c(NA, 50))
  expect_equal(n$count_pc, c(0.1, 0.15))
})

test_that("annual", {
  p <- paste0(rep(2018:2019, each = 4), "Q", rep(1:4, 2))
  a <- annual(national(mk(p, rep(0.5, 8))))
  expect_equal(a$count, c(4, 4))
  expect_equal(a$growth_count, c(NA, 0))
  expect_equal(a$users, c(10, 10))
  expect_equal(a$year, c("2018", "2019"))
})

test_that("annual labels partial last year and NA growth", {
  p <- paste0(rep(2025:2026, c(4, 2)), "Q", c(1:4, 1:2))
  a <- annual(national(mk(p, rep(1, 6))))
  expect_equal(a$year, c("2025", "2026 (Q1--Q2)"))
  expect_true(all(is.na(a$growth_count)))
})

test_that("load_part1 joins and stops on a missing state-period", {
  lab <- data.table(state_id = c("a", "b"), name = c("A", "B"), code = c("AA", "BB"),
                    in_main = c(TRUE, FALSE))
  long <- function(v, x, ids = c("a", "b"))
    data.table(state_id = ids, period = "2020Q1", variable = v, value = x, source = "t")
  pp <- rbind(long("phonepe_count", c(10, 20)), long("phonepe_amount_inr", c(100, 200)),
              long("phonepe_users", c(5, 6)))
  ty <- rbind(long("phonepe_count_p2p", c(1, 2)), long("phonepe_count_retail", c(3, 4)),
              long("phonepe_count_utility", c(6, 14)))
  pop <- long("population", c(1000, 2000))
  d <- load_part1(pp, ty, pop, lab)
  expect_equal(nrow(d), 2L)
  expect_equal(names(d), c("state_id", "period", "name", "code", "in_main", "count", "amount",
                           "users", "population", "count_p2p", "count_retail", "count_utility"))
  expect_equal(d$population, c(1000, 2000))
  expect_error(load_part1(pp, ty, pop[1], lab), "missing")
  expect_error(load_part1(pp[state_id == "a" | variable != "phonepe_users"], ty, pop, lab),
               "missing")
})
