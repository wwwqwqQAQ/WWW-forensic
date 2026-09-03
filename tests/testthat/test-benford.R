test_that("first_digit extracts significant digits", {
  expect_equal(first_digit(1:9), 1:9)
  expect_equal(first_digit(c(45, 0.0045, 4500, 45000)), c(4, 4, 4, 4))
  expect_equal(first_digit(c(-32, NA, 0, Inf)), 3)   # neg ok, junk dropped
  expect_equal(first_digit(c(10, 100, 1000)), c(1, 1, 1))
  expect_length(first_digit(c(0, NA, NaN)), 0)
})

test_that("second_digit extracts second significant digit", {
  expect_equal(second_digit(c(45, 0.0045, 4500)), c(5, 5, 5))
  expect_equal(second_digit(c(10, 100, 109)), c(0, 0, 0))
  expect_equal(second_digit(c(1.1, 11, 110)), c(1, 1, 1))
})

test_that("expected Benford proportions are known constants", {
  p1 <- benford_expected(1)
  expect_equal(unname(p1[1]), log10(2), tolerance = 1e-12)   # digit 1 -> 0.301
  expect_equal(sum(p1), 1)
  expect_equal(sum(benford_expected(2)), 1)
})

test_that("benford_test structure and verdict", {
  # perfectly Benford-conforming data
  n <- 20000
  set.seed(1)
  u <- runif(n)
  x <- 10^(u * 3)                    # log-uniform: exactly Benford in expectation
  bt <- benford_test(x)
  expect_s3_class(bt, "benford_test")
  expect_equal(bt$n, n)
  expect_equal(unname(bt$observed["1"] / n), log10(2), tolerance = 0.02)
  expect_true(bt$verdict %in% c("close conformity", "acceptable conformity"))

  # human-fabricated data: all numbers are rounded multiples of 10 (digit 1 only)
  h <- seq(10, 100000, by = 10)
  bh <- benford_test(h)
  expect_gt(bh$mad, 0.012)
  expect_equal(bh$verdict, "nonconformity")
})

test_that("benford_test warns on tiny samples", {
  expect_warning(benford_test(c(1, 2, 3, 4)), "fewer than 50")
})
