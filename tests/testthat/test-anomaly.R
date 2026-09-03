test_that("mad_z is robust and centred", {
  expect_equal(mad_z(c(1, 1, 1, 1)), c(0, 0, 0, 0))   # MAD zero -> zeros
  x <- c(10, 10.2, 9.8, 10.1, 60, 9.9, 10.3)          # one spike
  z <- mad_z(x)
  expect_gt(z[5], z[1])          # spike gets the largest score
  expect_true(all(z[z != z[5]] < 2))
})

test_that("detect_anomalies flags the spike only", {
  d <- data.frame(
    day = as.Date("2026-01-01") + 0:29,
    spend = c(rep(c(35, 42, 38, 40, 33), 6))
  )
  d$spend[15] <- 400             # one huge day
  out <- detect_anomalies(d, "spend")
  expect_true(all(out$.anomaly == (seq_len(nrow(d)) == 15)))
  expect_true(any(out$.anomaly))
})

test_that("detect_anomalies supports per-group baselines", {
  d <- data.frame(
    g = rep(c("a", "b"), each = 10),
    v = c(
      c(rep(c(1, 2, 3), 3), 100),              # a: tight baseline + huge spike
      c(rep(c(30, 50, 70, 90), 2), 30, 100)    # b: wide spread, 100 is normal
    )
  )
  out <- detect_anomalies(d, "v", group = "g", k = 3)
  flags <- out$.anomaly
  expect_true(flags[10])           # a: spike detected
  expect_false(any(flags[11:20]))  # b: nothing abnormal
})

test_that("weekly_totals sums and labels ISO weeks", {
  d <- data.frame(
    date = as.Date(c("2026-01-01", "2026-01-02", "2026-01-05", "2026-01-06")),
    amount = c(10, 20, 5, 5)
  )
  w <- weekly_totals(d, "date", "amount")
  expect_equal(nrow(w), 2)
  expect_equal(sort(w$total), c(10, 30))   # Mon starts a fresh ISO week
})
