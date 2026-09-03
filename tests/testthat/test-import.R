test_that("import_bills parses Alipay export", {
  f <- test_path("../../inst/extdata/sample-alipay.csv")
  d <- import_bills(f, source = "alipay")
  expect_equal(nrow(d), 5)              # 5 spending rows; income/transfer dropped
  expect_true(all(d$amount > 0))
  expect_equal(sum(d$amount), 15.5 + 86.2 + 128 + 45.8 + 66)
  expect_s3_class(d$date, "Date")
  expect_true(all(c("date", "amount", "counterparty", "description") %in% names(d)))
})

test_that("import_bills parses WeChat export", {
  f <- test_path("../../inst/extdata/sample-wechat.csv")
  d <- import_bills(f, source = "wechat")
  expect_equal(nrow(d), 5)
  expect_equal(sum(d$amount), 15.5 + 86.2 + 128 + 45.8 + 66)
  expect_true(all(d$direction %in% c("支出", "/ ")))
})

test_that("import_bills keeps income when spend_only = FALSE", {
  f <- test_path("../../inst/extdata/sample-alipay.csv")
  d <- import_bills(f, source = "alipay", spend_only = FALSE)
  expect_equal(nrow(d), 7)
  expect_true(any(grepl("收入", d$direction)))
})

test_that("import_stream auto-detects date and value columns", {
  f <- test_path("../../inst/extdata/sample-sleep.csv")
  s <- import_stream(f)
  expect_equal(names(s), c("date", "value"))
  expect_equal(nrow(s), 10)
  expect_true(all(s$value > 0))
  # explicit columns also work
  s2 <- import_stream(f, date_col = "日期", value_col = "睡眠时长(分钟)")
  expect_equal(s, s2)
})

test_that("importers fail loudly on unreadable file", {
  expect_error(import_bills(tempfile()), "could not read")
})
