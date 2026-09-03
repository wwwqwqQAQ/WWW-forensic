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

test_that("import_bills skips certificate preamble with legal text", {
  p <- tempfile(fileext = ".csv")
  writeLines(c(
    "------------------------------------------------------------------------------------",
    "支出信息:",
    "起始时间:[2025-09-04 00:00:00]",
    "共2笔记录",
    "特别提示:本回单金额与实际交易不符时以实际为准",   # 含"金额"但不是表头
    "",
    "交易号,交易时间,交易对方,商品名称,金额(元),收/支",
    "T1,2025-09-04 08:00:00,某某店,早餐,12.50,支出",
    "T2,2025-09-05 09:00:00,某某店,午餐,25.00,支出"
  ), p, useBytes = TRUE)
  d <- import_bills(p)
  expect_equal(nrow(d), 2)
  expect_equal(sum(d$amount), 37.5)
  expect_equal(format(min(d$date)), "2025-09-04")
})
