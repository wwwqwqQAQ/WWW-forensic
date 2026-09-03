skip_if_not_installed("ggplot2")

make_stream <- function(n = 90) {
  days <- seq(as.Date("2026-01-01"), by = "day", length.out = n)
  data.frame(date = days, amount = c(round(runif(n, 20, 80), 1)))
}

test_that("render_weekly writes index.html and plots", {
  s <- make_stream(90)
  s$amount[60] <- 900     # one spike
  out <- render_weekly(list(bills = s), out_dir = tempfile("report"))
  expect_true(file.exists(out$index))
  expect_true(all(file.exists(out$files)))
  html <- paste(readLines(out$index, warn = FALSE), collapse = "\n")
  expect_true(grepl("个人数据周报", html))
})

test_that("render_weekly handles small streams gracefully", {
  s <- make_stream(5)
  expect_warning(out <- render_weekly(list(tiny = s), out_dir = tempfile("report2")))
  expect_true(file.exists(out$index))
})

test_that("run_dashboard returns an app object when launch = FALSE", {
  skip_if_not_installed("shiny")
  s <- make_stream(30)
  app <- run_dashboard(list(bills = s), launch = FALSE)
  expect_s3_class(app, "shiny.appobj")
})

test_that("render_report_docx writes a real docx", {
  skip_if_not_installed("officer")
  s <- make_stream(90)
  s$amount[60] <- 900
  f <- render_report_docx(list(bills = s), out_dir = tempfile("docx"),
                          filename = "test.docx")
  expect_true(file.exists(f))
  expect_gt(file.size(f), 1000)   # a real zip-based docx, not an empty stub
})
