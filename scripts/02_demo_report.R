# selfcheck 演示 2:用合成数据生成真实周报(无需真实个人数据)
# 运行: Rscript scripts/02_demo_report.R
library(selfcheck)

set.seed(7)
days <- seq(as.Date("2026-01-01"), by = "day", length.out = 120)
bills <- data.frame(
  date = days,
  amount = round(exp(rnorm(120, 4.2, 0.8)), 1)   # 日常消费(对数正态,天然符合 Benford)
)
bills$amount[88] <- 1500                          # 埋一个暴击周

sleep <- import_stream("inst/extdata/sample-sleep.csv")

out <- render_weekly(
  list("支出账单" = bills, "睡眠" = sleep),
  value = "amount",
  out_dir = "report"
)
cat("报告已生成:", normalizePath(out$index), "\n")
cat("图片:", paste(basename(out$files), collapse = ", "), "\n")
