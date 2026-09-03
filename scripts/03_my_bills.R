# 跑你自己的支付宝账单:一键取证 + 周报
# 用法:
#   Rscript scripts/03_my_bills.R [账单CSV路径]
#   (默认 data-raw/alipay-utf8.csv;GBK 导出的先用工具转 UTF-8)
library(wwwforensic)

args <- commandArgs(trailingOnly = TRUE)
path <- if (length(args) >= 1) args[1] else "data-raw/alipay-utf8.csv"

bills <- import_bills(path, source = "alipay")

cat("=== 导入结果 ===\n")
cat("支出笔数:", nrow(bills), "\n")
cat("日期范围:", format(min(bills$date)), " ~ ", format(max(bills$date)), "\n")
cat("支出总额:", round(sum(bills$amount), 2), "元\n")
cat("单笔中位数:", median(bills$amount), "元 | 最大值:", max(bills$amount), "元\n\n")

cat("=== Benford 首位数字检验 ===\n")
print(benford_test(bills$amount))

wk <- detect_anomalies(weekly_totals(bills, "date", "amount"), "total")
n_anom <- sum(wk$.anomaly)
cat("\n=== 异常周 ===\n")
if (n_anom) {
  print(wk[wk$.anomaly, c("week", "week_start", "total", ".z")])
} else {
  cat("无异常周\n")
}

# 每月花销走势(按自然月汇总,供参考)
m <- aggregate(amount ~ substr(date, 1, 7), data = bills, FUN = sum)
names(m) <- c("月份", "支出")
cat("\n=== 月度支出 ===\n")
print(m, row.names = FALSE)

saveRDS(bills, "data-raw/bills.rds")
docx <- render_report_docx(list("支付宝支出" = bills), value = "amount",
                           out_dir = "report", filename = "周报.docx")
cat("\nWord 报表已生成:", normalizePath(docx), "\n")
cat("交互体检台: run_dashboard(list(支付宝支出 = readRDS('data-raw/bills.rds')), value = 'amount')\n")
