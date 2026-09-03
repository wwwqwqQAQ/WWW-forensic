# wwwforensic 首秀 demo(不依赖真实个人数据,用合成数据演示取证引擎)
# 运行: Rscript scripts/01_demo.R

# 用 source 直接加载源码(等价于 devtools::load_all,无需先安装包)
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

set.seed(42)
cat("==============================================\n")
cat("wwwforensic —— 个人数据数字取证 · 演示\n")
cat("==============================================\n\n")

# ---- 演示 1:Benford 首位数字检验 ------------------------------------
cat("【1】两类数据的 Benford 首位数字体检\n\n")
# (a) 自然产生的金额:模拟真实消费(对数正态 + 抹零到角分)
natural <- round(exp(rnorm(8000, mean = 4.5, sd = 1.1)) * 100) / 100
# (b) 人类编造的金额:全是"整齐"的数(整百/9.9 结尾/吉利数)
human <- sample(c(seq(10, 500, by = 10),
                  seq(9.9, 500, by = 10),
                  c(66, 88, 168, 520, 888)), 8000, replace = TRUE)

cat("— 自然生成金额(应基本符合 Benford):\n")
print(benford_test(natural))
cat("\n— 人类编造金额(应偏离 Benford):\n")
print(benford_test(human))

# ---- 演示 2:周度异常检测 --------------------------------------------
cat("\n【2】支出流的周度异常检测(稳健 MAD-z)\n\n")
days <- seq(as.Date("2025-07-01"), by = "day", length.out = 120)
spend <- round(runif(120, 30, 90), 0)
spend[seq(7, 120, by = 30)] <- 320          # 每月固定一笔"大额"(其实正常)
spend[80] <- 1500                            # 真正异常的暴击日
log <- data.frame(date = days, amount = spend)

wk <- weekly_totals(log, "date", "amount")
res <- detect_anomalies(wk, "total")
cat("周度合计 + 异常标记(TRUE = 异常周):\n")
print(res[res$.anomaly | seq_len(nrow(res)) %% 6 == 0, ])
cat("\n说明:每月固定大额因稳健基线而不误报,只有第 12 周的 1500 暴击被标出。\n")

# ---- 演示 3:stream_summary -------------------------------------------
cat("\n【3】单流统计摘要\n\n")
print(stream_summary(log, "amount"))
cat("\n演示完毕。下一步:接入你的真实数据(账单/睡眠/运动导出文件)。\n")
