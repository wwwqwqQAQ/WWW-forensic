# 生成 README 效果预览图(合成数据,不含任何真实个人数据)
# 运行: Rscript scripts/04_preview.R  → 输出仓库根目录 preview.png
library(wwwforensic)
library(ggplot2)
library(patchwork)

fam <- wwwforensic:::.plot_family()

set.seed(42)
days <- seq(as.Date("2025-07-01"), by = "day", length.out = 120)
bills <- data.frame(
  date = days,
  amount = round(exp(rnorm(120, 4.2, 0.8)), 1)
)
bills$amount[88] <- 1500   # 埋一个"暴击周",让红点有戏

p1 <- wwwforensic:::.weekly_plot(bills, "amount") +
  labs(title = "周度异常检测", y = "周合计(元)") +
  theme(plot.title = element_text(face = "bold", size = 15))

p2 <- wwwforensic:::.benford_plot(bills$amount, "amount") +
  labs(title = "Benford 首位数字检验", y = "比例") +
  theme(plot.title = element_text(face = "bold", size = 15))

combo <- p1 + p2 +
  plot_annotation(
    title = "wwwforensic · 个人数据数字取证",
    subtitle = "效果预览(合成数据)· 左:稳健 MAD-z 异常周检测  右:首位数字分布 vs Benford 理论值",
    theme = theme(
      plot.title = element_text(family = fam, face = "bold", size = 19,
                                colour = "#0ea5e9"),
      plot.subtitle = element_text(family = fam, size = 11, colour = "#64748b"),
      plot.background = element_rect(fill = "white", colour = NA)
    )
  )

ggsave("preview.png", combo, width = 12, height = 5.4, dpi = 150)
cat("preview.png generated:", normalizePath("preview.png"), "\n")
