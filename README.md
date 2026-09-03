# selfcheck

给个人数据做「数字取证」:用 Benford 定律、稳健统计和时间结构,检查你自己记录的数字(支出、睡眠、运动…)是不是"自然",哪里不对劲。

> Personal data forensics in R: Benford's law tests, robust (MAD-based) anomaly
> detection, and weekly structure checks for anything you record about your life.

## 为什么做

- 你的旧工具都在"采集/展示"数据;这个项目换一层:**检验数据本身的统计行为**
- 首位数字分布对"人类编造的数字"极其敏感——不只审计能用,你的个人记录同样适用
- R 包 + Shiny 仪表盘 + 每周自动报告,数统正统且真实可用

## 现状(Roadmap)

- [x] 包骨架(R/ 源码 + tests/ + DESCRIPTION/NAMESPACE)
- [x] Benford 检验族:`first_digit` / `second_digit` / `benford_expected` / `benford_test`(χ² + MAD + 结论)
- [x] 稳健异常检测:`mad_z` / `detect_anomalies`(支持分组基线)/ `weekly_totals` / `stream_summary`
- [ ] 真实数据适配器:支付宝/微信账单导出 → tidy 格式
- [ ] 真实数据适配器:睡眠 / 运动(App 导出)
- [ ] Shiny 交互体检台 `run_dashboard()`
- [ ] 每周自动报告(quarto → HTML/docx)
- [ ] 案例站(vignette,用真实脱敏数据演示)+ 发布 GitHub

## 快速上手(源码直跑,无需安装)

```r
# 加载源码(等价 load_all)
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

# 你的支出金额符不符合 Benford?
bt <- benford_test(my_amounts)

# 哪一周不对劲?(稳健,不受每月固定大额干扰)
wk <- weekly_totals(log, "date", "amount")
detect_anomalies(wk, "total")
```

合成数据演示:

```sh
Rscript scripts/01_demo.R
```

## 目录

```
R/benford.R        首位数字/二位数字检验,χ² 与 MAD 判定
R/anomaly.R        稳健 z 分数、异常标记、周聚合、流摘要
tests/testthat/    单元测试(testthat 3)
scripts/01_demo.R  合成数据演示(无需真实数据)
data-raw/          真实导出文件放这里(已被 .gitignore 忽略,绝不入库)
```
