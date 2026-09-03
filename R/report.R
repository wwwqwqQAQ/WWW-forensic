# Self-contained weekly HTML report (no pandoc needed) --------------------

.weekly_plot <- function(stream, value) {
  wk <- weekly_totals(stream, "date", value)
  res <- detect_anomalies(wk, "total", k = 3.5)
  res$week_start <- as.Date(res$week_start)
  ggplot2::ggplot(res, ggplot2::aes(.data$week_start, .data$total)) +
    ggplot2::geom_area(ggplot2::aes(group = 1), fill = "#e0f2fe", alpha = 0.6) +
    ggplot2::geom_line(ggplot2::aes(group = 1), colour = "#0284c7", linewidth = 0.8) +
    ggplot2::geom_point(ggplot2::aes(colour = .data$.anomaly), size = 3) +
    ggplot2::scale_colour_manual(values = c("FALSE" = "#38bdf8", "TRUE" = "#ef4444"),
                                 name = "异常周", labels = c("FALSE" = "正常", "TRUE" = "异常")) +
    ggplot2::labs(x = NULL, y = "周合计(元)", title = paste0("周度支出趋势 · ", value)) +
    ggplot2::theme_minimal(base_size = 13, base_family = .plot_family()) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(face = "bold"))
}

.benford_plot <- function(x, value) {
  bt <- benford_test(x)
  df <- data.frame(
    digit = names(bt$observed),
    observed = as.numeric(bt$proportion),
    expected = as.numeric(bt$expected) / bt$n
  )
  df$digit <- factor(df$digit, levels = df$digit)
  ggplot2::ggplot(df, ggplot2::aes(.data$digit)) +
    ggplot2::geom_col(ggplot2::aes(y = .data$observed, fill = .data$observed - .data$expected > 0),
                      width = 0.7, alpha = 0.9) +
    ggplot2::scale_fill_manual(values = c("TRUE" = "#f59e0b", "FALSE" = "#38bdf8"),
                               guide = "none") +
    ggplot2::geom_line(ggplot2::aes(y = .data$expected, group = 1),
                       colour = "#0f172a", linewidth = 1.1) +
    ggplot2::geom_point(ggplot2::aes(y = .data$expected), colour = "#0f172a", size = 2) +
    ggplot2::labs(x = if (bt$digits == 1) "首位数字" else "第二位数字",
                  y = "比例",
                  title = paste0("Benford 检验 · ", value, "(判定:", bt$verdict, ")")) +
    ggplot2::theme_minimal(base_size = 13, base_family = .plot_family()) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(face = "bold"))
}

# One styled card per stream: stat chips + plots + anomaly & monthly tables.
.card_html <- function(nm, s, vcol, dcol, safe, out_dir) {
  p1 <- file.path(out_dir, paste0(safe, "-weekly.png"))
  p2 <- file.path(out_dir, paste0(safe, "-benford.png"))
  ggplot2::ggsave(p1, .weekly_plot(s, vcol), width = 9, height = 4.2, dpi = 120)
  x <- s[[vcol]][is.finite(s[[vcol]]) & s[[vcol]] > 0]
  bt <- NULL
  if (length(x) >= 50) {
    ggplot2::ggsave(p2, .benford_plot(x, vcol), width = 9, height = 4.2, dpi = 120)
    bt <- benford_test(x)
  } else {
    p2 <- ""
  }
  wk <- detect_anomalies(weekly_totals(s, dcol, vcol), "total")
  anom <- wk[wk$.anomaly, c("week", "week_start", "total")]
  rows <- if (nrow(anom) > 0) {
    paste0("<tr><td>", anom$week, "</td><td>", anom$week_start,
           "</td><td style='text-align:right'>",
           format(round(anom$total, 1), big.mark = ","), "</td></tr>", collapse = "")
  } else {
    "<tr><td colspan='3'>无异常周</td></tr>"
  }
  # monthly totals
  m <- stats::aggregate(
    stats::as.formula(paste0("`", vcol, "` ~ `", dcol, "`")), data = s, FUN = sum
  )
  names(m) <- c("date", "total")
  m$month <- format(as.Date(m$date), "%Y-%m")
  mt <- stats::aggregate(total ~ month, data = m, FUN = sum)
  mt <- mt[order(mt$month), , drop = FALSE]
  mrows <- paste0("<tr><td>", mt$month, "</td><td style='text-align:right'>",
                  format(round(mt$total, 1), big.mark = ","), "</td></tr>",
                  collapse = "")
  stats_html <- paste0(
    "<span class='chip'>笔数 ", nrow(s), "</span>",
    "<span class='chip'>总额 ", format(round(sum(s[[vcol]], na.rm = TRUE), 1),
                                       big.mark = ","), " 元</span>",
    "<span class='chip'>单笔中位 ", format(stats::median(s[[vcol]], na.rm = TRUE),
                                           big.mark = ","), " 元</span>",
    "<span class='chip'>最大单笔 ", format(max(s[[vcol]], na.rm = TRUE),
                                           big.mark = ","), " 元</span>"
  )
  if (!is.null(bt)) {
    red <- bt$verdict %in% c("marginal conformity", "nonconformity")
    stats_html <- paste0(stats_html,
      "<span class='chip", if (red) " red" else "", "'>Benford: ", bt$verdict, "</span>")
  }
  stats_html <- paste0(stats_html, "<span class='chip'>异常周 ", nrow(anom), "</span>")
  imgs <- if (p2 != "") {
    paste0("<img src='", basename(p1), "'><img src='", basename(p2), "'>")
  } else {
    paste0("<img src='", basename(p1), "'>")
  }
  sprintf(
    "<div class='card'><h2>%s</h2>
     <div class='meta'>%s ~ %s</div>
     <div class='chips'>%s</div>
     %s
     <h3>异常周</h3>
     <table><tr><th>周</th><th>周起始</th><th style='text-align:right'>合计(元)</th></tr>%s</table>
     <h3>月度支出</h3>
     <table><tr><th>月份</th><th style='text-align:right'>支出(元)</th></tr>%s</table>
     </div>",
    nm,
    format(min(as.Date(s[[dcol]]), na.rm = TRUE)),
    format(max(as.Date(s[[dcol]]), na.rm = TRUE)),
    stats_html, imgs, rows, mrows
  )
}

#' Render a Word (docx) report — the primary deliverable
#'
#' Uses the \code{officer} package to build a real Word document (no pandoc
#' needed): per-stream stat summary, trend & Benford charts, anomaly weeks and
#' monthly totals tables.
#'
#' @inheritParams render_weekly
#' @param filename output file name (inside \code{out_dir})
#' @return invisibly, the path to the generated .docx
#' @export
render_report_docx <- function(streams, value = "amount", date = "date",
                               out_dir = "report", filename = "周报.docx") {
  if (!requireNamespace("officer", quietly = TRUE)) {
    stop("package 'officer' is required; install with install.packages('officer')")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("package 'ggplot2' is required to render the report")
  }
  if (is.data.frame(streams)) streams <- list(stream = streams)
  if (is.null(names(streams))) names(streams) <- paste0("stream", seq_along(streams))
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  doc <- officer::read_docx()
  doc <- officer::body_add_par(doc, "个人数据数字取证 · 周报", style = "heading 1")
  doc <- officer::body_add_par(doc,
    paste0("生成日期:", Sys.Date(), " · wwwforensic 引擎 · 数据仅本地处理"),
    style = "Normal")
  for (nm in names(streams)) {
    s <- streams[[nm]]
    if (nrow(s) < 7) {
      warning("stream '", nm, "' has fewer than 7 rows; skipped")
      next
    }
    vcol <- .resolve_col(s, nm, value, default = "value")
    dcol <- .resolve_col(s, nm, date, default = "date")
    safe <- gsub("[^A-Za-z0-9_-]", "", nm)
    if (safe == "") safe <- paste0("stream", match(nm, names(streams)))
    p1 <- file.path(out_dir, paste0(safe, "-weekly.png"))
    p2 <- file.path(out_dir, paste0(safe, "-benford.png"))
    ggplot2::ggsave(p1, .weekly_plot(s, vcol), width = 9, height = 4.2, dpi = 120)
    x <- s[[vcol]][is.finite(s[[vcol]]) & s[[vcol]] > 0]
    bt <- NULL
    if (length(x) >= 50) {
      ggplot2::ggsave(p2, .benford_plot(x, vcol), width = 9, height = 4.2, dpi = 120)
      bt <- benford_test(x)
    } else {
      p2 <- ""
    }
    wk <- detect_anomalies(weekly_totals(s, dcol, vcol), "total")
    anom <- wk[wk$.anomaly, c("week", "week_start", "total")]
    anom$total <- round(anom$total, 1)
    names(anom) <- c("周", "周起始", "合计")
    # monthly totals
    m <- stats::aggregate(
      stats::as.formula(paste0("`", vcol, "` ~ `", dcol, "`")), data = s, FUN = sum
    )
    names(m) <- c("date", "total")
    m$month <- format(as.Date(m$date), "%Y-%m")
    mt <- stats::aggregate(total ~ month, data = m, FUN = sum)
    mt <- mt[order(mt$month), ]
    mt$total <- round(mt$total, 1)
    names(mt) <- c("月份", "支出")

    stats_df <- data.frame(
      指标 = c("记录笔数", "日期范围", "总额(元)", "单笔中位数(元)", "最大单笔(元)",
               "Benford 判定", "异常周数"),
      数值 = c(
        nrow(s),
        paste(format(min(as.Date(s[[dcol]]), na.rm = TRUE)), "~",
              format(max(as.Date(s[[dcol]]), na.rm = TRUE))),
        round(sum(s[[vcol]], na.rm = TRUE), 1),
        stats::median(s[[vcol]], na.rm = TRUE),
        max(s[[vcol]], na.rm = TRUE),
        if (!is.null(bt)) bt$verdict else "样本不足",
        nrow(anom)
      ),
      stringsAsFactors = FALSE
    )

    doc <- officer::body_add_par(doc, nm, style = "heading 2")
    doc <- officer::body_add_par(doc, "统计摘要", style = "heading 3")
    doc <- officer::body_add_table(doc, stats_df)
    doc <- officer::body_add_par(doc, "周度趋势", style = "heading 3")
    doc <- officer::body_add_img(doc, src = p1, width = 6.3, height = 2.94)
    if (p2 != "") {
      doc <- officer::body_add_par(doc, "Benford 首位数字检验", style = "heading 3")
      doc <- officer::body_add_img(doc, src = p2, width = 6.3, height = 2.94)
    }
    doc <- officer::body_add_par(doc, "异常周", style = "heading 3")
    doc <- officer::body_add_table(doc, if (nrow(anom)) anom else
      data.frame(备注 = "无异常周"))
    doc <- officer::body_add_par(doc, "月度支出", style = "heading 3")
    doc <- officer::body_add_table(doc, mt)
  }
  target <- file.path(out_dir, filename)
  print(doc, target = target)
  invisible(target)
}

#' Render a self-contained weekly forensics report (HTML + PNG, no pandoc)
#'
#' @param streams named list of data.frames (each with a date column and one
#'   numeric column), or a single data.frame
#' @param value name of the numeric column in the stream(s)
#' @param date name of the date column
#' @param out_dir output directory (created if needed)
#' @return invisibly, the list of generated file paths
#' @export
render_weekly <- function(streams, value = "amount", date = "date",
                          out_dir = "report") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("package 'ggplot2' is required to render the report")
  }
  if (is.data.frame(streams)) streams <- list(stream = streams)
  if (is.null(names(streams))) names(streams) <- paste0("stream", seq_along(streams))
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  files <- character()
  sections <- character()
  for (nm in names(streams)) {
    s <- streams[[nm]]
    if (nrow(s) < 7) {
      warning("stream '", nm, "' has fewer than 7 rows; skipped")
      next
    }
    vcol <- .resolve_col(s, nm, value, default = "value")
    dcol <- .resolve_col(s, nm, date, default = "date")
    safe <- gsub("[^A-Za-z0-9_-]", "", nm)
    if (safe == "") safe <- paste0("stream", match(nm, names(streams)))
    sections <- c(sections, .card_html(nm, s, vcol, dcol, safe, out_dir))
    files <- c(files,
               file.path(out_dir, paste0(safe, "-weekly.png")),
               file.path(out_dir, paste0(safe, "-benford.png")))
  }
  html <- paste0(
    "<!doctype html><html><head><meta charset='utf-8'><title>个人数据周报</title>",
    "<style>",
    "body{font-family:'Segoe UI','Microsoft YaHei',sans-serif;margin:0;background:#f1f5f9;color:#0f172a}",
    ".wrap{max-width:900px;margin:0 auto;padding:26px 16px}",
    ".banner{background:linear-gradient(135deg,#0ea5e9,#6366f1);color:#fff;border-radius:14px;padding:24px 28px}",
    ".banner h1{margin:0;font-size:25px}.banner p{margin:6px 0 0;opacity:.85;font-size:13px}",
    ".card{background:#fff;border-radius:14px;padding:22px 26px;margin-top:18px;box-shadow:0 1px 3px rgba(15,23,42,.08)}",
    ".card h2{margin:0;font-size:20px}.card h3{margin:22px 0 6px;font-size:15px;color:#475569}",
    ".meta{color:#64748b;font-size:13px;margin:4px 0 10px}",
    ".chips{margin-bottom:4px}",
    ".chip{display:inline-block;background:#e0f2fe;color:#0369a1;border-radius:999px;padding:4px 12px;font-size:13px;margin:4px 6px 0 0}",
    ".chip.red{background:#fee2e2;color:#b91c1c}",
    "img{max-width:100%;border:1px solid #e2e8f0;border-radius:10px;margin-top:12px}",
    "table{border-collapse:collapse;width:100%;margin-top:10px;font-size:14px}",
    "th{background:#f8fafc;text-align:left}td,th{border:1px solid #e2e8f0;padding:6px 12px}",
    ".foot{margin:26px 0 40px;text-align:center;color:#94a3b8;font-size:12px}",
    "</style></head><body><div class='wrap'>",
    "<div class='banner'><h1>个人数据数字取证 · 周报</h1>",
    "<p>生成于 ", Sys.Date(), " · wwwforensic 引擎 · 数据仅本地处理</p></div>",
    paste(sections, collapse = "\n"),
    "<div class='foot'>由 wwwforensic 生成 · 异常周按稳健 MAD-z 阈值 3.5 判定</div>",
    "</div></body></html>"
  )
  idx <- file.path(out_dir, "index.html")
  writeLines(html, idx, useBytes = TRUE)
  invisible(list(index = idx, files = unique(files[file.exists(files)])))
}
