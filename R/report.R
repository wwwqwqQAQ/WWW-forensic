# Self-contained weekly HTML report (no pandoc needed) --------------------

.weekly_plot <- function(stream, value) {
  wk <- weekly_totals(stream, "date", value)
  res <- detect_anomalies(wk, "total", k = 3.5)
  res$week_start <- as.Date(res$week_start)
  ggplot2::ggplot(res, ggplot2::aes(.data$week_start, .data$total)) +
    ggplot2::geom_line(ggplot2::aes(group = 1), colour = "#64748b") +
    ggplot2::geom_point(ggplot2::aes(colour = .data$.anomaly), size = 2.5) +
    ggplot2::scale_colour_manual(values = c("FALSE" = "#38bdf8", "TRUE" = "#ef4444"),
                                 name = "异常") +
    ggplot2::labs(x = NULL, y = "周合计", title = paste0("周度趋势 · ", value)) +
    ggplot2::theme_minimal(base_size = 12)
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
    ggplot2::geom_col(ggplot2::aes(y = .data$observed), fill = "#38bdf8", alpha = 0.85) +
    ggplot2::geom_line(ggplot2::aes(y = .data$expected, group = 1),
                       colour = "#0f172a", linewidth = 1) +
    ggplot2::geom_point(ggplot2::aes(y = .data$expected), colour = "#0f172a") +
    ggplot2::labs(x = if (bt$digits == 1) "首位数字" else "第二位数字",
                  y = "比例", title = paste0("Benford 检验 · ", value)) +
    ggplot2::theme_minimal(base_size = 12)
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
    p1 <- file.path(out_dir, paste0(safe, "-weekly.png"))
    p2 <- file.path(out_dir, paste0(safe, "-benford.png"))
    ggplot2::ggsave(p1, .weekly_plot(s, vcol), width = 8, height = 4, dpi = 110)
    x <- s[[vcol]][is.finite(s[[vcol]]) & s[[vcol]] > 0]
    if (length(x) >= 50) {
      ggplot2::ggsave(p2, .benford_plot(x, vcol), width = 8, height = 4, dpi = 110)
      bt <- benford_test(x)
    } else {
      p2 <- ""
      bt <- NULL
    }
    wk <- detect_anomalies(weekly_totals(s, dcol, vcol), "total")
    anom <- wk[wk$.anomaly, c("week", "week_start", "total")]
    rows <- if (nrow(anom) > 0) {
      paste0("<tr><td>", anom$week, "</td><td>", anom$week_start,
             "</td><td>", round(anom$total, 1), "</td></tr>", collapse = "")
    } else {
      "<tr><td colspan='3'>无异常周</td></tr>"
    }
    verdict <- if (!is.null(bt)) bt$verdict else "样本不足"
    sections <- c(sections, sprintf(
      "<h2>%s</h2>
       <img src='%s' style='max-width:100%%'>
       <img src='%s' style='max-width:100%%'>
       <p><b>Benford 判定:</b> %s &nbsp;|&nbsp; <b>异常周:</b> %d</p>
       <table border='1' cellspacing='0' cellpadding='6'>
         <tr><th>周</th><th>周起始</th><th>合计</th></tr>%s</table>",
      nm, basename(p1), if (p2 == "") "" else basename(p2),
      verdict, nrow(anom), rows
    ))
    files <- c(files, p1, if (p2 != "") p2)
  }
  html <- paste0(
    "<!doctype html><html><head><meta charset='utf-8'><title>个人数据周报</title>",
    "<style>body{font-family:-apple-system,'Segoe UI',sans-serif;margin:40px;",
    "max-width:860px;color:#0f172a}h1{color:#38bdf8}table{border-collapse:collapse}",
    "td,th{padding:4px 10px;font-size:14px}</style></head><body>",
    "<h1>个人数据数字取证 · 周报</h1><p>生成于 ", Sys.Date(), "</p>",
    paste(sections, collapse = "\n"), "</body></html>"
  )
  idx <- file.path(out_dir, "index.html")
  writeLines(html, idx, useBytes = TRUE)
  invisible(list(index = idx, files = files))
}
