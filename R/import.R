# Real-world data importers ----------------------------------------------

# Read a CSV robustly: try several encodings, optionally locate the real
# header row (bills ship with a certificate preamble whose legal text may
# mention "金额" too, so header detection requires ALL header_patterns to
# match), and require a header pattern so mojibake reads are rejected.
.read_csv_lenient <- function(path, encoding = NULL, header_patterns = NULL,
                              required_pattern = NULL) {
  encodings <- if (!is.null(encoding)) encoding else c("UTF-8-BOM", "UTF-8", "CP936", "GBK", "GB18030", "")
  tried <- character()
  for (enc in encodings) {
    ok <- tryCatch({
      skip <- 0L
      if (length(header_patterns)) {
        enc_lines <- sub("-BOM$", "", enc)
        lns <- suppressWarnings(
          tryCatch(readLines(path, warn = FALSE, encoding = enc_lines),
                   error = function(e) character())
        )
        if (length(lns)) {
          hits <- Reduce(intersect, lapply(header_patterns, function(p) grep(p, lns)))
          if (length(hits)) skip <- as.integer(hits[1] - 1)
        }
      }
      suppressWarnings(
        utils::read.csv(path, stringsAsFactors = FALSE, fileEncoding = enc,
                        skip = skip, check.names = FALSE)
      )
    }, error = function(e) NULL)
    tried <- c(tried, enc)
    if (is.null(ok)) next
    if (!is.null(required_pattern) && !any(grepl(required_pattern, names(ok)))) {
      next
    }
    return(ok)
  }
  stop("could not read '", path, "' (tried encodings: ",
       paste(tried, collapse = ", "), ")")
}

.pick_col <- function(nms, patterns) {
  for (p in patterns) {
    hit <- which(grepl(p, nms))[1]
    if (!is.na(hit)) return(hit)
  }
  NA_integer_
}

.parse_amount <- function(x) {
  suppressWarnings(as.numeric(gsub("[^0-9.+-]", "", as.character(x))))
}

#' Import spending bills (Alipay / WeChat export CSV)
#'
#' Auto-detects the relevant columns by their Chinese headers, so both Alipay
#' and WeChat wallet exports work without manual column mapping.
#'
#' @param path path to the exported CSV
#' @param source "auto" (default), "alipay" or "wechat" (only used for labelling)
#' @param spend_only keep only spending rows (支出); refunds/income are dropped
#' @return data.frame with columns \code{date}, \code{amount} (positive spend),
#'   \code{direction}, \code{counterparty}, \code{description}, \code{source}
#' @export
import_bills <- function(path, source = c("auto", "alipay", "wechat"),
                         spend_only = TRUE) {
  source <- match.arg(source)
  d <- .read_csv_lenient(path, header_patterns = c("金额", "时间|收/支|收支"),
                         required_pattern = "金额")
  nms <- names(d)
  i_amount <- .pick_col(nms, c("金额"))
  i_date   <- .pick_col(nms, c("交易时间", "交易创建时间", "付款时间", "时间", "日期"))
  i_dir    <- .pick_col(nms, c("收/支", "收支"))
  i_party  <- .pick_col(nms, c("交易对方", "对方"))
  i_desc   <- .pick_col(nms, c("商品名称", "商品", "描述"))
  if (is.na(i_amount) || is.na(i_date)) {
    stop("could not locate amount/date columns; headers: ",
         paste(nms, collapse = ", "))
  }
  out <- data.frame(
    date = as.Date(as.character(d[[i_date]])),
    amount = .parse_amount(d[[i_amount]]),
    direction = if (is.na(i_dir)) "" else as.character(d[[i_dir]]),
    counterparty = if (is.na(i_party)) "" else as.character(d[[i_party]]),
    description = if (is.na(i_desc)) "" else as.character(d[[i_desc]]),
    source = source,
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$date) & is.finite(out$amount), ]
  if (spend_only) {
    # WeChat marks spending rows with "/" in 收/支; Alipay uses "支出".
    # Uniform rule: drop income rows and internal transfers, keep the rest.
    dir_clean <- trimws(out$direction)
    drop <- grepl("收入|不计收支", dir_clean)
    out <- out[!drop, ]
  }
  out[order(out$date), ]
}

#' Import a generic date-value stream (sleep / sport / anything)
#'
#' @param path CSV path with at least a date column and a numeric column
#' @param date_col name of the date column (auto-detected if omitted)
#' @param value_col name of the numeric column (auto-detected if omitted)
#' @return data.frame with columns \code{date} and \code{value}
#' @export
import_stream <- function(path, date_col = NULL, value_col = NULL) {
  d <- .read_csv_lenient(path, header_patterns = "日期|时间|date",
                         required_pattern = "日期|时间|date")
  nms <- names(d)
  if (is.null(date_col)) {
    j <- .pick_col(nms, c("时间", "日期", "date"))
    if (is.na(j)) stop("cannot auto-detect date column")
    date_col <- nms[j]
  }
  if (is.null(value_col)) {
    j <- .pick_col(nms, c("数值", "值", "value", "分钟", "卡路里", "公里", "步数"))
    if (is.na(j)) stop("cannot auto-detect value column")
    value_col <- nms[j]
  }
  out <- data.frame(
    date = as.Date(as.character(d[[date_col]])),
    value = .parse_amount(d[[value_col]])
  )
  out <- out[!is.na(out$date) & is.finite(out$value), ]
  out[order(out$date), ]
}
