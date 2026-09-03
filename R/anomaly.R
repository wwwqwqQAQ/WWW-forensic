# Robust anomaly detection ------------------------------------------------

#' Robust MAD-based z-score
#'
#' \code{(x - median) / (1.4826 * MAD)}, i.e. the usual robust standardized
#' score. Returns all zeros when MAD is zero (e.g. a constant run of values).
#'
#' @param x numeric vector
#' @return numeric z-score vector, same length as \code{x}
#' @export
mad_z <- function(x) {
  m <- stats::median(x, na.rm = TRUE)
  a <- stats::mad(x, na.rm = TRUE)
  if (is.na(a) || a == 0) return(rep(0, length(x)))
  0.6745 * (x - m) / a
}

#' Flag anomalies in a numeric column
#'
#' Applies \code{\link{mad_z}} to a column, optionally per group, and flags
#' rows with |z| > \code{k}. Robust to outliers by construction (median/MAD),
#' so the flagging itself is not skewed by the extreme values.
#'
#' @param data data.frame
#' @param value name of the numeric column to scan
#' @param group optional name of a grouping column (per-group baseline)
#' @param k z threshold; default 3.5 (approx. 0.05% false positive rate
#'   under normality)
#' @return \code{data} with two added columns: \code{.z} and \code{.anomaly}
#' @export
detect_anomalies <- function(data, value, group = NULL, k = 3.5) {
  data <- as.data.frame(data)
  if (!value %in% names(data)) stop("column '", value, "' not found")
  if (!is.null(group) && !group %in% names(data)) {
    stop("group column '", group, "' not found")
  }
  if (is.null(group)) {
    z <- mad_z(data[[value]])
  } else {
    z <- ave(data[[value]], data[[group]], FUN = mad_z)
  }
  data$.z <- round(z, 4)
  data$.anomaly <- abs(z) > k
  data
}

#' Aggregate a date-value stream into weekly totals
#'
#' ISO weeks (Mon-Sun) are used so calendar boundaries do not split weeks.
#'
#' @param data data.frame with a date-like column and a numeric column
#' @param date name of the date column (coerced with \code{as.Date})
#' @param value name of the numeric column to sum
#' @return data.frame with columns \code{week} (ISO week label),
#'   \code{week_start} (Monday of that week) and \code{total}
#' @export
weekly_totals <- function(data, date, value) {
  d <- data.frame(
    date = as.Date(data[[date]]),
    value = as.numeric(data[[value]])
  )
  d <- d[order(d$date), ]
  d <- d[is.finite(d$value), ]
  d$week <- format(d$date, "%G-W%V")
  # Monday of the ISO week, computed directly from the date (%u: Mon=1..Sun=7)
  dow <- as.integer(format(d$date, "%u"))
  d$week_start <- d$date - (dow - 1L)
  grp <- d$week
  tot <- ave(d$value, grp, FUN = function(v) sum(v, na.rm = TRUE))
  ws <- ave(as.integer(d$week_start), grp, FUN = min)
  idx <- !duplicated(grp)
  out <- data.frame(
    week = grp[idx],
    week_start = as.Date(ws[idx], origin = "1970-01-01"),
    total = tot[idx]
  )
  out[order(out$week_start), ]
}

#' Compact statistical summary of one stream
#'
#' @param data data.frame
#' @param value name of the numeric column
#' @return list with n, mean, median, sd, MAD, zero count and a quick verdict
#' @export
stream_summary <- function(data, value) {
  v <- as.numeric(data[[value]])
  v <- v[is.finite(v)]
  list(
    n = length(v),
    mean = mean(v),
    median = stats::median(v),
    sd = stats::sd(v),
    mad = stats::mad(v),
    zeros = sum(v == 0),
    verdict = if (stats::median(v) == 0) "mostly zero; check recording" else "ok"
  )
}
