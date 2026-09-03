# Benford's law: digit distribution checks --------------------------------

#' First significant digit of each positive number
#'
#' E.g. \code{first_digit(c(45, 0.0045, 4500))} is \code{c(4, 4, 4)}.
#' Negative values, zero, \code{NA}, \code{NaN} and infinite values are dropped.
#'
#' @param x numeric vector
#' @return integer vector of first significant digits (1-9)
#' @export
first_digit <- function(x) {
  x <- abs(x)
  x <- x[is.finite(x) & x > 0]
  if (!length(x)) return(integer(0))
  e <- floor(log10(x))
  d <- floor(x / 10^e)
  d[d >= 10] <- 1L # floating point guard (x close to a power of ten)
  as.integer(d)
}

#' Second significant digit of each positive number
#'
#' E.g. \code{second_digit(c(45, 0.0045, 4500))} is \code{c(5, 5, 5)}.
#'
#' @inheritParams first_digit
#' @return integer vector of second significant digits (0-9)
#' @export
second_digit <- function(x) {
  x <- abs(x)
  x <- x[is.finite(x) & x > 0]
  if (!length(x)) return(integer(0))
  e <- floor(log10(x))
  m <- x / 10^e          # in [1, 10)
  dd <- floor(m * 10)     # first two digits as 10..99
  dd[dd >= 100] <- 10L    # floating point guard: "10" -> second digit 0
  as.integer(dd %% 10)
}

#' Expected Benford proportions
#'
#' @param digits 1 (first digit) or 2 (second digit)
#' @return named numeric vector of expected probabilities
#' @export
benford_expected <- function(digits = 1) {
  digits <- as.integer(digits[1])
  stopifnot(digits %in% 1:2)
  if (digits == 1) {
    d <- 1:9
    p <- log10(1 + 1 / d)
    names(p) <- as.character(d)
  } else {
    b <- 0:9
    p <- vapply(b, function(j) sum(log10(1 + 1 / (10 * (1:9) + j))), numeric(1))
    names(p) <- as.character(b)
  }
  p
}

#' Benford's law conformity test
#'
#' Runs a Pearson chi-squared test of the observed digit distribution against
#' Benford's law, plus Nigrini's Mean Absolute Deviation (MAD) statistic and a
#' plain-language verdict.
#'
#' @param x numeric vector of amounts/counts (positive numbers)
#' @param digits 1 = first digit test (default), 2 = second digit test
#' @return object of class \code{"benford_test"} with observed/expected tables,
#'   chi-squared result, MAD and verdict
#' @export
benford_test <- function(x, digits = 1) {
  digits <- as.integer(digits[1])
  stopifnot(digits %in% 1:2)
  d <- if (digits == 1) first_digit(x) else second_digit(x)
  n <- length(d)
  if (n < 50) {
    warning("fewer than 50 usable numbers (n = ", n, "); test is unreliable")
  }
  probs <- benford_expected(digits)
  lev <- names(probs)
  obs <- as.vector(table(factor(d, levels = lev))) # full 1:9 / 0:9 range
  names(obs) <- lev
  expc <- n * probs
  ct <- suppressWarnings(stats::chisq.test(obs, p = probs))
  obs_p <- obs / n
  mad <- mean(abs(obs_p - probs))
  verdict <- if (mad < 0.004) {
    "close conformity"
  } else if (mad < 0.008) {
    "acceptable conformity"
  } else if (mad < 0.012) {
    "marginal conformity"
  } else {
    "nonconformity"
  }
  structure(list(
    digits = digits, n = n,
    observed = obs, expected = expc, proportion = obs_p,
    chi2 = unname(ct$statistic), p_value = ct$p.value,
    mad = mad, verdict = verdict
  ), class = "benford_test")
}

#' @export
print.benford_test <- function(x, ...) {
  cat(sprintf(
    "Benford test — digit %d | n = %d | chi2 = %.2f (p = %.3f) | MAD = %.4f\n",
    x$digits, x$n, x$chi2, x$p_value, x$mad
  ))
  cat("Verdict:", x$verdict, "\n")
  tab <- cbind(observed = x$observed, expected = round(x$expected, 1),
               proportion = round(x$proportion, 3))
  print(tab)
  invisible(x)
}
