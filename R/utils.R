# Internal helpers ---------------------------------------------------------

# A CJK-capable font family for plots: ggplot's default font renders Chinese
# as boxes on Windows, so force Microsoft YaHei there.
.plot_family <- function() {
  if (.Platform$OS.type == "windows") "Microsoft YaHei" else "sans"
}

# Resolve the actual column name for one stream: explicit column first (scalar
# or per-stream named vector), then a sensible default ("value"/"date").
.resolve_col <- function(s, nm, col, default = "value") {
  cand <- character()
  if (is.character(col) && length(col) >= 1) {
    if (!is.null(names(col)) && nm %in% names(col)) {
      cand <- c(col[[nm]], default)
    } else {
      cand <- c(col[1], default)
    }
  }
  cand <- unique(cand)
  hit <- cand[cand %in% names(s)]
  if (!length(hit)) {
    stop("stream '", nm, "': cannot find column ",
         paste(cand, collapse = " or "))
  }
  hit[1]
}
