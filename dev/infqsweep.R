# Sweeps every quantile / median / IQR / percentile helper in rmorie for the
# infinite-value flaw rmoriebricklayer 1.1.1 fixed in cqnt(), cmed(), ciqr()
# and ctkyfnc(): a type-7 quantile written lo + g * (hi - lo) gives NaN beside
# an infinite value (0 * Inf, Inf - Inf) and an infinity when two finite
# values differ by more than the largest double.
#
# The helpers are found by parsing R/ (names that say quantile, median,
# percentile, iqr or q7), taken from the installed namespace and called on
# small inputs; the reference is R's own quantile(type = 7), median() and
# IQR(). A helper that cannot be called as f(x) or f(x, p) is listed as not
# probed.
#
# R_ENVIRON_USER=/dev/null R_PROFILE_USER=/dev/null Rscript dev/infqsweep.R [out.tsv]
a <- commandArgs(TRUE)
out <- if (length(a)) a[[1]] else ""
ns <- asNamespace("rmorie")
cat("rmorie from", find.package("rmorie"), "\n")
nm <- character()
for (f in list.files("R", "\\.[Rr]$", full.names = TRUE)) {
  ex <- tryCatch(parse(f, keep.source = FALSE), error = function(e) NULL)
  for (e in ex) {
    if (is.call(e) && is.name(e[[1]]) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) &&
          is.call(e[[3]]) && identical(as.character(e[[3]][[1]]), "function")) {
      nm <- c(nm, as.character(e[[2]]))
    }
  }
}
pat <- "(quantile|median|percentile|iqr|q7)"
skip <- paste0("(norm|gamma|beta|chi|student|_t_|gev|gpd|evt|mixture|weight|boot|filter|",
               "frequency|test|_se$|unbiased|rule|prior|kernel|grid|local|bayes|bnp|ci_|_ci|",
               "clr|time|trend|regress|reg$|plot|print|summary|format|fit|band|smooth|cpp$)")
nm <- unique(nm[grepl(pat, nm, ignore.case = TRUE) & !grepl(skip, nm, ignore.case = TRUE)])
qcases <- list(
  list(c(1, 2, Inf), 0.5), list(c(1, 2, Inf), 0.75), list(c(-Inf, 1, 2), 0.25),
  list(c(-Inf, 1), 0.5), list(c(Inf, Inf), 0.5), list(c(-1e308, 1e308), 0.5),
  list(c(1, 2, 3, 4), 0.5), list(c(1, 2, 3, 4), 0.1)
)
mcases <- list(c(-Inf, 1), c(1, Inf), c(1, 2, Inf), c(1, 2, 3, 4))
icases <- list(c(1, 2, 3, Inf), c(1, 2, 3, 4), c(-Inf, 1, 2, 3))
same <- function(x, y) {
  if (!is.numeric(x) || length(x) != 1L) return(NA)
  if (is.nan(y)) return(is.nan(x))
  if (is.infinite(y)) return(identical(x, y))
  isTRUE(abs(x - y) <= 1e-12 * max(1, abs(y)))
}
probe <- function(n) {
  f <- get(n, envir = ns)
  if (!is.function(f)) return(c("not probed", "not a function"))
  fm <- formals(f)
  req <- names(fm)[vapply(fm, function(v) identical(v, quote(expr = )), logical(1))]
  req <- setdiff(req, "...")
  k <- if (grepl("iqr", n, ignore.case = TRUE)) "iqr" else
    if (grepl("median", n, ignore.case = TRUE)) "median" else
      if (grepl("percentile", n, ignore.case = TRUE)) "percentile" else "quantile"
  bad <- character()
  res <- tryCatch({
    if (k %in% c("median", "iqr")) {
      if (length(req) != 1L) return(c("not probed", paste(length(req), "required arguments")))
      for (v in if (k == "median") mcases else icases) {
        want <- if (k == "median") stats::median(v) else stats::IQR(v)
        got <- suppressWarnings(f(v))
        s <- same(got, want)
        if (is.na(s)) return(c("not probed", paste("returns", class(got)[[1]])))
        if (!s) bad <- c(bad, sprintf("c(%s)->%s (R %s)", toString(v), format(got), format(want)))
      }
    } else {
      if (length(req) != 2L) return(c("not probed", paste(length(req), "required arguments")))
      for (cs in qcases) {
        want <- unname(stats::quantile(cs[[1]], cs[[2]], type = 7))
        p <- if (k == "percentile") 100 * cs[[2]] else cs[[2]]
        got <- suppressWarnings(f(cs[[1]], p))
        s <- same(got, want)
        if (is.na(s)) return(c("not probed", paste("returns", class(got)[[1]])))
        if (!s) bad <- c(bad, sprintf("c(%s),%s->%s (R %s)", toString(cs[[1]]), cs[[2]],
                                      format(got), format(want)))
      }
    }
    if (length(bad)) c("differs", paste(bad, collapse = "; ")) else c("matches R", "")
  }, error = function(e) c("error", gsub("\n", " ", substr(conditionMessage(e), 1, 80))))
  res
}
r <- t(vapply(nm, probe, character(2)))
d <- data.frame(fn = nm, status = r[, 1], detail = r[, 2], row.names = NULL)
print(table(d[["status"]]))
print(d[d[["status"]] %in% c("differs", "error"), ], row.names = FALSE, right = FALSE)
if (nzchar(out)) utils::write.table(d, out, sep = "\t", quote = FALSE, row.names = FALSE)
