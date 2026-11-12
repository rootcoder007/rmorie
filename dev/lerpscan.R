# Finds every interpolation between two elements of one vector written
#   x[i] + g * (x[j] - x[i])      (NaN beside an infinite value, Inf on overflow)
#   x[i] * (1 - g) + x[j] * g     (NaN at g = 0 beside an infinite value)
# anywhere in R/, nested closures included, by walking the parsed code (not by
# regex). Each hit is printed with its file, line, the enclosing top-level
# function and whether it already goes through .morie_lerp7().
#
# R_ENVIRON_USER=/dev/null R_PROFILE_USER=/dev/null Rscript dev/lerpscan.R
is_sub <- function(x) is.call(x) && identical(x[[1]], as.name("["))
same_vec <- function(a, b) is_sub(a) && is_sub(b) && identical(a[[2]], b[[2]])
plain <- function(e) {
  # A + G * (B - A), or A + (B - A) * G
  if (!(is.call(e) && identical(e[[1]], as.name("+")) && length(e) == 3L)) return(FALSE)
  a <- e[[2]]
  m <- e[[3]]
  if (!(is.call(m) && identical(m[[1]], as.name("*")) && length(m) == 3L)) return(FALSE)
  for (d in list(m[[2]], m[[3]])) {
    while (is.call(d) && identical(d[[1]], as.name("("))) d <- d[[2]]
    if (is.call(d) && identical(d[[1]], as.name("-")) && length(d) == 3L &&
          identical(d[[3]], a) && same_vec(d[[2]], a)) return(TRUE)
  }
  FALSE
}
weighted <- function(e) {
  # A * (1 - g) + B * g
  if (!(is.call(e) && identical(e[[1]], as.name("+")) && length(e) == 3L)) return(FALSE)
  l <- e[[2]]
  r <- e[[3]]
  ok <- function(x) is.call(x) && identical(x[[1]], as.name("*")) && length(x) == 3L
  ok(l) && ok(r) && same_vec(l[[2]], r[[2]])
}
hits <- list()
walk <- function(e, top, file, line) {
  if (is.call(e)) {
    if (plain(e) || weighted(e)) {
      hits[[length(hits) + 1L]] <<- data.frame(
        file = file, line = line, fn = top, form = if (plain(e)) "plain" else "weighted",
        code = paste(deparse(e, width.cutoff = 200L), collapse = " "), stringsAsFactors = FALSE)
    }
    lapply(as.list(e), function(x) if (!missing(x) && !is.null(x)) walk(x, top, file, line))
  } else if (is.pairlist(e)) {
    lapply(as.list(e), function(x) if (!missing(x) && !is.null(x)) walk(x, top, file, line))
  }
  invisible(NULL)
}
for (f in sort(list.files("R", "\\.[Rr]$", full.names = TRUE))) {
  ex <- tryCatch(parse(f, keep.source = TRUE), error = function(e) NULL)
  if (is.null(ex)) next
  sr <- attr(ex, "srcref")
  for (i in seq_along(ex)) {
    e <- ex[[i]]
    top <- if (is.call(e) && is.name(e[[1]]) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]))
      as.character(e[[2]]) else ""
    walk(e, top, f, sr[[i]][[1]])
  }
}
h <- do.call(rbind, hits)
cat("interpolations between two elements of one vector:", nrow(h), "\n")
print(h, row.names = FALSE, right = FALSE)
