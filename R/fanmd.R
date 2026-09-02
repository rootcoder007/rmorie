# SPDX-License-Identifier: AGPL-3.0-or-later
#' Functional ANOVA (Sobol) decomposition
#'
#' Sobol (1993), Mathematical Modelling and Computational Experiments
#' 1(4), 407-414.  On the unit cube with independent inputs, f = f_0 +
#' sum_i f_i(x_i) + sum_(i<j) f_ij(x_i, x_j) + ... uniquely, every summand
#' integrating to zero over each of its own variables, with f_0 = E\[f\],
#' f_i = E\[f | x_i\] - f_0 and f_ij = E\[f | x_i, x_j\] - f_i - f_j - f_0;
#' the component variances sum to the total.  The 1993 paper was not
#' retrievable here; the decomposition and its orthogonality are quoted in
#' their standard published form.  The conditional expectations are
#' evaluated on a tensor grid rather than sampled, so the decomposition is
#' exact to the quadrature error; the sum-to-total identity is returned as
#' `closure` so the result can be checked rather than trusted.
#'
#' @param f the function.
#' @param input_dist optional per-dimension inverse CDFs.
#' @param d input dimension.
#' @param grid points per dimension.
#' @return list: estimate, f0, D, D_main, D_int, closure, method.
#' @keywords internal
#' @examples
#' Fanova(function(x) x[1] + 2 * x[2], NULL, 2, 6)$closure
#' @export
Fanova <- function(f, input_dist = NULL, d = 2, grid = 8) {
  dd <- as.integer(d); g <- as.integer(grid)
  pts <- (seq_len(g) - 1 + 0.5) / g
  tf <- function(row) {
    if (is.null(input_dist)) return(as.numeric(row))
    vapply(seq_len(dd), function(a) input_dist[[a]](row[a]), 0)
  }
  total <- g^dd
  vals <- numeric(total)
  rows <- matrix(0L, total, dd)
  for (cc in seq_len(total) - 1L) {
    rem <- cc
    idx <- integer(dd)
    for (a in seq(dd, 1L)) { idx[a] <- rem %% g; rem <- rem %/% g }
    rows[cc + 1L, ] <- idx
    vals[cc + 1L] <- as.numeric(f(tf(pts[idx + 1L])))
  }
  f0 <- .s03mean(vals)
  D <- 0
  for (v in vals) D <- D + (v - f0)^2 / total
  main <- numeric(dd); mainf <- vector("list", dd)
  for (a in seq_len(dd)) {
    m <- numeric(g); cnt <- numeric(g)
    for (cc in seq_len(total)) {
      m[rows[cc, a] + 1L] <- m[rows[cc, a] + 1L] + vals[cc]
      cnt[rows[cc, a] + 1L] <- cnt[rows[cc, a] + 1L] + 1
    }
    comp <- m / cnt - f0
    mainf[[a]] <- comp
    s <- 0
    for (t in seq_len(g)) s <- s + comp[t]^2 / g
    main[a] <- s
  }
  inter <- numeric(0)
  if (dd > 1L) for (a in seq_len(dd - 1L)) for (b in seq(a + 1L, dd)) {
    m <- matrix(0, g, g); cnt <- matrix(0, g, g)
    for (cc in seq_len(total)) {
      m[rows[cc, a] + 1L, rows[cc, b] + 1L] <- m[rows[cc, a] + 1L, rows[cc, b] + 1L] + vals[cc]
      cnt[rows[cc, a] + 1L, rows[cc, b] + 1L] <- cnt[rows[cc, a] + 1L, rows[cc, b] + 1L] + 1
    }
    s <- 0
    for (t in seq_len(g)) for (u in seq_len(g)) {
      comp <- m[t, u] / cnt[t, u] - mainf[[a]][t] - mainf[[b]][u] - f0
      s <- s + comp^2 / (g * g)
    }
    inter <- c(inter, s)
  }
  acc <- 0
  for (v in main) acc <- acc + v
  for (v in inter) acc <- acc + v
  list(estimate = f0, f0 = f0, D = D, D_main = main, D_int = inter,
       closure = if (D > 0) acc / D else NaN,
       method = "Sobol (1993) functional ANOVA decomposition on a tensor grid")
}
