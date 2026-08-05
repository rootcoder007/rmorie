# SPDX-License-Identifier: AGPL-3.0-or-later
#' Krippendorff's alpha reliability
#'
#' The coincidence matrix counts every ordered pair of values within a
#' unit, weighted by 1/(m_u - 1), so units coded by different numbers of
#' coders are handled without imputation.  The difference function is
#' what encodes the level of measurement.
#'
#' Formula: alpha = 1 - D_o / D_e with
#'   D_o = sum o_ck delta^2 / n and
#'   D_e = sum n_c n_k delta^2 / (n (n - 1)).
#'
#' @param data Coders-by-units matrix; NA marks a missing value.
#' @param level One of nominal, ordinal, interval, ratio.
#' @return List with \code{estimate}, \code{alpha}, \code{D_o},
#'   \code{D_e}, \code{n_pairable}, \code{n_units}, \code{method}.
#' @references Krippendorff (2004), Content Analysis: An Introduction to
#'   Its Methodology, 2nd ed., Sage, ch. 11.
#' @export
Krpalp <- function(data, level = "nominal") {
  levels_ok <- c("nominal", "ordinal", "interval", "ratio")
  if (!(level %in% levels_ok)) stop("krippendorff_alpha: level must be one of nominal, ordinal, interval, ratio")
  M <- .s03mat(data)
  if (nrow(M) == 0L || ncol(M) == 0L) stop("krippendorff_alpha: data is empty")
  units <- list()
  for (u in seq_len(ncol(M))) {
    col <- M[, u]
    col <- col[!is.na(col)]
    if (length(col) >= 2L) units[[length(units) + 1L]] <- col
  }
  if (length(units) == 0L) stop("krippendorff_alpha: no unit has two or more values")
  vals <- sort(unique(unlist(units)))
  V <- length(vals)
  o <- matrix(0, V, V)
  for (col in units) {
    mu <- length(col)
    idx <- match(col, vals)
    for (ai in seq_len(mu)) for (bi in seq_len(mu)) if (ai != bi)
      o[idx[ai], idx[bi]] <- o[idx[ai], idx[bi]] + 1 / (mu - 1)
  }
  nc <- rowSums(o)
  n <- sum(nc)
  if (n <= 1) stop("krippendorff_alpha: fewer than two pairable values")
  d2 <- function(i, j) {
    if (level == "nominal") return(if (i == j) 0 else 1)
    if (level == "interval") return((vals[i] - vals[j])^2)
    if (level == "ratio") {
      s <- vals[i] + vals[j]
      return(if (s == 0) 0 else ((vals[i] - vals[j]) / s)^2)
    }
    lo <- min(i, j); hi <- max(i, j)
    (sum(nc[lo:hi]) - (nc[i] + nc[j]) / 2)^2
  }
  do <- 0; de <- 0
  for (i in seq_len(V)) for (j in seq_len(V)) {
    dd <- d2(i, j)
    do <- do + o[i, j] * dd
    de <- de + nc[i] * nc[j] * dd
  }
  do <- do / n
  de <- de / (n * (n - 1))
  a <- if (de == 0) 1 else 1 - do / de
  .t1_result(estimate = a, alpha = a, D_o = do, D_e = de, n_pairable = n,
             n_units = length(units),
             method = "alpha = 1 - D_o/D_e on the coincidence matrix, Krippendorff (2004) ch. 11")
}
