# SPDX-License-Identifier: AGPL-3.0-or-later

#' Identification of beta and G in a single-index model
#'
#' Horowitz (2009), Semiparametric and Nonparametric Methods in
#' Econometrics, Section 2.3.1, Theorem 2.1 (pages 12-14).  Model
#' (2.1) is E(Y | X = x) = G(x'beta), and beta and G are identified if
#' (a) G is differentiable and not constant on the support of X'beta,
#' (b) the components of X are continuously distributed with a joint
#' density, (c) the support of X is not contained in any proper linear
#' subspace of R^d, and (d) beta_1 = 1.
#'
#' Condition (a) concerns the unknown G, so only observable evidence
#' for it is reported, and only when \code{y} is supplied.
#'
#' @param x Numeric matrix of covariates, n by d, with no constant column.
#' @param beta Numeric vector of index coefficients, scale normalised
#'   so that \code{beta\[1\] == 1}.
#' @param y Optional numeric outcome vector; supplied, the spread of
#'   the mean of Y across index deciles is reported as evidence on
#'   condition (a).
#' @param mindistinct Integer; a column of \code{x} counts as
#'   continuously distributed when it takes at least this many
#'   distinct values.  Fixed default, not chosen from the data.
#' @return Named list with identified, conda, condb, condc, condd,
#'   rank, dim, minsv, ncontin, gspread, nconstcol, n, method.
#' @keywords internal
#' @examples
#' x <- cbind(seq(-2, 2, length.out = 200), seq(3, -1, length.out = 200)^2)
#' Simident(x, c(1, 0.5))$identified
#' @export
Simident <- function(x, beta, y = NULL, mindistinct = 10L) {
  X <- if (is.null(dim(x))) matrix(x, ncol = 1L) else as.matrix(x)
  b <- as.numeric(beta)
  if (ncol(X) != length(b) && nrow(X) == length(b)) X <- t(X)
  n <- nrow(X)
  d <- ncol(X)

  condd <- abs(b[1L] - 1) <= 1e-12

  sv <- svd(X)$d
  rank <- if (length(sv) && sv[1L] > 0) sum(sv > sv[1L] * 1e-12) else 0L
  minsv <- if (length(sv)) sv[length(sv)] else 0
  condc <- rank == d

  sdcol <- apply(X, 2L, function(v) sqrt(mean((v - mean(v))^2)))
  nconstcol <- sum(sdcol <= 0)

  ncontin <- sum(vapply(seq_len(d),
                        function(j) length(unique(X[, j])) >= mindistinct,
                        logical(1L)))
  condb <- ncontin == d

  z <- as.numeric(X %*% b)
  if (is.null(y)) {
    conda <- NA
    gspread <- NaN
  } else {
    yv <- as.numeric(y)
    cuts <- as.numeric(stats::quantile(z, seq(0, 1, length.out = 11L)))
    means <- numeric(0)
    for (k in seq_len(10L)) {
      lo <- cuts[k]
      hi <- cuts[k + 1L]
      sel <- if (k == 10L) z >= lo & z <= hi else z >= lo & z < hi
      if (any(sel)) means <- c(means, mean(yv[sel]))
    }
    gspread <- if (length(means)) max(means) - min(means) else 0
    conda <- gspread > 0
  }

  identified <- condb && condc && condd && nconstcol == 0L &&
    (is.na(conda) || conda)
  list(identified = identified, conda = conda, condb = condb,
       condc = condc, condd = condd, rank = as.integer(rank),
       dim = as.integer(d), minsv = minsv, ncontin = as.integer(ncontin),
       gspread = gspread, nconstcol = as.integer(nconstcol), n = n,
       method = "Horowitz (2009) Theorem 2.1 identification conditions")
}

# canonical full-name alias (Py<->R API parity)
#' @rdname Simident
#' @keywords internal
#' @export
morie_horowitz_sim_identification <- Simident
