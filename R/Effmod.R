# SPDX-License-Identifier: AGPL-3.0-or-later
#' Effect modification on the additive and the multiplicative scale
#'
#' A single reference cell is used for all four exposure-by-modifier
#' strata and both scales are reported, because a positive interaction
#' on one scale is compatible with none or a negative one on the other.
#' With \code{p_av} the risk in cell \code{(A = a, V = v)},
#' \code{RR_av = p_av / p_00}, \code{RERI = RR_11 - RR_10 - RR_01 + 1},
#' \code{mult = RR_11 / (RR_10 RR_01)},
#' \code{RD_int = p_11 - p_10 - p_01 + p_00} and \code{AP = RERI/RR_11}.
#' With covariates the four cell risks are the coefficients of a least
#' squares fit on the four cell indicators plus mean-centred
#' covariates, so each is the adjusted risk of its cell at the
#' covariate mean; with covariates omitted they are the crude cell
#' proportions.
#'
#' @param y Binary outcome, 0/1.
#' @param A Binary exposure.
#' @param V Binary effect modifier.
#' @param H Optional covariates to adjust for; mean-centred internally.
#' @return List with \code{estimate}, \code{p00}, \code{p10},
#'   \code{p01}, \code{p11}, \code{rr10}, \code{rr01}, \code{rr11},
#'   \code{reri}, \code{mult}, \code{rd_int}, \code{ap}, \code{n00},
#'   \code{n10}, \code{n01}, \code{n11}, \code{n}.
#' @references Knol, M. J. and VanderWeele, T. J. (2012).
#'   Recommendations for presenting analyses of effect modification and
#'   interaction. International Journal of Epidemiology 41(2), 514-520.
#' @export
Effmod <- function(y, A, V, H = NULL) {
  yv <- .s03vec(y); av <- .s03vec(A); vv <- .s03vec(V)
  n <- length(yv)
  if (n == 0L) stop("Effmod: empty input, y has no observations")
  if (length(av) != n || length(vv) != n)
    stop("Effmod: y, A and V must have the same length")
  if (any(av != 0 & av != 1)) stop("Effmod: A must be binary 0/1")
  if (any(vv != 0 & vv != 1)) stop("Effmod: V must be binary 0/1")
  ca <- c(0, 1, 0, 1); cv <- c(0, 0, 1, 1)
  idx <- lapply(seq_len(4L), function(k) which(av == ca[k] & vv == cv[k]))
  cnt <- vapply(idx, length, 0L)
  if (any(cnt == 0L)) stop("Effmod: every A x V cell must be non-empty")
  if (is.null(H)) {
    p <- vapply(idx, function(z) .s03mean(yv[z]), 0)
  } else {
    Hr <- .s03mat(H)
    if (nrow(Hr) != n) stop("Effmod: H must have one row per observation")
    q <- ncol(Hr)
    cm <- colMeans(Hr)
    Z <- matrix(0, n, 4L + q)
    for (k in seq_len(4L)) Z[idx[[k]], k] <- 1
    for (j in seq_len(q)) Z[, 4L + j] <- Hr[, j] - cm[j]
    beta <- .s03lstsq(Z, yv)
    p <- beta[1:4]
  }
  p00 <- p[1L]; p10 <- p[2L]; p01 <- p[3L]; p11 <- p[4L]
  if (p00 <= 0) stop("Effmod: the reference cell risk must be strictly positive")
  rr10 <- p10 / p00; rr01 <- p01 / p00; rr11 <- p11 / p00
  reri <- rr11 - rr10 - rr01 + 1
  mult <- if (rr10 > 0 && rr01 > 0) rr11 / (rr10 * rr01) else NaN
  .t1_result(estimate = reri, p00 = p00, p10 = p10, p01 = p01, p11 = p11,
             rr10 = rr10, rr01 = rr01, rr11 = rr11, reri = reri,
             mult = mult, rd_int = p11 - p10 - p01 + p00,
             ap = if (rr11 != 0) reri / rr11 else NaN,
             n00 = cnt[1L], n10 = cnt[2L], n01 = cnt[3L], n11 = cnt[4L],
             n = n,
             method = "Effect modification on the additive vs multiplicative scale")
}
