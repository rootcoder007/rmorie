# SPDX-License-Identifier: AGPL-3.0-or-later

#' ARDL bounds test for a level relationship
#'
#' The bounds test asks whether a long-run level relationship exists
#' between \code{y} and \code{X} without first having to decide whether
#' the regressors are I(0) or I(1). It is run on the conditional
#' error-correction form
#' \deqn{\Delta y_t = c_0 [+ c_1 t] + \pi_{yy} y_{t-1}
#'   + \sum_j \pi_{yx,j} x_{j,t-1} + \sum_{i=1}^{p-1} \psi_i \Delta y_{t-i}
#'   + \sum_j \sum_{i=0}^{q-1} \beta_{ji} \Delta x_{j,t-i} + u_t,}
#' in which the lagged \emph{levels} carry the long-run information and
#' the differences absorb the short-run dynamics.
#'
#' Two statistics are reported. \code{f_statistic} is the Wald/F statistic
#' for the joint null \eqn{\pi_{yy} = 0} and \eqn{\pi_{yx,j} = 0} for
#' every \eqn{j} --- no level relationship --- which is the bounds test
#' proper. \code{t_statistic} is the t-ratio for \eqn{\pi_{yy} = 0} alone,
#' the complementary test on the speed of adjustment.
#'
#' Neither statistic has a standard null distribution: under the null both
#' limits depend on whether the regressors are I(0) or I(1), which is why
#' the test is read against a \emph{pair} of critical values --- a lower
#' bound assuming all regressors are I(0) and an upper bound assuming all
#' are I(1). A statistic above the upper bound rejects, one below the
#' lower bound does not, and one between the two is inconclusive.
#'
#' \strong{The critical value bounds are not returned.} They are tabulated
#' in Tables CI(i)--CI(v) of Pesaran, Shin & Smith (2001), one panel per
#' deterministic case, and no accessible copy of those tables could be
#' obtained when this function was written --- the article is paywalled
#' and no authoritative open restatement was found. Reproducing them from
#' memory would be inventing numbers, so the statistics are returned for
#' the caller to compare against the published table for their
#' \code{case} and number of regressors. \code{f_pvalue_iid} is a
#' diagnostic only: it is the p-value from the ordinary F distribution,
#' which is \emph{not} the null distribution of this test and is always
#' too small.
#'
#' \code{case} follows the paper's numbering of the deterministic terms:
#' 2 restricted intercept and no trend, 3 unrestricted intercept and no
#' trend, 4 unrestricted intercept and restricted trend, 5 unrestricted
#' intercept and unrestricted trend. Cases 3 and 5 are fitted here; cases
#' 2 and 4 place the deterministic term inside the error-correction term
#' and are rejected rather than silently fitted as case 3.
#'
#' Mirrors \code{morie.fn.ardlmd} on the Python side.
#'
#' @param y Numeric vector, the dependent series.
#' @param X Numeric matrix of regressors; a plain vector is read as a
#'   single regressor.
#' @param p Lag order of \code{y}: \eqn{p-1} lagged differences enter.
#' @param q Lag order of each regressor: \eqn{q-1} lagged differences of
#'   each \eqn{x} enter, alongside the contemporaneous difference.
#' @param case Deterministic specification, 3 or 5.
#' @return Named list with \code{f_statistic}, \code{t_statistic},
#'   \code{df_num}, \code{df_den}, \code{n_used}, \code{k},
#'   \code{f_pvalue_iid}, \code{pi_yy}, \code{case}, \code{p}, \code{q},
#'   \code{method}.
#' @references Pesaran M H, Shin Y & Smith R J (2001). Bounds testing
#'   approaches to the analysis of level relationships. \emph{Journal of
#'   Applied Econometrics} 16(3), 289--326, equation (16).
#' @examples
#' set.seed(8)
#' x <- cumsum(rnorm(80))
#' y <- 0.6 * x + rnorm(80)
#' Ardlmd(y, x, p = 2, q = 2)$f_statistic
#' @export
Ardlmd <- function(y, X, p = 1L, q = 1L, case = 3L) {
  yv <- as.numeric(y)
  xm <- if (is.null(dim(X))) matrix(as.numeric(X), ncol = 1L) else
    matrix(as.numeric(as.matrix(X)), nrow = nrow(X))
  n <- length(yv)
  if (nrow(xm) != n) {
    stop("y and X must have the same number of rows", call. = FALSE)
  }
  k <- ncol(xm)
  p <- as.integer(p); q <- as.integer(q); case <- as.integer(case)
  if (p < 1L || q < 1L) stop("p and q must be at least 1", call. = FALSE)
  if (!(case %in% c(3L, 5L))) {
    stop("only cases 3 and 5 are fitted here; cases 2 and 4 restrict the ",
         "deterministic term inside the error correction term and need a ",
         "different estimator", call. = FALSE)
  }

  dy <- diff(yv)
  dx <- apply(xm, 2L, diff)
  if (is.null(dim(dx))) dx <- matrix(dx, ncol = k)
  start <- max(p, q)                      # 0-based
  rows <- seq.int(start, n - 1L)
  if (length(rows) < 3L) {
    stop("not enough observations after lagging", call. = FALSE)
  }

  resp <- dy[rows]
  ndet <- if (case == 5L) 2L else 1L
  design <- matrix(0, nrow = length(rows), ncol = 0L)
  design <- cbind(design, rep(1, length(rows)))
  if (case == 5L) design <- cbind(design, rows + 1)
  design <- cbind(design, yv[rows])                       # pi_yy
  for (j in seq_len(k)) design <- cbind(design, xm[rows, j])
  if (p > 1L) {
    for (i in seq_len(p - 1L)) design <- cbind(design, dy[rows - i])
  }
  for (j in seq_len(k)) {
    for (i in seq.int(0L, q - 1L)) design <- cbind(design, dx[rows - i, j])
  }

  m <- ncol(design)
  nu <- length(rows)
  qrx <- qr(design)
  if (qrx$rank < m) {
    stop("the conditional ECM design is rank deficient; reduce p or q",
         call. = FALSE)
  }
  beta <- qr.coef(qrx, resp)
  rss_u <- sum((resp - as.vector(design %*% beta))^2)
  df_den <- nu - m
  if (df_den <= 0L) {
    stop("no residual degrees of freedom; reduce p or q", call. = FALSE)
  }
  sigma2 <- rss_u / df_den
  xtxi <- chol2inv(qr.R(qrx))
  se_pi <- sqrt(sigma2 * xtxi[ndet + 1L, ndet + 1L])

  ## Restricted fit: drop the k + 1 lagged level columns.
  drop_idx <- seq.int(ndet + 1L, ndet + k + 1L)
  rdes <- design[, -drop_idx, drop = FALSE]
  qrr <- qr(rdes)
  if (qrr$rank < ncol(rdes)) {
    stop("the restricted design is rank deficient", call. = FALSE)
  }
  rss_r <- sum((resp - as.vector(rdes %*% qr.coef(qrr, resp)))^2)

  df_num <- k + 1L
  fstat <- ((rss_r - rss_u) / df_num) / (rss_u / df_den)
  tstat <- unname(beta[ndet + 1L]) / se_pi

  list(f_statistic = fstat,
       t_statistic = tstat,
       df_num = df_num,
       df_den = df_den,
       n_used = nu,
       k = k,
       f_pvalue_iid = stats::pf(fstat, df_num, df_den, lower.tail = FALSE),
       pi_yy = unname(beta[ndet + 1L]),
       case = case,
       p = p,
       q = q,
       method = paste("ARDL bounds test for a level relationship",
                      "(Pesaran, Shin & Smith 2001, eq. 16)"))
}
