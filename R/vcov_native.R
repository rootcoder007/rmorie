# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Native robust covariance estimators (feat/native-specializations,
# module 27). Replaces the sandwich package: HC0-HC5
# heteroskedasticity-consistent, HAC (Newey-West / Bartlett), and
# one-way clustered (CR0/CR1) covariance for fitted lm / glm models.
# tests/cross validates each against sandwich to machine precision.
#
# All estimators are the sandwich V = bread * meat * bread where
# bread is the inverse expected-information scaled by n and meat is
# the (weighted) outer product of the score contributions (estfun).

#' Internal helper: score matrix (estfun) + bread for lm / glm
#' @noRd
.morie_vcov_pieces <- function(model) {
  X <- stats::model.matrix(model)
  n <- nrow(X)
  if (inherits(model, "glm")) {
    r <- model$residuals * model$weights          # working residuals
    ef <- X * r
    disp <- 1
    bread <- stats::summary.glm(model)$cov.unscaled * n
  } else {
    e <- stats::residuals(model)
    w <- stats::weights(model)
    if (is.null(w)) w <- rep(1, n)
    ef <- (X * (e * w))
    XtXinv <- chol2inv(chol(crossprod(X * sqrt(w))))
    bread <- XtXinv * n
    disp <- 1
  }
  list(X = X, estfun = ef, bread = bread, n = n, k = ncol(X),
       resid = stats::residuals(model))
}

#' Heteroskedasticity-consistent covariance (native, HC0-HC5)
#'
#' Reproduces \code{sandwich::vcovHC}. The meat is
#' \eqn{X' \Omega X} with \eqn{\Omega} the diagonal of adjusted
#' squared residuals, and the small-sample adjustment set by
#' \code{type}: HC0 (none), HC1 (\eqn{n/(n-k)}), HC2
#' (\eqn{1/(1-h_i)}), HC3 (\eqn{1/(1-h_i)^2}), HC4
#' (\eqn{1/(1-h_i)^{\delta_i}}, \eqn{\delta_i=\min(4, h_i/\bar h)}),
#' HC4m, HC5.
#'
#' @param model A fitted \code{lm} or \code{glm}.
#' @param type One of \code{"HC3"} (default), \code{"HC0"}-\code{"HC5"},
#'   \code{"HC4m"}, or \code{"const"} (the classical estimator).
#' @return The coefficient covariance matrix.
#' @references MacKinnon, J. G., & White, H. (1985). Some
#'   heteroskedasticity-consistent covariance matrix estimators.
#'   \emph{Journal of Econometrics}, 29(3), 305-325.
#' @examples
#' m <- lm(mpg ~ wt + hp, data = mtcars)
#' morie_vcov_hc(m, "HC1")
#' @export
morie_vcov_hc <- function(model, type = "HC3") {
  p <- .morie_vcov_pieces(model)
  X <- p$X; ef <- p$estfun; n <- p$n; k <- p$k
  br <- p$bread / n
  if (identical(type, "const")) {
    s2 <- sum(p$resid^2) / (n - k)
    return(.morie_vcov_name(s2 * (p$bread / n), colnames(X)))
  }
  # Adjust the score contributions (estfun rows) by a per-observation
  # factor whose square is the HC omega weight, so that
  # crossprod(scaled estfun) is X' Omega X for lm and the analogous
  # score outer product for glm.
  h <- stats::hatvalues(model)   # working-weight leverage (lm + glm)
  hbar <- mean(h)
  fac <- switch(type,
    HC0 = rep(1, n),
    HC1 = rep(sqrt(n / (n - k)), n),
    HC2 = 1 / sqrt(1 - h),
    HC3 = 1 / (1 - h),
    HC4 = 1 / (1 - h)^(pmin(4, h / hbar) / 2),
    HC4m = 1 / (1 - h)^((pmin(1, h / hbar) + pmin(1.5, h / hbar)) / 2),
    HC5 = 1 / (1 - h)^(pmin(pmax(4, 0.7 * max(h) / hbar),
                            h / hbar) / 2),
    stop("unknown HC type: ", type))
  meat <- crossprod(ef * fac)
  .morie_vcov_name(br %*% meat %*% br, colnames(X))
}

#' Internal: restore coefficient dimnames (chol2inv drops them)
#' @noRd
.morie_vcov_name <- function(V, nm) {
  dimnames(V) <- list(nm, nm)
  V
}

#' HAC (Newey-West) covariance (native)
#'
#' Reproduces \code{sandwich::vcovHAC} / \code{sandwich::NeweyWest}
#' with the Bartlett kernel and a fixed lag. The meat accumulates the
#' weighted lag-l autocovariances of the score contributions.
#'
#' @param model A fitted \code{lm} / \code{glm}.
#' @param lag Number of lags (default the Newey-West rule
#'   \eqn{\lfloor 4 (n/100)^{2/9} \rfloor}).
#' @param prewhite Currently ignored (kept for signature parity).
#' @param adjust Apply the \eqn{n/(n-k)} finite-sample factor
#'   (default TRUE, matching \code{sandwich::NeweyWest}).
#' @return The coefficient covariance matrix.
#' @references Newey, W. K., & West, K. D. (1987). A simple, positive
#'   semi-definite, heteroskedasticity and autocorrelation consistent
#'   covariance matrix. \emph{Econometrica}, 55(3), 703-708.
#' @export
morie_vcov_hac <- function(model, lag = NULL, prewhite = FALSE,
                           adjust = TRUE) {
  p <- .morie_vcov_pieces(model)
  ef <- p$estfun; n <- p$n; k <- p$k
  if (is.null(lag)) lag <- floor(4 * (n / 100)^(2 / 9))
  meat <- crossprod(ef)
  for (l in seq_len(lag)) {
    wl <- 1 - l / (lag + 1)
    G <- crossprod(ef[seq_len(n - l), , drop = FALSE],
                   ef[(l + 1):n, , drop = FALSE])
    meat <- meat + wl * (G + t(G))
  }
  if (adjust) meat <- meat * n / (n - k)
  br <- p$bread / n
  .morie_vcov_name(br %*% meat %*% br, colnames(p$X))
}

#' One-way clustered covariance (native, CR0 / CR1)
#'
#' Reproduces \code{sandwich::vcovCL} for one-way clustering: the meat
#' sums the outer products of cluster-summed score contributions,
#' with the CR1 small-sample factor
#' \eqn{\frac{G}{G-1}\cdot\frac{n-1}{n-k}} by default.
#'
#' @param model A fitted \code{lm} / \code{glm}.
#' @param cluster A grouping vector (length n).
#' @param type \code{"HC1"} (CR1, default) or \code{"HC0"} (CR0).
#' @return The coefficient covariance matrix.
#' @references Cameron, A. C., & Miller, D. L. (2015). A practitioner's
#'   guide to cluster-robust inference. \emph{Journal of Human
#'   Resources}, 50(2), 317-372.
#' @export
morie_vcov_cl <- function(model, cluster, type = "HC1") {
  p <- .morie_vcov_pieces(model)
  ef <- p$estfun; n <- p$n; k <- p$k
  cf <- as.factor(cluster)
  G <- nlevels(cf)
  scores <- rowsum(ef, cf, reorder = FALSE)
  meat <- crossprod(scores)
  # sandwich::vcovCL applies the G/(G-1) cluster factor for both types
  # (cadjust = TRUE); HC1 adds the (n-1)/(n-k) residual adjustment.
  adj <- switch(type,
    HC1 = (G / (G - 1)) * ((n - 1) / (n - k)),
    HC0 = G / (G - 1),
    stop("type must be HC0 or HC1"))
  br <- p$bread / n
  .morie_vcov_name(adj * (br %*% meat %*% br), colnames(p$X))
}

#' Unified robust-covariance dispatcher (native)
#'
#' A single entry point over the native HC / HAC / CL estimators,
#' mirroring the \code{type} vocabulary used by
#' \code{morie_causal_robust_se()}.
#'
#' @param model A fitted \code{lm} / \code{glm}.
#' @param type \code{"HC0"}-\code{"HC5"}, \code{"HC4m"},
#'   \code{"const"}, \code{"HAC"}, or \code{"CL"}.
#' @param cluster Cluster vector (required for \code{"CL"}).
#' @param ... Passed to the underlying estimator (e.g. \code{lag}).
#' @return The coefficient covariance matrix.
#' @export
morie_vcov_robust <- function(model, type = "HC3", cluster = NULL,
                              ...) {
  if (identical(type, "HAC")) return(morie_vcov_hac(model, ...))
  if (identical(type, "CL")) {
    if (is.null(cluster)) stop("type = 'CL' needs `cluster`.",
                               call. = FALSE)
    return(morie_vcov_cl(model, cluster, ...))
  }
  morie_vcov_hc(model, type)
}
