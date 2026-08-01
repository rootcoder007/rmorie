# SPDX-License-Identifier: AGPL-3.0-or-later

#' Spatial prediction in a generalized linear model
#'
#' Prediction happens on the pseudo-data scale, where the model is linear and
#' universal kriging applies, and is then carried back.
#'
#' There are two predictors of the original data here and their error
#' measures are not interchangeable. Eq (6.87) applies the inverse link to
#' the pseudo-scale prediction; the delta method applied to it gives (6.88),
#' and the text states outright that (6.88) "is not the mean-squared
#' prediction error of the inverse linked predictor (6.87). It is the
#' prediction error of a different predictor of the original data." That
#' other predictor is eq (6.90), whose mean-squared prediction error is
#' exactly (6.91). `prediction` and `mspe` are the matched pair;
#' `inverse_link_prediction` is returned separately and without an error
#' measure attached.
#'
#' @param nu Pseudo-data at the observed locations.
#' @param Sigma_nu Covariance of the pseudo-data.
#' @param sigma_0 Covariance between the pseudo-data at the target and at the
#'   observed sites.
#' @param X Design matrix.
#' @param x0 Covariates at the prediction location.
#' @param mu0 Current estimate of the mean at the target, the expansion locus.
#' @param link_kind Link function.
#' @param beta Optional GLS estimate; computed from the data when omitted.
#' @return A list with `prediction`, `mspe`, `prediction_error`,
#'   `inverse_link_prediction`, `pseudo_scale_prediction` and `mspe_is_for`.
#' @references Schabenberger Ch 6, Sec 6.3.6, eqs (6.87)-(6.91)
#' @export
spglmk <- function(nu, Sigma_nu, sigma_0, X, x0, mu0, link_kind = "log",
                   beta = NULL) {
  nu <- as.numeric(nu); S <- as.matrix(Sigma_nu)
  s0 <- as.numeric(sigma_0); X <- as.matrix(X); x0 <- as.numeric(x0)
  n <- length(nu)
  if (!all(dim(S) == c(n, n)) || length(s0) != n || nrow(X) != n) {
    stop("`nu`, `Sigma_nu`, `sigma_0` and `X` must agree on n", call. = FALSE)
  }
  if (length(x0) != ncol(X)) {
    stop("`x0` must have one entry per column of `X`", call. = FALSE)
  }
  sinv <- solve(S)
  xsx <- t(X) %*% sinv %*% X
  if (is.null(beta)) beta <- as.numeric(solve(xsx, t(X) %*% sinv %*% nu))
  beta <- as.numeric(beta)
  resid <- nu - as.numeric(X %*% beta)
  nu0 <- as.numeric(x0 %*% beta + t(s0) %*% sinv %*% resid)
  c00 <- max(diag(S))
  m <- x0 - as.numeric(t(X) %*% sinv %*% s0)
  var0 <- max(as.numeric(c00 - t(s0) %*% sinv %*% s0 +
                           t(m) %*% solve(xsx, m)), 0)
  out <- .schab_predict_glm(nu0, var0, mu0, link_kind)
  out$beta <- beta
  out$link <- link_kind
  out$pseudo_scale_note <- paste(
    "kriging is done on the pseudo-data, where the model is linear; the GLM",
    "enters only on the return trip")
  out
}
