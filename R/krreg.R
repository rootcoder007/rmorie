# SPDX-License-Identifier: AGPL-3.0-or-later
#' Kernel ridge regression in its dual form
#'
#' Written in the primal, a kernel fit needs the feature map; written in
#' the dual it needs only inner products, so the feature space can be
#' infinite-dimensional at no cost. The ridge penalty is not optional
#' here: the Gram matrix of a smooth kernel is numerically singular for
#' any sample worth fitting, so \code{lambda} is what makes the solve
#' exist at all rather than a tuning nicety.
#'
#' Formula: \code{alpha = (K + lambda I)^{-1} y} with
#' \code{K_ij = K_h(x_i - x_j)}; prediction
#' \code{m(x0) = sum_i alpha_i K_h(x0 - x_i)}.
#'
#' @param x,y Predictor and response, length n.
#' @param x_eval Evaluation points; defaults to \code{x}.
#' @param bandwidth Kernel bandwidth; Silverman's rule when NULL.
#' @param penalty Ridge penalty, strictly positive.
#' @param kernel One of \code{"gaussian"}, \code{"epanechnikov"},
#'   \code{"uniform"}.
#' @return List with \code{x_eval}, \code{y_hat}, \code{alpha},
#'   \code{bandwidth}, \code{penalty}, \code{n_obs}.
#' @references Saunders, C., Gammerman, A. and Vovk, V. (1998). Ridge
#'   regression learning algorithm in dual variables. Proceedings of the
#'   15th International Conference on Machine Learning, 515-521.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Krreg(V, V)
Krreg <- function(x, y, x_eval = NULL, bandwidth = NULL, penalty = 1,
                  kernel = "gaussian") {
  x <- as.numeric(x); y <- as.numeric(y); n <- length(x)
  if (length(y) != n) stop("x and y must have same length.")
  if (n < 3L) stop("Need at least 3 observations.")
  if (penalty <= 0) stop("penalty must be > 0.")
  kfn <- switch(kernel,
    gaussian = function(u) exp(-0.5 * u^2) / sqrt(2 * pi),
    epanechnikov = function(u) ifelse(abs(u) <= 1, 0.75 * (1 - u^2), 0),
    uniform = function(u) ifelse(abs(u) <= 1, 0.5, 0),
    stop("unknown kernel"))
  if (is.null(bandwidth)) {
    s <- .s03sd(x, 1L)
    qs <- .s03quantile7(sort(x), 0.75) - .s03quantile7(sort(x), 0.25)
    bandwidth <- 1.06 * min(s, qs / 1.34) * n^(-1 / 5)
  }
  bandwidth <- as.numeric(bandwidth)
  K <- kfn(outer(x, x, "-") / bandwidth)
  # a general solve, not Cholesky: the uniform and Epanechnikov
  # Gram matrices are not positive definite, and the Python arm
  # uses a general solve too.
  alpha <- as.numeric(solve(K + penalty * diag(n), y))
  if (is.null(x_eval)) x_eval <- x else x_eval <- as.numeric(x_eval)
  Ke <- kfn(outer(x_eval, x, "-") / bandwidth)
  .t1_result(x_eval = x_eval, y_hat = as.numeric(Ke %*% alpha),
             alpha = alpha, bandwidth = bandwidth,
             penalty = as.numeric(penalty), n_obs = n)
}
