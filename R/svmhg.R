# SPDX-License-Identifier: AGPL-3.0-or-later

#' Linear SVM (primal hinge loss) -- R parity
#'
#' Native linear C-SVC via SMO. The primal weight vector is recovered in
#' closed form from the dual solution, w = sum_i alpha_i y_i x_i.
#'
#' @param x Numeric predictor matrix.
#' @param y Binary response.
#' @param C Soft-margin inverse regularisation.
#' @param seed RNG seed.
#' @return Named list: estimate, intercept, weights, train_accuracy, C,
#'   classes, n, method.
#' @importFrom stats predict
#' @examples
#' # See the package vignettes for usage examples:
#' #   vignette(package = "morie")
#' @export
morie_svm_hinge_primal <- function(x, y, C = 1.0, seed = 0L) {
  if (is.null(dim(x))) x <- matrix(x, ncol = 1)
  x <- as.matrix(x)
  y <- as.factor(y)
  classes <- levels(y)
  if (length(classes) != 2) stop("morie_svm_hinge_primal requires binary y")
  set.seed(seed)
  ypm <- ifelse(y == classes[2L], 1, -1)
  fit <- .svm_fit_binary(x, ypm, C, 0L, 1, 0, 3)   # linear kernel
  # For the linear kernel the primal weight vector is recoverable in closed
  # form, w = sum_i alpha_i y_i x_i (LIBSVM Eq. 3), and b = -rho.
  w <- if (fit$n_sv) as.numeric(crossprod(fit$coef, fit$SV)) else rep(0, ncol(x))
  b <- -fit$rho
  preds <- ifelse(as.numeric(x %*% w) + b > 0, classes[2L], classes[1L])
  acc <- mean(preds == as.character(y))
  list(
    estimate       = c(b, w),
    intercept      = b,
    weights        = w,
    train_accuracy = as.numeric(acc),
    C              = as.numeric(C),
    classes        = classes,
    n              = nrow(x),
    method         = "Linear SVM (primal hinge loss)"
  )
}
