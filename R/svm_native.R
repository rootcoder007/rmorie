# SPDX-License-Identifier: AGPL-3.0-or-later

# ---------------------------------------------------------------------
# Native SVM front-ends over the SMO solver in src/morie_svm.cpp.
#
# The solver implements LIBSVM's own formulation (Chang & Lin): the C-SVC
# dual Eq. (2), the eps-SVR dual Eq. (11), optimality Eq. (17)-(18) and
# working set selection WSS 1 from Fan, Chen & Lin (2005). Kernels are
# LIBSVM's four, with its parameter names:
#
#   linear      u'v
#   polynomial  (gamma u'v + coef0)^degree
#   RBF         exp(-gamma |u - v|^2)
#   sigmoid     tanh(gamma u'v + coef0)
#
# Multi-class classification uses one-against-one voting over all
# k(k-1)/2 binary machines, as LIBSVM does.
# ---------------------------------------------------------------------

# LIBSVM kernel codes.
.SVM_KERNELS <- c(linear = 0L, poly = 1L, polynomial = 1L,
                  rbf = 2L, radial = 2L, sigmoid = 3L)

#' Internal helper: resolve a kernel name to LIBSVM's integer code
#' @noRd
.svm_kernel_code <- function(kernel) {
  # `[[` on a named vector raises "subscript out of bounds" for an unknown
  # name rather than returning NULL, so match on names explicitly.
  nm <- tolower(as.character(kernel)[1L])
  if (!nm %in% names(.SVM_KERNELS)) {
    stop(sprintf("Unknown kernel '%s'; use linear, poly, rbf or sigmoid.",
                 kernel))
  }
  as.integer(.SVM_KERNELS[[nm]])
}

#' Internal helper: resolve the gamma argument
#'
#' `"scale"` is 1 / (p * var(x)) and `"auto"` is 1 / p, matching the
#' convention these functions have always used. LIBSVM's own default is
#' 1 / p, i.e. `"auto"`.
#' @noRd
.svm_gamma <- function(gamma, x) {
  p <- ncol(x)
  if (identical(gamma, "scale")) {
    v <- stats::var(as.numeric(x))
    if (!is.finite(v) || v <= 0) v <- 1
    return(1 / (p * v))
  }
  if (identical(gamma, "auto")) return(1 / p)
  as.numeric(gamma)
}

#' Internal helper: fit one binary C-SVC and keep only its support vectors
#' @noRd
.svm_fit_binary <- function(X, y_pm1, C, ktype, gamma, coef0, degree,
                            tol = 1e-3, max_iter = 1e6) {
  fit <- morie_svc_train_cpp(X, as.numeric(y_pm1), as.numeric(C),
                             ktype, gamma, coef0, degree,
                             tol, as.integer(max_iter))
  keep <- which(abs(fit$coef) > 1e-8)
  list(SV = X[keep, , drop = FALSE], coef = fit$coef[keep],
       rho = fit$rho, n_sv = length(keep), iterations = fit$iterations)
}

#' Internal helper: decision values for a fitted binary machine
#' @noRd
.svm_decide <- function(fit, Xnew, ktype, gamma, coef0, degree) {
  if (!fit$n_sv) return(rep(0, nrow(Xnew)))
  morie_svm_decision_cpp(fit$SV, fit$coef, fit$rho, Xnew,
                         ktype, gamma, coef0, degree)
}
