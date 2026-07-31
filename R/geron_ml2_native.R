# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Geron shelf, wave 4 continuation (geron_ml2_native.R).
#
# Ports morie.fn hm* modules that were not already covered by
# geron_ml_native.R. Helpers from geron_ml_native.R (.morie_gr_*) and
# alammar_llm_native.R (.morie_al_softmax_rows, .morie_al_lcg) are
# reused rather than duplicated; this file defines no helper that
# already exists there.
#
# Porting conventions (identical to geron_ml_native.R):
#   * Indices that Python returns 0-based (argmax, predicted class,
#     sampled row index, best epoch, matched pairs) stay 0-based here.
#     Any index used to SUBSET an R object is converted with +1 first.
#   * numpy's default std/var is the POPULATION form (ddof = 0);
#     .morie_gr_pvar / .morie_gr_psd are used at every such site.
#   * numpy reshape/ravel are row-major: flat streams become matrices
#     with byrow = TRUE, and matrices are flattened through t().
#   * %/% binds tighter than + - in R, so floor-division of a sum is
#     always parenthesised.
#   * LCG-seeded modules draw the stream draw-for-draw in the same
#     order as Python so the weights agree bit for bit.
#
# Name mapping where several Python modules share one R function is
# documented on the function that serves them.

# ------------------------------------------------------------ activations

#' ELU activation (Geron Ch 11, morie.fn hmelu)
#'
#' ELU(z) = z for z >= 0, alpha * (exp(z) - 1) otherwise. The
#' derivative below zero is a + alpha, which is why alpha = 1 is the
#' only C1 member of the family.
#'
#' @param z Numeric vector of pre-activations.
#' @param alpha Positive saturation scale.
#' @return List with `a`, `activation`, `derivative`, `saturation`,
#'   `alpha`, `is_c1`, `estimate`, `n`.
#' @export
morie_geron_elu <- function(z, alpha = 1.0) {
  z <- as.numeric(z)
  .morie_gr_need(length(z) > 0L, "geron_elu: z is empty")
  .morie_gr_need(all(is.finite(z)), "geron_elu: z contains non-finite values")
  a_scale <- as.numeric(alpha)
  .morie_gr_need(is.finite(a_scale) && a_scale > 0,
                 "geron_elu: alpha must be positive and finite")
  pos <- z >= 0
  a <- ifelse(pos, z, a_scale * expm1(pmin(z, 0)))
  d <- ifelse(pos, 1.0, a + a_scale)
  list(a = a, activation = a, derivative = d, saturation = -a_scale,
       alpha = a_scale, is_c1 = a_scale == 1.0, estimate = mean(a),
       n = length(z),
       method = "ELU(z) = z if z >= 0 else alpha*(exp(z) - 1)")
}

#' SELU activation (Geron Ch 11, morie.fn hmselu)
#'
#' Scaled ELU with the Klambauer self-normalising constants. Note the
#' branch is z <= 0 here (not z < 0), matching the Python module.
#'
#' @param z Numeric vector.
#' @param lam,alpha Positive scale constants.
#' @return List with `a`, `grad`, `mean`, `var` (population), `lam`,
#'   `alpha`, `estimate`, `n`.
#' @export
morie_geron_selu <- function(z, lam = 1.0507009873554804934193349852946,
                             alpha = 1.6732632423543772848170429916717) {
  x <- as.numeric(z)
  .morie_gr_need(length(x) > 0L, "geron_selu: z is empty")
  .morie_gr_need(all(is.finite(x)), "geron_selu: z contains non-finite values")
  lm <- as.numeric(lam); al <- as.numeric(alpha)
  .morie_gr_need(is.finite(lm) && lm > 0 && is.finite(al) && al > 0,
                 "geron_selu: lam and alpha must be positive and finite")
  neg <- x <= 0
  a <- ifelse(neg, al * expm1(pmin(x, 0)), x) * lm
  grad <- ifelse(neg, lm * al * exp(pmin(x, 0)), lm)
  list(a = a, grad = grad, mean = mean(a), var = .morie_gr_pvar(a),
       lam = lm, alpha = al, estimate = mean(a), n = length(x),
       method = "Scaled ELU with the Klambauer self-normalising constants")
}

#' GELU activation (Geron Ch 11, morie.fn hmgelu)
#'
#' Exact form z * Phi(z) with Phi the standard normal CDF, plus the
#' tanh approximation. GELU is non-monotone: the derivative
#' Phi(z) + z phi(z) is negative near z = -1, which the parity test
#' checks by finite differences.
#'
#' @param z Numeric vector.
#' @param approximate Use the tanh form.
#' @return List with `a`, `activation`, `exact`, `approx`,
#'   `derivative`, `max_abs_gap`, `estimate`, `n`.
#' @export
morie_geron_gelu <- function(z, approximate = FALSE) {
  z <- as.numeric(z)
  .morie_gr_need(length(z) > 0L, "geron_gelu: z is empty")
  .morie_gr_need(all(is.finite(z)), "geron_gelu: z contains non-finite values")
  cdf <- pnorm(z)
  exact <- z * cdf
  approx <- 0.5 * z * (1 + tanh(sqrt(2 / pi) * (z + 0.044715 * z^3)))
  deriv <- cdf + z * dnorm(z)
  a <- if (isTRUE(approximate)) approx else exact
  list(a = a, activation = a, exact = exact, approx = approx,
       derivative = deriv, max_abs_gap = max(abs(exact - approx)),
       estimate = mean(a), n = length(z),
       method = paste0("GELU(z) = z * Phi(z)",
                       if (isTRUE(approximate)) " (tanh approximation)" else ""))
}

#' Heaviside step activation (Geron Ch 10, morie.fn hmhev)
#'
#' 1 above zero, 0 below, `at_zero` exactly at zero. The derivative is
#' zero almost everywhere, so the result carries that warning.
#'
#' @param z Numeric vector.
#' @param at_zero Value returned at z == 0.
#' @return List with `activation`, `derivative`, `n_active`,
#'   `warnings`, `estimate`, `n`.
#' @export
morie_geron_heaviside <- function(z, at_zero = 1.0) {
  a <- as.numeric(z)
  .morie_gr_need(length(a) > 0L, "geron_heaviside: z is empty")
  .morie_gr_need(all(is.finite(a)), "geron_heaviside: z contains non-finite values")
  tie <- as.numeric(at_zero)
  .morie_gr_need(is.finite(tie), "geron_heaviside: at_zero must be finite")
  out <- ifelse(a > 0, 1.0, ifelse(a < 0, 0.0, tie))
  list(activation = out, derivative = rep(0, length(out)),
       n_active = sum(out > 0),
       warnings = "The derivative is 0 almost everywhere, so this activation cannot be trained by backpropagation.",
       estimate = mean(out), n = length(a),
       method = "Heaviside step activation")
}

#' Leaky ReLU (Geron Ch 11, morie.fn hmlrel)
#'
#' max(alpha z, z) with alpha in \[0, 1). alpha = 0 is a plain ReLU and
#' is flagged, because that is the dying-unit case.
#'
#' @param z Numeric vector.
#' @param alpha Leak slope in \[0, 1).
#' @return List with `activation`, `derivative`, `n_leaky`, `alpha`,
#'   `warnings`, `estimate`, `n`.
#' @export
morie_geron_leaky_relu <- function(z, alpha = 0.01) {
  a <- as.numeric(z)
  .morie_gr_need(length(a) > 0L, "geron_leaky_relu: z is empty")
  .morie_gr_need(all(is.finite(a)), "geron_leaky_relu: z contains non-finite values")
  slope <- as.numeric(alpha)
  .morie_gr_need(is.finite(slope) && slope >= 0 && slope < 1,
                 "geron_leaky_relu: alpha must lie in [0, 1)")
  out <- ifelse(a >= 0, a, slope * a)
  deriv <- ifelse(a >= 0, 1.0, slope)
  list(activation = out, derivative = deriv, n_leaky = sum(a < 0),
       alpha = slope,
       warnings = if (slope == 0)
         "alpha = 0 is a plain ReLU: units with negative pre-activation get zero gradient and can die."
         else character(0),
       estimate = mean(out), n = length(a),
       method = "Leaky ReLU activation")
}

#' Logistic sigmoid (Geron Ch 4, morie.fn hmsigm)
#'
#' Overflow-safe two-branch evaluation, with the elementwise
#' derivative a (1 - a). The slope peaks at 0.25.
#'
#' @param t Numeric vector.
#' @return List with `a`, `sigma`, `grad`, `estimate`, `n`.
#' @export
morie_geron_sigmoid <- function(t) {
  z <- as.numeric(t)
  .morie_gr_need(length(z) > 0L, "geron_sigmoid: t is empty")
  .morie_gr_need(all(is.finite(z)), "geron_sigmoid: t contains non-finite values")
  a <- numeric(length(z))
  pos <- z >= 0
  a[pos] <- 1 / (1 + exp(-z[pos]))
  ez <- exp(z[!pos])
  a[!pos] <- ez / (1 + ez)
  list(a = a, sigma = a, grad = a * (1 - a), estimate = mean(a),
       n = length(z),
       method = "Logistic sigmoid, overflow-safe branch, with elementwise derivative")
}

#' Hyperbolic tangent activation (Geron Ch 10, morie.fn hmtanh)
#'
#' @param z Numeric vector.
#' @return List with `a`, `grad`, `estimate`, `n`.
#' @export
morie_geron_tanh <- function(z) {
  x <- as.numeric(z)
  .morie_gr_need(length(x) > 0L, "geron_tanh: z is empty")
  .morie_gr_need(all(is.finite(x)), "geron_tanh: z contains non-finite values")
  a <- tanh(x)
  list(a = a, grad = 1 - a * a, estimate = mean(a), n = length(x),
       method = "Hyperbolic tangent activation with elementwise derivative 1 - tanh^2")
}

#' Swish / SiLU activation (Geron Ch 11, morie.fn hmswi)
#'
#' z * sigmoid(beta z), with the sigmoid delegated to
#' `morie_geron_sigmoid` exactly as the Python module delegates to
#' hmsigm.
#'
#' @param z Numeric vector.
#' @param beta Finite gate scale.
#' @return List with `a`, `grad`, `gate`, `beta`, `estimate`, `n`.
#' @export
morie_geron_swish <- function(z, beta = 1.0) {
  x <- as.numeric(z)
  .morie_gr_need(length(x) > 0L, "geron_swish: z is empty")
  .morie_gr_need(all(is.finite(x)), "geron_swish: z contains non-finite values")
  b <- as.numeric(beta)
  .morie_gr_need(is.finite(b), "geron_swish: beta must be finite")
  s <- morie_geron_sigmoid(b * x)$a
  a <- x * s
  list(a = a, grad = s + b * x * s * (1 - s), gate = s, beta = b,
       estimate = mean(a), n = length(x),
       method = "Swish z*sigmoid(beta z) with sigmoid delegated to hmsigm")
}

#' Threshold logic unit (Geron Ch 10, morie.fn hmtlu)
#'
#' step(w^T x + b) with step(0) = 1. A vector `x` is one row.
#'
#' @param x Vector or (n, d) matrix.
#' @param w Weights.
#' @param b Bias.
#' @return List with `y` (0/1), `z`, `w`, `b`, `estimate`, `n`.
#' @export
morie_geron_tlu <- function(x, w, b = 0.0) {
  X <- if (is.matrix(x)) x else matrix(as.numeric(x), nrow = 1)
  storage.mode(X) <- "double"
  .morie_gr_need(length(X) > 0L, "geron_tlu: x must be a non-empty vector or (n, d) batch")
  wv <- as.numeric(w)
  .morie_gr_need(length(wv) == ncol(X),
                 "geron_tlu: x features and w weights disagree")
  bias <- as.numeric(b)
  .morie_gr_need(all(is.finite(X)) && all(is.finite(wv)) && is.finite(bias),
                 "geron_tlu: x, w and b must all be finite")
  z <- as.numeric(X %*% wv) + bias
  y <- as.integer(z >= 0)
  list(y = y, z = z, w = wv, b = bias, estimate = mean(y), n = nrow(X),
       method = "Threshold logic unit with Heaviside step (step(0) = 1)")
}

#' Softmax (Geron Ch 4, morie.fn hmsftm)
#'
#' Max-shift stabilised softmax. `axis` follows numpy: -1 (default) or
#' 1 is row-wise for a matrix, 0 is column-wise. For a vector input the
#' full Jacobian diag(p) - p p^T is returned as well.
#'
#' @param scores Numeric vector or matrix.
#' @param axis -1, 0 or 1.
#' @return List with `p`, `probabilities`, `argmax` (0-based),
#'   `jacobian` (vector input only), `estimate`, `n`.
#' @export
morie_geron_softmax_function <- function(scores, axis = -1) {
  s <- scores
  vecin <- !is.matrix(s)
  S <- if (vecin) matrix(as.numeric(s), nrow = 1) else as.matrix(s)
  storage.mode(S) <- "double"
  .morie_gr_need(length(S) > 0L, "geron_softmax_function: scores is empty")
  .morie_gr_need(all(is.finite(S)), "geron_softmax_function: scores contains non-finite values")
  ax <- as.integer(axis)
  if (vecin) {
    .morie_gr_need(ax %in% c(-1L, 0L), "geron_softmax_function: axis out of range")
    .morie_gr_need(ncol(S) >= 2L, "geron_softmax_function: softmax needs at least 2 classes")
    p <- as.numeric(.morie_al_softmax_rows(S))
    jac <- diag(p) - outer(p, p)
    return(list(p = p, probabilities = p, argmax = which.max(p) - 1L,
                jacobian = jac, estimate = max(p), n = length(p),
                method = "Softmax with max-shift stabilisation (exact, not an approximation)"))
  }
  .morie_gr_need(ax %in% c(-1L, 0L, 1L), "geron_softmax_function: axis out of range")
  rowwise <- ax != 0L
  k <- if (rowwise) ncol(S) else nrow(S)
  .morie_gr_need(k >= 2L, "geron_softmax_function: softmax needs at least 2 classes")
  P <- if (rowwise) .morie_al_softmax_rows(S) else .morie_gr_softmax_cols(S)
  am <- if (rowwise) apply(P, 1, which.max) - 1L else apply(P, 2, which.max) - 1L
  list(p = P, probabilities = P, argmax = am, jacobian = NULL,
       estimate = max(P), n = k,
       method = "Softmax with max-shift stabilisation (exact, not an approximation)")
}

#' Linear class scores for softmax regression (Geron Ch 4, morie.fn hmsfts)
#'
#' X theta normalised row by row through `morie_geron_softmax_function`,
#' the same delegation the Python module makes to hmsftm.
#'
#' @param X (m, d) design matrix.
#' @param theta (d, K) coefficients.
#' @return List with `scores`, `p`, `probabilities`, `predicted`
#'   (0-based), `estimate`, `n`.
#' @export
morie_geron_softmax_score <- function(X, theta) {
  Xm <- if (is.matrix(X)) X else matrix(as.numeric(X), nrow = 1)
  storage.mode(Xm) <- "double"
  .morie_gr_need(length(Xm) > 0L, "geron_softmax_score: X must be a non-empty 2-D design matrix")
  th <- if (is.matrix(theta)) theta else matrix(as.numeric(theta), ncol = 1)
  storage.mode(th) <- "double"
  .morie_gr_need(nrow(th) == ncol(Xm),
                 "geron_softmax_score: theta must be (n_features, n_classes)")
  .morie_gr_need(ncol(th) >= 2L, "geron_softmax_score: softmax regression needs >= 2 classes")
  .morie_gr_need(all(is.finite(Xm)) && all(is.finite(th)),
                 "geron_softmax_score: X and theta must be finite")
  scores <- Xm %*% th
  p <- .morie_al_softmax_rows(scores)
  pred <- apply(scores, 1, which.max) - 1L
  list(scores = scores, p = p, probabilities = p, predicted = pred,
       estimate = mean(apply(p, 1, max)), n = nrow(Xm),
       method = "Linear class scores X @ theta, normalised via hmsftm")
}

# ------------------------------------------------------------- impurities

#' Gini impurity (Geron Ch 6, morie.fn hmgini)
#'
#' G = 1 - sum_k p_k^2, with the K-class ceiling 1 - 1/K reported so a
#' value can be read against its own maximum.
#'
#' @param y Vector of labels.
#' @return List with `gini`, `proportions`, `classes`, `counts`,
#'   `n_classes`, `max_possible`, `estimate`, `n`.
#' @export
morie_geron_gini_impurity <- function(y) {
  y <- as.vector(y)
  .morie_gr_need(length(y) > 0L,
                 "geron_gini_impurity: y is empty; impurity is undefined for an empty node")
  tb <- table(y)
  classes <- sort(unique(y))
  counts <- as.integer(tb[as.character(classes)])
  p <- counts / length(y)
  K <- length(classes)
  gini <- 1 - sum(p * p)
  list(gini = gini, proportions = p, classes = classes, counts = counts,
       n_classes = K, max_possible = 1 - 1 / K, estimate = gini,
       n = length(y), method = "Gini impurity G = 1 - sum_k p_k^2")
}

#' Shannon entropy impurity (Geron Ch 6, morie.fn hment)
#'
#' H = -sum_k p_k log2 p_k, in bits, with the nats conversion and the
#' log2(K) ceiling alongside.
#'
#' @param y Vector of labels.
#' @return List with `entropy`, `entropy_nats`, `proportions`,
#'   `classes`, `counts`, `n_classes`, `max_possible`, `estimate`, `n`.
#' @export
morie_geron_entropy_impurity <- function(y) {
  y <- as.vector(y)
  .morie_gr_need(length(y) > 0L,
                 "geron_entropy_impurity: y is empty; entropy is undefined for an empty node")
  classes <- sort(unique(y))
  counts <- as.integer(table(y)[as.character(classes)])
  p <- counts / length(y)
  K <- length(classes)
  h <- -sum(p * log2(p))
  if (h == 0) h <- 0
  list(entropy = h, entropy_nats = h * log(2), proportions = p,
       classes = classes, counts = counts, n_classes = K,
       max_possible = log2(K), estimate = h, n = length(y),
       method = "Shannon entropy H = -sum_k p_k log2 p_k")
}

# ---------------------------------------------------------------- metrics

#' Root mean squared error (Geron Ch 2, morie.fn hmrms)
#'
#' @param y_true,y_pred Numeric vectors of equal length.
#' @return List with `rmse`, `mse`, `mae`, `residuals`, `estimate`, `n`.
#' @export
morie_geron_rmse <- function(y_true, y_pred) {
  a <- as.numeric(y_true); b <- as.numeric(y_pred)
  .morie_gr_need(length(a) > 0L, "geron_rmse: y_true is empty")
  .morie_gr_need(length(a) == length(b), "geron_rmse: lengths disagree")
  .morie_gr_need(all(is.finite(a)) && all(is.finite(b)),
                 "geron_rmse: inputs contain non-finite values")
  resid <- b - a
  mse <- mean(resid^2)
  list(rmse = sqrt(mse), mse = mse, mae = mean(abs(resid)),
       residuals = resid, estimate = sqrt(mse), n = length(a),
       method = "Root mean squared error (l2 norm of residuals / sqrt(m))")
}

#' Mean absolute error (Geron Ch 2, morie.fn hmmae)
#'
#' Reported next to RMSE: the ratio is how much a handful of large
#' residuals is driving the squared-error metric.
#'
#' @param y_true,y_pred Numeric vectors of equal length.
#' @return List with `mae`, `rmse`, `max_error`,
#'   `median_absolute_error`, `ratio`, `residuals`, `estimate`, `n`.
#' @export
morie_geron_mae <- function(y_true, y_pred) {
  yt <- as.numeric(y_true); yp <- as.numeric(y_pred)
  .morie_gr_need(length(yt) > 0L, "geron_mae: y_true is empty")
  .morie_gr_need(length(yt) == length(yp), "geron_mae: lengths disagree")
  .morie_gr_need(all(is.finite(yt)) && all(is.finite(yp)),
                 "geron_mae: y_true and y_pred must be finite")
  resid <- abs(yp - yt)
  mae <- mean(resid)
  rmse <- sqrt(mean((yp - yt)^2))
  list(mae = mae, rmse = rmse, max_error = max(resid),
       median_absolute_error = median(resid),
       ratio = if (mae > 0) rmse / mae else 1.0,
       residuals = resid, estimate = mae, n = length(yt),
       method = "Mean absolute error (l1 norm of the residuals / m)")
}

# ------------------------------------------------------- linear regression

#' Linear-regression MSE cost and gradient (Geron Ch 4, morie.fn hmmsec)
#'
#' If `theta` is one longer than the columns of `X`, a leading column
#' of ones is prepended, matching the Python implicit-bias rule.
#'
#' @param X Design matrix.
#' @param y Targets.
#' @param theta Coefficients.
#' @return List with `cost`, `mse`, `gradient`, `grad_norm`,
#'   `residuals`, `predictions`, `estimate`, `n`.
#' @export
morie_geron_linreg_mse_cost <- function(X, y, theta) {
  A <- if (is.matrix(X)) X else matrix(as.numeric(X), ncol = 1)
  storage.mode(A) <- "double"
  yv <- as.numeric(y); th <- as.numeric(theta)
  m <- nrow(A)
  .morie_gr_need(m > 0L, "geron_linreg_mse_cost: X has no rows")
  .morie_gr_need(length(yv) == m, "geron_linreg_mse_cost: X rows and y length disagree")
  if (length(th) == ncol(A) + 1L) {
    A <- cbind(1, A)
  } else {
    .morie_gr_need(length(th) == ncol(A),
                   "geron_linreg_mse_cost: theta must match the columns of X, or be one longer")
  }
  .morie_gr_need(all(is.finite(A)) && all(is.finite(yv)) && all(is.finite(th)),
                 "geron_linreg_mse_cost: inputs contain non-finite values")
  pred <- as.numeric(A %*% th)
  resid <- pred - yv
  cost <- mean(resid^2)
  grad <- (2 / m) * as.numeric(t(A) %*% resid)
  list(cost = cost, mse = cost, gradient = grad,
       grad_norm = sqrt(sum(grad^2)), residuals = resid,
       predictions = pred, estimate = cost, n = m,
       method = "MSE cost and analytic gradient for linear regression")
}

#' OLS by the normal equation (Geron Ch 4, morie.fn hmneq)
#'
#' Solves (X^T X) theta = X^T y as a linear system, never forming the
#' inverse, and refuses a rank-deficient Gram matrix rather than
#' returning an arbitrary member of the solution set.
#'
#' @param X Design matrix.
#' @param y Targets.
#' @param fit_intercept Prepend a column of ones.
#' @return List with `theta`, `residuals`, `rss`, `cond`, `estimate`, `n`.
#' @export
morie_geron_normal_equation <- function(X, y, fit_intercept = FALSE) {
  A <- if (is.matrix(X)) X else matrix(as.numeric(X), ncol = 1)
  storage.mode(A) <- "double"
  yv <- as.numeric(y)
  .morie_gr_need(nrow(A) > 0L, "geron_normal_equation: X has no rows")
  .morie_gr_need(length(yv) == nrow(A), "geron_normal_equation: X rows and y length disagree")
  .morie_gr_need(all(is.finite(A)) && all(is.finite(yv)),
                 "geron_normal_equation: inputs contain non-finite values")
  if (isTRUE(fit_intercept)) A <- cbind(1, A)
  .morie_gr_need(nrow(A) >= ncol(A),
                 "geron_normal_equation: X^T X is singular by shape")
  G <- t(A) %*% A
  sv <- svd(G)$d
  rank <- sum(sv > max(dim(G)) * .Machine$double.eps * max(sv))
  .morie_gr_need(rank >= ncol(A),
                 "geron_normal_equation: the columns are collinear and theta is not identified")
  theta <- as.numeric(solve(G, t(A) %*% yv))
  resid <- as.numeric(A %*% theta) - yv
  list(theta = theta, residuals = resid, rss = sum(resid * resid),
       cond = max(sv) / min(sv), estimate = theta, n = nrow(A),
       method = "Normal equation solved as a linear system (no explicit inverse)")
}

#' Ridge cost (Geron Ch 4, morie.fn hmridg)
#'
#' MSE + (alpha/2) ||theta||^2 with the intercept coefficient left
#' unpenalised. `intercept_index` is 0-based, as in Python; pass NULL
#' to penalise every coefficient.
#'
#' @param X,y,theta Design, targets, coefficients.
#' @param alpha Non-negative penalty.
#' @param intercept_index 0-based index left unpenalised, or NULL.
#' @return List with `cost`, `mse`, `penalty`, `gradient`, `alpha`,
#'   `estimate`, `n`.
#' @export
morie_geron_ridge_cost <- function(X, y, theta, alpha, intercept_index = 0) {
  A <- if (is.matrix(X)) X else matrix(as.numeric(X), ncol = 1)
  storage.mode(A) <- "double"
  yv <- as.numeric(y); th <- as.numeric(theta)
  m <- nrow(A)
  .morie_gr_need(m > 0L, "geron_ridge_cost: X has no rows")
  .morie_gr_need(length(yv) == m, "geron_ridge_cost: X rows and y length disagree")
  .morie_gr_need(length(th) == ncol(A), "geron_ridge_cost: theta and columns disagree")
  a <- as.numeric(alpha)
  .morie_gr_need(is.finite(a) && a >= 0,
                 "geron_ridge_cost: alpha must be finite and non-negative")
  mask <- rep(1, length(th))
  if (!is.null(intercept_index)) {
    k <- as.integer(intercept_index)
    .morie_gr_need(k >= 0L && k < length(th),
                   "geron_ridge_cost: intercept_index is outside theta")
    mask[k + 1L] <- 0
  }
  resid <- as.numeric(A %*% th) - yv
  mse <- mean(resid^2)
  penalty <- 0.5 * a * sum(mask * th^2)
  grad <- (2 / m) * as.numeric(t(A) %*% resid) + a * mask * th
  list(cost = mse + penalty, mse = mse, penalty = penalty,
       gradient = grad, alpha = a, estimate = mse + penalty, n = m,
       method = "Ridge cost MSE + (alpha/2)||theta||^2 with an unpenalised bias")
}

#' Ridge by the augmented normal equation (Geron Ch 4, morie.fn hmridn)
#'
#' (X^T X + alpha A) theta = X^T y with A the identity minus the
#' intercept slot. Effective df = tr(X (X^T X + alpha A)^-1 X^T) makes
#' the shrinkage visible.
#'
#' @param X,y Design and targets.
#' @param alpha Non-negative penalty.
#' @param intercept_index 0-based unpenalised index, or NULL.
#' @return List with `theta`, `residuals`, `rss`, `effective_df`,
#'   `alpha`, `estimate`, `n`.
#' @export
morie_geron_ridge_normal <- function(X, y, alpha, intercept_index = 0) {
  A <- if (is.matrix(X)) X else matrix(as.numeric(X), ncol = 1)
  storage.mode(A) <- "double"
  yv <- as.numeric(y)
  .morie_gr_need(nrow(A) > 0L, "geron_ridge_normal: X has no rows")
  .morie_gr_need(length(yv) == nrow(A), "geron_ridge_normal: X rows and y length disagree")
  a <- as.numeric(alpha)
  .morie_gr_need(is.finite(a) && a >= 0,
                 "geron_ridge_normal: alpha must be finite and non-negative")
  p <- ncol(A)
  d <- rep(1, p)
  if (!is.null(intercept_index)) {
    k <- as.integer(intercept_index)
    .morie_gr_need(k >= 0L && k < p, "geron_ridge_normal: intercept_index is outside the columns")
    d[k + 1L] <- 0
  }
  G <- t(A) %*% A + a * diag(d, nrow = p)
  sv <- svd(G)$d
  .morie_gr_need(sum(sv > max(dim(G)) * .Machine$double.eps * max(sv)) >= p,
                 "geron_ridge_normal: X^T X + alpha A is singular; raise alpha or drop a duplicated column")
  theta <- as.numeric(solve(G, t(A) %*% yv))
  resid <- as.numeric(A %*% theta) - yv
  edf <- sum(diag(A %*% solve(G, t(A))))
  list(theta = theta, residuals = resid, rss = sum(resid * resid),
       effective_df = edf, alpha = a, estimate = theta, n = nrow(A),
       method = "Ridge closed form (X^T X + alpha A) theta = X^T y")
}

#' L1 (Lasso) penalty, subgradient and prox (Geron Ch 4, morie.fn hml1r)
#'
#' `skip_bias` zeroes the mask at the FIRST element of the row-major
#' flattening, which for a matrix `theta` is entry \[1, 1\].
#'
#' @param theta Coefficients.
#' @param alpha Non-negative penalty.
#' @param skip_bias Leave the first coefficient unpenalised.
#' @return List with `penalty`, `gradient`, `prox`, `n_zero`,
#'   `estimate`, `n`.
#' @export
morie_geron_l1_regularization <- function(theta, alpha, skip_bias = FALSE) {
  t <- as.numeric(theta)
  .morie_gr_need(length(t) > 0L, "geron_l1_regularization: theta is empty")
  .morie_gr_need(all(is.finite(t)), "geron_l1_regularization: theta contains non-finite values")
  a <- as.numeric(alpha)
  .morie_gr_need(is.finite(a) && a >= 0,
                 "geron_l1_regularization: alpha must be finite and non-negative")
  mask <- rep(1, length(t))
  if (isTRUE(skip_bias)) {
    .morie_gr_need(length(t) >= 2L,
                   "geron_l1_regularization: skip_bias needs at least one non-bias parameter")
    mask[1L] <- 0
  }
  penalty <- a * sum(abs(t) * mask)
  grad <- a * sign(t) * mask
  prox <- ifelse(mask > 0, sign(t) * pmax(abs(t) - a, 0), t)
  list(penalty = penalty, gradient = grad, prox = prox,
       n_zero = sum(prox == 0), estimate = penalty, n = length(t),
       method = "L1 (Lasso) regularization penalty")
}

#' Max-norm projection (Geron Ch 11, morie.fn hmmnr)
#'
#' A projection, not a penalty: each weight vector keeps its direction
#' and only its length is capped at `r`. `axis` follows numpy on a
#' matrix: 0 = down columns, 1 (or -1) = across rows; NULL treats the
#' whole array as one vector.
#'
#' @param w Vector or matrix.
#' @param r Positive radius.
#' @param axis NULL, 0, 1 or -1.
#' @return List with `w`, `norm_before`, `norm_after`, `clipped`,
#'   `n_clipped`, `r`, `estimate`, `n`.
#' @export
morie_geron_max_norm <- function(w, r, axis = NULL) {
  W <- w
  .morie_gr_need(length(W) > 0L, "geron_max_norm: w is empty")
  .morie_gr_need(all(is.finite(W)), "geron_max_norm: w contains non-finite values")
  radius <- as.numeric(r)
  .morie_gr_need(is.finite(radius) && radius > 0,
                 "geron_max_norm: r must be a positive finite radius")
  if (is.null(axis)) {
    nb <- sqrt(sum(as.numeric(W)^2))
    if (nb > radius) {
      out <- W * (radius / nb); clipped <- TRUE; nc <- 1L
    } else {
      out <- W; clipped <- FALSE; nc <- 0L
    }
    na <- sqrt(sum(as.numeric(out)^2))
  } else {
    M <- as.matrix(W); storage.mode(M) <- "double"
    ax <- as.integer(axis)
    .morie_gr_need(ax %in% c(-1L, 0L, 1L), "geron_max_norm: axis is out of range")
    # numpy axis 0 reduces DOWN columns, axis 1 (or -1) ACROSS rows.
    norms <- if (ax == 0L) sqrt(colSums(M^2)) else sqrt(rowSums(M^2))
    factor <- ifelse(norms > radius, radius / ifelse(norms == 0, 1, norms), 1)
    out <- if (ax == 0L) M * rep(factor, each = nrow(M)) else M * factor
    nb <- norms
    na <- if (ax == 0L) sqrt(colSums(out^2)) else sqrt(rowSums(out^2))
    nc <- sum(norms > radius)
    clipped <- nc > 0L
  }
  list(w = out, norm_before = nb, norm_after = na, clipped = clipped,
       n_clipped = nc, r = radius, estimate = max(na), n = length(W),
       method = "Max-norm regularization (projection onto the l2 ball)")
}

#' Gradient clipping by global norm (Geron Ch 11, morie.fn hmgcl)
#'
#' The rescaling is uniform, so the update direction is unchanged.
#' Pass a numeric vector/matrix for the single-tensor form, or a list
#' of them for the multi-tensor form (which returns a list).
#'
#' @param grads Numeric vector/matrix, or a list of them.
#' @param max_norm Positive threshold.
#' @param norm_type p, or Inf.
#' @return List with `clipped`, `total_norm`, `scale`, `was_clipped`,
#'   `new_norm`, `max_norm`, `norm_type`, `estimate`, `n`.
#' @export
morie_geron_gradient_clipping <- function(grads, max_norm, norm_type = 2.0) {
  c_ <- as.numeric(max_norm)
  .morie_gr_need(is.finite(c_) && c_ > 0,
                 "geron_gradient_clipping: max_norm must be positive and finite")
  p <- as.numeric(norm_type)
  .morie_gr_need(p > 0, "geron_gradient_clipping: norm_type must be positive")
  single <- !is.list(grads)
  arrays <- if (single) list(grads) else grads
  flat <- unlist(lapply(arrays, as.numeric), use.names = FALSE)
  .morie_gr_need(length(flat) > 0L, "geron_gradient_clipping: grads is empty")
  .morie_gr_need(all(is.finite(flat)),
                 "geron_gradient_clipping: grads contains non-finite values")
  total <- if (is.infinite(p)) max(abs(flat)) else sum(abs(flat)^p)^(1 / p)
  scale <- if (total <= c_) 1 else c_ / total
  clipped <- lapply(arrays, function(a) a * scale)
  out <- if (single) clipped[[1L]] else clipped
  list(clipped = out, total_norm = total, scale = scale,
       was_clipped = scale < 1, new_norm = total * scale,
       max_norm = c_, norm_type = p, estimate = total * scale,
       n = length(flat), method = "global-norm gradient clipping")
}

# -------------------------------------------------- learning-rate schedules

#' Exponential learning-rate decay (Geron Ch 11, morie.fn hmlrex)
#'
#' eta_t = eta0 * decay^t, plus the number of steps per tenfold drop
#' (Inf when decay = 1).
#'
#' @param eta0 Positive initial rate.
#' @param decay In (0, 1\].
#' @param t Step or vector of steps.
#' @return List with `eta`, `steps_per_decade`, `eta0`, `decay`,
#'   `estimate`, `n`.
#' @export
morie_geron_lr_exponential <- function(eta0, decay, t) {
  e0 <- as.numeric(eta0); d <- as.numeric(decay)
  .morie_gr_need(is.finite(e0) && e0 > 0,
                 "geron_lr_exponential: eta0 must be a positive finite learning rate")
  .morie_gr_need(is.finite(d) && d > 0 && d <= 1,
                 "geron_lr_exponential: decay must lie in (0, 1]")
  tt <- as.numeric(t)
  scalar <- length(tt) == 1L
  .morie_gr_need(length(tt) > 0L, "geron_lr_exponential: t is empty")
  .morie_gr_need(all(is.finite(tt)) && all(tt >= 0),
                 "geron_lr_exponential: t must be finite and non-negative")
  eta <- e0 * d^tt
  spd <- if (d == 1) Inf else log(10) / -log(d)
  list(eta = if (scalar) eta[1L] else eta, steps_per_decade = spd,
       eta0 = e0, decay = d, estimate = eta[1L], n = length(tt),
       method = "Exponential learning-rate decay")
}

#' Power (1/t) learning-rate schedule (Geron Ch 4, morie.fn hmlrs)
#'
#' eta_t = eta0 / (t + t0). The Robbins-Monro partial sums over
#' 0..max(t) are returned so the divergence of sum eta and the
#' convergence of sum eta^2 can be checked directly.
#'
#' @param t Step or vector of steps.
#' @param eta0 Positive rate.
#' @param t0 Positive offset.
#' @return List with `eta`, `schedule`, `sum_eta`, `sum_eta_squared`,
#'   `eta0`, `t0`, `estimate`, `n`.
#' @export
morie_geron_learning_rate_schedule <- function(t, eta0, t0) {
  e0 <- as.numeric(eta0); off <- as.numeric(t0)
  .morie_gr_need(is.finite(e0) && e0 > 0,
                 "geron_learning_rate_schedule: eta0 must be positive and finite")
  .morie_gr_need(is.finite(off) && off > 0,
                 "geron_learning_rate_schedule: t0 must be positive")
  tt <- as.numeric(t)
  scalar <- length(tt) == 1L
  .morie_gr_need(length(tt) > 0L, "geron_learning_rate_schedule: t is empty")
  .morie_gr_need(all(is.finite(tt)) && all(tt >= 0),
                 "geron_learning_rate_schedule: t must be finite and non-negative")
  eta <- e0 / (tt + off)
  upto <- seq.int(0, as.integer(max(tt)))
  schedule <- e0 / (upto + off)
  list(eta = if (scalar) eta[1L] else eta, schedule = schedule,
       sum_eta = sum(schedule), sum_eta_squared = sum(schedule^2),
       eta0 = e0, t0 = off, estimate = eta[1L], n = length(tt),
       method = "Power (1/t) learning-rate schedule")
}

#' Cosine annealing schedule (Geron Ch 11, morie.fn hmlcos)
#'
#' eta_t = eta_min + 0.5 (eta_max - eta_min) (1 + cos(pi t / T)):
#' smooth at both ends, leaving eta_max and reaching eta_min with zero
#' slope.
#'
#' @param t Step(s) in 0..T.
#' @param T Cycle length.
#' @param eta_max,eta_min Rate bounds.
#' @return List with `eta`, `schedule`, `T`, `eta_max`, `eta_min`,
#'   `estimate`, `n`.
#' @export
morie_geron_cosine_annealing <- function(t, T, eta_max, eta_min = 0.0) {
  T_int <- as.integer(T)
  .morie_gr_need(T_int >= 1L, "geron_cosine_annealing: T must be a positive number of steps")
  hi <- as.numeric(eta_max); lo <- as.numeric(eta_min)
  .morie_gr_need(is.finite(hi) && is.finite(lo),
                 "geron_cosine_annealing: eta_max and eta_min must be finite")
  .morie_gr_need(lo >= 0, "geron_cosine_annealing: eta_min must be non-negative")
  .morie_gr_need(hi >= lo, "geron_cosine_annealing: eta_max must be at least eta_min")
  tt <- as.numeric(t)
  scalar <- length(tt) == 1L
  .morie_gr_need(length(tt) > 0L, "geron_cosine_annealing: t is empty")
  .morie_gr_need(all(tt >= 0) && all(tt <= T_int),
                 "geron_cosine_annealing: every t must lie in 0..T")
  f <- function(steps) lo + 0.5 * (hi - lo) * (1 + cos(pi * steps / T_int))
  eta <- f(tt)
  list(eta = if (scalar) eta[1L] else eta,
       schedule = f(seq.int(0, T_int)), T = T_int, eta_max = hi,
       eta_min = lo, estimate = eta[1L], n = length(tt),
       method = "Cosine annealing learning-rate schedule")
}

# --------------------------------------------------- capacity diagnostics

#' Neurons-per-layer heuristic (Geron Ch 10, morie.fn hmnpl)
#'
#' Constant width w (default 2d) across L hidden layers, with the
#' parameter count per layer. This is the LAYER-ALGEBRA route that the
#' parity test checks against a direct count.
#'
#' @param n_features Input width.
#' @param n_layers Hidden layers.
#' @param n_outputs Output width.
#' @param width Override for w.
#' @return List with `width`, `width_range`, `n_layers`,
#'   `n_parameters`, `parameters_per_layer`, `estimate`, `n`.
#' @export
morie_geron_neurons_per_layer <- function(n_features, n_layers = 1,
                                          n_outputs = 1, width = NULL) {
  d <- as.integer(n_features); L <- as.integer(n_layers); k <- as.integer(n_outputs)
  .morie_gr_need(d >= 1L, "geron_neurons_per_layer: n_features must be >= 1")
  .morie_gr_need(L >= 1L, "geron_neurons_per_layer: n_layers must be >= 1")
  .morie_gr_need(k >= 1L, "geron_neurons_per_layer: n_outputs must be >= 1")
  w <- if (is.null(width)) 2L * d else as.integer(width)
  .morie_gr_need(w >= 1L, "geron_neurons_per_layer: width must be >= 1")
  per <- c(d * w + w, rep(w * w + w, L - 1L), w * k + k)
  list(width = w, width_range = c(d, 2L * d), n_layers = L,
       n_parameters = sum(per), parameters_per_layer = as.integer(per),
       estimate = w, n = d,
       method = "Constant-width hidden-layer heuristic with parameter count")
}

#' Overfitting diagnostic (Geron Ch 4, morie.fn hmovf)
#'
#' Generalisation gap E_val - E_train with the early-stopping point.
#' `best_epoch` is 0-based, as in Python.
#'
#' @param train_err,val_err Equal-length non-negative error curves.
#' @param tol Non-negative gap tolerance.
#' @return List with `gap`, `gaps`, `ratio`, `overfitting`,
#'   `best_epoch`, `best_val`, `epochs_past_best`, `verdict`,
#'   `estimate`, `n`.
#' @export
morie_geron_overfitting <- function(train_err, val_err, tol = 0.0) {
  tr <- as.numeric(train_err); va <- as.numeric(val_err)
  .morie_gr_need(length(tr) > 0L && length(va) > 0L,
                 "geron_overfitting: train_err and val_err must be non-empty")
  .morie_gr_need(length(tr) == length(va), "geron_overfitting: lengths disagree")
  .morie_gr_need(all(is.finite(tr)) && all(is.finite(va)),
                 "geron_overfitting: errors contain non-finite values")
  .morie_gr_need(all(tr >= 0) && all(va >= 0),
                 "geron_overfitting: errors must be non-negative")
  t_ <- as.numeric(tol)
  .morie_gr_need(t_ >= 0, "geron_overfitting: tol must be non-negative")
  gaps <- va - tr
  gap <- gaps[length(gaps)]
  last_tr <- tr[length(tr)]; last_va <- va[length(va)]
  ratio <- if (last_tr == 0) (if (last_va > 0) Inf else 1) else last_va / last_tr
  k <- which.min(va) - 1L
  over <- gap > t_
  verdict <- if (over) {
    "overfitting: validation error exceeds training error by more than tol"
  } else if (last_va > 0 && last_tr > 0 && abs(gap) <= t_ &&
             last_va >= min(va) && length(va) == 1L) {
    "no gap: train and validation agree at this tolerance"
  } else {
    "no overfitting gap: errors agree, so look at their LEVEL for underfitting"
  }
  list(gap = gap, gaps = gaps, ratio = ratio, overfitting = over,
       best_epoch = k, best_val = va[k + 1L],
       epochs_past_best = length(va) - 1L - k, verdict = verdict,
       estimate = gap, n = length(tr),
       method = "Generalisation gap E_val - E_train with early-stopping point")
}

#' Underfitting diagnosis (Geron Ch 4, morie.fn hmuf)
#'
#' Verdict from the training error against a threshold plus the
#' train-validation gap. The plateau slope is the OLS slope of the
#' last max(2, floor(n/3)) points; `converged` compares it against
#' 1 percent of the final training error.
#'
#' @param train_err Training error curve.
#' @param threshold Bar to beat.
#' @param val_err Optional validation curve.
#' @param baseline Alias for
#'   `threshold`.
#' @param tol Gap tolerance.
#' @return List with `diagnosis`, `underfitting`, `train_error`,
#'   `val_error`, `gap`, `threshold`, `baseline`, `plateau_slope`,
#'   `converged`, `estimate`, `n`.
#' @export
morie_geron_underfitting <- function(train_err, threshold = NULL,
                                     val_err = NULL, baseline = NULL,
                                     tol = 0.05) {
  tr <- as.numeric(train_err)
  .morie_gr_need(length(tr) > 0L, "geron_underfitting: train_err is empty")
  .morie_gr_need(all(is.finite(tr)), "geron_underfitting: train_err contains non-finite values")
  .morie_gr_need(all(tr >= 0), "geron_underfitting: an error cannot be negative")
  thr <- if (!is.null(threshold)) threshold else baseline
  .morie_gr_need(!is.null(thr),
                 "geron_underfitting: supply `threshold` (or `baseline`) to judge the training error against")
  thr <- as.numeric(thr)
  .morie_gr_need(is.finite(thr) && thr >= 0,
                 "geron_underfitting: threshold must be non-negative and finite")
  t_ <- as.numeric(tol)
  .morie_gr_need(is.finite(t_) && t_ >= 0, "geron_underfitting: tol must be non-negative and finite")
  final_tr <- tr[length(tr)]
  va <- NULL; gap <- NULL
  if (!is.null(val_err)) {
    vv <- as.numeric(val_err)
    .morie_gr_need(length(vv) > 0L && all(is.finite(vv)),
                   "geron_underfitting: val_err must be non-empty and finite")
    va <- vv[length(vv)]
    gap <- va - final_tr
  }
  slope <- NULL
  if (length(tr) >= 3L) {
    keep <- max(2L, length(tr) %/% 3L)
    tail_v <- tr[(length(tr) - keep + 1L):length(tr)]
    idx <- seq.int(0, length(tail_v) - 1L)
    slope <- unname(stats::coef(stats::lm(tail_v ~ idx))[2L])
  }
  scale <- max(final_tr, .Machine$double.xmin)
  diagnosis <- if (!is.null(gap) && gap > t_) "overfitting"
               else if (final_tr > thr) "underfitting" else "adequate"
  list(diagnosis = diagnosis, underfitting = diagnosis == "underfitting",
       train_error = final_tr, val_error = va, gap = gap,
       threshold = thr, baseline = if (is.null(baseline)) NULL else as.numeric(baseline),
       plateau_slope = slope,
       converged = if (is.null(slope)) NULL else abs(slope) <= 0.01 * scale,
       estimate = final_tr, n = length(tr),
       method = "Bias/variance verdict from the training error against a threshold plus the train-validation gap")
}

#' Curse of dimensionality (Geron Ch 8, morie.fn hmcod)
#'
#' Exact uniform-cube nearest-neighbour radius
#' r = exp((lgamma(d/2 + 1) - log n - (d/2) log pi) / d), the mean
#' pairwise distance sqrt(d/6), and the border share.
#'
#' @param d Dimensions.
#' @param n Sample size.
#' @return List with `nn_distance`, `mean_pairwise_distance`,
#'   `border_fraction`, `border_tolerance`, `n_for_density`,
#'   `sparsity_factor`, `d`, `estimate`, `n`.
#' @export
morie_geron_curse_dimensionality <- function(d, n) {
  dd <- as.integer(d); nn <- as.integer(n)
  .morie_gr_need(dd >= 1L, "geron_curse_dimensionality: d must be >= 1")
  .morie_gr_need(nn >= 1L, "geron_curse_dimensionality: n must be >= 1")
  log_r <- (lgamma(dd / 2 + 1) - log(nn) - (dd / 2) * log(pi)) / dd
  r_nn <- exp(log_r)
  t_ <- 0.001
  r_1d <- 0.5 / nn
  list(nn_distance = r_nn, mean_pairwise_distance = sqrt(dd / 6),
       border_fraction = 1 - (1 - 2 * t_)^dd, border_tolerance = t_,
       n_for_density = exp(lgamma(dd / 2 + 1) - (dd / 2) * log(pi) - dd * log(r_1d)),
       sparsity_factor = r_nn / r_1d, d = dd, estimate = r_nn, n = nn,
       method = "exact uniform-cube nearest-neighbour radius, mean pairwise distance and border share")
}

#' Johnson-Lindenstrauss minimum dimension (Geron Ch 8, morie.fn hmjl)
#'
#' d_min = ceil(4 ln n / (eps^2/2 - eps^3/3)). Independent of the
#' original dimensionality, and loose in practice.
#'
#' @param n Number of points (>= 2).
#' @param eps Distortion in (0, 1).
#' @return List with `d_min`, `d_min_exact`, `eps`, `n_points`,
#'   `estimate`, `n`.
#' @export
morie_geron_johnson_lindenstrauss <- function(n, eps) {
  n_points <- as.integer(n)
  .morie_gr_need(n_points >= 2L, "geron_johnson_lindenstrauss: n must be at least 2 points")
  e <- as.numeric(eps)
  scalar <- length(e) == 1L
  .morie_gr_need(length(e) > 0L, "geron_johnson_lindenstrauss: eps is empty")
  .morie_gr_need(all(is.finite(e)), "geron_johnson_lindenstrauss: eps contains non-finite values")
  .morie_gr_need(all(e > 0) && all(e < 1),
                 "geron_johnson_lindenstrauss: eps must lie strictly in (0, 1)")
  denom <- e^2 / 2 - e^3 / 3
  exact <- 4 * log(n_points) / denom
  d_min <- ceiling(exact)
  list(d_min = if (scalar) as.integer(d_min[1L]) else as.integer(d_min),
       d_min_exact = if (scalar) exact[1L] else exact,
       eps = if (scalar) e[1L] else e, n_points = n_points,
       estimate = if (scalar) d_min[1L] else max(d_min), n = n_points,
       method = "Johnson-Lindenstrauss minimum dimension")
}

# ---------------------------------------------------------- preprocessing

#' Standardization / z-score (Geron Ch 2, morie.fn hmstz)
#'
#' Column-wise (x - mean) / sd. `ddof = 0` is numpy's default
#' POPULATION sd, which is not R's `sd()`; pass ddof = 1 for the n-1
#' form.
#'
#' @param X Numeric vector or matrix.
#' @param ddof Degrees of freedom.
#' @return List with `X_std`, `Z`, `mean`, `scale`, `ddof`,
#'   `estimate`, `n`.
#' @export
morie_geron_standardization <- function(X, ddof = 0) {
  A <- if (is.matrix(X)) X else matrix(as.numeric(X), ncol = 1)
  storage.mode(A) <- "double"
  .morie_gr_need(length(A) > 0L,
                 "geron_standardization: X must be a non-empty 1-D or 2-D array")
  .morie_gr_need(all(is.finite(A)), "geron_standardization: X contains non-finite values")
  dd <- as.integer(ddof)
  .morie_gr_need(dd >= 0L && dd < nrow(A),
                 "geron_standardization: ddof must satisfy 0 <= ddof < n rows")
  mu <- colMeans(A)
  sd_ <- apply(A, 2, function(col) sqrt(sum((col - mean(col))^2) / (length(col) - dd)))
  .morie_gr_need(!any(sd_ == 0),
                 "geron_standardization: a column is constant (sigma = 0) and cannot be scaled")
  Z <- sweep(sweep(A, 2, mu, "-"), 2, sd_, "/")
  list(X_std = Z, Z = Z, mean = mu, scale = sd_, ddof = dd,
       estimate = max(abs(Z)), n = nrow(A),
       method = paste0("Column-wise z-score with ddof=", dd))
}

#' One-hot encoding (Geron Ch 2, morie.fn hmohe)
#'
#' Levels default to the sorted unique values of each column. Column
#' names are `x<j>=<level>` with j 0-based, as in Python.
#'
#' @param X Vector or matrix of categories.
#' @param drop_first Drop the reference level of each variable.
#' @param categories Optional list of level vectors, one per column.
#' @return List with `encoded`, `categories`, `names`, `n_columns`,
#'   `drop_first`, `estimate`, `n`.
#' @export
morie_geron_one_hot_encoding <- function(X, drop_first = FALSE,
                                         categories = NULL) {
  A <- if (is.matrix(X)) X else matrix(X, ncol = 1)
  m <- nrow(A); p <- ncol(A)
  .morie_gr_need(m > 0L && p > 0L, "geron_one_hot_encoding: X is empty")
  cats <- if (is.null(categories)) {
    lapply(seq_len(p), function(j) sort(unique(A[, j])))
  } else {
    cs <- categories
    if (p == 1L && !is.list(cs)) cs <- list(cs)
    .morie_gr_need(length(cs) == p,
                   "geron_one_hot_encoding: categories entries and columns disagree")
    lapply(cs, function(c) c)
  }
  blocks <- list(); names_ <- character(0)
  for (j in seq_len(p)) {
    levels_ <- cats[[j]]
    .morie_gr_need(length(levels_) >= 1L,
                   "geron_one_hot_encoding: a column has no categories")
    eq <- outer(A[, j], levels_, "==")
    .morie_gr_need(all(rowSums(eq) > 0),
                   "geron_one_hot_encoding: a column has values not in categories")
    keep <- if (isTRUE(drop_first)) seq_along(levels_)[-1L] else seq_along(levels_)
    blocks[[length(blocks) + 1L]] <- matrix(as.numeric(eq[, keep, drop = FALSE]),
                                            nrow = m)
    names_ <- c(names_, paste0("x", j - 1L, "=", levels_[keep]))
  }
  enc <- do.call(cbind, blocks)
  list(encoded = enc, categories = cats, names = names_,
       n_columns = ncol(enc), drop_first = isTRUE(drop_first),
       estimate = enc, n = m,
       method = paste0("One-hot indicator encoding",
                       if (isTRUE(drop_first)) " with reference level dropped" else ""))
}

#' Ordinal encoding (Geron Ch 2, morie.fn hmord)
#'
#' Integer codes against a fixed level order; the codes are 0-based to
#' match Python. Implies order AND equal spacing, which is why one-hot
#' is the safer default for a linear model.
#'
#' @param X Vector or matrix of categories.
#' @param categories Optional list of level vectors.
#' @return List with `encoded` (0-based), `categories`,
#'   `n_categories`, `estimate`, `n`.
#' @export
morie_geron_ordinal_encoding <- function(X, categories = NULL) {
  A <- if (is.matrix(X)) X else matrix(X, ncol = 1)
  m <- nrow(A); p <- ncol(A)
  .morie_gr_need(m > 0L && p > 0L, "geron_ordinal_encoding: X is empty")
  cats <- if (is.null(categories)) {
    lapply(seq_len(p), function(j) sort(unique(A[, j])))
  } else {
    cs <- categories
    if (p == 1L && !is.list(cs)) cs <- list(cs)
    .morie_gr_need(length(cs) == p,
                   "geron_ordinal_encoding: categories entries and columns disagree")
    cs
  }
  enc <- matrix(0L, m, p)
  for (j in seq_len(p)) {
    levels_ <- cats[[j]]
    .morie_gr_need(length(levels_) == length(unique(levels_)),
                   "geron_ordinal_encoding: a column has duplicated categories")
    eq <- outer(A[, j], levels_, "==")
    .morie_gr_need(all(rowSums(eq) > 0),
                   "geron_ordinal_encoding: a column has values not in categories")
    enc[, j] <- apply(eq, 1, which.max) - 1L
  }
  list(encoded = enc, categories = cats,
       n_categories = vapply(cats, length, integer(1)),
       estimate = enc, n = m,
       method = "Ordinal (integer) encoding against a fixed level order")
}

#' Median imputation with a missingness indicator (Geron Ch 2, morie.fn hmimp)
#'
#' Numeric columns only in this port: the Python module also mode-fills
#' object columns, which has no faithful R equivalent without deciding
#' a factor/character convention, so that arm is NOT ported (see the
#' package NOTE in the parity tests).
#'
#' @param X Numeric vector or matrix, NA marking missing.
#' @param missing_values Optional extra sentinel value.
#' @param add_indicator Return the missingness mask.
#' @return List with `X_imputed`, `statistics`, `indicator`,
#'   `n_missing`, `missing_fraction`, `strategy`, `estimate`, `n`.
#' @export
morie_geron_imputation_median <- function(X, missing_values = NULL,
                                          add_indicator = TRUE) {
  A <- if (is.matrix(X)) X else matrix(as.numeric(X), ncol = 1)
  storage.mode(A) <- "double"
  .morie_gr_need(length(A) > 0L,
                 "geron_imputation_median: X must be a non-empty 2-D array")
  m <- nrow(A); n <- ncol(A)
  out <- A
  indicator <- matrix(FALSE, m, n)
  stats_ <- numeric(n)
  for (j in seq_len(n)) {
    col <- A[, j]
    miss <- is.na(col)
    if (!is.null(missing_values)) miss <- miss | (!is.na(col) & col == missing_values)
    indicator[, j] <- miss
    present <- col[!miss]
    .morie_gr_need(length(present) > 0L,
                   "geron_imputation_median: a column is entirely missing")
    fill <- median(present)
    stats_[j] <- fill
    if (any(miss)) out[miss, j] <- fill
  }
  n_missing <- sum(indicator)
  list(X_imputed = out, statistics = stats_,
       indicator = if (isTRUE(add_indicator)) indicator else NULL,
       n_missing = n_missing, missing_fraction = n_missing / (m * n),
       strategy = "median", estimate = n_missing / (m * n), n = m,
       method = "Median / mode imputation")
}

#' Glorot (Xavier) initialization (Geron Ch 11, morie.fn hmxav)
#'
#' Var(W) = 2/(fan_in + fan_out). The uniform arm draws the LCG stream
#' draw-for-draw; the normal arm draws a SECOND stream seeded
#' `seed + 7919` and combines the two by Box-Muller, exactly as the
#' Python module does. The reshape is row-major, so byrow = TRUE.
#'
#' @param fan_in,fan_out Positive layer widths.
#' @param seed LCG seed.
#' @param distribution "uniform" or "normal".
#' @return List with `W`, `limit`, `std`, `variance`, `fan_in`,
#'   `fan_out`, `distribution`, `estimate`, `n`.
#' @export
morie_geron_glorot_init <- function(fan_in, fan_out, seed = 0,
                                    distribution = "uniform") {
  fi <- as.integer(fan_in); fo <- as.integer(fan_out)
  .morie_gr_need(fi >= 1L && fo >= 1L,
                 "geron_glorot_init: fan_in and fan_out must be >= 1")
  dist <- tolower(as.character(distribution))
  .morie_gr_need(dist %in% c("uniform", "normal"),
                 "geron_glorot_init: distribution must be 'uniform' or 'normal'")
  var_ <- 2 / (fi + fo)
  std <- sqrt(var_)
  limit <- sqrt(6 / (fi + fo))
  u <- .morie_gr_lcg_u(fi * fo, seed)
  W <- if (dist == "uniform") {
    (2 * u - 1) * limit
  } else {
    u2 <- .morie_gr_lcg_u(fi * fo, as.integer(seed) + 7919L)
    std * sqrt(-2 * log(u)) * cos(2 * pi * u2)
  }
  list(W = matrix(W, nrow = fi, ncol = fo, byrow = TRUE), limit = limit,
       std = std, variance = var_, fan_in = fi, fan_out = fo,
       distribution = dist, estimate = var_, n = fi * fo,
       method = paste0("Glorot ", dist,
                       " initialization, Var(W) = 2/(fan_in + fan_out)"))
}
# ------------------------------------------------------------- optimisers

#' Momentum step (Geron Ch 11, morie.fn hmmom)
#'
#' v <- beta v + g; theta <- theta - eta v. On a constant gradient the
#' step approaches eta g / (1 - beta), which is the `terminal_step`
#' field: momentum effectively multiplies the learning rate.
#'
#' @param grads Gradient vector.
#' @param v Velocity; default zeros.
#' @param beta Decay in \[0, 1).
#' @param eta Positive rate.
#' @param theta Parameters; default zeros.
#' @param nesterov Look-ahead form.
#' @return List with `theta`, `theta_next`, `step`, `v`,
#'   `terminal_step`, `beta`, `estimate`, `n`.
#' @export
morie_geron_momentum <- function(grads, v = NULL, beta = 0.9, eta = 0.01,
                                 theta = NULL, nesterov = FALSE) {
  g <- as.numeric(grads)
  .morie_gr_need(length(g) > 0L, "geron_momentum: grads is empty")
  .morie_gr_need(all(is.finite(g)), "geron_momentum: grads contains non-finite values")
  vv <- if (is.null(v)) rep(0, length(g)) else as.numeric(v)
  th <- if (is.null(theta)) rep(0, length(g)) else as.numeric(theta)
  .morie_gr_need(length(vv) == length(g) && length(th) == length(g),
                 "geron_momentum: v and theta must match grads")
  b <- as.numeric(beta); lr <- as.numeric(eta)
  .morie_gr_need(b >= 0 && b < 1, "geron_momentum: beta must lie in [0, 1)")
  .morie_gr_need(is.finite(lr) && lr > 0,
                 "geron_momentum: eta must be a positive finite learning rate")
  v_new <- b * vv + g
  step <- if (isTRUE(nesterov)) -lr * (b * v_new + g) else -lr * v_new
  theta_next <- th + step
  list(theta = theta_next, theta_next = theta_next, step = step,
       v = v_new, terminal_step = lr * g / (1 - b), beta = b,
       estimate = sqrt(sum(step^2)), n = length(g),
       method = "Momentum optimization step")
}

#' Nesterov accelerated gradient (Geron Ch 11, morie.fn hmnag)
#'
#' The gradient is read at the look-ahead point theta - eta beta v.
#' Pass `grads` as a function to get the true look-ahead evaluation;
#' pass a vector for the pre-evaluated form (the look-ahead point is
#' still reported).
#'
#' @param grads Gradient vector, or function(theta) -> gradient.
#' @param v Velocity; default zeros.
#' @param beta Decay in \[0, 1).
#' @param eta Positive rate.
#' @param theta Parameters.
#' @return List with `theta`, `theta_next`, `v`, `lookahead`,
#'   `gradient`, `step`, `estimate`, `n`.
#' @export
morie_geron_nesterov <- function(grads, v = NULL, beta = 0.9, eta = 0.001,
                                 theta = NULL) {
  b <- as.numeric(beta); lr <- as.numeric(eta)
  .morie_gr_need(b >= 0 && b < 1, "geron_nesterov: beta must lie in [0, 1)")
  .morie_gr_need(is.finite(lr) && lr > 0,
                 "geron_nesterov: eta must be a positive finite learning rate")
  if (is.function(grads)) {
    .morie_gr_need(!is.null(theta),
                   "geron_nesterov: theta is required when grads is a function, to form the look-ahead")
    th <- as.numeric(theta)
    vv <- if (is.null(v)) rep(0, length(th)) else as.numeric(v)
    .morie_gr_need(length(vv) == length(th), "geron_nesterov: v and theta must match")
    look <- th - lr * b * vv
    g <- as.numeric(grads(look))
    .morie_gr_need(length(g) == length(th),
                   "geron_nesterov: grads returned the wrong length")
  } else {
    g <- as.numeric(grads)
    th <- if (is.null(theta)) rep(0, length(g)) else as.numeric(theta)
    vv <- if (is.null(v)) rep(0, length(g)) else as.numeric(v)
    .morie_gr_need(length(vv) == length(g) && length(th) == length(g),
                   "geron_nesterov: v and theta must have the same shape as grads")
    look <- th - lr * b * vv
  }
  .morie_gr_need(length(g) > 0L, "geron_nesterov: gradient is empty")
  .morie_gr_need(all(is.finite(g)), "geron_nesterov: gradient contains non-finite values")
  v_new <- b * vv + g
  step <- -lr * v_new
  theta_next <- th + step
  list(theta = theta_next, theta_next = theta_next, v = v_new,
       lookahead = look, gradient = g, step = step,
       estimate = theta_next, n = length(g),
       method = "Nesterov accelerated gradient with look-ahead evaluation")
}

#' NAdam step (Geron Ch 11, morie.fn hmnadm)
#'
#' Adam moments with the Nesterov-corrected first moment
#' m_hat = b1 m / (1 - b1^(t+1)) + (1 - b1) g / (1 - b1^t). Note the
#' t+1 exponent in the first term: it is not a typo, it is what pulls
#' the current gradient into the step without a second evaluation.
#'
#' @param grads Gradient vector.
#' @param m,v Moments; default zeros.
#' @param b1,b2 Decays in \[0, 1).
#' @param eta Positive rate.
#' @param t 1-based timestep.
#' @param eps Non-negative floor.
#' @param theta Parameters; default zeros.
#' @return List with `theta`, `theta_next`, `step`, `m`, `v`, `m_hat`,
#'   `v_hat`, `t`, `estimate`, `n`.
#' @export
morie_geron_nadam <- function(grads, m = NULL, v = NULL, b1 = 0.9, b2 = 0.999,
                              eta = 0.001, t = 1, eps = 1e-8, theta = NULL) {
  g <- as.numeric(grads)
  .morie_gr_need(length(g) > 0L, "geron_nadam: grads is empty")
  .morie_gr_need(all(is.finite(g)), "geron_nadam: grads contains non-finite values")
  mm <- if (is.null(m)) rep(0, length(g)) else as.numeric(m)
  vv <- if (is.null(v)) rep(0, length(g)) else as.numeric(v)
  th <- if (is.null(theta)) rep(0, length(g)) else as.numeric(theta)
  .morie_gr_need(length(mm) == length(g) && length(vv) == length(g) &&
                   length(th) == length(g),
                 "geron_nadam: m, v and theta must match grads")
  beta1 <- as.numeric(b1); beta2 <- as.numeric(b2)
  lr <- as.numeric(eta); e <- as.numeric(eps)
  .morie_gr_need(beta1 >= 0 && beta1 < 1 && beta2 >= 0 && beta2 < 1,
                 "geron_nadam: b1 and b2 must lie in [0, 1)")
  .morie_gr_need(is.finite(lr) && lr > 0,
                 "geron_nadam: eta must be a positive finite learning rate")
  .morie_gr_need(e >= 0, "geron_nadam: eps must be non-negative")
  step_t <- as.integer(t)
  .morie_gr_need(step_t >= 1L, "geron_nadam: t must be a 1-based timestep >= 1")
  .morie_gr_need(all(vv >= 0),
                 "geron_nadam: v must be non-negative (it accumulates squared gradients)")
  m_new <- beta1 * mm + (1 - beta1) * g
  v_new <- beta2 * vv + (1 - beta2) * g * g
  m_hat <- beta1 * m_new / (1 - beta1^(step_t + 1L)) +
    (1 - beta1) * g / (1 - beta1^step_t)
  v_hat <- v_new / (1 - beta2^step_t)
  step <- -lr * m_hat / (sqrt(v_hat) + e)
  theta_next <- th + step
  list(theta = theta_next, theta_next = theta_next, step = step,
       m = m_new, v = v_new, m_hat = m_hat, v_hat = v_hat, t = step_t,
       estimate = sqrt(sum(step^2)), n = length(g),
       method = "NAdam (Adam moments with a Nesterov-corrected first moment)")
}

#' Single-sample SGD step (Geron Ch 4, morie.fn hmsgdu)
#'
#' theta <- theta - eta * 2 x_i (x_i^T theta - y_i). With `index` NULL
#' the row is picked from ONE LCG draw, i = floor(u * n) capped at
#' n - 1, and is reported 0-based in `index`.
#'
#' @param X,y,theta Design, targets, coefficients.
#' @param eta Positive rate.
#' @param seed LCG seed.
#' @param index Optional 0-based row override.
#' @return List with `theta`, `theta_next`, `gradient`,
#'   `batch_gradient`, `residual`, `index` (0-based), `eta`,
#'   `estimate`, `n`.
#' @export
morie_geron_sgd_update <- function(X, y, theta, eta = 0.1, seed = 0,
                                   index = NULL) {
  Xa <- if (is.matrix(X)) X else matrix(as.numeric(X), ncol = 1)
  storage.mode(Xa) <- "double"
  .morie_gr_need(length(Xa) > 0L,
                 "geron_sgd_update: X must be a non-empty (n, d) design matrix")
  ya <- as.numeric(y); th <- as.numeric(theta)
  .morie_gr_need(length(ya) == nrow(Xa), "geron_sgd_update: X rows and y disagree")
  .morie_gr_need(length(th) == ncol(Xa), "geron_sgd_update: theta and features disagree")
  .morie_gr_need(all(is.finite(Xa)) && all(is.finite(ya)) && all(is.finite(th)),
                 "geron_sgd_update: X, y and theta must be finite")
  lr <- as.numeric(eta)
  .morie_gr_need(is.finite(lr) && lr > 0, "geron_sgd_update: eta must be positive and finite")
  n <- nrow(Xa)
  if (is.null(index)) {
    u <- .morie_gr_lcg_u(1L, seed)
    i <- min(as.integer(floor(u * n)), n - 1L)
  } else {
    i <- as.integer(index)
    .morie_gr_need(i >= 0L && i < n, "geron_sgd_update: index is outside 0..n-1")
  }
  xi <- Xa[i + 1L, ]
  resid <- sum(xi * th) - ya[i + 1L]
  grad <- 2 * xi * resid
  theta_next <- th - lr * grad
  full_resid <- as.numeric(Xa %*% th) - ya
  batch_grad <- 2 / n * as.numeric(t(Xa) %*% full_resid)
  list(theta = theta_next, theta_next = theta_next, gradient = grad,
       batch_gradient = batch_grad, residual = resid, index = i,
       eta = lr, estimate = sqrt(sum((lr * grad)^2)), n = n,
       method = "Single-sample least-squares SGD update theta <- theta - eta*2x(x^T theta - y)")
}

#' Mini-batch gradient descent (Geron Ch 4, morie.fn hmmbgd)
#'
#' b = m is batch GD, b = 1 is SGD. IMPORTANT: the Python module draws
#' its shuffle from `numpy.random.default_rng` (PCG64), which R cannot
#' reproduce, so this port takes the batch order explicitly. Pass
#' `order` as a 0-based permutation of 0..m-1 (the parity tests read
#' the permutation out of Python and pass it in); with `order = NULL`
#' the identity order is used. This is the one honest divergence in
#' this file.
#'
#' @param X,y,theta Design, targets, coefficients.
#' @param eta Positive rate.
#' @param b Batch size in 1..m.
#' @param order Optional 0-based permutation; NULL means identity.
#' @param n_steps Number of updates.
#' @return List with `theta`, `gradient`, `full_gradient`,
#'   `batch_indices` (0-based), `mse`, `batch_size`, `estimate`, `n`.
#' @export
morie_geron_minibatch_gd <- function(X, y, theta, eta, b, order = NULL,
                                     n_steps = 1) {
  A <- if (is.matrix(X)) X else matrix(as.numeric(X), ncol = 1)
  storage.mode(A) <- "double"
  .morie_gr_need(length(A) > 0L, "geron_minibatch_gd: X must be a non-empty 2-D array")
  yy <- as.numeric(y)
  .morie_gr_need(length(yy) == nrow(A), "geron_minibatch_gd: X rows and y disagree")
  t_ <- as.numeric(theta)
  .morie_gr_need(length(t_) == ncol(A), "geron_minibatch_gd: theta and columns disagree")
  .morie_gr_need(all(is.finite(A)) && all(is.finite(yy)) && all(is.finite(t_)),
                 "geron_minibatch_gd: X, y and theta must be finite")
  lr <- as.numeric(eta)
  .morie_gr_need(is.finite(lr) && lr > 0,
                 "geron_minibatch_gd: eta must be a positive finite learning rate")
  m <- nrow(A)
  bs <- as.integer(b)
  .morie_gr_need(bs >= 1L && bs <= m, "geron_minibatch_gd: batch size must lie in 1..m")
  steps <- as.integer(n_steps)
  .morie_gr_need(steps >= 1L, "geron_minibatch_gd: n_steps must be at least 1")
  ord <- if (is.null(order)) seq.int(0, m - 1L) else as.integer(order)
  .morie_gr_need(length(ord) == m && all(sort(ord) == seq.int(0, m - 1L)),
                 "geron_minibatch_gd: order must be a permutation of 0..m-1")
  cursor <- 0L; grad <- NULL; idx <- NULL
  for (s in seq_len(steps)) {
    if (cursor + bs > m) cursor <- 0L
    idx <- ord[(cursor + 1L):(cursor + bs)]
    cursor <- cursor + bs
    Xb <- A[idx + 1L, , drop = FALSE]
    yb <- yy[idx + 1L]
    grad <- (2 / bs) * as.numeric(t(Xb) %*% (Xb %*% t_ - yb))
    t_ <- t_ - lr * grad
  }
  full_grad <- (2 / m) * as.numeric(t(A) %*% (A %*% t_ - yy))
  mse <- mean((as.numeric(A %*% t_) - yy)^2)
  list(theta = t_, gradient = grad, full_gradient = full_grad,
       batch_indices = idx, mse = mse, batch_size = bs,
       estimate = mse, n = m,
       method = "Mini-batch gradient descent step")
}

# ------------------------------------------------------ logistic regression

#' Logistic probability (Geron Ch 4, morie.fn hmlogp)
#'
#' sigma(theta^T x) with the overflow-safe two-branch sigmoid. The
#' decision boundary is the linear set theta^T x = 0.
#'
#' @param X Design matrix (a vector is one row).
#' @param theta Coefficients.
#' @param add_bias Prepend a ones column.
#' @return List with `p_hat`, `logits`, `prediction` (0/1),
#'   `estimate`, `n`.
#' @export
morie_geron_logistic_probability <- function(X, theta, add_bias = FALSE) {
  A <- if (is.matrix(X)) X else matrix(as.numeric(X), nrow = 1)
  storage.mode(A) <- "double"
  .morie_gr_need(length(A) > 0L, "geron_logistic_probability: X is empty")
  if (isTRUE(add_bias)) A <- cbind(1, A)
  t_ <- as.numeric(theta)
  .morie_gr_need(length(t_) == ncol(A),
                 "geron_logistic_probability: theta and columns disagree")
  .morie_gr_need(all(is.finite(A)) && all(is.finite(t_)),
                 "geron_logistic_probability: X and theta must be finite")
  logits <- as.numeric(A %*% t_)
  p <- morie_geron_sigmoid(logits)$a
  list(p_hat = p, logits = logits, prediction = as.integer(p >= 0.5),
       estimate = mean(p), n = nrow(A),
       method = "Logistic regression probability")
}

#' Binary log loss (Geron Ch 4, morie.fn hmlogcl)
#'
#' Computed from the LOGITS as -y z + max(z, 0) + log1p(exp(-|z|)),
#' which is stable where -log(p) is not. Probabilities are delegated
#' to `morie_geron_logistic_probability`, as hmlogcl delegates to
#' hmlogp. The class-rate baseline is the bar to beat.
#'
#' @param X,y,theta Design, 0/1 labels, coefficients.
#' @param add_bias Prepend a ones column.
#' @return List with `cost`, `per_instance`, `p_hat`, `logits`,
#'   `baseline_cost`, `estimate`, `n`.
#' @export
morie_geron_logistic_cost <- function(X, y, theta, add_bias = FALSE) {
  yy <- as.numeric(y)
  .morie_gr_need(length(yy) > 0L, "geron_logistic_cost: y is empty")
  .morie_gr_need(all(yy %in% c(0, 1)), "geron_logistic_cost: y must contain only 0 and 1")
  inner <- morie_geron_logistic_probability(X, theta, add_bias = add_bias)
  p <- inner$p_hat; z <- inner$logits
  .morie_gr_need(length(p) == length(yy), "geron_logistic_cost: X rows and y disagree")
  per <- -yy * z + pmax(z, 0) + log1p(exp(-abs(z)))
  cost <- mean(per)
  rate <- mean(yy)
  baseline <- if (rate > 0 && rate < 1)
    -(rate * log(rate) + (1 - rate) * log1p(-rate)) else 0
  list(cost = cost, per_instance = per, p_hat = p, logits = z,
       baseline_cost = baseline, estimate = cost, n = length(yy),
       method = "Binary log loss (cross-entropy)")
}

#' Logistic gradient and Hessian (Geron Ch 4, morie.fn hmlogg)
#'
#' (1/m) X^T (p_hat - y) with the PSD Hessian (1/m) X^T diag(p(1-p)) X,
#' which is what makes the cost convex.
#'
#' @param X,y,theta Design, 0/1 labels, coefficients.
#' @param add_bias Prepend a ones column.
#' @return List with `gradient`, `hessian`, `p_hat`, `residuals`,
#'   `estimate`, `n`.
#' @export
morie_geron_logistic_gradient <- function(X, y, theta, add_bias = FALSE) {
  yy <- as.numeric(y)
  .morie_gr_need(length(yy) > 0L, "geron_logistic_gradient: y is empty")
  .morie_gr_need(all(yy %in% c(0, 1)),
                 "geron_logistic_gradient: y must contain only 0 and 1")
  A <- if (is.matrix(X)) X else matrix(as.numeric(X), nrow = 1)
  storage.mode(A) <- "double"
  inner <- morie_geron_logistic_probability(A, theta, add_bias = add_bias)
  p <- inner$p_hat
  if (isTRUE(add_bias)) A <- cbind(1, A)
  .morie_gr_need(length(p) == length(yy),
                 "geron_logistic_gradient: X rows and y disagree")
  m <- length(yy)
  resid <- p - yy
  grad <- as.numeric(t(A) %*% resid) / m
  w <- p * (1 - p)
  hess <- (t(A) * rep(w, each = ncol(A))) %*% A / m
  list(gradient = grad, hessian = hess, p_hat = p, residuals = resid,
       estimate = sqrt(sum(grad^2)), n = m,
       method = "Logistic regression cost gradient")
}

#' Perceptron learning rule (Geron Ch 10, morie.fn hmpcpt)
#'
#' w <- w + eta (y - y_hat) x with a step activation, run row by row
#' in index order for at most `n_iter` epochs, stopping the moment an
#' epoch makes no mistake. Converges iff the classes are linearly
#' separable, which is why XOR never converges.
#'
#' @param X Design matrix.
#' @param y Labels `in {0, 1}`.
#' @param eta Positive rate.
#' @param n_iter Maximum epochs.
#' @return List with `w`, `weights`, `bias`, `predictions`,
#'   `accuracy`, `mistakes_per_epoch`, `converged`, `estimate`, `n`.
#' @export
morie_geron_perceptron <- function(X, y, eta = 1.0, n_iter = 10) {
  A <- if (is.matrix(X)) X else matrix(as.numeric(X), ncol = 1)
  storage.mode(A) <- "double"
  .morie_gr_need(length(A) > 0L, "geron_perceptron: X must be a non-empty 2-D array")
  yv <- as.numeric(y)
  .morie_gr_need(length(yv) == nrow(A), "geron_perceptron: X rows and y disagree")
  .morie_gr_need(all(yv %in% c(0, 1)), "geron_perceptron: labels must be 0 or 1")
  lr <- as.numeric(eta)
  .morie_gr_need(is.finite(lr) && lr > 0, "geron_perceptron: eta must be positive and finite")
  T_ <- as.integer(n_iter)
  .morie_gr_need(T_ >= 1L, "geron_perceptron: n_iter must be >= 1")
  .morie_gr_need(all(is.finite(A)), "geron_perceptron: X contains non-finite values")
  m <- nrow(A)
  w <- rep(0, ncol(A)); b <- 0
  mistakes <- integer(0); converged <- FALSE
  for (ep in seq_len(T_)) {
    wrong <- 0L
    for (i in seq_len(m)) {
      yhat <- if (sum(A[i, ] * w) + b >= 0) 1 else 0
      err <- yv[i] - yhat
      if (err != 0) {
        w <- w + lr * err * A[i, ]
        b <- b + lr * err
        wrong <- wrong + 1L
      }
    }
    mistakes <- c(mistakes, wrong)
    if (wrong == 0L) { converged <- TRUE; break }
  }
  pred <- as.numeric((as.numeric(A %*% w) + b) >= 0)
  list(w = w, weights = w, bias = b, predictions = pred,
       accuracy = mean(pred == yv), mistakes_per_epoch = mistakes,
       converged = converged, estimate = w, n = m,
       method = "Perceptron rule w <- w + eta (y - y_hat) x with a step activation")
}

#' Hebbian update (Geron Ch 10, morie.fn hmhebb)
#'
#' dW = eta X^T Y, a pure outer product with no error signal, which is
#' why ||W|| grows without bound under repeated exposure.
#'
#' @param X Presynaptic activity (rows = presentations).
#' @param Y Postsynaptic activity.
#' @param eta Positive rate.
#' @param W Optional starting weights.
#' @return List with `dW`, `W`, `norm_before`, `norm_after`,
#'   `warnings`, `estimate`, `n`.
#' @export
morie_geron_hebb_rule <- function(X, Y, eta = 0.1, W = NULL) {
  A <- if (is.matrix(X)) X else matrix(as.numeric(X), nrow = 1)
  B <- if (is.matrix(Y)) Y else matrix(as.numeric(Y), nrow = 1)
  storage.mode(A) <- "double"; storage.mode(B) <- "double"
  .morie_gr_need(length(A) > 0L && length(B) > 0L,
                 "geron_hebb_rule: X and Y must be non-empty")
  .morie_gr_need(nrow(A) == nrow(B),
                 "geron_hebb_rule: X and Y must have the same number of presentations")
  .morie_gr_need(all(is.finite(A)) && all(is.finite(B)),
                 "geron_hebb_rule: X and Y must be finite")
  lr <- as.numeric(eta)
  .morie_gr_need(is.finite(lr) && lr > 0,
                 "geron_hebb_rule: eta must be a positive finite learning rate")
  n_pre <- ncol(A); n_post <- ncol(B)
  W0 <- if (is.null(W)) matrix(0, n_pre, n_post) else as.matrix(W)
  .morie_gr_need(nrow(W0) == n_pre && ncol(W0) == n_post,
                 "geron_hebb_rule: W has the wrong shape")
  dW <- lr * (t(A) %*% B)
  W_new <- W0 + dW
  nb <- sqrt(sum(W0^2)); na <- sqrt(sum(W_new^2))
  list(dW = dW, W = W_new, norm_before = nb, norm_after = na,
       warnings = if (na > nb)
         "Hebbian learning has no stabilising term: repeated exposure grows ||W|| without bound."
         else character(0),
       estimate = sqrt(sum(dW^2)), n = nrow(A),
       method = "Hebbian weight update")
}

#' Residual block (Geron Ch 14, morie.fn hmresn)
#'
#' y = F(x) + x, optionally with a projected skip. dy/dx = I + dF/dx
#' is what keeps a gradient path open through arbitrary depth.
#'
#' @param x Input array.
#' @param F Function of x.
#' @param projection Optional function applied to the skip path.
#' @return List with `y`, `output`, `residual`, `skip`,
#'   `residual_fraction`, `estimate`, `n`.
#' @export
morie_geron_resnet <- function(x, F, projection = NULL) {
  .morie_gr_need(is.function(F), "geron_resnet: F must be a function")
  a <- x
  .morie_gr_need(length(a) > 0L, "geron_resnet: x is empty")
  skip <- a
  if (!is.null(projection)) {
    .morie_gr_need(is.function(projection), "geron_resnet: projection must be a function")
    skip <- projection(a)
  }
  out <- F(a)
  .morie_gr_need(identical(dim(out), dim(skip)) && length(out) == length(skip),
                 "geron_resnet: F and the skip path have different shapes")
  .morie_gr_need(all(is.finite(out)) && all(is.finite(skip)),
                 "geron_resnet: the block produced non-finite values")
  y <- out + skip
  ns <- sqrt(sum(as.numeric(skip)^2)); nr <- sqrt(sum(as.numeric(out)^2))
  list(y = y, output = y, residual = out, skip = skip,
       residual_fraction = if ((nr + ns) > 0) nr / (nr + ns) else 0,
       estimate = y, n = length(a),
       method = "Residual block y = F(x) + x (optionally projected skip)")
}

#' Hard voting ensemble (Geron Ch 7, morie.fn hmvth)
#'
#' Plurality vote with deterministic smallest-label tie-breaking
#' (which.max takes the first maximum, matching np.argmax). Majority
#' voting only helps when the members err on DIFFERENT rows.
#'
#' @param models List of functions mapping X to a label vector.
#' @param X Design matrix.
#' @param y_true Optional gold labels.
#' @return List with `predicted`, `votes`, `classes`,
#'   `member_predictions`, `member_accuracy`, `accuracy`,
#'   `agreement`, `estimate`, `n`.
#' @export
morie_geron_voting_hard <- function(models, X, y_true = NULL) {
  ms <- models
  .morie_gr_need(length(ms) > 0L, "geron_voting_hard: no base models supplied")
  .morie_gr_need(all(vapply(ms, is.function, logical(1))),
                 "geron_voting_hard: each model must be a function mapping X to labels")
  A <- if (is.matrix(X)) X else matrix(X, ncol = 1)
  n <- nrow(A)
  .morie_gr_need(n > 0L, "geron_voting_hard: X is empty")
  preds <- lapply(ms, function(f) {
    p <- as.vector(f(X))
    .morie_gr_need(length(p) == n, "geron_voting_hard: a model returned the wrong number of labels")
    p
  })
  P <- do.call(rbind, preds)
  classes <- sort(unique(as.vector(P)))
  votes <- matrix(0L, n, length(classes))
  for (j in seq_along(classes)) votes[, j] <- colSums(P == classes[j])
  winner <- apply(votes, 1, which.max)
  pred <- classes[winner]
  agreement <- vapply(preds, function(p) mean(p == pred), numeric(1))
  acc <- NULL; member_acc <- NULL
  if (!is.null(y_true)) {
    g <- as.vector(y_true)
    .morie_gr_need(length(g) == n, "geron_voting_hard: gold labels and rows disagree")
    acc <- mean(pred == g)
    member_acc <- vapply(preds, function(p) mean(p == g), numeric(1))
  }
  list(predicted = pred, votes = votes, classes = classes,
       member_predictions = P, member_accuracy = member_acc,
       accuracy = acc, agreement = agreement,
       estimate = if (!is.null(acc)) acc
                  else mean(apply(votes, 1, max) / length(ms)),
       n = n,
       method = "Hard (plurality) voting with deterministic smallest-label tie-breaking")
}

#' Sinusoidal positional encoding (Geron Ch 16, morie.fn hmpe)
#'
#' PE\[p, 2i\] = sin(p / base^(2i/d)), PE\[p, 2i+1\] = cos(...). The
#' `rotation_check` field is the max error of expressing PE(p + 1) as
#' a fixed rotation of PE(p) -- the property that lets the encoding
#' extrapolate past the training length. It should be ~1e-16.
#'
#' @param pos Position or vector of positions.
#' @param d_model Even width.
#' @param base Frequency base (> 1).
#' @return List with `pe`, `wavelengths`, `rotation_check`, `d_model`,
#'   `estimate`, `n`.
#' @export
morie_geron_positional_encoding <- function(pos, d_model, base = 10000.0) {
  d <- as.integer(d_model)
  .morie_gr_need(d > 0L && d %% 2L == 0L,
                 "geron_positional_encoding: d_model must be a positive even integer")
  b <- as.numeric(base)
  .morie_gr_need(is.finite(b) && b > 1, "geron_positional_encoding: base must be > 1")
  p <- as.numeric(pos)
  scalar <- length(p) == 1L
  .morie_gr_need(length(p) > 0L, "geron_positional_encoding: pos is empty")
  .morie_gr_need(all(is.finite(p)), "geron_positional_encoding: pos contains non-finite values")
  i <- seq.int(0, d %/% 2L - 1L)
  inv <- b^(-2 * i / d)
  ang <- outer(p, inv)
  pe <- matrix(0, length(p), d)
  even <- seq.int(1L, d, by = 2L)   # numpy 0::2 -> R columns 1, 3, 5, ...
  odd <- seq.int(2L, d, by = 2L)    # numpy 1::2 -> R columns 2, 4, 6, ...
  pe[, even] <- sin(ang)
  pe[, odd] <- cos(ang)
  cc <- cos(inv); ss <- sin(inv)
  shifted <- matrix(0, length(p), d)
  shifted[, even] <- pe[, even, drop = FALSE] * rep(cc, each = length(p)) +
    pe[, odd, drop = FALSE] * rep(ss, each = length(p))
  shifted[, odd] <- pe[, odd, drop = FALSE] * rep(cc, each = length(p)) -
    pe[, even, drop = FALSE] * rep(ss, each = length(p))
  ang1 <- outer(p + 1, inv)
  direct <- matrix(0, length(p), d)
  direct[, even] <- sin(ang1)
  direct[, odd] <- cos(ang1)
  rot <- max(abs(shifted - direct))
  out <- if (scalar) as.numeric(pe[1L, ]) else pe
  list(pe = out, wavelengths = 2 * pi / inv, rotation_check = rot,
       d_model = d, estimate = out, n = length(p),
       method = "Sinusoidal positional encoding (Vaswani et al. form)")
}

# --------------------------------------------------- reinforcement learning

#' State value function by exact policy evaluation (Geron Ch 18, morie.fn hmvf)
#'
#' V = (I - gamma P_pi)^-1 r_pi, solved as a linear system. This is the
#' INDEPENDENT route the parity tests check `morie_geron_value_iteration`
#' against: the two must agree to solver tolerance on the same MDP.
#'
#' `pi` is either a length-n_states vector of 0-based action indices or
#' an (n_states, n_actions) stochastic matrix; `s` is 0-based.
#'
#' @param s 0-based query state.
#' @param pi Policy.
#' @param gamma In \[0, 1).
#' @param P (n_states, n_actions, n_states) array.
#' @param R (n_states, n_actions) or (n_states, n_actions, n_states).
#' @return List with `V`, `value`, `r_pi`, `P_pi`, `residual`,
#'   `gamma`, `state`, `estimate`, `n`.
#' @export
morie_geron_value_function <- function(s, pi, gamma, P = NULL, R = NULL) {
  .morie_gr_need(!is.null(P) && !is.null(R),
                 "geron_value_function: both P (transitions) and R (rewards) are required")
  Pt <- P
  .morie_gr_need(length(dim(Pt)) == 3L && dim(Pt)[1L] == dim(Pt)[3L],
                 "geron_value_function: P must have shape (n_states, n_actions, n_states)")
  n_s <- dim(Pt)[1L]; n_a <- dim(Pt)[2L]
  .morie_gr_need(all(abs(apply(Pt, c(1, 2), sum) - 1) < 1e-8),
                 "geron_value_function: every P[s, a, ] must sum to 1")
  Rt <- R
  Rsa <- if (is.matrix(Rt) && nrow(Rt) == n_s && ncol(Rt) == n_a) {
    Rt
  } else if (length(dim(Rt)) == 3L && all(dim(Rt) == c(n_s, n_a, n_s))) {
    apply(Pt * Rt, c(1, 2), sum)
  } else {
    stop("geron_value_function: R has the wrong shape", call. = FALSE)
  }
  g <- as.numeric(gamma)
  .morie_gr_need(g >= 0 && g < 1, "geron_value_function: gamma must lie in [0, 1)")
  Pi <- pi
  if (is.null(dim(Pi))) {
    .morie_gr_need(length(Pi) == n_s,
                   "geron_value_function: deterministic pi has the wrong length")
    act <- as.integer(Pi)
    .morie_gr_need(min(act) >= 0L && max(act) < n_a,
                   "geron_value_function: pi selects an action out of range")
    M <- matrix(0, n_s, n_a)
    M[cbind(seq_len(n_s), act + 1L)] <- 1
    Pi <- M
  }
  Pi <- as.matrix(Pi)
  .morie_gr_need(nrow(Pi) == n_s && ncol(Pi) == n_a,
                 "geron_value_function: pi has the wrong shape")
  .morie_gr_need(all(Pi >= 0) && all(abs(rowSums(Pi) - 1) < 1e-8),
                 "geron_value_function: every pi[s, ] must sum to 1")
  P_pi <- matrix(0, n_s, n_s)
  for (a in seq_len(n_a)) P_pi <- P_pi + Pi[, a] * Pt[, a, ]
  r_pi <- rowSums(Pi * Rsa)
  A <- diag(n_s) - g * P_pi
  .morie_gr_need(abs(det(A)) >= 1e-14,
                 "geron_value_function: (I - gamma*P_pi) is singular; the policy's value is unbounded")
  V <- as.numeric(solve(A, r_pi))
  residual <- max(abs(V - (r_pi + g * as.numeric(P_pi %*% V))))
  idx <- if (is.null(s)) 0L else as.integer(s)
  .morie_gr_need(idx >= 0L && idx < n_s, "geron_value_function: state out of range")
  list(V = V, value = V[idx + 1L], r_pi = r_pi, P_pi = P_pi,
       residual = residual, gamma = g, state = idx,
       estimate = V[idx + 1L], n = n_s,
       method = "Exact policy evaluation V = (I - gamma P_pi)^-1 r_pi")
}

#' TD(0) value update (Geron Ch 18, morie.fn hmtd)
#'
#' Transitions are applied SEQUENTIALLY, so a repeated state sees its
#' own earlier update -- the parity test replays a trajectory
#' step-for-step to pin that down. States are 0-based.
#'
#' @param V Value table.
#' @param s,r,s_next Equal-length transitions.
#' @param alpha In (0, 1\].
#' @param gamma In \[0, 1\].
#' @param terminal Optional logical vector; TRUE drops the bootstrap.
#' @return List with `V`, `td_error`, `target`, `updates`, `alpha`,
#'   `gamma`, `estimate`, `n`.
#' @export
morie_geron_td_learning <- function(V, s, r, s_next, alpha = 0.1,
                                    gamma = 0.9, terminal = NULL) {
  v <- as.numeric(V)
  .morie_gr_need(length(v) > 0L, "geron_td_learning: V is empty")
  .morie_gr_need(all(is.finite(v)), "geron_td_learning: V contains non-finite values")
  sa <- as.integer(s); sn <- as.integer(s_next); rr <- as.numeric(r)
  .morie_gr_need(length(sa) == length(sn) && length(sa) == length(rr),
                 "geron_td_learning: s, r and s_next must be the same length")
  .morie_gr_need(length(sa) > 0L, "geron_td_learning: no transitions supplied")
  .morie_gr_need(min(sa) >= 0L && max(sa) < length(v) &&
                   min(sn) >= 0L && max(sn) < length(v),
                 "geron_td_learning: a state indexes outside the value table")
  a <- as.numeric(alpha); g <- as.numeric(gamma)
  .morie_gr_need(a > 0 && a <= 1, "geron_td_learning: alpha must lie in (0, 1]")
  .morie_gr_need(g >= 0 && g <= 1, "geron_td_learning: gamma must lie in [0, 1]")
  term <- if (is.null(terminal)) rep(FALSE, length(sa)) else as.logical(terminal)
  .morie_gr_need(length(term) == length(sa),
                 "geron_td_learning: terminal flags and transitions disagree")
  errors <- numeric(length(sa)); targets <- numeric(length(sa))
  for (t in seq_along(sa)) {
    boot <- if (term[t]) 0 else g * v[sn[t] + 1L]
    targets[t] <- rr[t] + boot
    errors[t] <- targets[t] - v[sa[t] + 1L]
    v[sa[t] + 1L] <- v[sa[t] + 1L] + a * errors[t]
  }
  list(V = v, td_error = errors, target = targets, updates = length(sa),
       alpha = a, gamma = g, estimate = mean(abs(errors)), n = length(sa),
       method = "TD(0) value update applied sequentially")
}

#' Tabular Q-learning update (Geron Ch 18, morie.fn hmql)
#'
#' Off-policy TD control: the max over next actions is what biases the
#' estimate upward. `s`, `a` and `s_next` are 0-based; `max_next` is
#' NaN on a terminal transition, matching Python.
#'
#' @param Q (states, actions) table.
#' @param s,a 0-based state/action.
#' @param r Reward.
#' @param s_next 0-based next state.
#' @param alpha In (0, 1\].
#' @param gamma In \[0, 1\].
#' @param done Terminal.
#' @return List with `Q`, `td_error`, `target`, `old_value`,
#'   `new_value`, `max_next`, `estimate`, `n`.
#' @export
morie_geron_q_learning <- function(Q, s, a, r, s_next, alpha, gamma,
                                   done = FALSE) {
  T_ <- as.matrix(Q); storage.mode(T_) <- "double"
  ns <- nrow(T_); na <- ncol(T_)
  si <- as.integer(s); ai <- as.integer(a)
  .morie_gr_need(si >= 0L && si < ns, "geron_q_learning: state outside Q")
  .morie_gr_need(ai >= 0L && ai < na, "geron_q_learning: action outside Q")
  al <- as.numeric(alpha); ga <- as.numeric(gamma)
  .morie_gr_need(al > 0 && al <= 1, "geron_q_learning: alpha must lie in (0, 1]")
  .morie_gr_need(ga >= 0 && ga <= 1, "geron_q_learning: gamma must lie in [0, 1]")
  rr <- as.numeric(r)
  .morie_gr_need(is.finite(rr), "geron_q_learning: r must be finite")
  if (isTRUE(done)) {
    target <- rr
    best_next <- NaN
  } else {
    sn <- as.integer(s_next)
    .morie_gr_need(sn >= 0L && sn < ns, "geron_q_learning: next state outside Q")
    best_next <- max(T_[sn + 1L, ])
    target <- rr + ga * best_next
  }
  old <- T_[si + 1L, ai + 1L]
  td <- target - old
  T_[si + 1L, ai + 1L] <- old + al * td
  list(Q = T_, td_error = td, target = target, old_value = old,
       new_value = T_[si + 1L, ai + 1L], max_next = best_next,
       estimate = T_[si + 1L, ai + 1L], n = length(T_),
       method = "Tabular Q-learning update (off-policy TD control)")
}

#' Credit assignment over a trajectory (Geron Ch 18, morie.fn hmcrd)
#'
#' Discounted returns G_t = r_t + gamma `G_{t+1}` computed backwards,
#' accumulating eligibility traces e_t = gamma lambda `e_{t-1}` + 1
#' forwards, and the per-step share of the total return. `normalize`
#' uses the POPULATION sd (numpy default), not R's `sd()`.
#'
#' @param trajectory Numeric rewards, or a list of (state, action,
#'   reward) triples.
#' @param gamma Discount in \[0, 1\].
#' @param lam Trace decay; default gamma.
#' @param normalize Standardise the returns.
#' @return List with `returns`, `raw_returns`, `rewards`,
#'   `eligibility`, `credit`, `discounted_rewards`, `total_return`.
#' @export
morie_geron_credit_assignment <- function(trajectory, gamma = 0.95,
                                          lam = NULL, normalize = FALSE) {
  .morie_gr_need(!is.null(trajectory) && length(trajectory) > 0L,
                 "geron_credit_assignment: trajectory is empty")
  seqt <- trajectory
  rewards <- if (is.list(seqt) && all(vapply(seqt, length, integer(1)) == 3L)) {
    vapply(seqt, function(e) as.numeric(e[[3L]]), numeric(1))
  } else {
    as.numeric(unlist(seqt, use.names = FALSE))
  }
  .morie_gr_need(all(is.finite(rewards)),
                 "geron_credit_assignment: trajectory contains non-finite rewards")
  g <- as.numeric(gamma)
  .morie_gr_need(g >= 0 && g <= 1, "geron_credit_assignment: gamma must lie in [0, 1]")
  l <- if (is.null(lam)) g else as.numeric(lam)
  .morie_gr_need(l >= 0 && l <= 1, "geron_credit_assignment: lam must lie in [0, 1]")
  T_ <- length(rewards)
  ret <- numeric(T_)
  acc <- 0
  for (t in seq.int(T_, 1L)) {
    acc <- rewards[t] + g * acc
    ret[t] <- acc
  }
  elig <- numeric(T_)
  e <- 0
  for (t in seq_len(T_)) {
    e <- g * l * e + 1
    elig[t] <- e
  }
  total <- ret[1L]
  disc <- g^seq.int(0, T_ - 1L) * rewards
  credit <- if (total != 0) disc / total else rep(0, T_)
  out <- ret
  if (isTRUE(normalize)) {
    sd_ <- .morie_gr_psd(ret)
    .morie_gr_need(sd_ != 0,
                   "geron_credit_assignment: returns have zero variance; normalisation is undefined")
    out <- (ret - mean(ret)) / sd_
  }
  list(returns = out, raw_returns = ret, rewards = rewards,
       eligibility = elig, credit = credit, discounted_rewards = disc,
       total_return = total, gamma = g, lam = l, n = T_,
       method = "discounted returns, eligibility traces and per-step credit share")
}
# ------------------------------------------------------------------ trees

.morie_gr2_leaf <- function(y, criterion) {
  if (criterion == "mse") {
    yv <- as.numeric(y)
    return(list(leaf = TRUE, value = mean(yv), n = length(yv),
                impurity = mean((yv - mean(yv))^2)))
  }
  classes <- sort(unique(y))
  counts <- as.integer(table(y)[as.character(classes)])
  p <- counts / length(y)
  imp <- if (criterion == "gini") 1 - sum(p * p) else -sum(p * log2(p))
  proba <- as.list(p)
  names(proba) <- as.character(classes)
  list(leaf = TRUE, value = classes[which.max(counts)], proba = proba,
       n = length(y), impurity = imp)
}

.morie_gr2_best_split <- function(X, y, criterion, min_samples_leaf) {
  best <- NULL
  m <- nrow(X); n <- ncol(X)
  for (k in seq_len(n)) {
    col <- X[, k]
    vals <- sort(unique(col))
    if (length(vals) < 2L) next
    thresholds <- (vals[-length(vals)] + vals[-1L]) / 2
    for (t in thresholds) {
      left <- col <= t
      nl <- sum(left); nr <- m - nl
      if (nl < min_samples_leaf || nr < min_samples_leaf) next
      res <- morie_geron_cart_split_cost(X, y, feature = k - 1L,
                                         threshold = t, criterion = criterion)
      cost <- res$cost
      if (is.null(best) || cost < best$cost - 1e-15) {
        best <- list(cost = cost, feature = k - 1L, threshold = t,
                     impurity_decrease = res$impurity_decrease,
                     n_left = nl, n_right = nr)
      }
    }
  }
  best
}

.morie_gr2_grow <- function(X, y, criterion, max_depth, min_samples_split,
                            min_samples_leaf, min_impurity_decrease, depth,
                            stats) {
  node_imp <- .morie_gr2_leaf(y, criterion)$impurity
  stop_ <- (!is.null(max_depth) && depth >= max_depth) ||
    length(y) < min_samples_split || node_imp <= 0
  if (!stop_) {
    best <- .morie_gr2_best_split(X, y, criterion, min_samples_leaf)
    if (!is.null(best) && !(best$impurity_decrease < min_impurity_decrease)) {
      mask <- X[, best$feature + 1L] <= best$threshold
      stats$splits <- stats$splits + 1L
      left <- .morie_gr2_grow(X[mask, , drop = FALSE], y[mask], criterion,
                              max_depth, min_samples_split, min_samples_leaf,
                              min_impurity_decrease, depth + 1L, stats)
      right <- .morie_gr2_grow(X[!mask, , drop = FALSE], y[!mask], criterion,
                               max_depth, min_samples_split, min_samples_leaf,
                               min_impurity_decrease, depth + 1L, stats)
      return(list(leaf = FALSE, feature = best$feature,
                  threshold = best$threshold, impurity = node_imp,
                  impurity_decrease = best$impurity_decrease,
                  n = length(y), depth = depth, left = left, right = right))
    }
  }
  lf <- .morie_gr2_leaf(y, criterion)
  lf$depth <- depth
  stats$leaves <- stats$leaves + 1L
  stats$max_depth <- max(stats$max_depth, depth)
  lf
}

#' Route rows down a fitted CART tree (morie.fn hmcart::predict_tree)
#'
#' @param tree Tree from `morie_geron_cart_algorithm`.
#' @param X Rows to route.
#' @return Vector of leaf values, one per row.
#' @export
morie_geron_predict_tree <- function(tree, X) {
  X <- if (is.matrix(X)) X else matrix(as.numeric(X), nrow = 1)
  storage.mode(X) <- "double"
  out <- vector("list", nrow(X))
  for (i in seq_len(nrow(X))) {
    node <- tree
    while (!node$leaf) {
      node <- if (X[i, node$feature + 1L] <= node$threshold) node$left else node$right
    }
    out[[i]] <- node$value
  }
  unlist(out, use.names = FALSE)
}

#' CART algorithm (Geron Ch 5, morie.fn hmcart)
#'
#' Greedy binary splits minimising J(k, t) = (m_L/m) G_L + (m_R/m) G_R.
#' The split COST is delegated to `morie_geron_cart_split_cost` exactly
#' as the Python module delegates to grcart, so the impurity arithmetic
#' is never re-derived here. `feature` indices in the returned tree are
#' 0-based, matching Python.
#'
#' This one function serves the whole tree family: hmcdt
#' (`morie_geron_classification_tree`) is the only wrapper ported here,
#' but hmdtr, hmdthv, hmdtst, hmext, hmgbrt, hmrdt and hmrfc all route
#' their growth through this same core in Python.
#'
#' @param X Feature matrix.
#' @param y Labels or targets.
#' @param criterion "gini", "entropy" or "mse".
#' @param max_depth Optional depth cap.
#' @param min_samples_split Minimum
#'   node size to consider a split.
#' @param min_samples_leaf Minimum leaf
#'   size.
#' @param min_impurity_decrease Minimum gain to accept a split.
#' @return List with `tree`, `predictions`, `n_leaves`, `depth`,
#'   `n_splits`, `criterion`, `feature_importances`, `train_accuracy`
#'   or `train_mse`, `estimate`, `n`.
#' @export
morie_geron_cart_algorithm <- function(X, y, criterion = "gini",
                                       max_depth = NULL,
                                       min_samples_split = 2,
                                       min_samples_leaf = 1,
                                       min_impurity_decrease = 0.0) {
  Xa <- if (is.matrix(X)) X else matrix(as.numeric(X), ncol = 1)
  storage.mode(Xa) <- "double"
  .morie_gr_need(length(Xa) > 0L,
                 "geron_cart_algorithm: X must be a non-empty 2-D array")
  .morie_gr_need(all(is.finite(Xa)), "geron_cart_algorithm: X contains non-finite values")
  ya <- as.vector(y)
  .morie_gr_need(length(ya) == nrow(Xa), "geron_cart_algorithm: X rows and y disagree")
  .morie_gr_need(criterion %in% c("gini", "entropy", "mse"),
                 "geron_cart_algorithm: criterion must be 'gini', 'entropy' or 'mse'")
  if (criterion == "mse") {
    ya <- as.numeric(ya)
    .morie_gr_need(all(is.finite(ya)), "geron_cart_algorithm: y contains non-finite values")
  }
  md <- if (is.null(max_depth)) NULL else as.integer(max_depth)
  .morie_gr_need(is.null(md) || md >= 0L,
                 "geron_cart_algorithm: max_depth must be non-negative")
  mss <- as.integer(min_samples_split); msl <- as.integer(min_samples_leaf)
  .morie_gr_need(mss >= 2L, "geron_cart_algorithm: min_samples_split must be >= 2")
  .morie_gr_need(msl >= 1L, "geron_cart_algorithm: min_samples_leaf must be >= 1")
  mid <- as.numeric(min_impurity_decrease)
  .morie_gr_need(mid >= 0, "geron_cart_algorithm: min_impurity_decrease must be non-negative")
  stats <- new.env(parent = emptyenv())
  stats$leaves <- 0L; stats$splits <- 0L; stats$max_depth <- 0L
  tree <- .morie_gr2_grow(Xa, ya, criterion, md, mss, msl, mid, 0L, stats)
  preds <- morie_geron_predict_tree(tree, Xa)
  imp <- rep(0, ncol(Xa))
  walk <- function(node) {
    if (node$leaf) return(invisible(NULL))
    imp[node$feature + 1L] <<- imp[node$feature + 1L] +
      node$n * node$impurity_decrease
    walk(node$left); walk(node$right)
  }
  walk(tree)
  total <- sum(imp)
  importances <- if (total > 0) imp / total else imp
  out <- list(tree = tree, predictions = preds, n_leaves = stats$leaves,
              depth = stats$max_depth, n_splits = stats$splits,
              criterion = criterion, feature_importances = importances,
              n = nrow(Xa),
              method = "greedy CART; split cost delegated to grcart")
  if (criterion == "mse") {
    mse <- mean((as.numeric(preds) - ya)^2)
    out$train_mse <- mse; out$estimate <- mse
  } else {
    acc <- mean(preds == ya)
    out$train_accuracy <- acc; out$estimate <- acc
  }
  out
}

#' Classification tree with leaf-frequency probabilities (Geron Ch 6, morie.fn hmcdt)
#'
#' Growth is delegated to `morie_geron_cart_algorithm`; this adds the
#' per-row class probability vector read off the leaf frequencies, so a
#' pure leaf predicts with probability 1.
#'
#' @param X,y Design and discrete labels.
#' @param criterion "gini" or "entropy".
#' @param max_depth Optional cap.
#' @param min_samples_leaf Minimum leaf size.
#' @return List with `tree`, `predictions`, `probabilities`, `classes`,
#'   `n_leaves`, `depth`, `n_splits`, `train_accuracy`,
#'   `feature_importances`, `estimate`, `n`.
#' @export
morie_geron_classification_tree <- function(X, y, criterion = "gini",
                                            max_depth = NULL,
                                            min_samples_leaf = 1) {
  .morie_gr_need(criterion %in% c("gini", "entropy"),
                 "geron_classification_tree: criterion must be 'gini' or 'entropy'")
  ya <- as.vector(y)
  if (is.numeric(ya) && length(ya) > 0L) {
    .morie_gr_need(all(ya == round(ya)),
                   "geron_classification_tree: y looks continuous; a classification tree needs discrete labels")
  }
  base <- morie_geron_cart_algorithm(X, y, criterion = criterion,
                                     max_depth = max_depth,
                                     min_samples_leaf = min_samples_leaf)
  Xa <- if (is.matrix(X)) X else matrix(as.numeric(X), ncol = 1)
  storage.mode(Xa) <- "double"
  classes <- sort(unique(ya))
  proba <- t(vapply(seq_len(nrow(Xa)), function(i) {
    node <- base$tree
    while (!node$leaf) {
      node <- if (Xa[i, node$feature + 1L] <= node$threshold) node$left else node$right
    }
    p <- node$proba
    vapply(as.character(classes),
           function(cc) if (is.null(p[[cc]])) 0 else as.numeric(p[[cc]]),
           numeric(1))
  }, numeric(length(classes))))
  list(tree = base$tree, predictions = base$predictions,
       probabilities = proba, classes = classes,
       n_leaves = base$n_leaves, depth = base$depth,
       n_splits = base$n_splits, train_accuracy = base$train_accuracy,
       feature_importances = base$feature_importances,
       criterion = criterion, estimate = base$train_accuracy, n = base$n,
       method = "CART classification tree (growth delegated to hmcart) with leaf-frequency probabilities")
}

# ---------------------------------------------------- softmax cross-entropy

.morie_gr2_onehot <- function(Y, m, K) {
  Yarr <- Y
  if (is.null(dim(Yarr)) || (length(dim(Yarr)) == 2L && K != 1L &&
                             (nrow(Yarr) == 1L || ncol(Yarr) == 1L) &&
                             length(Yarr) == m)) {
    idx <- as.vector(Yarr)
    .morie_gr_need(length(idx) == m, "cross-entropy: label count and rows disagree")
    ii <- as.integer(idx)
    .morie_gr_need(all(ii == idx), "cross-entropy: class labels must be whole numbers")
    .morie_gr_need(min(ii) >= 0L && max(ii) < K,
                   "cross-entropy: labels must lie in 0..K-1")
    Yoh <- matrix(0, m, K)
    Yoh[cbind(seq_len(m), ii + 1L)] <- 1
    return(Yoh)
  }
  Yoh <- as.matrix(Yarr); storage.mode(Yoh) <- "double"
  .morie_gr_need(nrow(Yoh) == m && ncol(Yoh) == K,
                 "cross-entropy: Y has the wrong shape")
  .morie_gr_need(all(Yoh >= 0),
                 "cross-entropy: target probabilities must be non-negative")
  .morie_gr_need(all(abs(rowSums(Yoh) - 1) < 1e-8),
                 "cross-entropy: each row of Y must sum to 1")
  Yoh
}

#' Softmax cross-entropy cost (Geron Ch 4, morie.fn hmcec)
#'
#' J = -(1/m) sum_i sum_k y_ik log p_ik, evaluated from shifted logits
#' so the log is never taken of an underflowed probability. `Y` is
#' either a vector of 0-based class labels or an (m, K) matrix of row
#' distributions.
#'
#' @param X (m, d) design.
#' @param Y Labels or (m, K) targets.
#' @param theta (d, K) coefficients.
#' @return List with `cost`, `probabilities`, `log_probabilities`,
#'   `per_sample_cost`, `scores`, `accuracy`, `chance_cost`,
#'   `n_classes`, `estimate`, `n`.
#' @export
morie_geron_cross_entropy_cost <- function(X, Y, theta) {
  Xm <- if (is.matrix(X)) X else matrix(as.numeric(X), nrow = 1)
  storage.mode(Xm) <- "double"
  th <- if (is.matrix(theta)) theta else matrix(as.numeric(theta), nrow = 1)
  storage.mode(th) <- "double"
  .morie_gr_need(length(Xm) > 0L, "geron_cross_entropy_cost: X is empty")
  .morie_gr_need(ncol(Xm) == nrow(th),
                 "geron_cross_entropy_cost: X columns and theta rows disagree")
  .morie_gr_need(all(is.finite(Xm)) && all(is.finite(th)),
                 "geron_cross_entropy_cost: X and theta must be finite")
  m <- nrow(Xm); K <- ncol(th)
  Yoh <- .morie_gr2_onehot(Y, m, K)
  S <- Xm %*% th
  logp <- .morie_gr_log_softmax_rows(S)
  P <- exp(logp)
  per <- -rowSums(Yoh * logp)
  cost <- mean(per)
  acc <- mean((apply(P, 1, which.max) - 1L) == (apply(Yoh, 1, which.max) - 1L))
  list(cost = cost, probabilities = P, log_probabilities = logp,
       per_sample_cost = per, scores = S, accuracy = acc,
       chance_cost = log(K), n_classes = K, estimate = cost, n = m,
       method = "softmax cross-entropy J = -(1/m) sum_i sum_k y_ik log p_ik")
}

#' Softmax cross-entropy gradient (Geron Ch 4, morie.fn hmceg)
#'
#' (1/m) X^T (P - Y). Softmax is over-parameterised, so every ROW of
#' the gradient sums to zero -- `column_sum_max_abs` measures exactly
#' that and the parity test asserts it is ~0.
#'
#' @param X,Y,theta As for `morie_geron_cross_entropy_cost`.
#' @return List with `gradient`, `probabilities`, `cost`, `grad_norm`,
#'   `column_sum_max_abs`, `estimate`, `n`.
#' @export
morie_geron_cross_entropy_gradient <- function(X, Y, theta) {
  fwd <- morie_geron_cross_entropy_cost(X, Y, theta)
  Xa <- if (is.matrix(X)) X else matrix(as.numeric(X), nrow = 1)
  storage.mode(Xa) <- "double"
  P <- fwd$probabilities
  m <- nrow(P); K <- ncol(P)
  Yoh <- .morie_gr2_onehot(Y, m, K)
  G <- (t(Xa) %*% (P - Yoh)) / m
  list(gradient = G, probabilities = P, cost = fwd$cost,
       grad_norm = sqrt(sum(G^2)),
       column_sum_max_abs = max(abs(rowSums(G))),
       estimate = sqrt(sum(G^2)), n = m,
       method = "softmax cross-entropy gradient (1/m) X^T (P - Y); forward pass delegated to hmcec")
}

#' Classification MLP (Geron Ch 10, morie.fn hmclsn)
#'
#' ReLU hidden stack, softmax output, trained by full backpropagation.
#' Weights come from the shared LCG with He scaling sqrt(2/fan_in), one
#' layer after another out of a SINGLE continuing stream, so the init
#' matches Python draw-for-draw. The final loss is delegated to
#' `morie_geron_cross_entropy_cost` on the augmented last layer, as in
#' the Python module.
#'
#' @param X,y Design and labels.
#' @param hidden_sizes Integer vector.
#' @param epochs Training epochs.
#' @param lr Learning rate.
#' @param seed LCG seed.
#' @return List with `weights`, `biases`, `loss_history`, `final_loss`,
#'   `predictions`, `probabilities`, `logits`, `accuracy`, `classes`,
#'   `layer_sizes`, `n_params`, `estimate`, `n`.
#' @export
morie_geron_classification_mlp <- function(X, y, hidden_sizes = c(4),
                                           epochs = 100, lr = 0.1, seed = 0) {
  Xa <- if (is.matrix(X)) X else matrix(as.numeric(X), nrow = 1)
  storage.mode(Xa) <- "double"
  ya <- as.vector(y)
  .morie_gr_need(length(Xa) > 0L, "geron_classification_mlp: X is empty")
  .morie_gr_need(length(ya) == nrow(Xa), "geron_classification_mlp: X rows and y disagree")
  .morie_gr_need(all(is.finite(Xa)), "geron_classification_mlp: X contains non-finite values")
  labels <- sort(unique(ya))
  K <- length(labels)
  .morie_gr_need(K >= 2L, "geron_classification_mlp: need at least 2 classes")
  idx <- match(ya, labels) - 1L
  E <- as.integer(epochs)
  .morie_gr_need(E >= 1L, "geron_classification_mlp: epochs must be >= 1")
  eta <- as.numeric(lr)
  .morie_gr_need(is.finite(eta) && eta >= 0,
                 "geron_classification_mlp: lr must be non-negative and finite")
  hs <- as.integer(hidden_sizes)
  .morie_gr_need(all(hs >= 1L), "geron_classification_mlp: hidden sizes must be >= 1")
  m <- nrow(Xa); n <- ncol(Xa)
  sizes <- c(n, hs, K)
  # mlp_init: one continuing LCG stream across every layer.
  total_draws <- sum(sizes[-length(sizes)] * sizes[-1L])
  stream <- .morie_gr_lcg_u(total_draws, seed)
  Ws <- vector("list", length(sizes) - 1L)
  bs <- vector("list", length(sizes) - 1L)
  pos <- 1L
  for (i in seq_len(length(sizes) - 1L)) {
    fan_in <- sizes[i]; fan_out <- sizes[i + 1L]
    sd_ <- sqrt(2 / fan_in)
    nn <- fan_in * fan_out
    u <- stream[pos:(pos + nn - 1L)]
    pos <- pos + nn
    Ws[[i]] <- matrix((2 * u - 1) * sqrt(3) * sd_, nrow = fan_in,
                      ncol = fan_out, byrow = TRUE)
    bs[[i]] <- rep(0, fan_out)
  }
  Y <- matrix(0, m, K)
  Y[cbind(seq_len(m), idx + 1L)] <- 1
  L <- length(Ws)
  hist <- numeric(E)
  for (ep in seq_len(E)) {
    acts <- list(Xa); zs <- list()
    h <- Xa
    if (L > 1L) for (i in seq_len(L - 1L)) {
      z <- sweep(h %*% Ws[[i]], 2, bs[[i]], "+")
      zs[[i]] <- z
      h <- pmax(z, 0)
      acts[[i + 1L]] <- h
    }
    logits <- sweep(h %*% Ws[[L]], 2, bs[[L]], "+")
    logp <- .morie_gr_log_softmax_rows(logits)
    P <- exp(logp)
    hist[ep] <- mean(-rowSums(Y * logp))
    delta <- (P - Y) / m
    for (i in seq.int(L, 1L)) {
      gW <- t(acts[[i]]) %*% delta
      gb <- colSums(delta)
      if (i > 1L) delta <- (delta %*% t(Ws[[i]])) * (zs[[i - 1L]] > 0)
      Ws[[i]] <- Ws[[i]] - eta * gW
      bs[[i]] <- bs[[i]] - eta * gb
    }
  }
  h <- Xa
  if (L > 1L) for (i in seq_len(L - 1L)) h <- pmax(sweep(h %*% Ws[[i]], 2, bs[[i]], "+"), 0)
  logits <- sweep(h %*% Ws[[L]], 2, bs[[L]], "+")
  final <- morie_geron_cross_entropy_cost(cbind(h, 1), Y,
                                          rbind(Ws[[L]], bs[[L]]))
  P <- final$probabilities
  pred <- labels[apply(P, 1, which.max)]
  n_params <- sum(vapply(Ws, length, integer(1))) + sum(vapply(bs, length, integer(1)))
  list(weights = Ws, biases = bs, loss_history = hist,
       final_loss = final$cost, predictions = pred, probabilities = P,
       logits = logits, accuracy = mean(pred == ya), classes = labels,
       layer_sizes = sizes, n_params = n_params,
       estimate = mean(pred == ya), n = m,
       method = "ReLU MLP trained by backpropagation; loss delegated to hmcec")
}

# --------------------------------------------------- metrics and attention

#' Confusion matrix over arbitrary labels (Geron Ch 3, morie.fn hmcfm)
#'
#' Maps arbitrary labels to 0-based indices and then DELEGATES the
#' counting to `morie_geron_confusion_matrix` (the grcfm core), exactly
#' as the Python module does. Non-integer labels are ordered by their
#' string form, matching `sorted(..., key=str)`.
#'
#' This is the metrics core: hmeaf, hmf1 and hmmlb all read their
#' precision/recall/F1 out of this same routine in Python.
#'
#' @param y_true,y_pred Equal-length label vectors.
#' @param n_classes Optional class count for integer labels.
#' @param labels Optional explicit level order.
#' @return List with `matrix`, `labels`, `accuracy`, `precision`,
#'   `recall`, `f1`, `support`, `predicted_totals`, `macro_f1`,
#'   `n_classes`, `estimate`, `n`.
#' @export
morie_geron_confusion_matrix_labeled <- function(y_true, y_pred,
                                                 n_classes = NULL,
                                                 labels = NULL) {
  yt <- as.vector(y_true); yp <- as.vector(y_pred)
  .morie_gr_need(length(yt) == length(yp),
                 "geron_confusion_matrix: y_true and y_pred lengths disagree")
  .morie_gr_need(length(yt) > 0L, "geron_confusion_matrix: no observations supplied")
  int_like <- is.numeric(yt) && is.numeric(yp) &&
    all(yt == floor(yt)) && all(yp == floor(yp))
  if (!is.null(labels)) {
    lab <- labels
    .morie_gr_need(length(unique(as.character(lab))) == length(lab),
                   "geron_confusion_matrix: labels contains duplicates")
    .morie_gr_need(all(yt %in% lab) && all(yp %in% lab),
                   "geron_confusion_matrix: observed labels missing from `labels`")
    it <- match(yt, lab) - 1L; ip <- match(yp, lab) - 1L
    K <- length(lab)
  } else if (int_like) {
    it <- as.integer(yt); ip <- as.integer(yp)
    .morie_gr_need(min(it) >= 0L && min(ip) >= 0L,
                   "geron_confusion_matrix: integer class indices must be non-negative")
    K <- if (is.null(n_classes)) max(max(it), max(ip)) + 1L else as.integer(n_classes)
    lab <- seq.int(0, K - 1L)
  } else {
    lab <- unique(c(yt, yp))
    lab <- lab[order(as.character(lab), method = "radix")]
    it <- match(yt, lab) - 1L; ip <- match(yp, lab) - 1L
    K <- length(lab)
  }
  base <- morie_geron_confusion_matrix(it, ip, n_classes = K)
  cm <- base$matrix
  list(matrix = cm, labels = lab, accuracy = base$accuracy,
       precision = base$precision, recall = base$recall, f1 = base$f1,
       support = base$support, predicted_totals = as.integer(colSums(cm)),
       macro_f1 = base$macro_f1, n_classes = K,
       estimate = base$accuracy, n = length(yt),
       method = "confusion matrix C[i,j] = count(actual i, predicted j); counting delegated to grcfm")
}

#' Cross-attention with per-query entropy (Geron Ch 15, morie.fn hmcatt)
#'
#' The attention itself is DELEGATED to `morie_geron_cross_attention`
#' (the grca core); this adds the per-query entropy in bits, its
#' log2(T_enc) ceiling and the 0-based argmax. Queries come from the
#' decoder and keys/values from the encoder, so no causal mask is
#' needed.
#'
#' @param dec_h Decoder states (T_dec, d).
#' @param enc_h Encoder states.
#' @param W_Q,W_K,W_V Projection matrices.
#' @param mask Optional mask.
#' @return List with `context`, `output`, `attention_weights`,
#'   `logits`, `scale`, `d_k`, `entropy`, `max_entropy`, `argmax`
#'   (0-based), `estimate`, `n`.
#' @export
morie_geron_cross_attention_report <- function(dec_h, enc_h, W_Q, W_K, W_V,
                                               mask = NULL) {
  base <- morie_geron_cross_attention(dec_h, enc_h, W_Q, W_K, W_V, mask = mask)
  A <- as.matrix(base$attention_weights)
  terms <- ifelse(A > 0, A * log2(ifelse(A > 0, A, 1)), 0)
  ent <- -rowSums(terms)
  ent <- ifelse(ent == 0, 0, ent)
  out <- as.matrix(base$output)
  list(context = out, output = out, attention_weights = A,
       logits = base$logits, scale = base$scale, d_k = base$d_k,
       entropy = ent, max_entropy = log2(ncol(A)),
       argmax = apply(A, 1, which.max) - 1L, estimate = mean(out),
       n = nrow(A),
       method = "scaled dot-product cross-attention (delegated to grca) with per-query entropy")
}

#' CLIP pretraining with zero-shot prompt matching (Geron Ch 16, morie.fn hmclip)
#'
#' The symmetric InfoNCE is DELEGATED to
#' `morie_geron_clip_contrastive_loss` (the grclp core). The batch's
#' off-diagonal pairs are the negatives, so a bigger batch is a harder
#' task and the chance loss log(B) rises with it.
#'
#' @param images,texts Paired embedding matrices of the same shape.
#' @param tau Positive temperature.
#' @param normalize Cosine-normalise.
#' @param class_prompts Optional (C, d) prompt embeddings.
#' @return List with `loss`, `loss_i2t`, `loss_t2i`, `similarity`,
#'   `matched_similarity`, `accuracy_i2t`, `accuracy_t2i`,
#'   `chance_loss`, `zero_shot`, `tau`, `estimate`, `n`.
#' @export
morie_geron_clip <- function(images, texts, tau = 0.07, normalize = TRUE,
                             class_prompts = NULL) {
  I <- .morie_gr_mat(images, "images"); T_ <- .morie_gr_mat(texts, "texts")
  .morie_gr_need(all(dim(I) == dim(T_)),
                 "geron_clip: images and texts must be paired row for row")
  .morie_gr_need(length(I) > 0L, "geron_clip: no embeddings supplied")
  base <- morie_geron_clip_contrastive_loss(I, T_, tau = tau,
                                            normalize = normalize)
  S <- as.matrix(base$similarity)
  zs <- NULL
  if (!is.null(class_prompts)) {
    P <- .morie_gr_mat(class_prompts, "class_prompts")
    .morie_gr_need(ncol(P) == ncol(I),
                   "geron_clip: class_prompts width != embedding width")
    .morie_gr_need(nrow(P) > 0L, "geron_clip: class_prompts is empty")
    In <- I; Pn <- P
    if (isTRUE(normalize)) {
      ni <- sqrt(rowSums(I^2)); np <- sqrt(rowSums(P^2))
      .morie_gr_need(!any(ni == 0) && !any(np == 0),
                     "geron_clip: cannot cosine-normalise a zero embedding")
      In <- I / ni; Pn <- P / np
    }
    sim <- In %*% t(Pn)
    zs <- list(similarity = sim,
               predictions = apply(sim, 1, which.max) - 1L,
               n_classes = nrow(P))
  }
  list(loss = base$loss, loss_i2t = base$loss_i2t,
       loss_t2i = base$loss_t2i, similarity = S,
       matched_similarity = diag(S), accuracy_i2t = base$accuracy_i2t,
       accuracy_t2i = base$accuracy_t2i, chance_loss = base$chance_loss,
       zero_shot = zs, tau = as.numeric(tau), estimate = base$loss,
       n = nrow(I),
       method = "CLIP symmetric InfoNCE (delegated to grclp) plus zero-shot prompt matching")
}

#' Classification + localization head (Geron Ch 14, morie.fn hmclc)
#'
#' The model returns \[class scores..., x, y, w, h\] per image. One head
#' is scored in nats and the other in squared pixels, so `alpha` is
#' what makes the two comparable. Boxes are centre-form and converted
#' to corners for the IoU.
#'
#' @param image Passed straight to `model`.
#' @param model Function of `image` returning a vector or (B, K+4) matrix.
#' @param n_classes Optional K.
#' @param gt_class Optional 0-based labels.
#' @param gt_box Optional ground-truth boxes.
#' @param alpha Loss weight.
#' @return List with `class_probs`, `log_probs`, `predicted_class`
#'   (0-based), `box`, `box_corners`, `iou`, `loss`, `loss_class`,
#'   `loss_box`, `alpha`, `n_classes`, `estimate`, `n`.
#' @export
morie_geron_classification_localization <- function(image, model,
                                                    n_classes = NULL,
                                                    gt_class = NULL,
                                                    gt_box = NULL,
                                                    alpha = 1.0) {
  .morie_gr_need(is.function(model),
                 "geron_classification_localization: model must be a function")
  out <- model(image)
  out <- if (is.matrix(out)) out else matrix(as.numeric(out), nrow = 1)
  storage.mode(out) <- "double"
  .morie_gr_need(length(out) > 0L,
                 "geron_classification_localization: model returned nothing")
  .morie_gr_need(all(is.finite(out)),
                 "geron_classification_localization: model returned non-finite values")
  B <- nrow(out); width <- ncol(out)
  K <- if (is.null(n_classes)) width - 4L else as.integer(n_classes)
  .morie_gr_need(K >= 1L,
                 "geron_classification_localization: output leaves no class scores")
  .morie_gr_need(width == K + 4L,
                 "geron_classification_localization: width and n_classes disagree")
  scores <- out[, seq_len(K), drop = FALSE]
  box <- out[, (K + 1L):width, drop = FALSE]
  .morie_gr_need(all(box[, 3] > 0) && all(box[, 4] > 0),
                 "geron_classification_localization: a box has non-positive width or height")
  logp <- .morie_gr_log_softmax_rows(scores)
  P <- exp(logp)
  corners <- function(b) cbind(b[, 1] - b[, 3] / 2, b[, 2] - b[, 4] / 2,
                               b[, 1] + b[, 3] / 2, b[, 2] + b[, 4] / 2)
  C <- corners(box)
  iou <- NULL; loss_cls <- NULL; loss_box <- NULL; total <- NULL
  if (!is.null(gt_box)) {
    G <- if (is.matrix(gt_box)) gt_box else matrix(as.numeric(gt_box), nrow = 1)
    storage.mode(G) <- "double"
    .morie_gr_need(all(dim(G) == dim(box)),
                   "geron_classification_localization: gt_box has the wrong shape")
    .morie_gr_need(all(G[, 3] > 0) && all(G[, 4] > 0),
                   "geron_classification_localization: a ground-truth box has non-positive width or height")
    GC <- corners(G)
    x1 <- pmax(C[, 1], GC[, 1]); y1 <- pmax(C[, 2], GC[, 2])
    x2 <- pmin(C[, 3], GC[, 3]); y2 <- pmin(C[, 4], GC[, 4])
    inter <- pmax(x2 - x1, 0) * pmax(y2 - y1, 0)
    area_p <- box[, 3] * box[, 4]; area_g <- G[, 3] * G[, 4]
    iou <- inter / (area_p + area_g - inter)
    loss_box <- mean(rowSums((box - G)^2))
  }
  if (!is.null(gt_class)) {
    yv <- as.integer(gt_class)
    .morie_gr_need(length(yv) == B,
                   "geron_classification_localization: gt_class and image count disagree")
    .morie_gr_need(min(yv) >= 0L && max(yv) < K,
                   "geron_classification_localization: a label lies outside 0..K-1")
    loss_cls <- -mean(logp[cbind(seq_len(B), yv + 1L)])
  }
  if (!is.null(loss_cls) || !is.null(loss_box)) {
    total <- (if (is.null(loss_cls)) 0 else loss_cls) +
      as.numeric(alpha) * (if (is.null(loss_box)) 0 else loss_box)
  }
  list(class_probs = P, log_probs = logp,
       predicted_class = apply(P, 1, which.max) - 1L, box = box,
       box_corners = C, iou = iou, loss = total, loss_class = loss_cls,
       loss_box = loss_box, alpha = as.numeric(alpha), n_classes = K,
       estimate = if (is.null(total)) max(P) else total, n = B,
       method = "two-headed classification+localization output with cross-entropy, box MSE and IoU")
}
# ----------------------------------------------------- computational graph

.morie_gr2_BINARY <- c("add", "sub", "mul", "div", "pow")
.morie_gr2_UNARY <- c("neg", "exp", "log", "sin", "cos", "tanh", "sqrt", "square")

# Forward evaluation of the expression DAG. Python memoises on object
# IDENTITY (id(node)); R has no stable object identity for literals, so
# every occurrence becomes its own node here. For expressions written
# out literally -- which is every documented use -- the two agree,
# because distinct Python literals are distinct objects too.
.morie_gr2_forward <- function(node, env, st) {
  if (is.numeric(node) && length(node) == 1L) {
    st$nodes[[length(st$nodes) + 1L]] <-
      list(op = "const", value = as.numeric(node), inputs = integer(0))
    return(length(st$nodes))
  }
  if (is.character(node) && length(node) == 1L) {
    .morie_gr_need(!is.null(env[[node]]),
                   paste0("geron_computational_graph: variable '", node,
                          "' has no value in `values`"))
    st$nodes[[length(st$nodes) + 1L]] <-
      list(op = "var", name = node, value = as.numeric(env[[node]]),
           inputs = integer(0))
    return(length(st$nodes))
  }
  .morie_gr_need(is.list(node) && length(node) >= 1L,
                 "geron_computational_graph: cannot interpret node")
  op <- node[[1L]]
  .morie_gr_need(is.character(op) && length(op) == 1L,
                 "geron_computational_graph: the first element of a node must be an op name")
  args <- vapply(node[-1L], function(a) .morie_gr2_forward(a, env, st), integer(1))
  vals <- vapply(args, function(i) st$nodes[[i]]$value, numeric(1))
  v <- if (op %in% .morie_gr2_BINARY) {
    .morie_gr_need(length(args) == 2L,
                   paste0("geron_computational_graph: op '", op, "' takes 2 inputs"))
    a <- vals[1L]; b <- vals[2L]
    switch(op,
      add = a + b, sub = a - b, mul = a * b,
      div = { .morie_gr_need(b != 0, "geron_computational_graph: division by zero"); a / b },
      pow = {
        .morie_gr_need(!(a <= 0 && b != floor(b)),
                       "geron_computational_graph: pow is not real-differentiable here")
        a^b
      })
  } else if (op %in% .morie_gr2_UNARY) {
    .morie_gr_need(length(args) == 1L,
                   paste0("geron_computational_graph: op '", op, "' takes 1 input"))
    a <- vals[1L]
    switch(op,
      neg = -a, exp = exp(a),
      log = { .morie_gr_need(a > 0, "geron_computational_graph: log requires a positive argument"); log(a) },
      sin = sin(a), cos = cos(a), tanh = tanh(a),
      sqrt = { .morie_gr_need(a > 0, "geron_computational_graph: sqrt is not differentiable at 0"); sqrt(a) },
      square = a * a)
  } else {
    stop(paste0("geron_computational_graph: unknown op '", op, "'"), call. = FALSE)
  }
  st$nodes[[length(st$nodes) + 1L]] <- list(op = op, value = v, inputs = args)
  length(st$nodes)
}

#' Computational graph with reverse-mode differentiation (Geron Ch 12, morie.fn hmcgrf)
#'
#' Builds the expression DAG, evaluates it forwards, then makes ONE
#' reverse adjoint sweep that yields every partial derivative at once
#' -- whatever the number of variables. A central-difference check at
#' h = 1e-5 is returned alongside, so the analytic gradient is verified
#' by an independent route rather than asserted.
#'
#' Expressions are nested lists: `list("mul", "x", list("add", "y", 2))`,
#' with bare strings for variables and bare numbers for constants.
#' Supported ops: add, sub, mul, div, pow, neg, exp, log, sin, cos,
#' tanh, sqrt, square.
#'
#' @param expr Expression tree.
#' @param values Named list of variable values.
#' @return List with `value`, `grad`, `gradient`, `node_grads`,
#'   `nodes`, `n_nodes`, `topo_order`, `fd_check`, `estimate`, `n`.
#' @export
morie_geron_computational_graph <- function(expr, values = NULL) {
  env <- if (is.null(values)) list() else as.list(values)
  st <- new.env(parent = emptyenv())
  st$nodes <- list()
  root <- .morie_gr2_forward(expr, env, st)
  nodes <- st$nodes
  value <- nodes[[root]]$value
  adj <- rep(0, length(nodes))
  adj[root] <- 1
  for (i in seq.int(length(nodes), 1L)) {
    g <- adj[i]
    nd <- nodes[[i]]
    if (g == 0 || length(nd$inputs) == 0L) next
    ins <- nd$inputs
    vals <- vapply(ins, function(j) nodes[[j]]$value, numeric(1))
    op <- nd$op
    if (op == "add") {
      adj[ins[1L]] <- adj[ins[1L]] + g; adj[ins[2L]] <- adj[ins[2L]] + g
    } else if (op == "sub") {
      adj[ins[1L]] <- adj[ins[1L]] + g; adj[ins[2L]] <- adj[ins[2L]] - g
    } else if (op == "mul") {
      adj[ins[1L]] <- adj[ins[1L]] + g * vals[2L]
      adj[ins[2L]] <- adj[ins[2L]] + g * vals[1L]
    } else if (op == "div") {
      adj[ins[1L]] <- adj[ins[1L]] + g / vals[2L]
      adj[ins[2L]] <- adj[ins[2L]] - g * vals[1L] / (vals[2L]^2)
    } else if (op == "pow") {
      a <- vals[1L]; b <- vals[2L]
      adj[ins[1L]] <- adj[ins[1L]] + g * b * a^(b - 1)
      if (a > 0) adj[ins[2L]] <- adj[ins[2L]] + g * (a^b) * log(a)
    } else if (op == "neg") {
      adj[ins[1L]] <- adj[ins[1L]] - g
    } else if (op == "exp") {
      adj[ins[1L]] <- adj[ins[1L]] + g * nd$value
    } else if (op == "log") {
      adj[ins[1L]] <- adj[ins[1L]] + g / vals[1L]
    } else if (op == "sin") {
      adj[ins[1L]] <- adj[ins[1L]] + g * cos(vals[1L])
    } else if (op == "cos") {
      adj[ins[1L]] <- adj[ins[1L]] - g * sin(vals[1L])
    } else if (op == "tanh") {
      adj[ins[1L]] <- adj[ins[1L]] + g * (1 - nd$value^2)
    } else if (op == "sqrt") {
      adj[ins[1L]] <- adj[ins[1L]] + g / (2 * nd$value)
    } else if (op == "square") {
      adj[ins[1L]] <- adj[ins[1L]] + g * 2 * vals[1L]
    }
  }
  grad <- list()
  for (i in seq_along(nodes)) {
    nd <- nodes[[i]]
    if (identical(nd$op, "var")) {
      prev <- if (is.null(grad[[nd$name]])) 0 else grad[[nd$name]]
      grad[[nd$name]] <- prev + adj[i]
    }
  }
  h <- 1e-5
  fd <- list()
  for (nm in names(grad)) {
    up <- env; dn <- env
    up[[nm]] <- env[[nm]] + h
    dn[[nm]] <- env[[nm]] - h
    su <- new.env(parent = emptyenv()); su$nodes <- list()
    sd <- new.env(parent = emptyenv()); sd$nodes <- list()
    # `su$nodes[[f(...)]]` would extract the (still empty) list before
    # calling f, so the root index is taken first.
    ru <- .morie_gr2_forward(expr, up, su)
    rd <- .morie_gr2_forward(expr, dn, sd)
    vu <- su$nodes[[ru]]$value
    vd <- sd$nodes[[rd]]$value
    fd[[nm]] <- (vu - vd) / (2 * h)
  }
  list(value = value, grad = grad, gradient = grad, node_grads = adj,
       nodes = nodes, n_nodes = length(nodes),
       topo_order = seq.int(0, length(nodes) - 1L), fd_check = fd,
       estimate = value, n = length(nodes),
       method = "expression DAG with forward evaluation and reverse-mode adjoint sweep")
}

# --------------------------------------------------------- character RNN

#' Character-level RNN language model (Geron Ch 16, morie.fn hmchrn)
#'
#' A vanilla RNN trained with FULL backpropagation through time on the
#' single sequence `text`. The vocabulary is the sorted unique
#' characters in codepoint order (`method = "radix"`, matching Python's
#' `sorted(..., key=str)`; R's locale collation would NOT match).
#' Weights come from one continuing LCG stream: Wxh (V x H) then Whh
#' (H x H), each reshaped row-major.
#'
#' A uniform model scores log V, so `final_loss` below that is
#' structure the RNN has captured.
#'
#' @param text Character scalar of at least 2 characters.
#' @param hidden Hidden width.
#' @param epochs Training epochs.
#' @param lr Learning rate.
#' @param seed LCG seed.
#' @param generate Number of characters to sample greedily.
#' @return List with `loss_history`, `final_loss`, `chance_loss`,
#'   `perplexity`, `vocab`, `vocab_size`, `weights`, `hidden`,
#'   `generated`, `estimate`, `n`.
#' @export
morie_geron_char_rnn <- function(text, hidden = 8, epochs = 50, lr = 0.1,
                                 seed = 0, generate = 0) {
  chars <- strsplit(as.character(text), "", fixed = TRUE)[[1L]]
  .morie_gr_need(length(chars) >= 2L,
                 "geron_char_rnn: text needs at least 2 characters")
  vocab <- sort(unique(chars), method = "radix")
  V <- length(vocab)
  .morie_gr_need(V >= 2L,
                 "geron_char_rnn: text uses a single symbol; a language model over it is trivial")
  seqi <- match(chars, vocab) - 1L
  H <- as.integer(hidden)
  .morie_gr_need(H >= 1L, "geron_char_rnn: hidden must be >= 1")
  E <- as.integer(epochs)
  .morie_gr_need(E >= 1L, "geron_char_rnn: epochs must be >= 1")
  eta <- as.numeric(lr)
  .morie_gr_need(eta >= 0, "geron_char_rnn: lr must be non-negative")
  G <- as.integer(generate)
  .morie_gr_need(G >= 0L, "geron_char_rnn: generate must be non-negative")
  stream <- .morie_gr_lcg_u(V * H + H * H, seed)
  Wxh <- matrix((2 * stream[seq_len(V * H)] - 1) * sqrt(3) * (1 / sqrt(V)),
                nrow = V, ncol = H, byrow = TRUE)
  Whh <- matrix((2 * stream[(V * H + 1L):(V * H + H * H)] - 1) * sqrt(3) * (1 / sqrt(H)),
                nrow = H, ncol = H, byrow = TRUE)
  bh <- rep(0, H)
  Why <- matrix(0, H, V)
  by <- rep(0, V)
  T_ <- length(seqi) - 1L
  hist <- numeric(E)
  for (ep in seq_len(E)) {
    hs <- vector("list", T_ + 1L)
    hs[[1L]] <- rep(0, H)
    ps <- vector("list", T_)
    loss <- 0
    for (t in seq_len(T_)) {
      h <- tanh(Wxh[seqi[t] + 1L, ] + as.numeric(hs[[t]] %*% Whh) + bh)
      hs[[t + 1L]] <- h
      z <- as.numeric(h %*% Why) + by
      z <- z - max(z)
      p <- exp(z); p <- p / sum(p)
      ps[[t]] <- p
      loss <- loss - log(p[seqi[t + 1L] + 1L])
    }
    hist[ep] <- loss / T_
    dWxh <- matrix(0, V, H); dWhh <- matrix(0, H, H); dbh <- rep(0, H)
    dWhy <- matrix(0, H, V); dby <- rep(0, V)
    dh_next <- rep(0, H)
    for (t in seq.int(T_, 1L)) {
      dz <- ps[[t]]
      dz[seqi[t + 1L] + 1L] <- dz[seqi[t + 1L] + 1L] - 1
      dz <- dz / T_
      dWhy <- dWhy + outer(hs[[t + 1L]], dz)
      dby <- dby + dz
      dh <- as.numeric(Why %*% dz) + dh_next
      dr <- (1 - hs[[t + 1L]]^2) * dh
      dWxh[seqi[t] + 1L, ] <- dWxh[seqi[t] + 1L, ] + dr
      dWhh <- dWhh + outer(hs[[t]], dr)
      dbh <- dbh + dr
      dh_next <- as.numeric(Whh %*% dr)
    }
    Wxh <- Wxh - eta * dWxh; Whh <- Whh - eta * dWhh; bh <- bh - eta * dbh
    Why <- Why - eta * dWhy; by <- by - eta * dby
  }
  h <- rep(0, H)
  loss <- 0
  for (t in seq_len(T_)) {
    h <- tanh(Wxh[seqi[t] + 1L, ] + as.numeric(h %*% Whh) + bh)
    z <- as.numeric(h %*% Why) + by
    z <- z - max(z)
    p <- exp(z); p <- p / sum(p)
    loss <- loss - log(p[seqi[t + 1L] + 1L])
  }
  final <- loss / T_
  gen <- ""
  if (G > 0L) {
    gh <- rep(0, H)
    cur <- seqi[1L]
    for (t in seq.int(0, T_)) {
      gh <- tanh(Wxh[cur + 1L, ] + as.numeric(gh %*% Whh) + bh)
      cur <- if (t < T_) seqi[t + 2L] else which.max(as.numeric(gh %*% Why) + by) - 1L
    }
    for (k in seq_len(G)) {
      gen <- paste0(gen, vocab[cur + 1L])
      gh <- tanh(Wxh[cur + 1L, ] + as.numeric(gh %*% Whh) + bh)
      cur <- which.max(as.numeric(gh %*% Why) + by) - 1L
    }
  }
  list(loss_history = hist, final_loss = final, chance_loss = log(V),
       perplexity = exp(final), vocab = vocab, vocab_size = V,
       weights = list(Wxh = Wxh, Whh = Whh, bh = bh, Why = Why, by = by),
       hidden = H, generated = gen, estimate = final, n = T_,
       method = "vanilla char-RNN trained with full backpropagation through time")
}

# ------------------------------------------------- convolutional autoencoder

#' Trained patch convolutional autoencoder (Geron Ch 18, morie.fn hmcae)
#'
#' Splits every image into non-overlapping p x p patches (row-major
#' over patch rows then patch columns, each patch flattened row-major),
#' normalises by the RMS pixel value, and trains a linear
#' encoder/decoder pair by gradient descent on the reconstruction MSE.
#' Weights come from one continuing LCG stream: W_enc (p^2 x F) then
#' V_dec (F x p^2), both reshaped row-major.
#'
#' A bottleneck narrower than the patch cannot reach zero loss; the
#' residual IS the compression cost, which the parity test checks by
#' comparing `compression_ratio` against `final_loss`.
#'
#' `grcae_check` re-runs the single forward pass of
#' `morie_geron_convolutional_autoencoder` on the first image, exactly
#' as the Python module cross-checks itself against grcae.
#'
#' @param X Image (H, W) or a list of images.
#' @param filters Bottleneck width.
#' @param epochs Training epochs.
#' @param lr Learning rate.
#' @param seed LCG seed.
#' @param patch Patch size.
#' @return List with `loss_history`, `final_loss`, `encoder`, `decoder`,
#'   `codes` (list of images, each a list of F patch-grid matrices),
#'   `code_shape`, `reconstruction` (list of images),
#'   `compression_ratio`, `n_patches`, `patch`, `grcae_check`,
#'   `estimate`, `n`.
#' @export
morie_geron_conv_autoencoder_trained <- function(X, filters = 2, epochs = 100,
                                                 lr = 0.05, seed = 0,
                                                 patch = 2) {
  imgs <- if (is.list(X)) lapply(X, as.matrix) else list(as.matrix(X))
  imgs <- lapply(imgs, function(A) { storage.mode(A) <- "double"; A })
  .morie_gr_need(length(imgs) > 0L && length(imgs[[1L]]) > 0L,
                 "geron_convolutional_autoencoder: X must be (H, W) or a list of images")
  .morie_gr_need(all(vapply(imgs, function(A) all(is.finite(A)), logical(1))),
                 "geron_convolutional_autoencoder: X contains non-finite values")
  P <- as.integer(patch)
  .morie_gr_need(P >= 1L, "geron_convolutional_autoencoder: patch must be >= 1")
  m <- length(imgs); H <- nrow(imgs[[1L]]); Wd <- ncol(imgs[[1L]])
  .morie_gr_need(all(vapply(imgs, function(A) nrow(A) == H && ncol(A) == Wd, logical(1))),
                 "geron_convolutional_autoencoder: images must share a shape")
  .morie_gr_need(H %% P == 0L && Wd %% P == 0L,
                 "geron_convolutional_autoencoder: image is not divisible by patch; crop or pad it first")
  F_ <- as.integer(filters)
  .morie_gr_need(F_ >= 1L, "geron_convolutional_autoencoder: filters must be >= 1")
  E <- as.integer(epochs)
  .morie_gr_need(E >= 1L, "geron_convolutional_autoencoder: epochs must be >= 1")
  eta <- as.numeric(lr)
  .morie_gr_need(is.finite(eta) && eta > 0,
                 "geron_convolutional_autoencoder: lr must be positive and finite")
  ph <- H %/% P; pw <- Wd %/% P
  N <- m * ph * pw
  patches <- matrix(0, N, P * P)
  row <- 1L
  for (k in seq_len(m)) for (i in seq_len(ph)) for (j in seq_len(pw)) {
    blk <- imgs[[k]][((i - 1L) * P + 1L):(i * P), ((j - 1L) * P + 1L):(j * P), drop = FALSE]
    patches[row, ] <- as.numeric(t(blk))   # row-major flatten
    row <- row + 1L
  }
  scale <- sqrt(mean(patches^2))
  if (scale == 0) scale <- 1
  patches_n <- patches / scale
  n1 <- P * P * F_
  stream <- .morie_gr_lcg_u(2L * n1, seed)
  Wenc <- matrix((2 * stream[seq_len(n1)] - 1) * sqrt(3) * (1 / sqrt(P * P)),
                 nrow = P * P, ncol = F_, byrow = TRUE)
  Vdec <- matrix((2 * stream[(n1 + 1L):(2L * n1)] - 1) * sqrt(3) * (1 / sqrt(F_)),
                 nrow = F_, ncol = P * P, byrow = TRUE)
  hist <- numeric(E)
  for (ep in seq_len(E)) {
    code <- patches_n %*% Wenc
    recon <- code %*% Vdec
    diff <- recon - patches_n
    hist[ep] <- mean(diff^2) * scale^2
    gV <- (2 / (N * P * P)) * (t(code) %*% diff)
    gW <- (2 / (N * P * P)) * (t(patches_n) %*% (diff %*% t(Vdec)))
    Wenc <- Wenc - eta * gW
    Vdec <- Vdec - eta * gV
    .morie_gr_need(all(is.finite(Wenc)) && all(is.finite(Vdec)),
                   "geron_convolutional_autoencoder: training diverged; lower lr")
  }
  code <- (patches_n %*% Wenc) * scale
  recon <- (patches_n %*% Wenc) %*% Vdec * scale
  final <- mean((recon - patches)^2)
  img <- vector("list", m)
  codes <- vector("list", m)
  row <- 1L
  for (k in seq_len(m)) {
    out <- matrix(0, H, Wd)
    cgrid <- lapply(seq_len(F_), function(f) matrix(0, ph, pw))
    for (i in seq_len(ph)) for (j in seq_len(pw)) {
      out[((i - 1L) * P + 1L):(i * P), ((j - 1L) * P + 1L):(j * P)] <-
        t(matrix(recon[row, ], nrow = P, ncol = P))
      for (f in seq_len(F_)) cgrid[[f]][i, j] <- code[row, f]
      row <- row + 1L
    }
    img[[k]] <- out
    codes[[k]] <- cgrid
  }
  check <- morie_geron_convolutional_autoencoder(
    imgs[[1L]], list(matrix(1, 1, 1)), list(matrix(1, P, P)), stride = P)
  list(loss_history = hist, final_loss = final, encoder = Wenc,
       decoder = Vdec, codes = codes, code_shape = c(F_, ph, pw),
       reconstruction = img, compression_ratio = P * P / F_,
       n_patches = N, patch = P,
       grcae_check = list(loss = check$loss, code_shape = check$code_shape),
       estimate = final, n = m,
       method = "stride-p convolutional autoencoder trained by gradient descent on reconstruction MSE")
}
