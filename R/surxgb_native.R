# morie.fn -- function file (rootcoder007/morie)
# Accelerated failure time survival regression by gradient boosting.
#
# XGBoost's second-order boosting supplies the machinery and the AFT
# model supplies the loss that knows about censoring.
#
# The machinery: with g_i, h_i the first and second derivatives of the
# loss at the current prediction, the optimal leaf weight is
# w* = -sum g / (sum h + lambda), and a candidate split is scored by
# the loss reduction
#   L_split = 1/2 [ GL^2/(HL+lam) + GR^2/(HR+lam)
#                   - (GL+GR)^2/(HL+HR+lam) ] - gamma.
# gamma is the price of one more leaf, so a split whose gain does not
# clear it is not taken -- the pruning is part of the score.
#
# The loss: write ln y = T(x) + sigma Z. With s(y) = (ln y - T(x))/sigma,
#   l_AFT = -ln[f_Z(s(y)) / (sigma y)]           (observed)
#         = -ln[F_Z(s(ybar)) - F_Z(s(yunder))]   (censored interval)
# One expression covers uncensored, right-, left- and interval-
# censored labels. Three distributions for Z are offered (normal,
# logistic, extreme) and they are not interchangeable in the tails.
#
# Numerical guard: s is clamped and the censored-interval probability
# floored, so the exponentials in the logistic and extreme densities
# do not overflow.
#
# References
# ----------
# Chen, T. & Guestrin, C. (2016) "XGBoost: A Scalable Tree Boosting
# System", KDD '16, 785-794, doi:10.1145/2939672.2939785. Secs. 2.1
# and 2.2, eqs (5)-(7).
#
# Barnwal, A., Cho, H. & Hocking, T. (2022) "Survival Regression with
# Accelerated Failure Time Model in XGBoost", Journal of Computational
# and Graphical Statistics 31(4), 1292-1302,
# doi:10.1080/10618600.2022.2067548 (arXiv:2006.04920). Table 1,
# Definitions 1-2, Table 2, Sec. 4.
#
# Ishwaran, H., Kogalur, U. B., Blackstone, E. H. & Lauer, M. S.
# (2008) "Random Survival Forests", Annals of Applied Statistics 2(3),
# 841-860, doi:10.1214/08-AOAS169, Sec. 5.1, for the concordance index
# reused here from survrsf.

morie_surxgb_DISTRIBUTIONS <- c("normal", "logistic", "extreme")
.surxgb_CLAMP <- 30.0
.surxgb_FLOOR <- 1e-16
.surxgb_SQRT2PI <- sqrt(2.0 * pi)

#' .surxgb_check_dist
#'
#' A step of the surxgb_native implementation. Called by \code{morie_surxgb_aft_loss},
#' \code{morie_surxgb_boost}, \code{morie_surxgb_cdf} and 3 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param dist Passed to \code{\%in\%}.
#' @return One of two values, depending on the branch taken.
#' @export
.surxgb_check_dist <- function(dist) {
  if (!(dist %in% morie_surxgb_DISTRIBUTIONS)) {
    stop(sprintf(
      "surxgb: distribution must be one of %s, got %s",
      paste(morie_surxgb_DISTRIBUTIONS, collapse = ", "), dist
    ))
  }
}

#' Table 2: the density of Z
#'
#' A step of the surxgb_native implementation. Called by
#' \code{morie_surxgb_aft_gradient_hessian}, \code{morie_surxgb_aft_loss},
#' \code{morie_surxgb_ddpdf} and 1 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param z Numeric; passed to \code{abs}.
#' @param dist One of \code{"logistic"}, \code{"normal"}. Defaults to \code{"normal"}.
#' @return One of two values, depending on the branch taken.
#' @export
morie_surxgb_pdf <- function(z, dist = "normal") {
  # Table 2: the density of Z.
  .surxgb_check_dist(dist)
  z <- max(-.surxgb_CLAMP, min(.surxgb_CLAMP, as.numeric(z)))
  if (dist == "normal") {
    return(exp(-z * z / 2.0) / .surxgb_SQRT2PI)
  }
  if (dist == "logistic") {
    e <- exp(-abs(z))
    return(e / ((1.0 + e)^2))
  }
  if (z < .surxgb_CLAMP) exp(z - exp(z)) else 0.0
}

#' Table 2: the distribution function of Z
#'
#' A step of the surxgb_native implementation. Called by
#' \code{morie_surxgb_aft_gradient_hessian}, \code{morie_surxgb_aft_loss},
#' \code{morie_surxgb_ddpdf} and 1 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param z Numeric; passed to \code{exp}.
#' @param dist One of \code{"logistic"}, \code{"normal"}. Defaults to \code{"normal"}.
#' @return A numeric value.
#' @export
morie_surxgb_cdf <- function(z, dist = "normal") {
  # Table 2: the distribution function of Z.
  .surxgb_check_dist(dist)
  zf <- as.numeric(z)
  if (is.infinite(zf) && zf > 0) {
    return(1.0)
  }
  if (is.infinite(zf) && zf < 0) {
    return(0.0)
  }
  z <- max(-.surxgb_CLAMP, min(.surxgb_CLAMP, zf))
  if (dist == "normal") {
    return(stats::pnorm(z))
  }
  if (dist == "logistic") {
    return(1.0 / (1.0 + exp(-z)))
  }
  1.0 - exp(-exp(z))
}

#' Table 2: f_Z\'(z)
#'
#' A step of the surxgb_native implementation. Called by \code{morie_surxgb_aft_gradient_hessian}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param z Passed to \code{morie_surxgb_pdf}.
#' @param dist One of \code{"logistic"}, \code{"normal"}. Defaults to \code{"normal"}.
#' @return A numeric value.
#' @export
morie_surxgb_dpdf <- function(z, dist = "normal") {
  # Table 2: f_Z'(z).
  .surxgb_check_dist(dist)
  f <- morie_surxgb_pdf(z, dist)
  zc <- max(-.surxgb_CLAMP, min(.surxgb_CLAMP, as.numeric(z)))
  if (dist == "normal") {
    return(-zc * f)
  }
  if (dist == "logistic") {
    return(f * (1.0 - 2.0 * morie_surxgb_cdf(zc, dist)))
  }
  f * (1.0 - exp(zc))
}

#' Table 2: f_Z\'\'(z)
#'
#' A step of the surxgb_native implementation. Called by \code{morie_surxgb_aft_gradient_hessian}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param z Passed to \code{morie_surxgb_pdf}.
#' @param dist One of \code{"logistic"}, \code{"normal"}. Defaults to \code{"normal"}.
#' @return A numeric value.
#' @export
morie_surxgb_ddpdf <- function(z, dist = "normal") {
  # Table 2: f_Z''(z).
  .surxgb_check_dist(dist)
  f <- morie_surxgb_pdf(z, dist)
  zc <- max(-.surxgb_CLAMP, min(.surxgb_CLAMP, as.numeric(z)))
  if (dist == "normal") {
    return((zc * zc - 1.0) * f)
  }
  if (dist == "logistic") {
    F <- morie_surxgb_cdf(zc, dist)
    return(f * ((1.0 - 2.0 * F)^2 - 2.0 * f))
  }
  e <- exp(zc)
  f * ((1.0 - e)^2 - e)
}

#' .surxgb_s
#'
#' A step of the surxgb_native implementation. Called by
#' \code{morie_surxgb_aft_gradient_hessian}, \code{morie_surxgb_aft_loss}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y Numeric; passed to \code{log}.
#' @param u Numeric; combined arithmetically in the body.
#' @param sigma Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.surxgb_s <- function(y, u, sigma) {
  if (is.infinite(y) && y > 0) {
    return(Inf)
  }
  if (y <= 0.0) {
    return(-Inf)
  }
  (log(y) - u) / sigma
}

#' morie_surxgb_aft_loss
#'
#' A step of the surxgb_native implementation. Called by
#' \code{morie_surxgb_aft_gradient_hessian}, \code{morie_surxgb_boost}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y_lower Coerced to numeric by the body, with \code{as.numeric}.
#' @param y_upper Coerced to numeric by the body, with \code{as.numeric}.
#' @param u Passed to \code{.surxgb_s}.
#' @param sigma Numeric; combined arithmetically in the body. Defaults to \code{1}.
#' @param dist Passed to \code{.surxgb_check_dist}. Defaults to \code{"normal"}.
#' @return A numeric value.
#' @export
morie_surxgb_aft_loss <- function(y_lower, y_upper, u, sigma = 1.0,
                                  dist = "normal") {
  # Definition 2, covering all four label types of Table 1.
  # y_lower == y_upper is an observed event; y_upper = Inf is
  # right-censored; y_lower = 0 is left-censored; otherwise interval.
  .surxgb_check_dist(dist)
  if (sigma <= 0.0) {
    stop(sprintf("surxgb: sigma must be positive, got %g", sigma))
  }
  lo <- as.numeric(y_lower)
  hi <- as.numeric(y_upper)
  if (hi < lo) {
    stop(sprintf(
      "surxgb: the upper bound %g is below the lower bound %g",
      hi, lo
    ))
  }
  if (lo < 0.0) {
    stop("surxgb: a survival time cannot be negative")
  }
  if (lo == hi) {
    if (lo <= 0.0) {
      stop("surxgb: an observed event needs a positive time")
    }
    d <- morie_surxgb_pdf(.surxgb_s(lo, u, sigma), dist)
    return(-log(max(d, .surxgb_FLOOR) / (sigma * lo)))
  }
  p <- (morie_surxgb_cdf(.surxgb_s(hi, u, sigma), dist) -
    morie_surxgb_cdf(.surxgb_s(lo, u, sigma), dist))
  -log(max(p, .surxgb_FLOOR))
}

#' morie_surxgb_aft_gradient_hessian
#'
#' A step of the surxgb_native implementation. Called by \code{morie_surxgb_boost}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y_lower Passed to \code{morie_surxgb_aft_loss}.
#' @param y_upper Passed to \code{morie_surxgb_aft_loss}.
#' @param u Numeric; combined arithmetically in the body.
#' @param sigma Numeric; combined arithmetically in the body. Defaults to \code{1}.
#' @param dist Passed to \code{morie_surxgb_aft_loss}. Defaults to \code{"normal"}.
#' @param method One of \code{"analytic"}, \code{"numeric"}. Defaults to \code{"analytic"}.
#' @param eps Numeric; combined arithmetically in the body. Defaults to \code{1e-05}.
#' @return A list with \code{gradient}, \code{hessian}, \code{loss},
#' \code{hessian_floored}, \code{derivative_method}.
#' @export
morie_surxgb_aft_gradient_hessian <- function(y_lower, y_upper, u,
                                              sigma = 1.0, dist = "normal",
                                              method = "analytic",
                                              eps = 1e-5) {
  # Gradient and hessian of the loss in u. "analytic" differentiates
  # Definition 2 in closed form; "numeric" central-differences the
  # loss. The hessian is floored at a small positive value.
  if (!(method %in% c("analytic", "numeric"))) {
    stop(sprintf(
      "surxgb: method must be 'analytic' or 'numeric', got %s",
      method
    ))
  }
  f0 <- morie_surxgb_aft_loss(y_lower, y_upper, u, sigma, dist)
  if (method == "numeric") {
    fp <- morie_surxgb_aft_loss(y_lower, y_upper, u + eps, sigma, dist)
    fm <- morie_surxgb_aft_loss(y_lower, y_upper, u - eps, sigma, dist)
    g <- (fp - fm) / (2.0 * eps)
    h <- (fp - 2.0 * f0 + fm) / (eps * eps)
  } else {
    lo <- as.numeric(y_lower)
    hi <- as.numeric(y_upper)
    if (lo == hi) {
      sv <- .surxgb_s(lo, u, sigma)
      f <- max(morie_surxgb_pdf(sv, dist), .surxgb_FLOOR)
      fp_ <- morie_surxgb_dpdf(sv, dist)
      fpp <- morie_surxgb_ddpdf(sv, dist)
      g <- fp_ / (sigma * f)
      h <- (fp_ * fp_ - fpp * f) / (sigma * sigma * f * f)
    } else {
      s_hi <- .surxgb_s(hi, u, sigma)
      s_lo <- .surxgb_s(lo, u, sigma)
      f_hi <- if (!is.infinite(s_hi)) morie_surxgb_pdf(s_hi, dist) else 0.0
      f_lo <- if (!is.infinite(s_lo)) morie_surxgb_pdf(s_lo, dist) else 0.0
      d_hi <- if (!is.infinite(s_hi)) morie_surxgb_dpdf(s_hi, dist) else 0.0
      d_lo <- if (!is.infinite(s_lo)) morie_surxgb_dpdf(s_lo, dist) else 0.0
      D <- max(
        morie_surxgb_cdf(s_hi, dist) - morie_surxgb_cdf(s_lo, dist),
        .surxgb_FLOOR
      )
      A <- f_hi - f_lo
      g <- A / (sigma * D)
      h <- (A * A - (d_hi - d_lo) * D) / (sigma * sigma * D * D)
    }
  }
  list(
    gradient = g, hessian = if (h > 1e-8) h else 1e-8, loss = f0,
    hessian_floored = (h <= 1e-8), derivative_method = method
  )
}

#' Equation (5): w* = -G/(H+lambda)
#'
#' A step of the surxgb_native implementation. Called by \code{.surxgb_build}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param G Coerced to numeric by the body, with \code{as.numeric}.
#' @param H Numeric; combined arithmetically in the body.
#' @param lam Numeric; combined arithmetically in the body. Defaults to \code{1}.
#' @return A numeric value.
#' @export
morie_surxgb_leaf_weight <- function(G, H, lam = 1.0) {
  # Equation (5): w* = -G/(H+lambda).
  if (H + lam <= 0.0) {
    stop("surxgb: H + lambda must be positive")
  }
  -as.numeric(G) / (as.numeric(H) + as.numeric(lam))
}

#' Equation (7): the loss reduction, net of the leaf price
#'
#' A step of the surxgb_native implementation. Called by \code{.surxgb_build}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param GL Numeric; combined arithmetically in the body.
#' @param HL Numeric; combined arithmetically in the body.
#' @param GR Numeric; combined arithmetically in the body.
#' @param HR Numeric; combined arithmetically in the body.
#' @param lam Numeric; combined arithmetically in the body. Defaults to \code{1}.
#' @param gamma Numeric; combined arithmetically in the body. Defaults to \code{0}.
#' @return A numeric value.
#' @export
morie_surxgb_split_gain <- function(GL, HL, GR, HR, lam = 1.0, gamma = 0.0) {
  # Equation (7): the loss reduction, net of the leaf price.
  term <- function(g, h) g * g / (h + lam)
  0.5 * (term(GL, HL) + term(GR, HR) - term(GL + GR, HL + HR)) - gamma
}

#' .surxgb_build
#'
#' A step of the surxgb_native implementation. Called by \code{morie_surxgb_boost}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A matrix; indexed by row and column.
#' @param g A vector; indexed elementwise.
#' @param h A vector; indexed elementwise.
#' @param idx A vector; its length is taken and its elements indexed.
#' @param depth Numeric; combined arithmetically in the body.
#' @param max_depth Passed to \code{.surxgb_build}.
#' @param lam Passed to \code{morie_surxgb_leaf_weight}.
#' @param gamma Passed to \code{morie_surxgb_split_gain}.
#' @param min_child Numeric; combined arithmetically in the body.
#' @return A list with \code{leaf}, \code{variable}, \code{cut}, \code{gain},
#' \code{left}, \code{right}.
#' @export
.surxgb_build <- function(X, g, h, idx, depth, max_depth, lam, gamma,
                          min_child) {
  G <- sum(g[idx])
  H <- sum(h[idx])
  leaf <- list(
    leaf = TRUE, weight = morie_surxgb_leaf_weight(G, H, lam),
    n = length(idx)
  )
  if (depth >= max_depth || length(idx) < 2L * min_child) {
    return(leaf)
  }
  best <- NULL
  for (j in seq_len(ncol(X))) {
    order_ <- idx[order(X[idx, j])]
    GL <- 0.0
    HL <- 0.0
    no <- length(order_)
    for (k in seq_len(no - 1L)) {
      i <- order_[k]
      GL <- GL + g[i]
      HL <- HL + h[i]
      if (X[order_[k], j] == X[order_[k + 1L], j]) {
        next
      }
      if (k < min_child || (no - k) < min_child) {
        next
      }
      gain <- morie_surxgb_split_gain(GL, HL, G - GL, H - HL, lam, gamma)
      if (gain > 0.0 && (is.null(best) || gain > best$gain)) {
        best <- list(
          gain = gain, variable = j,
          cut = (X[order_[k], j] + X[order_[k + 1L], j]) / 2.0,
          left = order_[seq_len(k)],
          right = order_[seq.int(k + 1L, no)]
        )
      }
    }
  }
  if (is.null(best)) {
    return(leaf)
  }
  list(
    leaf = FALSE, variable = best$variable, cut = best$cut, gain = best$gain,
    left = .surxgb_build(
      X, g, h, best$left, depth + 1L, max_depth,
      lam, gamma, min_child
    ),
    right = .surxgb_build(
      X, g, h, best$right, depth + 1L, max_depth,
      lam, gamma, min_child
    )
  )
}

#' .surxgb_eval_tree
#'
#' A step of the surxgb_native implementation. Called by \code{morie_surxgb_boost},
#' \code{morie_surxgb_predict}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param node A list; the body reads \code{$cut}, \code{$leaf}, \code{$left},
#' \code{$right}, \code{$variable}, \code{$weight} from it.
#' @param x A vector; indexed elementwise.
#' @return The value of \code{$}.
#' @export
.surxgb_eval_tree <- function(node, x) {
  while (!node$leaf) {
    node <- if (x[node$variable] > node$cut) node$right else node$left
  }
  node$weight
}

#' morie_surxgb_boost
#'
#' A step of the surxgb_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A matrix; indexed by row and column.
#' @param y_lower A vector; its length is taken.
#' @param y_upper A vector; its length is taken.
#' @param n_rounds Coerced to integer by the body, with \code{as.integer}. Defaults to \code{50}.
#' @param eta Numeric; combined arithmetically in the body. Defaults to \code{0.1}.
#' @param max_depth Coerced to integer by the body, with \code{as.integer}. Defaults to \code{3}.
#' @param lam Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.
#' @param gamma Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.
#' @param min_child Coerced to integer by the body, with \code{as.integer}. Defaults to \code{5}.
#' @param sigma Passed to \code{morie_surxgb_aft_gradient_hessian}. Defaults to \code{1}.
#' @param dist Passed to \code{.surxgb_check_dist}. Defaults to \code{"normal"}.
#' @param base_score Optional; may be \code{NULL}. Coerced to numeric by the body, with
#' \code{as.numeric}.
#' @param derivatives Passed to \code{morie_surxgb_aft_gradient_hessian}. Defaults to
#' \code{"analytic"}.
#' @return A list with \code{estimate}, \code{trees}, \code{eta}, \code{lam},
#' \code{gamma}, \code{sigma}, \code{dist}, \code{base_score}, \code{derivatives},
#' \code{loss_history}, \code{prediction}, \code{n_rounds}, \code{max_depth},
#' \code{method}.
#' @export
morie_surxgb_boost <- function(X, y_lower, y_upper, n_rounds = 50, eta = 0.1,
                               max_depth = 3, lam = 1.0, gamma = 0.0,
                               min_child = 5, sigma = 1.0, dist = "normal",
                               base_score = NULL, derivatives = "analytic") {
  # Fit the AFT model by second-order gradient boosting.
  .surxgb_check_dist(dist)
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  n <- length(y_lower)
  if (!(n == length(y_upper) && n == nrow(X))) {
    stop("surxgb: X, y_lower and y_upper must have the same length")
  }
  if (n == 0L) {
    stop("surxgb: no observations")
  }
  yl <- as.numeric(y_lower)
  yu <- as.numeric(y_upper)
  if (is.null(base_score)) {
    obs <- log(yl[yl > 0.0])
    base_score <- if (length(obs) > 0L) mean(obs) else 0.0
  }
  pred <- rep(as.numeric(base_score), n)
  trees <- list()
  history <- numeric(0)
  for (r in seq_len(as.integer(n_rounds))) {
    g <- numeric(n)
    h <- numeric(n)
    for (i in seq_len(n)) {
      d <- morie_surxgb_aft_gradient_hessian(
        yl[i], yu[i], pred[i],
        sigma, dist, derivatives
      )
      g[i] <- d$gradient
      h[i] <- d$hessian
    }
    tree <- .surxgb_build(
      X, g, h, seq_len(n), 0L, as.integer(max_depth),
      as.numeric(lam), as.numeric(gamma),
      as.integer(min_child)
    )
    for (i in seq_len(n)) {
      pred[i] <- pred[i] + eta * .surxgb_eval_tree(tree, X[i, ])
    }
    trees[[length(trees) + 1L]] <- tree
    loss <- 0.0
    for (i in seq_len(n)) {
      loss <- loss + morie_surxgb_aft_loss(
        yl[i], yu[i], pred[i], sigma,
        dist
      )
    }
    history <- c(history, loss / n)
  }
  list(
    estimate = if (length(history) > 0L) history[length(history)] else NaN,
    trees = trees, eta = as.numeric(eta), lam = as.numeric(lam),
    gamma = as.numeric(gamma), sigma = as.numeric(sigma),
    dist = dist, base_score = as.numeric(base_score),
    derivatives = derivatives,
    loss_history = history, prediction = pred,
    n_rounds = length(trees), max_depth = as.integer(max_depth),
    method = paste0(
      "AFT survival regression by second-order gradient ",
      "boosting; Chen & Guestrin (2016) eqs (5)-(7), ",
      "Barnwal et al. (2022) Definition 2"
    )
  )
}

#' Predicted ln y for new cases
#'
#' A step of the surxgb_native implementation. Called by \code{morie_surxgb_concordance}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param fit A list; the body reads \code{$base_score}, \code{$eta}, \code{$trees} from it.
#' @param X A matrix; indexed by row and column.
#' @return The value of \code{out}, as built in the body.
#' @export
morie_surxgb_predict <- function(fit, X) {
  # Predicted ln y for new cases.
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  out <- numeric(nrow(X))
  for (r in seq_len(nrow(X))) {
    v <- fit$base_score
    for (t in fit$trees) {
      v <- v + fit$eta * .surxgb_eval_tree(t, X[r, ])
    }
    out[r] <- v
  }
  out
}

#' Harrell\'s C for the fit; a larger prediction is a longer life, so
#'
#' the score is negated before it is ranked.
#'
#' @param fit Passed to \code{morie_surxgb_predict}.
#' @param X Passed to \code{morie_surxgb_predict}.
#' @param times Passed to \code{morie_survrsf_c_index}.
#' @param events Passed to \code{morie_survrsf_c_index}.
#' @return The value of \code{morie_survrsf_c_index}.
#' @export
morie_surxgb_concordance <- function(fit, X, times, events) {
  # Harrell's C for the fit; a larger prediction is a longer life, so
  # the score is negated before it is ranked.
  p <- morie_surxgb_predict(fit, X)
  morie_survrsf_c_index(times, events, -p)
}

#' morie_surxgb_cheatsheet
#'
#' A step of the surxgb_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
morie_surxgb_cheatsheet <- function() {
  paste0(
    "surxgb: AFT loss (Barnwal et al. Definition 2) driven by ",
    "XGBoost's second-order boosting -- leaf weight ",
    "-G/(H+lambda), split gain the eq (7) difference of three ",
    "such terms, gamma the price of a leaf. One loss covers ",
    "uncensored, right-, left- and interval-censored labels. ",
    "Three distributions for Z (normal, logistic, extreme) ",
    "and they are NOT interchangeable in the tails. The ",
    "gradient and hessian are checked against the loss they ",
    "belong to, because a sign error there still trains."
  )
}

# compact alias per ledger/NAMING.md
morie_surxgb_survival_xgboost <- morie_surxgb_boost

#' @export
morie_surxgb <- morie_surxgb_boost
