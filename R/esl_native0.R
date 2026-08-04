# SPDX-License-Identifier: AGPL-3.0-or-later
#
# The Elements of Statistical Learning shelf, after Hastie, T.,
# Tibshirani, R. and Friedman, J. (2009), 2nd ed., Springer.
# Equation numbers are the book's and were read off the PDF.
#
# R mirror of morie.fn.{eslsig,eslkrn,eslboo,eslo63,eslrft}.
#
# Sec. 7.11 is the spine here, and it is an argument rather than a
# list of formulas. Err_boot (7.54) trains and tests on overlapping
# samples and is biased LOW -- the book's worked case has it
# expecting 0.184 against a true 0.5. Err^(1) (7.56) drops the
# overlap and is then biased HIGH, because a bootstrap sample holds
# only about 0.632N distinct points. The .632 estimator (7.57)
# corrects that, and .632+ (7.61) corrects .632 when the rule
# overfits. Each function below is one step of that chain and says
# which.

.esl_K <- function(u) exp(-0.5 * u^2) / sqrt(2 * pi)

# (7.55): Pr{i in bootstrap sample b} = 1 - (1 - 1/n)^n -> 1 - e^-1.
# The exact finite-n value, not the limit; they differ by about 1% at
# n = 20, which is the regime where anyone reaches for a bootstrap.
.esl_inclusion <- function(n) {
  n <- as.integer(n)
  if (is.na(n) || n < 1L) {
    stop("need at least one observation.", call. = FALSE)
  }
  1 - (1 - 1 / n)^n
}

# The book's defaults for the number of variables sampled at each
# split. They cross at p = 9, where both give 3: below it the
# classification rule is larger, above it the regression rule is, by
# a widening margin (33 against 10 at p = 100). Both are tuning
# parameters, not laws.
.esl_mtry <- function(p, task = "regression") {
  p <- as.integer(p)
  if (is.na(p) || p < 1L) stop("need at least one predictor.", call. = FALSE)
  if (identical(task, "classification")) {
    return(max(1L, as.integer(floor(sqrt(p)))))
  }
  if (identical(task, "regression")) {
    return(max(1L, as.integer(floor(p / 3))))
  }
  stop("task must be 'regression' or 'classification'.", call. = FALSE)
}

.esl_stop <- function(y, depth, max_depth, min_node) {
  depth >= max_depth || length(y) <= min_node || stats::var(y) < 1e-12
}

# Algorithm 15.1 step 1(b). The mtry-of-p subset is drawn at EVERY
# node, not once per tree: averaging B identically distributed trees
# leaves rho sigma^2 behind, so the gain is bounded by how
# decorrelated they are, and a per-tree draw decorrelates far less.
.esl_grow <- function(X, y, mtry, depth, max_depth, min_node) {
  if (.esl_stop(y, depth, max_depth, min_node)) {
    return(list(leaf = TRUE, value = mean(y)))
  }
  p <- ncol(X)
  feats <- sample.int(p, min(mtry, p))
  best <- NULL
  for (j in feats) {
    cuts <- sort(unique(X[, j]))
    if (length(cuts) < 2L) next
    cuts <- (cuts[-1L] + cuts[-length(cuts)]) / 2
    for (t in cuts) {
      left <- X[, j] <= t
      nl <- sum(left)
      if (nl == 0L || nl == length(y)) next
      sse <- nl * .esl_var_p(y[left]) + (length(y) - nl) * .esl_var_p(y[!left])
      if (is.null(best) || sse < best$sse) {
        best <- list(sse = sse, j = j, t = t)
      }
    }
  }
  if (is.null(best)) {
    return(list(leaf = TRUE, value = mean(y)))
  }
  left <- X[, best$j] <= best$t
  list(
    leaf = FALSE, feature = best$j, threshold = best$t,
    left = .esl_grow(
      X[left, , drop = FALSE], y[left], mtry, depth + 1L,
      max_depth, min_node
    ),
    right = .esl_grow(
      X[!left, , drop = FALSE], y[!left], mtry, depth + 1L,
      max_depth, min_node
    )
  )
}

# population variance, matching numpy's np.var default (ddof = 0);
# stats::var uses ddof = 1 and would weight the split criterion
# differently
.esl_var_p <- function(y) {
  n <- length(y)
  if (n < 1L) {
    return(0)
  }
  mean((y - mean(y))^2)
}

.esl_predict_tree <- function(node, X) {
  vapply(seq_len(nrow(X)), function(i) {
    nd <- node
    while (!nd$leaf) {
      nd <- if (X[i, nd$feature] <= nd$threshold) nd$left else nd$right
    }
    nd$value
  }, numeric(1))
}

.esl_matrix <- function(X, y, what = "X") {
  A <- as.matrix(X)
  storage.mode(A) <- "double"
  yv <- as.numeric(y)
  if (nrow(A) != length(yv)) A <- t(A)
  if (nrow(A) != length(yv)) {
    stop(sprintf(
      "%s has %d rows for %d responses.", what, nrow(A),
      length(yv)
    ), call. = FALSE)
  }
  list(X = A, y = yv)
}


#' Unbiased residual variance for a linear model
#'
#' ESL Eq. (3.8), `sigma^2 = RSS / (N - p - 1)`. The book is explicit
#' about the denominator: `N - p - 1` rather than `N` is what makes
#' `E(sigma^2_hat) = sigma^2`. The `p + 1` counts the slopes plus the
#' intercept, so a design that already carries a column of ones must
#' not be charged for it twice.
#'
#' Dividing by `N` gives the maximum-likelihood estimator, biased
#' downward by exactly `(N-p-1)/N`; both are returned. Eq. (3.11)
#' adds `(N-p-1) sigma^2_hat ~ sigma^2 chi^2_{N-p-1}`.
#'
#' @param X design matrix, with or without a leading column of ones.
#' @param y response.
#' @param beta optional coefficients; least squares when `NULL`.
#' @return list: value, sigma, rss, df, n, p, intercept_in_X,
#'   mle_variance, bias_factor, fitted, residuals, method.
#' @references Hastie, Tibshirani and Friedman (2009), Sec. 3.2,
#'   Eqs. (3.8) and (3.11).
#' @examples
#' X <- matrix(stats::rnorm(120), 40)
#' morie_esl_residual_variance(X, stats::rnorm(40))$df
#' @export
morie_esl_residual_variance <- function(X, y, beta = NULL) {
  d <- .esl_matrix(X, y)
  A <- d$X
  yv <- d$y
  n <- length(yv)
  has_int <- any(apply(A, 2L, function(cc) isTRUE(all.equal(cc, rep(1, n)))))
  D <- if (has_int) A else cbind(1, A)
  p <- ncol(D) - 1L
  df <- n - p - 1L
  if (df <= 0L) {
    stop(sprintf(paste(
      "(3.8) needs N > p + 1; got N = %d and p = %d, so",
      "the residual degrees of freedom would be %d and the",
      "estimate is undefined."
    ), n, p, df), call. = FALSE)
  }
  b <- if (is.null(beta)) {
    qr.coef(qr(D), yv)
  } else {
    bb <- as.numeric(beta)
    if (length(bb) != ncol(D)) {
      stop(sprintf(
        "beta has %d entries for a design of %d columns.",
        length(bb), ncol(D)
      ), call. = FALSE)
    }
    bb
  }
  fitted <- as.numeric(D %*% b)
  resid <- yv - fitted
  rss <- sum(resid^2)
  list(
    value = rss / df, sigma = sqrt(rss / df), rss = rss,
    df = df, n = n, p = p, intercept_in_X = has_int,
    mle_variance = rss / n, bias_factor = df / n,
    fitted = fitted, residuals = resid,
    denominator_note = "N - p - 1, not N: that is what makes it unbiased (3.8)",
    chi_square_fact = "(N-p-1) sigma_hat^2 ~ sigma^2 chi^2_{N-p-1} (3.11)",
    method = "ESL (3.8) unbiased residual variance"
  )
}


#' Parzen kernel density estimate
#'
#' ESL Eqs. (6.23)-(6.24). In one dimension the estimate IS the
#' empirical distribution convolved with a Gaussian of standard
#' deviation `lambda`: `F_hat` puts mass `1/N` at each observation
#' and is jumpy, and the estimate smooths it by adding independent
#' Gaussian noise to each point. That is why `lambda` is a standard
#' deviation here rather than a generic bandwidth.
#'
#' In `p` dimensions the Gaussian product kernel (6.24) normalises by
#' `N (2 lambda^2 pi)^(p/2)` -- the exponent is `p/2`, not `p`.
#'
#' @param x evaluation points: a vector, or a matrix with `p` columns.
#' @param data sample: a vector, or a matrix with `p` columns.
#' @param lambda_ kernel standard deviation; Silverman's normal
#'   reference `1.06 sigma N^(-1/5)` when `NULL`, which is a DENSITY
#'   rule and correct here.
#' @return list: x, density, lambda, n, p, mass (1-D only),
#'   normaliser, is_convolution, method.
#' @references Hastie, Tibshirani and Friedman (2009), Sec. 6.6,
#'   Eqs. (6.22)-(6.24); Parzen (1962).
#' @examples
#' morie_esl_kernel_density(c(-1, 0, 1), stats::rnorm(200))$density
#' @export
morie_esl_kernel_density <- function(x, data, lambda_ = NULL) {
  D <- if (is.matrix(data)) data else matrix(as.numeric(data), ncol = 1L)
  storage.mode(D) <- "double"
  n <- nrow(D)
  p <- ncol(D)
  if (n < 2L) {
    stop(sprintf("need at least 2 observations, got %d.", n), call. = FALSE)
  }
  lam <- if (is.null(lambda_)) {
    s <- stats::sd(D[, 1L])
    1.06 * (if (s > 0) s else 1) * n^(-0.2)
  } else {
    as.numeric(lambda_)
  }
  if (lam <= 0) {
    stop(sprintf("lambda must be positive, got %g.", lam), call. = FALSE)
  }
  Q <- if (is.matrix(x)) x else matrix(as.numeric(x), ncol = p)
  storage.mode(Q) <- "double"
  d2 <- matrix(0, nrow(Q), n)
  for (k in seq_len(p)) d2 <- d2 + outer(Q[, k], D[, k], "-")^2
  norm <- n * (2 * lam^2 * pi)^(p / 2)
  dens <- rowSums(exp(-0.5 * d2 / lam^2)) / norm
  mass <- NULL
  if (p == 1L && nrow(Q) > 2L && all(diff(Q[, 1L]) > 0)) {
    g <- Q[, 1L]
    mass <- sum(diff(g) * (dens[-1L] + dens[-length(dens)])) / 2
  }
  list(
    x = if (p > 1L) Q else Q[, 1L], density = dens, lambda = lam,
    n = n, p = p, mass = mass, normaliser = norm,
    is_convolution = TRUE,
    convolution_note = paste(
      "(6.23): the empirical df convolved with a",
      "Gaussian of standard deviation lambda"
    ),
    method = "ESL (6.23)/(6.24) Parzen density with a Gaussian product kernel"
  )
}


#' Bootstrap estimates of prediction error
#'
#' Returns BOTH estimates of ESL Sec. 7.11, because the first is
#' misleading alone.
#'
#' Eq. (7.54), `Err_boot = (1/B)(1/N) sum_b sum_i L(y_i, f*b(x_i))`,
#' is biased downward: the bootstrap datasets act as training samples
#' while the original training set acts as the test sample, and the
#' two overlap. For a 1-nearest-neighbour rule on labels independent
#' of the inputs the true error is 0.5, but a contribution is
#' non-zero only when observation `i` is absent from sample `b`,
#' which by (7.55) has probability 0.368 -- so (7.54) expects 0.184.
#'
#' Eq. (7.56), the leave-one-out bootstrap, averages only over the
#' replicates `C^{-i}` that exclude observation `i`. That removes the
#' overlap but introduces a training-set-size bias in the other
#' direction, which [morie_esl_oob_632()] corrects.
#'
#' Observations present in every replicate have an empty `C^{-i}` and
#' are dropped, as the book allows; `n_dropped` reports how many,
#' since a large count means `B` is too small.
#'
#' @param X predictors.
#' @param y response.
#' @param model optional `function(Xtr, ytr)` returning a predict
#'   function; least squares when `NULL`.
#' @param B bootstrap replicates.
#' @param loss optional elementwise `function(y, yhat)`; squared
#'   error when `NULL`.
#' @param seed resampling seed.
#' @return list: err_boot, err_loo_boot, err_train,
#'   inclusion_probability, optimistic, n_dropped, per_observation,
#'   B, n, method.
#' @references Hastie, Tibshirani and Friedman (2009), Sec. 7.11,
#'   Eqs. (7.54)-(7.56); Efron and Tibshirani (1997).
#' @examples
#' X <- matrix(stats::rnorm(150), 50)
#' morie_esl_bootstrap_err(X, stats::rnorm(50), B = 20)$err_loo_boot
#' @export
morie_esl_bootstrap_err <- function(X, y, model = NULL, B = 100,
                                    loss = NULL, seed = 0) {
  d <- .esl_matrix(X, y)
  A <- d$X
  yv <- d$y
  n <- length(yv)
  if (n < 4L) {
    stop(sprintf("need at least 4 observations, got %d.", n), call. = FALSE)
  }
  Bn <- as.integer(B)
  if (is.na(Bn) || Bn < 1L) {
    stop("need at least one bootstrap replicate.", call. = FALSE)
  }
  fit <- if (is.null(model)) {
    function(Xtr, ytr) {
      b <- qr.coef(qr(cbind(1, Xtr)), ytr)
      function(Xn) as.numeric(cbind(1, Xn) %*% b)
    }
  } else {
    model
  }
  L <- if (is.null(loss)) function(a, b) (a - b)^2 else loss

  old <- if (exists(".Random.seed", envir = globalenv())) {
    get(".Random.seed", envir = globalenv())
  } else {
    NULL
  }
  set.seed(seed)
  on.exit(if (!is.null(old)) assign(".Random.seed", old, envir = globalenv()))

  losses <- matrix(0, Bn, n)
  inbag <- matrix(FALSE, Bn, n)
  for (b in seq_len(Bn)) {
    idx <- sample.int(n, n, replace = TRUE)
    inbag[b, idx] <- TRUE
    losses[b, ] <- L(yv, fit(A[idx, , drop = FALSE], yv[idx])(A))
  }
  err_boot <- mean(losses)
  oob <- !inbag
  counts <- colSums(oob)
  keep <- counts > 0
  per_i <- rep(NA_real_, n)
  per_i[keep] <- colSums(losses * oob)[keep] / counts[keep]
  err_loo <- if (any(keep)) mean(per_i[keep]) else NA_real_
  list(
    err_boot = err_boot, err_loo_boot = err_loo,
    err_train = mean(L(yv, fit(A, yv)(A))),
    inclusion_probability = .esl_inclusion(n),
    optimistic = TRUE,
    optimism_note = paste(
      "(7.54) trains and tests on overlapping samples,",
      "so it is biased DOWNWARD; use err_loo_boot",
      "(7.56)"
    ),
    n_dropped = sum(!keep), per_observation = per_i,
    which_to_use = paste(
      "err_loo_boot for an honest estimate; feed it and",
      "err_train to morie_esl_oob_632"
    ),
    B = Bn, n = n,
    method = "ESL (7.54) Err_boot and (7.56) leave-one-out bootstrap Err^(1)"
  )
}


#' The .632 and .632+ prediction-error estimators
#'
#' ESL Eq. (7.57), `Err^(.632) = .368 err_bar + .632 Err^(1)`, and
#' Eq. (7.61), `Err^(.632+) = (1-w) err_bar + w Err^(1)` with
#' `w = .632 / (1 - .368 R)`.
#'
#' The second argument is the LEAVE-ONE-OUT bootstrap (7.56), not the
#' naive (7.54). That distinction is the point of the section: (7.54)
#' is biased downward by train/test overlap, while (7.56) is biased
#' upward because a bootstrap sample holds only about `0.632N`
#' distinct points. The `.632` constant is the inclusion probability
#' of (7.55). Passing (7.54) here applies a downward correction to a
#' quantity that was already too small.
#'
#' `.632` breaks down on overfit rules. The book's case: a
#' 1-nearest-neighbour rule on independent labels has `err_bar = 0`
#' and `Err^(1) = 0.5`, giving `.632 * 0.5 = 0.316` against a true
#' rate of 0.5. `.632+` repairs it using the no-information error
#' rate `gamma` -- (7.58) `gamma = N^-2 sum_i sum_i' L(y_i, f(x_i'))`
#' or (7.59) `p1(1-q1) + (1-p1)q1` -- through the relative
#' overfitting rate (7.60) `R = (Err^(1) - err_bar)/(gamma -
#' err_bar)`. On that example `R = w = 1` and it returns 0.5.
#'
#' @param err_train the training error `err_bar`.
#' @param err_loo_boot the leave-one-out bootstrap error (7.56).
#' @param gamma optional no-information error rate.
#' @param y,y_pred optional response and fitted values, for (7.58).
#' @param p1,q1 optional proportions, for (7.59).
#' @return list: value, err_632, err_632_plus, weight,
#'   relative_overfitting_rate, gamma, err_train, err_loo_boot,
#'   uses_leave_one_out, method.
#' @references Hastie, Tibshirani and Friedman (2009), Sec. 7.11,
#'   Eqs. (7.55)-(7.61); Efron (1983); Efron and Tibshirani (1997).
#' @examples
#' # the book's 1-NN counterexample
#' morie_esl_oob_632(0, 0.5, gamma = 0.5)$err_632_plus
#' @export
morie_esl_oob_632 <- function(err_train, err_loo_boot, gamma = NULL,
                              y = NULL, y_pred = NULL, p1 = NULL, q1 = NULL) {
  et <- as.numeric(err_train)
  e1 <- as.numeric(err_loo_boot)
  if (!is.finite(et) || !is.finite(e1)) {
    stop("both error estimates must be finite.", call. = FALSE)
  }
  err632 <- 0.368 * et + 0.632 * e1
  if (is.null(gamma) && !is.null(y) && !is.null(y_pred)) {
    yv <- as.numeric(y)
    pv <- as.numeric(y_pred)
    if (length(yv) != length(pv)) {
      stop(sprintf(
        "y has %d entries and y_pred has %d.", length(yv),
        length(pv)
      ), call. = FALSE)
    }
    gamma <- mean(outer(yv, pv, "-")^2)
  }
  if (is.null(gamma) && !is.null(p1) && !is.null(q1)) {
    pp <- as.numeric(p1)
    qq <- as.numeric(q1)
    if (pp < 0 || pp > 1 || qq < 0 || qq > 1) {
      stop("p1 and q1 are proportions and must lie in [0, 1].", call. = FALSE)
    }
    gamma <- pp * (1 - qq) + (1 - pp) * qq
  }
  w <- NULL
  rr <- NULL
  err632p <- NULL
  if (!is.null(gamma)) {
    g <- as.numeric(gamma)
    rr <- if (g <= et) 0 else min(max((e1 - et) / (g - et), 0), 1)
    w <- 0.632 / (1 - 0.368 * rr)
    err632p <- (1 - w) * et + w * e1
  }
  list(
    value = err632, err_632 = err632, err_632_plus = err632p,
    weight = w, relative_overfitting_rate = rr, gamma = gamma,
    err_train = et, err_loo_boot = e1, uses_leave_one_out = TRUE,
    second_argument_note = paste(
      "(7.57) takes Err^(1) from (7.56), NOT",
      "Err_boot from (7.54); the latter is",
      "biased downward and needs no correction",
      "downward"
    ),
    weight_range = paste(
      "w runs from .632 at R = 0 to 1 at R = 1, so the",
      ".632+ estimate runs from Err^(.632) to Err^(1)"
    ),
    method = "ESL (7.57) .632 and (7.61) .632+ prediction-error estimators"
  )
}


#' Random forest for regression
#'
#' ESL Algorithm 15.1. For `b = 1..B`: draw a bootstrap sample of
#' size N, and grow a tree by repeating, at each terminal node,
#' select `mtry` of the `p` variables at random, pick the best
#' variable and split-point among those, and split. Predict by
#' `f_rf(x) = (1/B) sum_b T_b(x)`.
#'
#' The variable subset is drawn per NODE, not per tree. Bagging
#' already averages `B` identically distributed trees, so the bias is
#' unchanged and the only gain is variance reduction; averaging `B`
#' i.d. variables with pairwise correlation `rho` leaves `rho sigma^2`
#' behind, so the gain is bounded by how decorrelated the trees are.
#' The per-node draw is where that decorrelation comes from.
#'
#' The default `mtry` is `floor(p/3)` for regression. The
#' classification default `floor(sqrt(p))` is a different number --
#' they cross at `p = 9`, and above it the regression rule is the
#' larger of the two.
#'
#' Out-of-bag error is free: each observation is absent from about
#' 36.8% of the trees (7.55), and averaging only those gives a
#' nearly-cross-validated error with no extra fitting.
#'
#' @param X predictors.
#' @param y numeric response.
#' @param B number of trees.
#' @param mtry variables sampled per node; `floor(p/3)` when `NULL`.
#' @param max_depth,min_node stopping rules standing in for `n_min`.
#' @param newdata optional points to predict.
#' @param seed seed for the bootstrap and per-node draws.
#' @return list: prediction, oob_prediction, oob_mse, train_mse,
#'   mtry, mtry_rule, subset_drawn_per, n_oob_missing, B, n, p,
#'   method.
#' @references Hastie, Tibshirani and Friedman (2009), Ch. 15,
#'   Algorithm 15.1 and Secs. 15.2-15.3; Breiman (2001).
#' @examples
#' X <- matrix(stats::rnorm(300), 100)
#' morie_esl_random_forest(X, X[, 1] + stats::rnorm(100), B = 10)$mtry
#' @export
morie_esl_random_forest <- function(X, y, B = 100, mtry = NULL,
                                    max_depth = 8L, min_node = 5L,
                                    newdata = NULL, seed = 0) {
  d <- .esl_matrix(X, y)
  A <- d$X
  yv <- d$y
  n <- length(yv)
  p <- ncol(A)
  if (n < 4L) {
    stop(sprintf("need at least 4 observations, got %d.", n), call. = FALSE)
  }
  Bn <- as.integer(B)
  if (is.na(Bn) || Bn < 1L) {
    stop(sprintf("need at least one tree, got %s.", format(B)), call. = FALSE)
  }
  m <- if (is.null(mtry)) .esl_mtry(p, "regression") else as.integer(mtry)
  if (is.na(m) || m < 1L || m > p) {
    stop(sprintf("mtry must lie in 1..%d, got %s.", p, format(mtry)),
      call. = FALSE
    )
  }
  Q <- if (is.null(newdata)) {
    A
  } else {
    QQ <- as.matrix(newdata)
    storage.mode(QQ) <- "double"
    QQ
  }
  if (ncol(Q) != p) {
    stop(sprintf("newdata has %d columns, expected %d.", ncol(Q), p),
      call. = FALSE
    )
  }
  old <- if (exists(".Random.seed", envir = globalenv())) {
    get(".Random.seed", envir = globalenv())
  } else {
    NULL
  }
  set.seed(seed)
  on.exit(if (!is.null(old)) assign(".Random.seed", old, envir = globalenv()))

  pred <- numeric(nrow(Q))
  oob_sum <- numeric(n)
  oob_cnt <- numeric(n)
  for (b in seq_len(Bn)) {
    rows <- sample.int(n, n, replace = TRUE)
    tree <- .esl_grow(
      A[rows, , drop = FALSE], yv[rows], m, 0L,
      as.integer(max_depth), as.integer(min_node)
    )
    pred <- pred + .esl_predict_tree(tree, Q)
    out <- setdiff(seq_len(n), rows)
    if (length(out)) {
      oob_sum[out] <- oob_sum[out] +
        .esl_predict_tree(tree, A[out, , drop = FALSE])
      oob_cnt[out] <- oob_cnt[out] + 1
    }
  }
  pred <- pred / Bn
  has_oob <- oob_cnt > 0
  oob_pred <- rep(NA_real_, n)
  oob_pred[has_oob] <- oob_sum[has_oob] / oob_cnt[has_oob]
  list(
    prediction = pred,
    oob_prediction = oob_pred,
    oob_mse = if (any(has_oob)) {
      mean((yv[has_oob] - oob_pred[has_oob])^2)
    } else {
      NA_real_
    },
    train_mse = if (is.null(newdata)) mean((yv - pred)^2) else NULL,
    mtry = m,
    mtry_rule = paste(
      "floor(p/3) for regression; floor(sqrt(p)) is the",
      "CLASSIFICATION default. They cross at p = 9; above",
      "it the regression rule is the larger of the two"
    ),
    subset_drawn_per = "node",
    why_per_node = paste(
      "averaging B identically distributed trees leaves",
      "rho sigma^2 behind, so the gain is bounded by",
      "how decorrelated they are; a per-tree subset",
      "decorrelates far less than a per-node one"
    ),
    n_oob_missing = sum(!has_oob), B = Bn, n = n, p = p,
    method = "ESL Algorithm 15.1 random forest for regression"
  )
}
