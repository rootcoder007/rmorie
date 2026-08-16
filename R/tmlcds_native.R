# tmlcds.R -- function file (rootcoder007/morie)
# Collaborative targeted minimum loss-based estimation (C-TMLE).
#
# The sequence of treatment mechanisms is selected by the loss of the
# TARGETED OUTCOME fit, not by how well G predicts treatment, so an
# instrument stays out of the propensity model.
#
# References:
# van der Laan, M. J. & Rose, S. (eds.) (2018) Targeted Learning in Data
# Science: Causal Inference for Complex Longitudinal Studies, Springer
# Series in Statistics, doi:10.1007/978-3-319-65304-4, Ch. 10 (Sec. 10.1,
# Examples 10.2 and 10.3, Sec. 10.1.1).
# van der Laan, M. J. & Gruber, S. (2010) "Collaborative double robust
# targeted maximum likelihood estimation", The International Journal of
# Biostatistics 6(1), article 17, doi:10.2202/1557-4679.1181.
# Gruber, S. & van der Laan, M. J. (2010) "A targeted maximum likelihood
# estimator of a causal effect on a bounded continuous outcome", The
# International Journal of Biostatistics 6(1), article 26,
# doi:10.2202/1557-4679.1260.

#' .RichResult
#'
#' A step of the tmlcds_native implementation. Called by \code{tmle_cdrs}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param payload See Usage.
#' @return The value of \code{payload}, as built in the body.
#' @export
.RichResult <- function(payload) {
  payload
}

#' .tmlcds_sigmoid
#'
#' A step of the tmlcds_native implementation. Called by \code{.tmlcds_fluctuate}, \code{.tmlcds_logit_irls}, \code{.tmlcds_propensity} and 1 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.tmlcds_sigmoid <- function(x) {
  1.0 / (1.0 + exp(-x))
}

#' .tmlcds_logit
#'
#' A step of the tmlcds_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param p Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.tmlcds_logit <- function(p) {
  log(p / (1.0 - p))
}

#' .tmlcds_vec
#'
#' A step of the tmlcds_native implementation. Called by \code{ctmle_sequence}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x See Usage.
#' @return A vector, from \code{as.numeric}.
#' @export
.tmlcds_vec <- function(x) {
  if (is.null(x)) return(numeric(0))
  as.numeric(x)
}

#' .tmlcds_mat
#'
#' A step of the tmlcds_native implementation. Called by \code{ctmle_sequence}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.
#' @return One of two values, depending on the branch taken.
#' @export
.tmlcds_mat <- function(X) {
  if (is.null(X)) return(matrix(numeric(0), nrow = 0, ncol = 0))
  if (is.data.frame(X)) X <- as.matrix(X)
  if (is.vector(X)) matrix(X, ncol = 1) else as.matrix(X)
}

# cols is a list of column vectors; prepend the intercept
#' Cols is a list of column vectors; prepend the intercept
#'
#' A step of the tmlcds_native implementation. Called by \code{.tmlcds_propensity}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param cols Optional; may be \code{NULL}. A vector; its length is taken.
#' @param n A count; the body uses it as \code{matrix(...)}.
#' @return The value of \code{cbind}.
#' @export
.tmlcds_design <- function(cols, n) {
  if (is.null(cols) || length(cols) == 0) {
    return(matrix(1.0, nrow = n, ncol = 1))
  }
  M <- do.call(cbind, lapply(cols, as.numeric))
  cbind(1.0, M)
}

#' .tmlcds_matvec
#'
#' A step of the tmlcds_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param M A matrix; passed to \code{\%*\%}.
#' @param v A matrix; passed to \code{\%*\%}.
#' @return A vector, from \code{as.numeric}.
#' @export
.tmlcds_matvec <- function(M, v) {
  as.numeric(M %*% v)
}

# ridge-stabilised least squares -- Python k.lstsq ALWAYS adds the 1e-10
# ridge, so the R arm must too or near-singular designs drift
#' Ridge-stabilised least squares -- Python k.lstsq ALWAYS adds the
#' 1e-10
#'
#' ridge, so the R arm must too or near-singular designs drift
#'
#' @param Z A matrix; passed to \code{ncol}.
#' @param y A matrix; passed to \code{crossprod}.
#' @return A vector, from \code{as.numeric}.
#' @export
.tmlcds_lstsq <- function(Z, y) {
  as.numeric(solve(crossprod(Z) + 1e-10 * diag(ncol(Z)), crossprod(Z, y)))
}

# logistic regression by IRLS, mirroring k.logit_irls: beta starts at 0,
# `penalty` is a ridge on the OBJECTIVE for the slopes only (intercept
# unpenalized), `ridge` stabilises the Newton solve without changing the
# fitted coefficients
#' Logistic regression by IRLS, mirroring k.logit_irls: beta starts at
#' 0,
#'
#' `penalty` is a ridge on the OBJECTIVE for the slopes only (intercept
#' unpenalized), `ridge` stabilises the Newton solve without changing
#' the fitted coefficients
#'
#' @param X A matrix; passed to \code{nrow}.
#' @param y Numeric; combined arithmetically in the body.
#' @param iters A count; the body uses it as \code{seq_len(...)}. Defaults to \code{60L}.
#' @param ridge Numeric; combined arithmetically in the body. Defaults to \code{1e-10}.
#' @param tol Defaults to \code{1e-13}.
#' @param penalty Defaults to \code{0}.
#' @return The value of \code{beta}, as built in the body.
#' @export
.tmlcds_logit_irls <- function(X, y, iters = 60L, ridge = 1e-10,
                               tol = 1e-13, penalty = 0.0) {
  n <- nrow(X)
  p <- ncol(X)
  beta <- numeric(p)
  for (it in seq_len(iters)) {
    eta <- as.numeric(X %*% beta)
    mu <- .tmlcds_sigmoid(eta)
    w <- mu * (1.0 - mu)
    r <- y - mu
    Xtr <- as.numeric(crossprod(X, r))
    XtWX <- crossprod(X, X * w)
    pen <- as.numeric(penalty)
    if (pen > 0.0 && p > 1L) {
      idx <- 2:p
      Xtr[idx] <- Xtr[idx] - pen * beta[idx]
      diag(XtWX)[idx] <- diag(XtWX)[idx] + pen
    }
    step <- as.numeric(solve(XtWX + ridge * diag(p), Xtr))
    beta <- beta + step
    if (max(abs(step)) < tol) break
  }
  beta
}

# one targeting step: the Example 10.3 submodel, solved by Newton.
# clever covariate A/G - (1-A)/(1-G); returns updated fits and epsilon
#' One targeting step: the Example 10.3 submodel, solved by Newton
#'
#' clever covariate A/G - (1-A)/(1-G); returns updated fits and epsilon
#'
#' @param qa Numeric; combined arithmetically in the body.
#' @param q1 Numeric; combined arithmetically in the body.
#' @param q0 Numeric; combined arithmetically in the body.
#' @param y A vector; its length is taken.
#' @param d Numeric; combined arithmetically in the body.
#' @param g Numeric; combined arithmetically in the body.
#' @param clip Numeric; combined arithmetically in the body. Defaults to \code{1e-08}.
#' @return A list with \code{qa}, \code{q1}, \code{q0}, \code{eps}.
#' @export
.tmlcds_fluctuate <- function(qa, q1, q0, y, d, g, clip = 1e-8) {
  n <- length(y)
  h <- d / g - (1.0 - d) / (1.0 - g)
  eps <- 0.0
  for (it in seq_len(60L)) {
    z <- log(qa / (1.0 - qa)) + eps * h
    p <- .tmlcds_sigmoid(z)
    num <- sum(h * (y - p))
    den <- sum(h * h * p * (1.0 - p))
    if (den <= 0.0) break
    step <- num / den
    eps <- eps + step
    if (abs(step) < 1e-13) break
  }
  qa2 <- pmin(pmax(.tmlcds_sigmoid(log(qa / (1.0 - qa)) + eps * h),
                   clip), 1.0 - clip)
  q1b <- pmin(pmax(.tmlcds_sigmoid(log(q1 / (1.0 - q1)) + eps / g),
                   clip), 1.0 - clip)
  q0b <- pmin(pmax(.tmlcds_sigmoid(log(q0 / (1.0 - q0)) - eps / (1.0 - g)),
                   clip), 1.0 - clip)
  list(qa = qa2, q1 = q1b, q0 = q0b, eps = eps)
}

# L(Qbar) = -{Y log Qbar + (1-Y) log(1-Qbar)}
#' L(Qbar) = -{Y log Qbar + (1-Y) log(1-Qbar)}
#'
#' A step of the tmlcds_native implementation. Called by \code{ctmle_sequence}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param qa Numeric; passed to \code{log}.
#' @param y A vector; its length is taken.
#' @return A numeric value.
#' @export
.tmlcds_qloss <- function(qa, y) {
  -sum(y * log(qa) + (1.0 - y) * log(1.0 - qa)) / length(y)
}

# treatment mechanism fit; cols is a list of column vectors
#' Treatment mechanism fit; cols is a list of column vectors
#'
#' A step of the tmlcds_native implementation. Called by \code{.tmlcds_refit_on}, \code{ctmle_sequence}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param d Passed to \code{.tmlcds_logit_irls}.
#' @param cols Passed to \code{.tmlcds_design}.
#' @param n Passed to \code{.tmlcds_design}.
#' @param penalty Defaults to \code{0}.
#' @param trim Numeric; combined arithmetically in the body. Defaults to \code{0.005}.
#' @return A list with \code{g}, \code{b}.
#' @export
.tmlcds_propensity <- function(d, cols, n, penalty = 0.0, trim = 0.005) {
  Z <- .tmlcds_design(cols, n)
  b <- .tmlcds_logit_irls(Z, d, 60L, 1e-10, penalty = as.numeric(penalty))
  g <- pmin(pmax(.tmlcds_sigmoid(as.numeric(Z %*% b)), trim), 1.0 - trim)
  list(g = g, b = b)
}

.tmlcds_TUNING <- c("discrete", "continuous")

# build the nested sequence of (G, targeted Q) and score each step.
# q_covariates uses 0-based indices, matching the Python arm exactly.
#' Build the nested sequence of (G, targeted Q) and score each step
#'
#' q_covariates uses 0-based indices, matching the Python arm exactly.
#'
#' @param y Passed to \code{.tmlcds_vec}.
#' @param D Passed to \code{.tmlcds_vec}.
#' @param X Optional; may be \code{NULL}. Passed to \code{.tmlcds_mat}.
#' @param tuning Compared against \code{"discrete"}. Defaults to \code{"discrete"}.
#' @param penalties Optional; may be \code{NULL}. A vector; its length is taken and its elements indexed.
#' @param trim Passed to \code{.tmlcds_propensity}. Defaults to \code{0.005}.
#' @param scale Optional; may be \code{NULL}. A flag; the body branches on it.
#' @param q_covariates Defaults to \code{NULL}.
#' @return A list with \code{steps}, \code{info}.
#' @export
ctmle_sequence <- function(y, D, X, tuning = "discrete", penalties = NULL,
                           trim = 0.005, scale = NULL,
                           q_covariates = NULL) {
  if (!(tuning %in% .tmlcds_TUNING)) {
    stop(sprintf("ctmle_sequence: tuning must be 'discrete' or 'continuous', got '%s'",
                 tuning))
  }
  yv <- .tmlcds_vec(y)
  d <- .tmlcds_vec(D)
  n <- length(yv)
  if (length(d) != n) {
    stop(sprintf("ctmle_sequence: %d outcomes but %d treatments",
                 n, length(d)))
  }
  if (any(!(d %in% c(0.0, 1.0)))) {
    stop("ctmle_sequence: treatment must be binary 0/1")
  }
  Xm <- if (is.null(X)) matrix(numeric(0), nrow = n, ncol = 0) else .tmlcds_mat(X)
  p <- ncol(Xm)
  cols <- if (p > 0L) lapply(seq_len(p), function(j) as.numeric(Xm[, j])) else list()

  # Gruber & van der Laan (2010): scale Y into [0,1] so the logistic
  # fluctuation is valid for a continuous outcome too
  lo <- min(yv)
  hi <- max(yv)
  rng <- if (hi > lo) hi - lo else 1.0
  if (is.null(scale)) {
    scale <- !all(yv %in% c(0.0, 1.0))
  }
  ys <- if (isTRUE(scale)) (yv - lo) / rng else yv
  ys <- pmin(pmax(ys, 1e-8), 1.0 - 1e-8)

  # initial outcome fit; q_covariates (0-based) picks which covariates
  # the outcome model saw -- NULL means all of them
  qcols <- if (is.null(q_covariates)) {
    if (p > 0L) 0:(p - 1L) else integer(0)
  } else {
    as.integer(q_covariates)
  }
  for (cc in qcols) {
    if (cc < 0L || cc >= p) {
      stop(sprintf("ctmle_sequence: q_covariates index %d is outside the %d covariates supplied",
                   cc, p))
    }
  }
  q_rows <- lapply(seq_len(n), function(i) {
    c(d[i], if (length(qcols)) Xm[i, qcols + 1L] else numeric(0))
  })
  Zq <- cbind(1.0, do.call(rbind, q_rows))
  bq <- .tmlcds_lstsq(Zq, ys)

  q_at <- function(a, i) {
    row <- c(1.0, a, if (length(qcols)) Xm[i, qcols + 1L] else numeric(0))
    min(max(sum(bq * row), 1e-8), 1.0 - 1e-8)
  }
  qa <- vapply(seq_len(n), function(i) q_at(d[i], i), numeric(1))
  q1 <- vapply(seq_len(n), function(i) q_at(1.0, i), numeric(1))
  q0 <- vapply(seq_len(n), function(i) q_at(0.0, i), numeric(1))

  steps <- list()
  if (tuning == "discrete") {
    chosen <- integer(0)
    remaining <- if (p > 0L) 0:(p - 1L) else integer(0)
    pr <- .tmlcds_propensity(d, list(), n, trim = trim)
    fl <- .tmlcds_fluctuate(qa, q1, q0, ys, d, pr$g)
    qa <- fl$qa; q1 <- fl$q1; q0 <- fl$q0
    steps[[1L]] <- list(step = 0L, covariates = integer(0),
                        loss = .tmlcds_qloss(qa, ys), epsilon = fl$eps,
                        g = pr$g, psi = rng * mean(q1 - q0))
    while (length(remaining) > 0L) {
      best <- NULL
      for (j in remaining) {
        pj <- .tmlcds_propensity(d, cols[c(chosen, j) + 1L], n, trim = trim)
        # nested: start from the CURRENT targeted fit
        fj <- .tmlcds_fluctuate(qa, q1, q0, ys, d, pj$g)
        loss <- .tmlcds_qloss(fj$qa, ys)
        if (is.null(best) || loss < best$loss) {
          best <- list(loss = loss, j = j, g = pj$g, fl = fj)
        }
      }
      chosen <- c(chosen, best$j)
      remaining <- setdiff(remaining, best$j)
      qa <- best$fl$qa; q1 <- best$fl$q1; q0 <- best$fl$q0
      steps[[length(steps) + 1L]] <- list(step = length(chosen),
                                          covariates = as.integer(chosen),
                                          loss = best$loss,
                                          epsilon = best$fl$eps,
                                          g = best$g,
                                          psi = rng * mean(q1 - q0))
    }
  } else {
    if (is.null(penalties)) {
      penalties <- c(1e4, 1e3, 1e2, 10.0, 1.0, 0.1, 1e-2, 0.0)
    }
    for (s in seq_along(penalties)) {
      lam <- as.numeric(penalties[s])
      pr <- .tmlcds_propensity(d, cols, n, penalty = lam, trim = trim)
      fl <- .tmlcds_fluctuate(qa, q1, q0, ys, d, pr$g)
      qa <- fl$qa; q1 <- fl$q1; q0 <- fl$q0
      steps[[s]] <- list(step = s - 1L, penalty = lam,
                         loss = .tmlcds_qloss(qa, ys), epsilon = fl$eps,
                         g = pr$g, psi = rng * mean(q1 - q0))
    }
  }
  list(steps = steps,
       info = list(scale = rng, shift = lo, n = n, p = p,
                   y_scaled = ys, treatment = d, columns = cols,
                   q_covariates = qcols))
}

# out-of-sample loss of step s (1-based), refitting on the training rows.
# tr and fold hold 1-based row indices.
#' Out-of-sample loss of step s (1-based), refitting on the training
#' rows
#'
#' tr and fold hold 1-based row indices.
#'
#' @param info A list; the body reads \code{$columns}, \code{$q_covariates}, \code{$treatment}, \code{$y_scaled} from it.
#' @param steps A vector; indexed elementwise.
#' @param s See Usage.
#' @param tr A vector; its length is taken.
#' @param fold A vector; its length is taken and its elements indexed.
#' @param tuning See Usage.
#' @param penalties See Usage.
#' @param trim Numeric; combined arithmetically in the body.
#' @return One of two values, depending on the branch taken.
#' @export
.tmlcds_refit_on <- function(info, steps, s, tr, fold, tuning, penalties,
                             trim) {
  ys <- info$y_scaled; d <- info$treatment; cols <- info$columns
  ntr <- length(tr)
  if (ntr < 5L) return(Inf)
  st <- steps[[s]]
  use <- st$covariates            # 0-based or NULL (continuous tuning)
  lam <- if (is.null(st$penalty)) 0.0 else st$penalty
  sub_cols <- if (!is.null(use)) {
    lapply(use, function(cc) cols[[cc + 1L]][tr])
  } else {
    lapply(cols, function(col) col[tr])
  }
  dtr <- d[tr]
  ytr <- ys[tr]
  if (length(unique(dtr)) < 2L) return(Inf)
  pr <- .tmlcds_propensity(dtr, sub_cols, ntr, penalty = lam, trim = trim)
  g_tr <- pr$g; bg <- pr$b
  qc <- info$q_covariates          # 0-based
  Xtr <- lapply(tr, function(i) {
    vapply(qc, function(cc) cols[[cc + 1L]][i], numeric(1))
  })
  Zq <- cbind(1.0, do.call(rbind, lapply(seq_len(ntr), function(j) {
    c(dtr[j], Xtr[[j]])
  })))
  bq <- .tmlcds_lstsq(Zq, ytr)

  q_at <- function(a, row) {
    r <- c(1.0, a, row)
    min(max(sum(bq * r), 1e-8), 1.0 - 1e-8)
  }
  qa <- vapply(seq_len(ntr), function(j) q_at(dtr[j], Xtr[[j]]), numeric(1))
  q1 <- vapply(seq_len(ntr), function(j) q_at(1.0, Xtr[[j]]), numeric(1))
  q0 <- vapply(seq_len(ntr), function(j) q_at(0.0, Xtr[[j]]), numeric(1))
  fl <- .tmlcds_fluctuate(qa, q1, q0, ytr, dtr, g_tr)
  eps <- fl$eps

  use_idx <- if (!is.null(use)) use else if (length(cols)) 0:(length(cols) - 1L) else integer(0)
  have_cols <- length(cols) > 0L && (is.null(use) || length(use) > 0L)
  Zg <- if (have_cols) {
    cbind(1.0, do.call(rbind, lapply(fold, function(i) {
      vapply(use_idx, function(cc) cols[[cc + 1L]][i], numeric(1))
    })))
  } else {
    matrix(1.0, nrow = length(fold), ncol = 1L)
  }
  gs <- if (length(bg) == ncol(Zg)) {
    pmin(pmax(.tmlcds_sigmoid(as.numeric(Zg %*% bg)), trim), 1.0 - trim)
  } else {
    NULL
  }
  tot <- 0.0
  m <- 0L
  for (idx in seq_along(fold)) {
    i <- fold[idx]
    row <- vapply(qc, function(cc) cols[[cc + 1L]][i], numeric(1))
    q <- q_at(d[i], row)
    gi <- if (!is.null(gs)) gs[idx] else 0.5
    h <- d[i] / gi - (1.0 - d[i]) / (1.0 - gi)
    qs <- .tmlcds_sigmoid(log(q / (1.0 - q)) + eps * h)
    qs <- min(max(qs, 1e-8), 1.0 - 1e-8)
    tot <- tot - (ys[i] * log(qs) + (1.0 - ys[i]) * log(1.0 - qs))
    m <- m + 1L
  }
  if (m > 0L) tot / m else Inf
}

#' Collaborative TMLE for the ATE.
#'
#' Builds a nested sequence of treatment mechanisms and selects by the
#' cross-validated loss of the targeted outcome fit, so an instrument
#' stays out of the propensity model. van der Laan & Rose (2018) Ch. 10
#' Example 10.3.
#' @export
tmle_cdrs <- function(y, D, X, tuning = "discrete", penalties = NULL,
                      n_folds = 5L, trim = 0.005, scale = NULL,
                      q_covariates = NULL) {
  seq_out <- ctmle_sequence(y, D, X, tuning = tuning,
                            penalties = penalties, trim = trim,
                            scale = scale, q_covariates = q_covariates)
  steps <- seq_out$steps
  info <- seq_out$info
  n <- info$n
  nf <- as.integer(n_folds)
  # Python: folds[f] = {i : i %% n_folds == f} over 0-based i
  folds <- lapply(0:(nf - 1L), function(f) {
    which(((seq_len(n) - 1L) %% nf) == f)
  })
  cv <- numeric(length(steps))
  for (s in seq_along(steps)) {
    tot <- 0.0
    for (f in folds) {
      tr <- setdiff(seq_len(n), f)
      tot <- tot + .tmlcds_refit_on(info, steps, s, tr, f, tuning,
                                    penalties, trim)
    }
    cv[s] <- tot / length(folds)
  }
  sel <- which.min(cv)
  best <- steps[[sel]]

  .RichResult(list(
    estimate = best$psi,
    psi = best$psi,
    selected = sel - 1L,                       # 0-based, matching Python
    selected_covariates = best$covariates,
    selected_penalty = best$penalty,
    steps = lapply(steps, function(st) st[setdiff(names(st), "g")]),
    cv_loss = cv,
    in_sample_loss = vapply(steps, function(st) st$loss, numeric(1)),
    epsilon = best$epsilon,
    tuning = tuning, n = n, n_covariates = info$p,
    method = sprintf(paste0("collaborative TMLE, van der Laan & Rose ",
                            "(2018) Ch. 10 Example 10.3 with %s tuning"),
                     tuning)
  ))
}

#' .tmlcds_cheatsheet
#'
#' A step of the tmlcds_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
.tmlcds_cheatsheet <- function() {
  paste0("tmlcds: collaborative TMLE. Build a NESTED sequence of ",
         "treatment mechanisms and select by the cross-validated loss of ",
         "the TARGETED OUTCOME fit, not of G itself -- so an instrument ",
         "stays out of the propensity model. Submodel logit Q* = logit Q ",
         "+ eps A/G (vdL & Rose 2018 Ch.10 Ex.10.3). tuning = discrete ",
         "(greedy covariates) or continuous (penalty).")
}

#' @rdname tmle_cdrs
#' @export
morie_tmlcds <- tmle_cdrs

# compact alias per ledger/NAMING.md
tmlecdrs <- tmle_cdrs
