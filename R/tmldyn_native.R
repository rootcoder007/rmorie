# CV-TMLE for the mean outcome under an optimal dynamic treatment rule.
# Sources: Luedtke, A. R. & van der Laan, M. J. (2018) Optimal Dynamic
# Treatment Rules, Ch. 22 in Targeted Learning in Data Science,
# Springer, pp. 399-419, doi:10.1007/978-3-319-65304-4_22 (Theorem
# 22.1 blip representation, eq. 22.4 EIC, Sec. 22.6 CV-TMLE);
# van der Laan, M. J. & Luedtke, A. R. (2015) Targeted Learning of
# the Mean Outcome Under an Optimal Dynamic Treatment Rule, Journal
# of Causal Inference 3(1), 61-95, doi:10.1515/jci-2013-0022;
# Robins, J. M. (2004) Optimal Structural Nested Models for Optimal
# Sequential Decisions, Springer LNS 179, pp. 189-326,
# doi:10.1007/978-1-4419-9076-1_11 (term "blip function",
# exceptional-law condition); Zheng, W. & van der Laan, M. J. (2011)
# Cross-Validated Targeted Minimum-Loss-Based Estimation, in Targeted
# Learning, Springer, pp. 459-474.
#
# Native implementation mirroring Python morie.fn.tmldyn exactly: the
# same backward induction with the second-stage rule carried into the
# first-stage contrast, the same H2 and H1 clever covariates, the same
# one-scalar-epsilon-per-fluctuation Newton update, the same static
# comparators, the same exceptional-law share, the same validation
# messages.

.TMLDYN_METHODS <- c("cv-tmle", "tmle", "ipw", "gcomp")
.EPS <- 1e-9

.tmldyn_logit <- function(p) {
  q <- min(max(as.numeric(p), .EPS), 1 - .EPS)
  log(q / (1 - q))
}

.tmldyn_expit <- function(z) {
  if (z > 700) 1 else if (z < -700) 0 else 1 / (1 + exp(-z))
}

.blocks <- function(covariate_history, n) {
  if (is.null(covariate_history))
    stop("tmldyn: covariate_history is required")
  ch <- as.list(covariate_history)
  if (length(ch) != 2L)
    stop("tmldyn: covariate_history must be two blocks [L0, L1]")
  L0 <- as.matrix(ch[[1]]); storage.mode(L0) <- "double"
  L1 <- as.matrix(ch[[2]]); storage.mode(L1) <- "double"
  if (nrow(L0) != n || nrow(L1) != n)
    stop("tmldyn: covariate blocks have ", nrow(L0), " and ",
         nrow(L1), " rows but there are ", n, " outcomes")
  list(L0 = L0, L1 = L1)
}

.project <- function(values, basis, n, ridge) {
  if (is.null(basis)) return(as.numeric(values))
  Z <- cbind(1, as.matrix(basis))
  storage.mode(Z) <- "double"
  b <- as.numeric(solve(crossprod(Z) + ridge * diag(ncol(Z)),
                         crossprod(Z, as.numeric(values))))
  as.numeric(Z %*% b)
}

#' The intervention mechanism
#'
#' \code{known} supplies the true probabilities of receiving treatment
#' -- the SMART case, where the design fixes them and nothing needs to
#' be estimated.
#'
#' @param L0 Covariate matrix at time 0.
#' @param A0 Treatment at time 0.
#' @param L1 Covariate matrix at time 1.
#' @param A1 Treatment at time 1.
#' @param trim Truncation lower bound for positivity.
#' @param known Optional \code{(p0, p1)} of known probabilities.
#' @param penalty Optional lasso penalty (unused unless overridden).
#' @return \code{g0}, \code{g1}, info with \code{p0}, \code{p1},
#'   \code{min_g0}, \code{min_g1}, \code{max_weight}, \code{known}.
#' @references Luedtke, A. R. & van der Laan, M. J. (2018).
#' @export
intervention_mechanism <- function(L0, A0, L1, A1, trim = 0.01,
                                    known = NULL, penalty = 0) {
  n <- length(A0)
  if (!is.null(known)) {
    p0 <- as.numeric(known[[1]]); p1 <- as.numeric(known[[2]])
    if (length(p0) != n || length(p1) != n)
      stop("tmldyn: known g has the wrong length")
  } else {
    X0 <- cbind(1, L0)
    b0 <- as.numeric(suppressWarnings(
      coef(glm(A0 ~ ., data = data.frame(X0[, -1, drop = FALSE]),
               family = binomial()))))
    p0 <- .tmldyn_expit(as.numeric(X0 %*% b0))
    X1 <- cbind(1, A0, L0, L1)
    b1 <- as.numeric(suppressWarnings(
      coef(glm(A1 ~ ., data = data.frame(X1[, -1, drop = FALSE]),
               family = binomial()))))
    p1 <- .tmldyn_expit(as.numeric(X1 %*% b1))
  }
  t <- as.numeric(trim)
  if (!(t >= 0 && t < 0.5))
    stop("tmldyn: trim must be in [0, 0.5)")
  lo <- max(t, .EPS); hi <- 1 - max(t, .EPS)
  p0 <- pmin(pmax(p0, lo), hi)
  p1 <- pmin(pmax(p1, lo), hi)
  g0 <- ifelse(A0 == 1, p0, 1 - p0)
  g1 <- ifelse(A1 == 1, p1, 1 - p1)
  list(g0 = g0, g1 = g1,
       info = list(p0 = p0, p1 = p1, min_g0 = min(g0),
                   min_g1 = min(g1),
                   max_weight = max(1 / (g0 * g1)),
                   known = !is.null(known)))
}

.fit_q2 <- function(y, L0, A0, L1, A1, idx, ridge) {
  p0 <- ncol(L0); p1 <- ncol(L1)
  row_q2 <- function(a0, a1, i) {
    r <- c(1, a0, a1, a0 * a1)
    r <- c(r, L0[i, ], L1[i, ])
    if (p1 > 0) r <- c(r, a1 * L1[i, ])
    if (p0 > 0) r <- c(r, a1 * L0[i, ])
    if (p0 > 0) r <- c(r, a0 * L0[i, ])
    r
  }
  X <- do.call(rbind, lapply(idx, function(i) row_q2(A0[i], A1[i], i)))
  b <- as.numeric(solve(crossprod(X) + ridge * diag(ncol(X)),
                         crossprod(X, y[idx])))
  q2 <- function(a0, a1, i) sum(b * row_q2(a0, a1, i))
  list(q2 = q2, b = b)
}

.fit_q1 <- function(pseudo, L0, A0, idx, ridge) {
  p0 <- ncol(L0)
  row_q1 <- function(a0, i) {
    r <- c(1, a0)
    r <- c(r, L0[i, ])
    if (p0 > 0) r <- c(r, a0 * L0[i, ])
    r
  }
  X <- do.call(rbind, lapply(idx, function(i) row_q1(A0[i], i)))
  b <- as.numeric(solve(crossprod(X) + ridge * diag(ncol(X)),
                         crossprod(X, pseudo[idx])))
  q1 <- function(a0, i) sum(b * row_q1(a0, i))
  list(q1 = q1, b = b)
}

#' Theorem 22.1: the two blip functions and the V-optimal rule
#'
#' Fitted on \code{idx} (all rows by default) and evaluated on
#' \code{eval_idx}. Splitting the two is what makes the cross-validated
#' estimator possible: the rule must not be read off the same rows it
#' is then scored on.
#'
#' @param y Outcome vector (rescaled).
#' @param L0 Covariate matrix at time 0.
#' @param A0 Treatment at time 0.
#' @param L1 Covariate matrix at time 1.
#' @param A1 Treatment at time 1.
#' @param V0 Summary for the first-stage rule.
#' @param V1 Summary for the second-stage rule.
#' @param ridge Ridge regulariser.
#' @param idx Rows to fit on.
#' @param eval_idx Rows to evaluate on.
#' @return A list with \code{blip1}, \code{blip2}, \code{d0},
#'   \code{d1}, \code{q2}, \code{q1}, \code{coef_q2}, \code{coef_q1},
#'   \code{pseudo}, \code{eval_idx}.
#' @references Luedtke, A. R. & van der Laan, M. J. (2018). Thm 22.1.
#' @export
sequential_blips <- function(y, L0, A0, L1, A1, V0 = NULL, V1 = NULL,
                             ridge = 1e-8, idx = NULL,
                             eval_idx = NULL) {
  n <- length(y)
  if (is.null(idx)) idx <- seq_len(n) else idx <- as.integer(idx)
  if (is.null(eval_idx)) eval_idx <- seq_len(n)
  fq2 <- .fit_q2(y, L0, A0, L1, A1, idx, ridge)
  q2 <- fq2$q2
  raw2 <- lapply(c(0, 1), function(a0)
    vapply(seq_len(n), function(i) q2(a0, 1, i) - q2(a0, 0, i),
            numeric(1)))
  basis1 <- if (is.null(V1)) L1 else as.matrix(V1)
  blip2 <- lapply(raw2, function(r) .project(r, basis1, n, ridge))
  d1 <- lapply(blip2, function(b) ifelse(b > 0, 1, 0))
  pseudo <- vapply(seq_len(n), function(i)
    q2(A0[i], d1[[as.integer(A0[i]) + 1L]][i], i), numeric(1))
  fq1 <- .fit_q1(pseudo, L0, A0, idx, ridge)
  q1 <- fq1$q1
  raw1 <- vapply(seq_len(n), function(i) q1(1, i) - q1(0, i),
                  numeric(1))
  basis0 <- if (is.null(V0)) L0 else as.matrix(V0)
  blip1 <- .project(raw1, basis0, n, ridge)
  d0 <- ifelse(blip1 > 0, 1, 0)
  list(blip1 = blip1, blip2 = blip2, d0 = d0, d1 = d1, q2 = q2,
       q1 = q1, coef_q2 = fq2$b, coef_q1 = fq1$b, pseudo = pseudo,
       eval_idx = eval_idx)
}

#' The estimated V-optimal rule, as (d0, d1)
#'
#' @param y Outcome vector.
#' @param L0 Covariate matrix at time 0.
#' @param A0 Treatment at time 0.
#' @param L1 Covariate matrix at time 1.
#' @param A1 Treatment at time 1.
#' @param V0 Summary for the first-stage rule.
#' @param V1 Summary for the second-stage rule.
#' @param ridge Ridge regulariser.
#' @return A list with \code{d0} and \code{d1}.
#' @references Luedtke, A. R. & van der Laan, M. J. (2018).
#' @export
optimal_rule <- function(y, L0, A0, L1, A1, V0 = NULL, V1 = NULL,
                          ridge = 1e-8) {
  r <- sequential_blips(y, L0, A0, L1, A1, V0 = V0, V1 = V1,
                        ridge = ridge)
  list(d0 = r$d0, d1 = r$d1)
}

#' Share of subjects whose blip sits within tol of zero
#'
#' @param blips Blip vector.
#' @param tol Tolerance.
#' @return Scalar in [0,1].
#' @references Robins, J. M. (2004).
#' @export
exceptional_law_share <- function(blips, tol = 0.01) {
  v <- abs(as.numeric(blips))
  if (length(v) == 0L) return(0)
  sum(v <= tol) / length(v)
}

.fluctuate <- function(outcome, offset_logit, H, rows, iters = 100,
                       tol = 1e-12) {
  if (length(rows) == 0L ||
      all(abs(H[rows]) < 1e-14)) return(0)
  e <- 0
  for (k in seq_len(as.integer(iters))) {
    num <- 0; den <- 0
    for (i in rows) {
      p <- .tmldyn_expit(offset_logit[i] + e * H[i])
      num <- num + H[i] * (outcome[i] - p)
      den <- den + H[i]^2 * p * (1 - p)
    }
    if (den < 1e-14) break
    step <- num / den
    e <- e + step
    if (abs(step) < tol) break
  }
  e
}

.tmldyn_folds <- function(n, n_folds) {
  J <- max(2L, min(as.integer(n_folds), n))
  lapply(seq_len(J) - 1L, function(j) which(seq_len(n) %% J == j))
}

#' Plug-in value of a given rule by sequential regression
#'
#' No targeting: this is the g-computation arm.
#'
#' @param y Outcome vector (rescaled).
#' @param L0 Covariate matrix at time 0.
#' @param A0 Treatment at time 0.
#' @param L1 Covariate matrix at time 1.
#' @param A1 Treatment at time 1.
#' @param d0 First-stage rule.
#' @param d1 Second-stage rule (two n-long branches).
#' @param g0 Treatment probability at time 0.
#' @param g1 Treatment probability at time 1.
#' @param ridge Ridge regulariser.
#' @return Scalar mean outcome.
#' @references Luedtke, A. R. & van der Laan, M. J. (2018).
#' @export
rule_value_seq <- function(y, L0, A0, L1, A1, d0, d1, g0, g1,
                            ridge = 1e-8) {
  n <- length(y)
  fq2 <- .fit_q2(y, L0, A0, L1, A1, seq_len(n), ridge)
  q2 <- fq2$q2
  pseudo <- vapply(seq_len(n), function(i)
    q2(A0[i], d1[[as.integer(A0[i]) + 1L]][i], i), numeric(1))
  fq1 <- .fit_q1(pseudo, L0, A0, seq_len(n), ridge)
  q1 <- fq1$q1
  mean(vapply(seq_len(n), function(i) q1(d0[i], i), numeric(1)))
}

.coerce_regime <- function(regime, n) {
  if (is.null(regime) || (is.character(regime) &&
      tolower(regime) %in% c("optimal", "v-optimal"))) return(NULL)
  if (is.character(regime))
    stop("tmldyn: regime must be 'optimal' or an array")
  r <- as.list(regime)
  if (length(r) == 2L && is.matrix(r[[1]]) == FALSE &&
      length(r[[1]]) == n) {
    d0 <- as.numeric(r[[1]])
    second <- r[[2]]
    if (length(second) == 2L && is.matrix(second) == FALSE &&
        length(second[[1]]) == n) {
      d1 <- lapply(second, function(s) as.numeric(s))
    } else if (length(second) == n) {
      d1 <- list(as.numeric(second), as.numeric(second))
    } else {
      stop("tmldyn: regime's second component has the wrong length")
    }
    return(list(d0 = d0, d1 = d1))
  }
  if (is.matrix(regime) && nrow(regime) == n && ncol(regime) == 2L) {
    d0 <- regime[, 1]
    col <- regime[, 2]
    return(list(d0 = d0, d1 = list(col, col)))
  }
  if (length(r) == n) {
    d0 <- vapply(r, function(rr) as.numeric(rr[1]), numeric(1))
    col <- vapply(r, function(rr) as.numeric(rr[2]), numeric(1))
    return(list(d0 = d0, d1 = list(col, col)))
  }
  stop("tmldyn: cannot read regime of length ", length(r), " for n = ", n)
}

#' Mean outcome under the (V-)optimal dynamic treatment rule
#'
#' \code{method} is one of \code{"cv-tmle"} (the default, Sec. 22.6),
#' \code{"tmle"} (same fluctuation without the split),
#' \code{"ipw"} (Horvitz-Thompson mean under the rule), \code{"gcomp"}
#' (the untargeted sequential regression).
#'
#' @param y Outcome vector.
#' @param treatment_history n-by-2 binary matrix.
#' @param covariate_history List \code{[L0, L1]}.
#' @param regime \code{"optimal"} or supplied rule.
#' @param method Method choice.
#' @param n_folds Number of folds.
#' @param V0 First-stage summary.
#' @param V1 Second-stage summary.
#' @param trim Propensity truncation.
#' @param known_g Optional known treatment probabilities.
#' @param ridge Ridge regulariser.
#' @param level Confidence level.
#' @return A list with \code{estimate}, \code{se}, \code{n}, \code{ci},
#'   \code{level}, \code{d0}, \code{d1}, \code{blip1}, \code{blip2},
#'   \code{treated_first}, \code{treated_second}, \code{eic_mean},
#'   \code{epsilon}, \code{max_weight}, \code{min_g0}, \code{min_g1},
#'   \code{known_g}, \code{exceptional_share_1},
#'   \code{exceptional_share_2}, \code{value_gcomp},
#'   \code{best_static}, \code{n_folds}, \code{method},
#'   \code{rule_source}, \code{algorithm}, plus four \code{static_*}
#'   keys.
#' @references Luedtke, A. R. & van der Laan, M. J. (2018).
#' @export
morie_tmldyn <- function(y, treatment_history, covariate_history,
                          regime = "optimal", method = "cv-tmle",
                          n_folds = 10, V0 = NULL, V1 = NULL,
                          trim = 0.01, known_g = NULL, ridge = 1e-8,
                          level = 0.95) {
  if (!method %in% .TMLDYN_METHODS)
    stop("tmldyn: method must be one of cv-tmle, tmle, ipw, gcomp")
  yv <- as.numeric(y); n <- length(yv)
  if (n < 4L) stop("tmldyn: need at least 4 observations")
  Am <- as.matrix(treatment_history); storage.mode(Am) <- "double"
  if (nrow(Am) != n || ncol(Am) != 2L)
    stop("tmldyn: treatment_history must be n-by-2")
  A0 <- Am[, 1]; A1 <- Am[, 2]
  if (any(!(A0 %in% c(0, 1))) || any(!(A1 %in% c(0, 1))))
    stop("tmldyn: treatments must be binary 0/1")
  bl <- .blocks(covariate_history, n)
  L0 <- bl$L0; L1 <- bl$L1
  ymin <- min(yv); rng <- max(yv) - ymin
  if (rng <= 0) stop("tmldyn: the outcome is constant")
  ys <- (yv - ymin) / rng
  im <- intervention_mechanism(L0, A0, L1, A1, trim = trim,
                                known = known_g)
  g0 <- im$g0; g1 <- im$g1; ginfo <- im$info
  supplied <- .coerce_regime(regime, n)
  if (!is.null(supplied)) {
    d0 <- supplied$d0; d1 <- supplied$d1
    full <- sequential_blips(ys, L0, A0, L1, A1, V0 = V0, V1 = V1,
                              ridge = ridge)
    blip1 <- full$blip1; blip2 <- full$blip2
    splits <- list(list(train = seq_len(n), val = seq_len(n)))
    rules <- list(list(d0 = d0, d1 = d1))
  } else if (method == "cv-tmle") {
    splits <- list(); rules <- list()
    d0 <- rep(0, n); d1 <- list(rep(0, n), rep(0, n))
    blip1 <- rep(0, n); blip2 <- list(rep(0, n), rep(0, n))
    for (val in .tmldyn_folds(n, n_folds)) {
      train <- setdiff(seq_len(n), val)
      fit <- sequential_blips(ys, L0, A0, L1, A1, V0 = V0, V1 = V1,
                               ridge = ridge, idx = train)
      splits[[length(splits) + 1L]] <- list(train = train, val = val)
      rules[[length(rules) + 1L]] <- list(d0 = fit$d0, d1 = fit$d1)
      for (i in val) {
        d0[i] <- fit$d0[i]; blip1[i] <- fit$blip1[i]
        for (a in 1:2) {
          d1[[a]][i] <- fit$d1[[a]][i]
          blip2[[a]][i] <- fit$blip2[[a]][i]
        }
      }
    }
  } else {
    fit <- sequential_blips(ys, L0, A0, L1, A1, V0 = V0, V1 = V1,
                             ridge = ridge)
    d0 <- fit$d0; d1 <- fit$d1
    blip1 <- fit$blip1; blip2 <- fit$blip2
    splits <- list(list(train = seq_len(n), val = seq_len(n)))
    rules <- list(list(d0 = d0, d1 = d1))
  }
  follow0 <- ifelse(A0 == d0, 1, 0)
  follow1 <- ifelse(A1 == d1[[as.integer(A0) + 1L]], 1, 0)
  H1 <- follow0 / g0
  H2 <- follow0 * follow1 / (g0 * g1)
  if (method == "ipw") {
    psi_s <- sum(H2 * ys) / n
    eic <- H2 * ys - psi_s
    q2d <- ys; q1d <- rep(psi_s, n)
    eps1 <- 0; eps2 <- 0
  } else {
    q2d <- rep(0, n); q1d <- rep(0, n)
    for (k in seq_along(splits)) {
      tr <- splits[[k]]$train; val <- splits[[k]]$val
      rd0 <- rules[[k]]$d0; rd1 <- rules[[k]]$d1
      fq2 <- .fit_q2(ys, L0, A0, L1, A1, tr, ridge)
      q2 <- fq2$q2
      pseudo <- vapply(seq_len(n), function(i)
        q2(A0[i], rd1[[as.integer(A0[i]) + 1L]][i], i), numeric(1))
      fq1 <- .fit_q1(pseudo, L0, A0, tr, ridge)
      q1 <- fq1$q1
      for (i in val) {
        q2d[i] <- min(max(q2(rd0[i], rd1[[as.integer(rd0[i]) + 1L]][i],
                              i), .EPS), 1 - .EPS)
        q1d[i] <- min(max(q1(rd0[i], i), .EPS), 1 - .EPS)
      }
    }
    if (method == "gcomp") {
      psi_s <- mean(q1d); eic <- q1d - psi_s
      eps1 <- 0; eps2 <- 0
    } else {
      off2 <- vapply(q2d, .tmldyn_logit, numeric(1))
      eps2 <- .fluctuate(ys, off2, H2, seq_len(n))
      q2d <- .tmldyn_expit(off2 + eps2 * H2)
      off1 <- vapply(q1d, .tmldyn_logit, numeric(1))
      eps1 <- .fluctuate(q2d, off1, H1, seq_len(n))
      q1d <- .tmldyn_expit(off1 + eps1 * H1)
      psi_s <- mean(q1d)
      eic <- (q1d - psi_s) + H1 * (q2d - q1d) + H2 * (ys - q2d)
    }
  }
  psi <- ymin + rng * psi_s
  se <- if (n > 1L) sd(eic) * rng / sqrt(n) else NaN
  z <- qnorm(0.5 + 0.5 * as.numeric(level))
  static <- list()
  for (a0 in c(0, 1)) for (a1 in c(0, 1)) {
    kk <- paste0("static_", a0, a1)
    static[[kk]] <- ymin + rng *
      rule_value_seq(ys, L0, A0, L1, A1, rep(a0, n),
                     list(rep(a1, n), rep(a1, n)), g0, g1, ridge)
  }
  out <- list(estimate = psi, se = se, n = n,
              ci = c(psi - z * se, psi + z * se),
              level = as.numeric(level),
              d0 = d0, d1 = d1, blip1 = blip1, blip2 = blip2,
              treated_first = mean(d0),
              treated_second = mean(d1[[as.integer(A0) + 1L]]),
              eic_mean = mean(eic), epsilon = c(eps1, eps2),
              max_weight = ginfo$max_weight,
              min_g0 = ginfo$min_g0, min_g1 = ginfo$min_g1,
              known_g = ginfo$known,
              exceptional_share_1 = exceptional_law_share(blip1),
              exceptional_share_2 = max(
                exceptional_law_share(blip2[[1]]),
                exceptional_law_share(blip2[[2]])),
              value_gcomp = ymin + rng * mean(q1d),
              best_static = max(unlist(static)),
              n_folds = length(splits), method = method,
              rule_source = if (!is.null(supplied)) "supplied"
                            else "estimated",
              algorithm = paste0("CV-TMLE for the mean outcome under ",
                                 "the V-optimal dynamic rule, Luedtke ",
                                 "& van der Laan (2018) Thm 22.1 and ",
                                 "Sec. 22.6"))
  out <- c(out, static)
  out
}

#' Compact alias per ledger/NAMING.md
#' @export
morie_tmledynamicregime <- morie_tmldyn
