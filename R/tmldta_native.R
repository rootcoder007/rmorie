# Data-adaptive target parameters: honest inference after snooping.
# Sources: Hubbard, A. E., Kennedy, C. J. & van der Laan, M. J. (2018)
# Data-Adaptive Target Parameters, Ch. 9 in Targeted Learning in Data
# Science, Springer, pp. 125-142, doi:10.1007/978-3-319-65304-4_9
# (eq. 9.1-9.16); Hubbard, A. E., Kherad-Pajouh, S. & van der Laan,
# M. J. (2016) Statistical Inference for Data Adaptive Target
# Parameters, International Journal of Biostatistics 12(1), 3-19,
# doi:10.1515/ijb-2015-0013 (sample-splitting theory); van der Laan,
# M. J. & Luedtke, A. R. (2015) Targeted Learning of the Mean Outcome
# Under an Optimal Dynamic Treatment Rule, Journal of Causal Inference
# 3(1), 61-95, doi:10.1515/jci-2013-0022 (CV-TMLE for data-adaptive
# parameters); Zheng, W. & van der Laan, M. J. (2011) Cross-Validated
# Targeted Minimum-Loss-Based Estimation, in Targeted Learning, Springer,
# pp. 459-474.
#
# Native implementation mirroring Python morie.fn.tmldta exactly: the
# same argmin/argmax level discovery on the parameter-generating sample,
# the same Q(a, W) with level dummies and level-by-W interactions, the
# same three-category propensity normalisation, the same eq. 9.14 / 9.15
# aggregation, the same near-tie diagnostics, the same validation
# messages.

.TMLDTA_METHODS <- c("cv-tmle", "sample-split", "naive")
.tmldta_EPS <- 1e-9

#' .tmldta_logit
#'
#' A step of the tmldta_native implementation. Called by \code{split_specific_tmle}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param p Coerced to numeric by the body, with \code{as.numeric}.
#' @return A numeric value.
#' @export
.tmldta_logit <- function(p) {
  q <- min(max(as.numeric(p), .tmldta_EPS), 1 - .tmldta_EPS)
  log(q / (1 - q))
}

#' .tmldta_expit
#'
#' A step of the tmldta_native implementation. Called by \code{.fit_g}, \code{.fit_q}, \code{split_specific_tmle}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Numeric; combined arithmetically in the body.
#' @return One of two values, depending on the branch taken.
#' @export
.tmldta_expit <- function(x) if (x > -700) 1 / (1 + exp(-x)) else 0

#' .levels
#'
#' A step of the tmldta_native implementation. Called by \code{morie_tmldta}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param A Coerced to numeric by the body, with \code{as.numeric}.
#' @param candidate_strata Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @return The value of \code{lv}, as built in the body.
#' @export
.levels <- function(A, candidate_strata) {
  if (!is.null(candidate_strata)) {
    lv <- as.numeric(candidate_strata)
    lv <- lv[!duplicated(lv)]
  } else {
    lv <- sort(unique(as.numeric(A)))
  }
  if (length(lv) < 2L) stop("tmldta: need at least 2 exposure levels")
  lv
}

#' .fit_q
#'
#' A step of the tmldta_native implementation. Called by \code{discover_levels}, \code{split_specific_tmle}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y A vector; its length is taken and its elements indexed.
#' @param A A vector; indexed elementwise.
#' @param W A matrix; indexed by row and column.
#' @param levels A vector; indexed elementwise.
#' @param rows Iterated over elementwise, with \code{lapply}.
#' @param ridge Accepted by the signature and not used anywhere in the body.
#' @return A list with \code{q}, \code{b}.
#' @export
.fit_q <- function(y, A, W, levels, rows, ridge) {
  ref <- levels[1]; others <- levels[-1]
  n <- length(y); p <- ncol(W)
  design_row <- function(a, i) {
    d <- as.numeric(levels( factor(rep(0, length(others)),
                                    levels = seq_along(others)) ) == 0)
    for (k in seq_along(others)) d[k] <- if (a == others[k]) 1 else 0
    if (p > 0) {
      r <- c(1, d, W[i, ], unlist(lapply(seq_along(others), function(k)
        d[k] * W[i, ])))
    } else {
      r <- c(1, d)
    }
    r
  }
  X <- do.call(rbind, lapply(rows, function(i) design_row(A[i], i)))
  b <- as.numeric(suppressWarnings(
    coef(glm(y[rows] ~ X - 1, family = binomial()))))
  q_fn <- function(a, i) {
    r <- design_row(a, i)
    .tmldta_expit(sum(r * b))
  }
  list(q = q_fn, b = b)
}

#' .fit_g
#'
#' A step of the tmldta_native implementation. Called by \code{split_specific_tmle}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param A A vector; its length is taken.
#' @param W A matrix; passed to \code{ncol}.
#' @param aL See Usage.
#' @param aH See Usage.
#' @param rows See Usage.
#' @param ridge Accepted by the signature and not used anywhere in the body.
#' @param trim Numeric; passed to \code{max}.
#' @return A list with \code{gH}, \code{gL}.
#' @export
.fit_g <- function(A, W, aL, aH, rows, ridge, trim) {
  n <- length(A)
  X <- if (ncol(W) > 0) cbind(1, W) else matrix(1, nrow = n, ncol = 1)
  cat_fit <- function(mask) {
    b <- as.numeric(suppressWarnings(
      coef(glm(mask[rows] ~ .,
               data = data.frame(X[rows, , drop = FALSE]),
               family = binomial()))))
    .tmldta_expit(as.numeric(X %*% b))
  }
  pH <- cat_fit(ifelse(A == aH, 1, 0))
  pL <- cat_fit(ifelse(A == aL, 1, 0))
  pO <- cat_fit(ifelse(!(A %in% c(aH, aL)), 1, 0))
  gH <- gL <- numeric(n)
  for (i in seq_len(n)) {
    tot <- pH[i] + pL[i] + pO[i]
    if (tot <= 0) { gH[i] <- 0.5; gL[i] <- 0.5; next }
    gH[i] <- min(max(pH[i] / tot, trim), 1 - trim)
    gL[i] <- min(max(pL[i] / tot, trim), 1 - trim)
  }
  list(gH = gH, gL = gL)
}

#' Eq. (9.2)-(9.3): the levels that minimise and maximise the mean
#' predicted outcome
#'
#' @param y Outcome vector.
#' @param A Exposure vector.
#' @param W Covariate matrix.
#' @param levels Candidate levels.
#' @param rows Rows to fit Q on.
#' @param eval_rows Rows the mean is taken over.
#' @param ridge Ridge regulariser.
#' @return \code{aL}, \code{aH}, info with \code{means} and
#'   \code{spread}.
#' @references Hubbard, A. E. et al. (2018).
#' @export
discover_levels <- function(y, A, W, levels, rows = NULL,
                            eval_rows = NULL, ridge = 1e-8) {
  n <- length(y)
  if (is.null(rows)) rows <- seq_len(n) else rows <- as.integer(rows) + 1L
  if (is.null(eval_rows)) eval_rows <- rows
  fq <- .fit_q(y, A, W, levels, rows, ridge)
  means <- vapply(levels, function(a)
    sum(vapply(eval_rows, function(i) fq$q(a, i), numeric(1))) /
      length(eval_rows), numeric(1))
  names(means) <- as.character(levels)
  aL <- levels[which.min(means)]
  aH <- levels[which.max(means)]
  list(aL = aL, aH = aH,
       info = list(means = means, spread = max(means) - min(means)))
}

#' Eq. (9.9)-(9.13): one split's TMLE at fixed levels
#'
#' @param y Outcome vector.
#' @param A Exposure vector.
#' @param W Covariate matrix.
#' @param levels Candidate levels.
#' @param aL Lower level.
#' @param aH Upper level.
#' @param fit_rows Rows to fit Q and g on.
#' @param est_rows Rows to fit the fluctuation and average over.
#' @param ridge Ridge regulariser.
#' @param trim Propensity trimming.
#' @param target Whether to target.
#' @return \code{psi}, \code{D}, info with \code{eps} and
#'   \code{max_weight}.
#' @references Hubbard, A. E. et al. (2018).
#' @export
split_specific_tmle <- function(y, A, W, levels, aL, aH,
                                fit_rows, est_rows,
                                ridge = 1e-8, trim = 0.01,
                                target = TRUE) {
  n <- length(y)
  fq <- .fit_q(y, A, W, levels, fit_rows, ridge)
  fg <- .fit_g(A, W, aL, aH, fit_rows, ridge, trim)
  gH <- fg$gH; gL <- fg$gL
  H <- ifelse(A == aH, 1 / gH, 0) - ifelse(A == aL, 1 / gL, 0)
  off <- vapply(seq_len(n), function(i) .tmldta_logit(fq$q(A[i], i)),
                 numeric(1))
  eps <- 0
  if (target) {
    e <- 0
    for (it in seq_len(60L)) {
      p <- .tmldta_expit(off[est_rows] + e * H[est_rows])
      gr <- sum(H[est_rows] * (y[est_rows] - p))
      he <- sum(H[est_rows]^2 * p * (1 - p))
      if (he < 1e-12) break
      step <- gr / he
      e <- e + step
      if (abs(step) < 1e-12) break
    }
    eps <- e
  }
  qstar <- function(a, i) {
    h <- if (a == aH) 1 / gH[i] else -1 / gL[i]
    .tmldta_expit(.tmldta_logit(fq$q(a, i)) + eps * h)
  }
  m <- length(est_rows)
  psi <- sum(vapply(est_rows, function(i)
    qstar(aH, i) - qstar(aL, i), numeric(1))) / m
  D <- numeric(m)
  for (k in seq_along(est_rows)) {
    i <- est_rows[k]
    resid <- .tmldta_expit(off[i] + eps * H[i])
    D[k] <- H[i] * (y[i] - resid) + qstar(aH, i) - qstar(aL, i) - psi
  }
  names(D) <- as.character(est_rows)
  max_w <- max(vapply(est_rows, function(i)
    max(1 / gH[i], 1 / gL[i]), numeric(1)))
  list(psi = psi, D = D, info = list(eps = eps, max_weight = max_w))
}

#' .tmldta_folds
#'
#' A step of the tmldta_native implementation. Called by \code{morie_tmldta}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n A count; the body uses it as \code{seq_len(...)}.
#' @param n_folds Coerced to integer by the body, with \code{as.integer}.
#' @return The value of \code{lapply}.
#' @export
.tmldta_folds <- function(n, n_folds) {
  V <- max(2L, min(as.integer(n_folds), n))
  lapply(seq_len(V) - 1L, function(v) which(seq_len(n) %% V == v))
}

#' Contrast between data-discovered exposure levels, done honestly
#'
#' \code{cv-tmle} fits Q and g on the parameter-generating split and
#' only the fluctuation on the estimation split. \code{sample-split}
#' fits everything on the estimation sample. \code{naive} is the
#' substitution estimator 9.5 with no split at all -- kept because its
#' bias is the point of the chapter.
#'
#' @param y Outcome vector.
#' @param D Exposure vector.
#' @param X Covariate matrix W.
#' @param candidate_strata Candidate levels.
#' @param method One of \code{"cv-tmle"}, \code{"sample-split"},
#'   \code{"naive"}.
#' @param n_folds Number of folds.
#' @param trim Propensity trimming.
#' @param ridge Ridge regulariser.
#' @param level Confidence level.
#' @param bounds Optional \code{c(lo, hi)}; otherwise inferred.
#' @return A list with \code{estimate}, \code{se}, \code{n},
#'   \code{ci}, \code{level}, \code{levels_by_split},
#'   \code{level_counts}, \code{modal_levels}, \code{level_agreement},
#'   \code{separation}, \code{near_tie}, \code{level_means},
#'   \code{split_estimates}, \code{n_splits}, \code{epsilon},
#'   \code{candidate_levels}, \code{method}, \code{sigma},
#'   \code{algorithm}.
#' @references Hubbard, A. E. et al. (2018).
#' @export
morie_tmldta <- function(y, D, X, candidate_strata = NULL,
                         method = "cv-tmle", n_folds = 10,
                         trim = 0.01, ridge = 1e-8, level = 0.95,
                         bounds = NULL) {
  if (!method %in% .TMLDTA_METHODS)
    stop("tmldta: method must be one of cv-tmle, sample-split, naive")
  yv <- as.numeric(y); Av <- as.numeric(D); n <- length(yv)
  if (length(Av) != n) stop("tmldta: outcome and exposure differ in length")
  Wm <- if (is.null(X)) matrix(0, nrow = n, ncol = 0) else as.matrix(X)
  storage.mode(Wm) <- "double"
  if (nrow(Wm) != n) stop("tmldta: covariate rows and outcomes differ in length")
  tr <- as.numeric(trim)
  if (!(tr > 0 && tr < 0.5)) stop("tmldta: trim must be in (0, 0.5)")
  if (n < 8L) stop("tmldta: need at least 8 observations")
  lv <- .levels(Av, candidate_strata)
  missing <- lv[!vapply(lv, function(a) any(Av == a), logical(1))]
  if (length(missing) > 0L)
    stop("tmldta: candidate levels never occur")
  lo <- if (is.null(bounds)) min(yv) else as.numeric(bounds)[1]
  hi <- if (is.null(bounds)) max(yv) else as.numeric(bounds)[2]
  rng <- hi - lo
  if (rng <= 0) stop("tmldta: the outcome has no range")
  if (any(yv < lo - 1e-12 | yv > hi + 1e-12))
    stop("tmldta: an outcome falls outside bounds")
  ys <- pmin(pmax((yv - lo) / rng, 0), 1)
  all_rows <- seq_len(n)
  if (method == "naive") {
    dl <- discover_levels(ys, Av, Wm, lv, all_rows, all_rows, ridge)
    aL <- dl$aL; aH <- dl$aH; dinfo <- dl$info
    ss <- split_specific_tmle(ys, Av, Wm, lv, aL, aH, all_rows,
                              all_rows, ridge, trim, target = FALSE)
    splits <- list(list(aL = aL, aH = aH, estimate = rng * ss$psi,
                        n_est = n))
    sigma2 <- sum(ss$D^2) / n
    psi_hat <- ss$psi
    eps_all <- c(0)
  } else {
    folds <- .tmldta_folds(n, n_folds)
    splits <- list(); per_split <- list(); ics <- list(); eps_all <- c()
    for (est in folds) {
      gen <- setdiff(all_rows, est)
      if (length(gen) == 0L || length(est) == 0L) next
      dl <- discover_levels(ys, Av, Wm, lv, gen, gen, ridge)
      aL <- dl$aL; aH <- dl$aH
      fit <- if (method == "cv-tmle") gen else est
      ss <- split_specific_tmle(ys, Av, Wm, lv, aL, aH, fit, est,
                                ridge, trim)
      per_split[[length(per_split) + 1L]] <- ss$psi
      eps_all <- c(eps_all, ss$info$eps)
      ics[[length(ics) + 1L]] <- ss$D
      splits[[length(splits) + 1L]] <- list(aL = aL, aH = aH,
        estimate = rng * ss$psi, n_est = length(est))
    }
    if (length(per_split) == 0L) stop("tmldta: no usable splits")
    psi_hat <- mean(unlist(per_split))
    sigma2 <- sum(vapply(ics, function(ic) sum(ic^2) / length(ic),
                          numeric(1))) / length(ics)
  }
  psi <- rng * psi_hat
  se <- rng * sqrt(sigma2 / n)
  z <- qnorm(0.5 + 0.5 * as.numeric(level))
  chosen <- list()
  for (sp in splits) {
    kk <- paste0(sp$aL, "|", sp$aH)
    chosen[[kk]] <- if (is.null(chosen[[kk]])) 1L
                    else chosen[[kk]] + 1L
  }
  modal_kk <- names(which.max(unlist(lapply(chosen, identity))))
  modal <- as.numeric(strsplit(modal_kk, "|", fixed = TRUE)[[1]])
  agreement <- chosen[[modal_kk]] / length(splits)
  dl_all <- discover_levels(ys, Av, Wm, lv, all_rows, all_rows, ridge)
  ordered_means <- sort(unlist(dl_all$info$means))
  separation <- min(ordered_means[2] - ordered_means[1],
                    ordered_means[length(ordered_means)] -
                      ordered_means[length(ordered_means) - 1]) * rng
  level_means <- dl_all$info$means * rng + lo
  list(estimate = psi, se = se, n = n,
       ci = c(psi - z * se, psi + z * se), level = as.numeric(level),
       levels_by_split = lapply(splits, function(sp) c(sp$aL, sp$aH)),
       level_counts = chosen, modal_levels = modal,
       level_agreement = agreement, separation = separation,
       near_tie = (separation < 2 * se) || (agreement < 0.6),
       level_means = level_means,
       split_estimates = vapply(splits, function(sp) sp$estimate,
                                 numeric(1)),
       n_splits = length(splits), epsilon = eps_all,
       candidate_levels = lv, method = method,
       sigma = sqrt(sigma2) * rng,
       algorithm = paste0("data-adaptive target parameter, Hubbard, ",
                          "Kennedy & van der Laan (2018) Ch. 9 eq. ",
                          "(9.2)-(9.16)"))
}

#' Loop the contrast over every column of X in turn
#'
#' @param y Outcome vector.
#' @param X Covariate matrix.
#' @param candidate_strata Candidate levels.
#' @param method One of \code{"cv-tmle"}, \code{"sample-split"},
#'   \code{"naive"}.
#' @param n_folds Number of folds.
#' @param names Optional column names.
#' @param ... Passed through to \code{tmle_data_adaptive}.
#' @return A list of per-variable results, sorted by absolute estimate.
#' @references Hubbard, A. E. et al. (2018).
#' @export
morie_variable_importance <- function(y, X, candidate_strata = NULL,
                                      method = "cv-tmle",
                                      n_folds = 10, names = NULL, ...) {
  Xm <- as.matrix(X); storage.mode(Xm) <- "double"
  n <- nrow(Xm); p <- ncol(Xm)
  if (p < 2L) stop("variable_importance: need at least 2 columns")
  nm <- if (is.null(names)) paste0("X", seq_len(p)) else as.character(names)
  if (length(nm) != p)
    stop("variable_importance: names and columns differ in length")
  out <- list()
  for (j in seq_len(p)) {
    A <- Xm[, j]
    W <- if (p > 1L) Xm[, -j, drop = FALSE] else
           matrix(0, nrow = n, ncol = 0)
    r <- morie_tmldta(y, A, W, candidate_strata = candidate_strata,
                      method = method, n_folds = n_folds, ...)
    out[[length(out) + 1L]] <- list(variable = nm[j], index = j,
      estimate = r$estimate, se = r$se, ci = r$ci,
      levels = r$modal_levels)
  }
  ord <- order(-abs(vapply(out, function(d) d$estimate, numeric(1))))
  out <- out[ord]
  for (rank in seq_along(out)) out[[rank]]$rank <- rank
  out
}

#' Compact alias per ledger/NAMING.md
#' @export
morie_tmledataadaptive <- morie_tmldta
