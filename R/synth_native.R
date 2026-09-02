# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Native synthetic-control engines (feat/native-specializations,
# module 15). Replaces Synth / coresynth with base-R implementations:
#
#   * Abadie-Gardeazabal (2003) / Abadie-Diamond-Hainmueller (2010)
#     synthetic control: constrained QP over the donor simplex solved
#     by accelerated projected gradient with exact simplex projection,
#     nested V-matrix optimization via stats::optim, in-space placebo
#     inference (RMSPE ratios).
#   * Arkhangelsky et al. (2021) synthetic difference-in-differences:
#     ridge-regularized unit and time weights (Algorithm 1 of the
#     paper) with placebo / jackknife / bootstrap variance.

#' Internal helper: Euclidean projection onto the probability simplex
#'
#' Held-Wolfe-Crowder algorithm; exact, O(n log n).
#' @noRd
.morie_simplex_proj <- function(v) {
  n <- length(v)
  u <- sort(v, decreasing = TRUE)
  css <- cumsum(u)
  rho <- max(which(u + (1 - css) / seq_len(n) > 0))
  theta <- (css[rho] - 1) / rho
  pmax(v - theta, 0)
}

#' Internal helper: simplex-constrained least squares
#'
#' Minimizes ||b - A w||_2^2 (+ zeta^2 * n_reg * ||w||_2^2) subject to
#' w >= 0, sum(w) = 1, by accelerated projected gradient (FISTA) with
#' exact simplex projection. Deterministic.
#'
#' @srrstats {G3.0} Convergence is a fixed tolerance on the objective
#'   decrease, never floating-point equality.
#' @noRd
.morie_simplex_ls <- function(A, b, zeta2 = 0, max_iter = 10000L,
                              tol = 1e-12) {
  n <- ncol(A)
  if (n == 1L) return(1)
  AtA <- crossprod(A)
  if (zeta2 > 0) AtA <- AtA + diag(zeta2, n)
  Atb <- as.numeric(crossprod(A, b))
  L <- max(eigen(AtA, symmetric = TRUE, only.values = TRUE)$values)
  if (L <= 0) return(rep(1 / n, n))
  w <- rep(1 / n, n)
  y <- w
  t_k <- 1
  obj <- function(w) sum(w * (AtA %*% w)) / 2 - sum(Atb * w)
  f_old <- obj(w)
  for (it in seq_len(max_iter)) {
    grad <- as.numeric(AtA %*% y) - Atb
    w_new <- .morie_simplex_proj(y - grad / L)
    t_new <- (1 + sqrt(1 + 4 * t_k^2)) / 2
    y <- w_new + ((t_k - 1) / t_new) * (w_new - w)
    w <- w_new
    t_k <- t_new
    f_new <- obj(w)
    if (abs(f_old - f_new) < tol * max(1, abs(f_old))) break
    f_old <- f_new
  }
  w
}

#' Internal helper: synthetic-control weights for one treated unit
#'
#' `X1` (k-vector) treated predictors, `X0` (k x J) donor predictors,
#' `Z1` (T0-vector) treated pre-period outcomes, `Z0` (T0 x J) donor
#' pre-period outcomes. Nested optimization: the outer loop picks the
#' diagonal V (softmax-parameterized, via optim/Nelder-Mead) that
#' minimizes pre-period outcome MSPE of the inner simplex-LS solution.
#'
#' @srrstats {G1.0} Abadie & Gardeazabal (2003); Abadie, Diamond &
#'   Hainmueller (2010), JASA 105(490).
#' @noRd
.morie_scm_weights <- function(X1, X0, Z1, Z0, optimize_v = TRUE) {
  k <- length(X1)
  # Standardize predictor rows (Abadie's scaling) so V is comparable.
  sds <- apply(cbind(X1, X0), 1L, stats::sd)
  sds[sds == 0 | !is.finite(sds)] <- 1
  X1s <- X1 / sds
  X0s <- X0 / sds
  solve_w <- function(v_diag) {
    sv <- sqrt(v_diag)
    .morie_simplex_ls(X0s * sv, X1s * sv)
  }
  mspe <- function(w) mean((Z1 - as.numeric(Z0 %*% w))^2)
  v0 <- rep(1 / k, k)
  w_eq <- solve_w(v0)
  best <- list(v = v0, w = w_eq, mspe = mspe(w_eq))
  if (optimize_v && k > 1L) {
    fn <- function(theta) {
      v <- exp(theta) / sum(exp(theta))
      mspe(solve_w(v))
    }
    opt <- tryCatch(
      stats::optim(rep(0, k), fn, method = "Nelder-Mead",
                   control = list(maxit = 500L)),
      error = function(e) NULL)
    if (!is.null(opt) && is.finite(opt$value) && opt$value < best$mspe) {
      v_opt <- exp(opt$par) / sum(exp(opt$par))
      best <- list(v = v_opt, w = solve_w(v_opt), mspe = opt$value)
    }
  }
  best
}

#' Internal helper: long panel -> unit x time outcome matrix
#' @noRd
.morie_synth_matrix <- function(df, outcome, unit, time) {
  units <- sort(unique(df[[unit]]))
  times <- sort(unique(df[[time]]))
  Y <- matrix(NA_real_, length(units), length(times),
              dimnames = list(as.character(units), as.character(times)))
  Y[cbind(match(df[[unit]], units), match(df[[time]], times))] <-
    as.numeric(df[[outcome]])
  if (anyNA(Y)) {
    stop("Synthetic control requires a balanced panel: every unit ",
         "must be observed in every period.", call. = FALSE)
  }
  Y
}

#' Native synthetic control (Abadie-Diamond-Hainmueller)
#'
#' Builds a synthetic version of the treated unit as a convex
#' combination of donor units, chosen so the synthetic unit tracks the
#' treated unit's pre-treatment outcomes (and optional predictors).
#' The donor-weight problem is the constrained QP of Abadie &
#' Gardeazabal (2003), solved by accelerated projected gradient on the
#' simplex; the predictor-weight matrix V is optimized by nested
#' minimization of pre-period MSPE. In-space placebo inference
#' reassigns treatment to every donor and compares post/pre RMSPE
#' ratios (Abadie, Diamond & Hainmueller 2010).
#'
#' @param data Long panel data frame (balanced).
#' @param outcome,unit,time Column names.
#' @param treated_unit The identifier of the treated unit.
#' @param treatment_time First treated period.
#' @param predictors Optional character vector of predictor columns
#'   (averaged over the pre-period). Pre-period outcomes are always
#'   included as predictors.
#' @param optimize_v Optimize the predictor-weight matrix V (default
#'   TRUE); FALSE uses equal weights on standardized predictors.
#' @return An object of class \code{morie_synth}: a list with
#'   \code{weights} (named donor weights), \code{donor_pool},
#'   \code{treated_unit}, \code{time_series} (data frame with observed,
#'   synthetic, and gap paths), \code{att} (mean post-period gap),
#'   \code{pre_rmspe}, \code{post_rmspe}, \code{rmspe_ratio},
#'   \code{placebo_pvalue} and \code{placebo_ratios}, \code{v_weights},
#'   \code{method}.
#' @references Abadie, A., Diamond, A., & Hainmueller, J. (2010).
#'   Synthetic control methods for comparative case studies.
#'   \emph{JASA}, 105(490), 493--505.
#' @examples
#' pan <- expand.grid(unit = letters[1:6], time = 1:10)
#' pan$y <- rnorm(nrow(pan)) + as.integer(pan$time) * 0.2 +
#'   ifelse(pan$unit == "a" & pan$time >= 7, 2, 0)
#' fit <- morie_synth_control(pan, "y", "unit", "time",
#'                            treated_unit = "a", treatment_time = 7)
#' fit$att
#' @export
morie_synth_control <- function(data, outcome, unit, time,
                                treated_unit, treatment_time,
                                predictors = NULL,
                                optimize_v = TRUE) {
  df <- as.data.frame(data)
  Y <- .morie_synth_matrix(df, outcome, unit, time)
  times <- as.numeric(colnames(Y))
  pre <- times < treatment_time
  post <- !pre
  if (sum(pre) < 2L) {
    stop("Need at least two pre-treatment periods.", call. = FALSE)
  }
  tu <- as.character(treated_unit)
  if (!tu %in% rownames(Y)) {
    stop("treated_unit not found in `", unit, "`.", call. = FALSE)
  }
  donors <- setdiff(rownames(Y), tu)
  build_predictors <- function(target, pool) {
    # Pre-period outcomes are always predictors; user predictors are
    # pre-period means.
    X1 <- Y[target, pre]
    X0 <- t(Y[pool, pre, drop = FALSE])
    if (length(predictors)) {
      pre_df <- df[df[[time]] < treatment_time, , drop = FALSE]
      for (p in predictors) {
        mns <- tapply(as.numeric(pre_df[[p]]), as.character(pre_df[[unit]]),
                      mean, na.rm = TRUE)
        X1 <- c(X1, mns[[target]])
        X0 <- rbind(X0, mns[pool])
      }
    }
    list(X1 = as.numeric(X1), X0 = unname(as.matrix(X0)))
  }
  fit_one <- function(target, pool) {
    px <- build_predictors(target, pool)
    sol <- .morie_scm_weights(px$X1, px$X0,
                              Z1 = Y[target, pre],
                              Z0 = t(Y[pool, pre, drop = FALSE]),
                              optimize_v = optimize_v)
    synth <- as.numeric(t(Y[pool, , drop = FALSE]) %*% sol$w)
    gap <- Y[target, ] - synth
    pre_rmspe <- sqrt(mean(gap[pre]^2))
    post_rmspe <- sqrt(mean(gap[post]^2))
    list(w = sol$w, v = sol$v, synth = synth, gap = gap,
         pre_rmspe = pre_rmspe, post_rmspe = post_rmspe,
         ratio = post_rmspe / max(pre_rmspe, .Machine$double.eps))
  }
  main <- fit_one(tu, donors)
  # In-space placebos: every donor takes a turn as pseudo-treated.
  placebo_ratios <- vapply(donors, function(d) {
    fit_one(d, setdiff(rownames(Y), d))$ratio
  }, numeric(1))
  all_ratios <- c(main$ratio, placebo_ratios)
  pval <- mean(all_ratios >= main$ratio)
  ts <- data.frame(time = times,
                   observed = as.numeric(Y[tu, ]),
                   synthetic = main$synth,
                   gap = main$gap,
                   post = post)
  structure(
    list(
      weights = stats::setNames(main$w, donors),
      donor_pool = donors,
      treated_unit = treated_unit,
      time_series = ts,
      att = mean(main$gap[post]),
      pre_rmspe = main$pre_rmspe,
      post_rmspe = main$post_rmspe,
      rmspe_ratio = main$ratio,
      placebo_pvalue = pval,
      placebo_ratios = stats::setNames(placebo_ratios, donors),
      v_weights = main$v,
      method = "synthetic_control (rmorie native)"
    ),
    class = c("morie_synth", "list")
  )
}

#' Print method for \code{morie_synth} objects
#'
#' @param x A \code{morie_synth} object.
#' @param ... Ignored; accepted for S3 consistency.
#' @examples
#' \donttest{
#' pan <- expand.grid(unit = letters[1:6], time = 1:10)
#' pan$y <- rnorm(nrow(pan)) + as.integer(pan$time) * 0.2 +
#'   ifelse(pan$unit == "a" & pan$time >= 7, 2, 0)
#' fit <- morie_synth_control(pan, "y", "unit", "time",
#'                            treated_unit = "a", treatment_time = 7)
#' fit$att
#' print(fit)
#' }
#' @references
#'   Abadie, A., Diamond, A., & Hainmueller, J. (2010).
#'   Synthetic control methods for comparative case studies.
#'   \emph{JASA}, 105(490), 493--505.
#' @export
print.morie_synth <- function(x, ...) {
  cat("Synthetic control (rmorie native)\n")
  cat("  treated unit :", as.character(x$treated_unit), "\n")
  w <- sort(x$weights[x$weights > 1e-3], decreasing = TRUE)
  cat("  donors (w>0.001):",
      paste(sprintf("%s=%.3f", names(w), w), collapse = ", "), "\n")
  cat(sprintf("  ATT (mean post gap): %.4f\n", x$att))
  cat(sprintf("  RMSPE pre %.4f / post %.4f (ratio %.2f)\n",
              x$pre_rmspe, x$post_rmspe, x$rmspe_ratio))
  cat(sprintf("  placebo p-value: %.3f (%d placebos)\n",
              x$placebo_pvalue, length(x$placebo_ratios)))
  invisible(x)
}

# ---------------------------------------------------------------------------
# Synthetic difference-in-differences (Arkhangelsky et al. 2021)
# ---------------------------------------------------------------------------

#' Internal helper: native SDID estimator (Algorithm 1, AER 2021)
#'
#' `Y` is the units x periods outcome matrix with the N_co control
#' rows first and N_tr treated rows last; `T_pre` pre-periods come
#' first. Unit weights solve a zeta-regularized simplex LS matching
#' pre-period trajectories (plus a free intercept); time weights match
#' pre-period to mean post-period levels among controls.
#'
#' @srrstats {G1.0} Arkhangelsky, Athey, Hirshberg, Imbens & Wager
#'   (2021), American Economic Review 111(12) 4088-4118.
#' @noRd
.morie_sdid_native <- function(Y, N_co, T_pre) {
  N <- nrow(Y)
  T_all <- ncol(Y)
  N_tr <- N - N_co
  T_post <- T_all - T_pre
  Y_co <- Y[seq_len(N_co), , drop = FALSE]
  Y_tr <- Y[N_co + seq_len(N_tr), , drop = FALSE]
  # Regularization zeta (paper eq. for the unit weights): based on the
  # sd of first differences of control pre-period outcomes.
  D1 <- Y_co[, 2:T_pre, drop = FALSE] - Y_co[, 1:(T_pre - 1), drop = FALSE]
  sig <- stats::sd(as.numeric(D1))
  zeta <- (N_tr * T_post)^(1 / 4) * sig
  # --- Unit weights (omega): intercept + simplex LS with ridge ---
  # min over (a, w in simplex) sum_t<pre (mean(Y_tr[,t]) - a - w'Y_co[,t])^2
  #   + zeta^2 * T_pre * ||w||^2
  target_u <- colMeans(Y_tr[, seq_len(T_pre), drop = FALSE])
  A_u <- t(Y_co[, seq_len(T_pre), drop = FALSE])
  # Absorb the intercept by centering target and columns.
  A_uc <- sweep(A_u, 2L, colMeans(A_u))
  b_uc <- target_u - mean(target_u)
  omega <- .morie_simplex_ls(A_uc, b_uc, zeta2 = zeta^2 * T_pre)
  # --- Time weights (lambda): intercept + simplex LS, no ridge ---
  target_t <- rowMeans(Y_co[, T_pre + seq_len(T_post), drop = FALSE])
  A_t <- Y_co[, seq_len(T_pre), drop = FALSE]
  A_tc <- sweep(A_t, 2L, colMeans(A_t))
  b_tc <- target_t - mean(target_t)
  lambda <- .morie_simplex_ls(A_tc, b_tc)
  # --- Weighted DiD ---
  w_unit <- c(-omega, rep(1 / N_tr, N_tr))
  w_time <- c(-lambda, rep(1 / T_post, T_post))
  tau <- as.numeric(t(w_unit) %*% Y %*% w_time)
  list(estimate = tau, unit_weights = omega, time_weights = lambda,
       zeta = zeta, N_tr = N_tr, T_pre = T_pre)
}

#' Internal helper: SDID with placebo / jackknife / bootstrap variance
#' @noRd
.morie_sdid_inference <- function(Y, N_co, T_pre,
                                  method = "placebo",
                                  n_boot = 200L, seed = 42L) {
  fit <- .morie_sdid_native(Y, N_co, T_pre)
  N <- nrow(Y)
  N_tr <- N - N_co
  se <- NA_real_
  placebo_effects <- NULL
  if (identical(method, "placebo")) {
    # Assign N_tr placebo-treated units among the controls.
    if (N_co > N_tr + 1L) {
      set.seed(seed)
      reps <- min(n_boot, 200L)
      placebo_effects <- vapply(seq_len(reps), function(b) {
        idx <- sample(N_co)
        Yp <- Y[c(idx[seq_len(N_co - N_tr)],
                  idx[N_co - N_tr + seq_len(N_tr)]), , drop = FALSE]
        .morie_sdid_native(Yp, N_co - N_tr, T_pre)$estimate
      }, numeric(1))
      se <- stats::sd(placebo_effects)
    }
  } else if (identical(method, "jackknife")) {
    if (N_tr >= 2L) {
      jk <- vapply(seq_len(N), function(i) {
        Ni <- if (i <= N_co) N_co - 1L else N_co
        .morie_sdid_native(Y[-i, , drop = FALSE], Ni, T_pre)$estimate
      }, numeric(1))
      se <- sqrt((N - 1) / N * sum((jk - mean(jk))^2))
    }
  } else if (identical(method, "bootstrap")) {
    set.seed(seed)
    boot <- rep(NA_real_, n_boot)
    for (b in seq_len(n_boot)) {
      co_idx <- sample(seq_len(N_co), N_co, replace = TRUE)
      tr_idx <- N_co + sample(seq_len(N_tr), N_tr, replace = TRUE)
      boot[b] <- tryCatch(
        .morie_sdid_native(Y[c(co_idx, tr_idx), , drop = FALSE],
                           N_co, T_pre)$estimate,
        error = function(e) NA_real_)
    }
    boot <- boot[is.finite(boot)]
    if (length(boot) > 1L) se <- stats::sd(boot)
    placebo_effects <- boot
  }
  c(fit, list(se = se, placebo_effects = placebo_effects,
              inference = method))
}

#' Internal helper: assemble the SDID input matrix from a long panel
#'
#' Returns Y with control rows first, treated rows last, and the
#' pre-period count, given a 0/1 treatment indicator that switches on
#' at a common adoption period.
#' @noRd
.morie_sdid_prepare <- function(df, outcome, unit, time, treat01) {
  Y <- .morie_synth_matrix(df, outcome, unit, time)
  times <- as.numeric(colnames(Y))
  tr_units <- unique(as.character(df[[unit]][df[[treat01]] == 1]))
  if (!length(tr_units)) stop("No treated units found.", call. = FALSE)
  onset <- min(as.numeric(df[[time]][df[[treat01]] == 1]))
  T_pre <- sum(times < onset)
  if (T_pre < 2L) stop("Need >= 2 pre-treatment periods.", call. = FALSE)
  co_units <- setdiff(rownames(Y), tr_units)
  list(Y = Y[c(co_units, tr_units), , drop = FALSE],
       N_co = length(co_units), T_pre = T_pre,
       treated_units = tr_units, control_units = co_units)
}
