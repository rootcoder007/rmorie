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
#' \donttest{
#' set.seed(1)
#' pan <- expand.grid(unit = letters[1:6], time = 1:10)
#' pan$y <- rnorm(nrow(pan)) + as.integer(pan$time) * 0.2 +
#'   ifelse(pan$unit == "a" & pan$time >= 7, 2, 0)
#' fit <- morie_synth_control(pan, "y", "unit", "time",
#'                            treated_unit = "a", treatment_time = 7)
#' fit$att
#' }
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
  # The placebo pool leaves out the treated unit, whose post-period
  # carries the effect (Abadie, Diamond & Hainmueller 2010, sec. 5).
  placebo_ratios <- vapply(donors, function(d) {
    fit_one(d, setdiff(donors, d))$ratio
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
#' @return The value of `invisible`.
#' @examples
#' D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
#' rmorie:::print.morie_synth(D)
#' @references
#'   Abadie, A., Diamond, A., & Hainmueller, J. (2010).
#'   Synthetic control methods for comparative case studies.
#'   \emph{JASA}, 105(490), 493--505.
#' @export
#' @keywords internal
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
.morie_sdid_fw <- function(Ymat, zeta, lambda0, min_decrease, max_iter) {
  # synthdid:::sc.weight.fw with intercept: Frank-Wolfe with exact line
  # search on the simplex, stopping when the objective decrease falls
  # below min_decrease^2
  T0 <- ncol(Ymat) - 1L
  N0 <- nrow(Ymat)
  lam <- if (is.null(lambda0)) rep(1 / T0, T0) else lambda0
  Y <- sweep(Ymat, 2L, colMeans(Ymat))
  A <- Y[, seq_len(T0), drop = FALSE]
  b <- Y[, T0 + 1L]
  eta <- N0 * zeta^2
  vals <- numeric(0)
  t <- 0L
  while (t < max_iter && (t < 2L || vals[t - 1L] - vals[t] > min_decrease^2)) {
    t <- t + 1L
    Ax <- as.numeric(A %*% lam)
    hg <- as.numeric(crossprod(A, Ax - b)) + eta * lam
    i <- which.min(hg)
    dx <- -lam
    dx[i] <- 1 - lam[i]
    if (any(dx != 0)) {
      derr <- A[, i] - Ax
      step <- -sum(hg * dx) / (sum(derr^2) + eta * sum(dx^2))
      lam <- lam + min(1, max(0, step)) * dx
    }
    err <- as.numeric(Y %*% c(lam, -1))
    vals[t] <- zeta^2 * sum(lam^2) + sum(err^2) / N0
  }
  lam
}

.morie_sdid_sparsify <- function(v) {
  v[v <= max(v) / 4] <- 0
  v / sum(v)
}

.morie_sdid_core <- function(Y, N0, T0, opts, omega = NULL, lambda = NULL,
                             update_omega = TRUE, update_lambda = TRUE) {
  # synthdid::synthdid_estimate without covariates
  N1 <- nrow(Y) - N0
  T1 <- ncol(Y) - T0
  Yc <- rbind(cbind(Y[seq_len(N0), seq_len(T0), drop = FALSE],
                    rowMeans(Y[seq_len(N0), T0 + seq_len(T1), drop = FALSE])),
              c(colMeans(Y[N0 + seq_len(N1), seq_len(T0), drop = FALSE]),
                mean(Y[N0 + seq_len(N1), T0 + seq_len(T1)])))
  if (update_lambda) {
    l1 <- .morie_sdid_fw(Yc[seq_len(N0), , drop = FALSE], opts$zeta_lambda, lambda,
                         opts$min_decrease, 100L)
    lambda <- .morie_sdid_fw(Yc[seq_len(N0), , drop = FALSE], opts$zeta_lambda,
                             .morie_sdid_sparsify(l1), opts$min_decrease, 10000L)
  }
  if (update_omega) {
    Yo <- t(Yc[, seq_len(T0), drop = FALSE])
    o1 <- .morie_sdid_fw(Yo, opts$zeta_omega, omega, opts$min_decrease, 100L)
    omega <- .morie_sdid_fw(Yo, opts$zeta_omega, .morie_sdid_sparsify(o1),
                            opts$min_decrease, 10000L)
  }
  tau <- as.numeric(t(c(-omega, rep(1 / N1, N1))) %*% Y %*% c(-lambda, rep(1 / T1, T1)))
  list(estimate = tau, unit_weights = omega, time_weights = lambda)
}

.morie_sdid_opts <- function(Y, N_co, T_pre) {
  N1 <- nrow(Y) - N_co
  T1 <- ncol(Y) - T_pre
  D1 <- t(apply(Y[seq_len(N_co), seq_len(T_pre), drop = FALSE], 1, diff))
  noise <- stats::sd(as.numeric(D1))
  list(zeta_omega = ((N1 * T1)^(1 / 4)) * noise, zeta_lambda = 1e-6 * noise,
       min_decrease = 1e-5 * noise, noise = noise)
}

.morie_sdid_native <- function(Y, N_co, T_pre) {
  # Arkhangelsky, Athey, Hirshberg, Imbens and Wager (2021), computed as
  # synthdid::synthdid_estimate: Frank-Wolfe weights with a sparsify pass,
  # zeta_omega = (N1 T1)^(1/4) sigma, zeta_lambda = 1e-6 sigma, sigma the
  # sd of the controls' pre-period first differences
  opts <- .morie_sdid_opts(Y, N_co, T_pre)
  fit <- .morie_sdid_core(Y, N_co, T_pre, opts)
  c(fit, list(zeta = opts$zeta_omega, N_tr = nrow(Y) - N_co, T_pre = T_pre, opts = opts))
}

#' Internal helper: SDID with placebo / jackknife / bootstrap variance
#' @noRd
.morie_sdid_inference <- function(Y, N_co, T_pre,
                                  method = "placebo",
                                  n_boot = 200L, seed = 42L) {
  # standard errors as synthdid::vcov: the jackknife holds the weights
  # fixed (omega renormalised over the kept controls); the bootstrap and
  # the placebo re-solve them from the renormalised full-sample weights
  # with the full-sample zeta and stopping rule
  fit <- .morie_sdid_native(Y, N_co, T_pre)
  opts <- fit$opts
  N <- nrow(Y)
  N_tr <- N - N_co
  norm1 <- function(v) if (sum(v) > 0) v / sum(v) else rep(1 / length(v), length(v))
  se <- NA_real_
  placebo_effects <- NULL
  if (identical(method, "placebo")) {
    if (N_co > N_tr) {
      .rmorie_local_seed(seed)
      placebo_effects <- vapply(seq_len(n_boot), function(b) {
        ind <- sample(seq_len(N_co))
        n0 <- length(ind) - N_tr
        .morie_sdid_core(Y[ind, , drop = FALSE], n0, T_pre, opts,
                         omega = norm1(fit$unit_weights[ind[seq_len(n0)]]),
                         lambda = fit$time_weights)$estimate
      }, numeric(1))
      se <- sqrt((n_boot - 1) / n_boot) * stats::sd(placebo_effects)
    }
  } else if (identical(method, "jackknife")) {
    if (N_co < N - 1L && sum(fit$unit_weights != 0) > 1L) {
      jk <- vapply(seq_len(N), function(i) {
        ind <- setdiff(seq_len(N), i)
        n0 <- sum(ind <= N_co)
        .morie_sdid_core(Y[ind, , drop = FALSE], n0, T_pre, opts,
                         omega = norm1(fit$unit_weights[ind[ind <= N_co]]),
                         lambda = fit$time_weights,
                         update_omega = FALSE, update_lambda = FALSE)$estimate
      }, numeric(1))
      se <- sqrt((N - 1) / N * sum((jk - mean(jk))^2))
    }
  } else if (identical(method, "bootstrap")) {
    if (N_co < N - 1L) {
      .rmorie_local_seed(seed)
      boot <- numeric(0)
      while (length(boot) < n_boot) {
        ind <- sort(sample(seq_len(N), replace = TRUE))
        if (all(ind <= N_co) || all(ind > N_co)) next
        boot <- c(boot, .morie_sdid_core(Y[ind, , drop = FALSE], sum(ind <= N_co), T_pre, opts,
                                         omega = norm1(fit$unit_weights[ind[ind <= N_co]]),
                                         lambda = fit$time_weights)$estimate)
      }
      se <- sqrt((n_boot - 1) / n_boot) * stats::sd(boot)
      placebo_effects <- boot
    }
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
