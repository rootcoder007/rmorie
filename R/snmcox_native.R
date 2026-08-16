# snmcox.R -- function file (rootcoder007/morie)
# Structural nested failure time model fitted by g-estimation.
#
# Rank-preserving model: each subject has a latent untreated failure time U,
# recovered from the observed time by the blip-down transform
#   U(psi) = int_0^T exp(psi A(u)) du,
# so a never-treated subject has U = T and one treated throughout has
# U = T exp(psi). psi > 0 means treatment LENGTHENS survival.
#
# Under sequential randomisation U is independent of the treatment given the
# covariate history, so psi solves sum_i (A_i - E[A_i | L_i]) (U_i - Ubar) = 0.
# Censoring uses Robins' artificial censoring at C min(1, e^psi), and the
# interval inverts the score test rather than assuming a Wald variance the
# semiparametric model does not provide.
#
# References:
# Robins, J. M. (1992) "Estimation of the time-dependent accelerated failure
# time model in the presence of confounding factors", Biometrika 79(2),
# 321-334, doi:10.1093/biomet/79.2.321.
# Robins, J. M., Blevins, D., Ritter, G. and Wulfsohn, M. (1992) "G-estimation
# of the effect of prophylaxis therapy for Pneumocystis carinii pneumonia on
# the survival of AIDS patients", Epidemiology 3(4), 319-336,
# doi:10.1097/00001648-199207000-00007.
# Hernan, M. A., Cole, S. R., Margolick, J., Cohen, M. and Robins, J. M. (2005)
# "Structural accelerated failure time models for survival analysis in studies
# with time-varying treatments", Pharmacoepidemiology and Drug Safety 14(7),
# 477-491, doi:10.1002/pds.1064.

#' The blipped-down (untreated) time U(psi) = int_0^T exp(psi A(u)) du.
#' @export
morie_snmcox_blip_down <- function(time, treat_times, psi) {
  T <- as.numeric(time)
  if (T < 0) stop("snmcox: a failure time cannot be negative")
  p <- as.numeric(psi)
  on <- 0.0
  last <- 0.0
  if (length(treat_times)) {
    iv <- lapply(treat_times, function(v) as.numeric(v))
    ord <- order(vapply(iv, function(v) v[1], numeric(1)))
    for (idx in ord) {
      a <- iv[[idx]][1]
      b <- iv[[idx]][2]
      lo <- max(a, last, 0.0)
      hi <- min(b, T)
      if (hi > lo) {
        on <- on + (hi - lo)
        last <- hi
      }
    }
  }
  off <- max(T - on, 0.0)
  off + on * exp(p)
}

# fitted E[A | L]: logistic when the treatment is binary, least squares else
#' Fitted E[A | L]: logistic when the treatment is binary, least squares
#' else
#'
#' A step of the snmcox_native implementation. Called by \code{morie_snmcox_gest_score}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param A A matrix; passed to \code{crossprod}.
#' @param L Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.
#' @param ridge Numeric; combined arithmetically in the body. Defaults to \code{1e-08}.
#' @return A list, whose contents depend on the branch taken; across the branches its names are \code{e}, \code{b}, \code{kind}.
#' @export
.snmcox_treat_model <- function(A, L, ridge = 1e-8) {
  n <- length(A)
  Z <- if (is.null(L)) matrix(1.0, n, 1L) else cbind(1.0, as.matrix(L))
  storage.mode(Z) <- "double"
  binary <- all(A %in% c(0, 1))
  if (binary && sum(A) > 0 && sum(A) < n) {
    b <- .snmcox_logit_irls(Z, A, 60L, ridge)
    list(e = 1 / (1 + exp(-as.numeric(Z %*% b))), b = b, kind = "logistic")
  } else {
    b <- as.numeric(solve(crossprod(Z) + ridge * diag(ncol(Z)), crossprod(Z, A)))
    list(e = as.numeric(Z %*% b), b = b, kind = "linear")
  }
}

#' .snmcox_logit_irls
#'
#' A step of the snmcox_native implementation. Called by \code{.snmcox_treat_model}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A matrix; passed to \code{ncol}.
#' @param y Numeric; combined arithmetically in the body.
#' @param iters A count; the body uses it as \code{seq_len(...)}. Defaults to \code{60L}.
#' @param ridge Numeric; combined arithmetically in the body. Defaults to \code{1e-08}.
#' @param tol Defaults to \code{1e-13}.
#' @return The value of \code{beta}, as built in the body.
#' @export
.snmcox_logit_irls <- function(X, y, iters = 60L, ridge = 1e-8, tol = 1e-13) {
  p <- ncol(X)
  beta <- numeric(p)
  for (it in seq_len(iters)) {
    mu <- 1 / (1 + exp(-as.numeric(X %*% beta)))
    w <- mu * (1 - mu)
    step <- as.numeric(solve(crossprod(X, X * w) + ridge * diag(p),
                             crossprod(X, y - mu)))
    beta <- beta + step
    if (max(abs(step)) < tol) break
  }
  beta
}

#' The g-estimation score S(psi) and its standardisation.
#' @export
morie_snmcox_gest_score <- function(psi, time, event, A, L, treat_times,
                                    censor_time = NULL, ridge = 1e-8) {
  n <- length(time)
  tm <- .snmcox_treat_model(A, L, ridge)
  ehat <- tm$e
  U <- vapply(seq_len(n),
              function(i) morie_snmcox_blip_down(time[i], treat_times[[i]], psi),
              numeric(1))
  shrink <- min(1.0, exp(as.numeric(psi)))
  keep <- if (is.null(censor_time)) rep(TRUE, n) else
    (U <= as.numeric(censor_time) * shrink)
  if (is.null(censor_time)) {
    # No censoring: the blipped-down time is observed for every failure.
    used <- which(event == 1)
    if (length(used) < 2L) {
      return(list(s = 0.0, z = 0.0, m = 0L, U = U, e = ehat))
    }
    ubar <- mean(U[used])
    terms <- (A[used] - ehat[used]) * (U[used] - ubar)
  } else {
    # Censoring: U(psi) is NOT observed for a censored subject, so scoring
    # the raw U while merely filtering on it lets psi drift to a region
    # where the rule excludes nobody and the estimate is wrong. Robins'
    # device scores the artificial-censoring INDICATOR, which is
    # computable for everyone and, at the true psi, independent of
    # treatment given the covariate history.
    used <- seq_len(n)
    delta <- as.numeric(keep)
    dbar <- mean(delta)
    if (dbar <= 0 || dbar >= 1) {
      return(list(s = 0.0, z = 0.0, m = 0L, U = U, e = ehat))
    }
    terms <- (A - ehat) * (delta - dbar)
  }
  s <- sum(terms)
  v <- sum(terms ^ 2)
  list(s = s, z = if (v > 0) s / sqrt(v) else 0.0, m = length(used),
       U = U, e = ehat)
}

#' G-estimate psi in the structural nested failure time model.
#'
#' @references
#' Robins, J. M. (1992) Biometrika 79(2), 321-334,
#' doi:10.1093/biomet/79.2.321.
#' @export
morie_snmcox <- function(time, event, treatment_history,
                         covariate_history = NULL, treat_times = NULL,
                         censor_time = NULL, level = 0.95,
                         psi_range = c(-3.0, 3.0), n_grid = 241L,
                         tol = 1e-10, ridge = 1e-8) {
  T <- as.numeric(time)
  n <- length(T)
  if (n == 0L) stop("snmcox: no subjects")
  ev <- as.numeric(event)
  if (length(ev) != n) {
    stop(sprintf("snmcox: %d times but %d event indicators", n, length(ev)))
  }
  if (!all(ev %in% c(0, 1))) stop("snmcox: event must be 0/1")
  A <- as.numeric(treatment_history)
  if (length(A) != n) {
    stop(sprintf("snmcox: %d times but %d treatment values", n, length(A)))
  }
  L <- covariate_history
  if (is.null(treat_times)) {
    treat_times <- lapply(seq_len(n),
                          function(i) if (A[i] > 0) list(c(0.0, T[i])) else list())
  }
  if (length(treat_times) != n) {
    stop(sprintf("snmcox: %d times but %d treatment histories",
                 n, length(treat_times)))
  }
  if (!is.null(censor_time)) {
    stop(paste0("snmcox: g-estimation under administrative censoring needs ",
                "Robins' artificial-censoring construction, which is not ",
                "implemented here and is NOT the same as filtering the score ",
                "on U(psi) <= C min(1, e^psi). Pass uncensored failure times, ",
                "or subset to the uncensored, until the counting-process form ",
                "of the estimating equation is implemented and anchored."))
  }
  ct <- if (is.null(censor_time)) NULL else as.numeric(censor_time)

  lo <- as.numeric(psi_range[1])
  hi <- as.numeric(psi_range[2])
  if (!(lo < hi)) stop("snmcox: psi_range must be increasing")
  ng <- as.integer(n_grid)
  grid <- lo + (hi - lo) * (seq_len(ng) - 1L) / (ng - 1)
  sc <- lapply(grid, function(p)
    morie_snmcox_gest_score(p, T, ev, A, L, treat_times, ct, ridge))
  svals <- vapply(sc, function(r) r$s, numeric(1))
  zvals <- vapply(sc, function(r) r$z, numeric(1))

  root <- NULL
  for (t in seq_len(ng - 1L)) {
    s0 <- svals[t]; s1 <- svals[t + 1L]
    if (s0 == 0) { root <- grid[t]; break }
    if (s0 * s1 < 0) {
      a <- grid[t]; b <- grid[t + 1L]; fa <- s0
      for (it in seq_len(200L)) {
        mid <- 0.5 * (a + b)
        fm <- morie_snmcox_gest_score(mid, T, ev, A, L, treat_times,
                                      ct, ridge)$s
        if (fa * fm <= 0) {
          b <- mid
        } else {
          a <- mid; fa <- fm
        }
        if (b - a < tol) break
      }
      root <- 0.5 * (a + b)
      break
    }
  }
  converged <- !is.null(root)
  if (is.null(root)) root <- grid[which.min(abs(svals))]

  zq <- stats::qnorm(0.5 + 0.5 * as.numeric(level))
  inside <- grid[abs(zvals) <= zq]
  ci_lo <- if (length(inside)) min(inside) else NA_real_
  ci_hi <- if (length(inside)) max(inside) else NA_real_

  fin <- morie_snmcox_gest_score(root, T, ev, A, L, treat_times, ct, ridge)
  n_art <- if (is.null(ct)) 0L else
    sum(!(fin$U <= ct * min(1.0, exp(root))))

  list(
    estimate = root,
    psi = root,
    time_ratio = exp(root),
    lower = ci_lo,
    upper = ci_hi,
    score_at_estimate = fin$s,
    z_at_estimate = fin$z,
    n_used = fin$m,
    artificial_censored = as.integer(n_art),
    blipped = fin$U,
    propensity = fin$e,
    converged = converged,
    grid_psi = grid,
    grid_score = svals,
    n = n,
    level = as.numeric(level),
    method = paste0("g-estimation of a rank-preserving structural nested ",
                    "failure time model, Robins (1992) Biometrika 79, 321"),
    note = paste0("psi > 0 means treatment LENGTHENS survival; U(psi) = ",
                  "int_0^T exp(psi A(u)) du, so a never-treated subject has ",
                  "U = T and one treated throughout has U = T exp(psi); the ",
                  "interval inverts the score test rather than assuming a ",
                  "Wald variance the semiparametric model does not give")
  )
}

#' @rdname morie_snmcox
#' @export
morie_snm_cox <- morie_snmcox

#' morie_snmcox_cheatsheet
#'
#' A step of the snmcox_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
morie_snmcox_cheatsheet <- function() {
  paste0("snmcox: structural nested failure time model by g-estimation. ",
         "Blip down U(psi) = int_0^T exp(psi A(u)) du; the true psi is the ",
         "one making U independent of treatment given the covariate history, ",
         "so solve sum (A - E[A|L])(U - Ubar) = 0. Never treated -> U = T; ",
         "always treated -> U = T exp(psi). Censoring needs Robins' ",
         "artificial censoring at C min(1, e^psi). CI by inverting the score ",
         "test. Robins (1992) Biometrika 79, 321.")
}
