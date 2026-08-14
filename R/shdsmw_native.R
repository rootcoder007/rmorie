# morie.fn -- function file (rootcoder007/morie)
# R arm of shdsmw (shrinkage_msm / penalty_path).
# Sources:
#   Setoguchi, S., Schneeweiss, S., Brookhart, M. A., Glynn, R. J. &
#   Cook, E. F. (2008) "Evaluating uses of data mining techniques in
#   propensity score estimation: a simulation study",
#   Pharmacoepidemiology and Drug Safety 17(6), 546-555,
#   doi:10.1002/pds.1555.
#   Westreich, D., Lessler, J. & Funk, M. J. (2010) "Propensity score
#   estimation: neural networks, support vector machines, decision trees
#   (CART), and meta-classifiers as alternatives to logistic regression",
#   Journal of Clinical Epidemiology 63(8), 826-833,
#   doi:10.1016/j.jclinepi.2009.11.020.
#   Hernan, M. A. & Robins, J. M. (2020) Causal Inference: What If,
#   Chapman & Hall/CRC, Sec. 12.3 -- the stabilized weights and the
#   mean-1 diagnostic; Fine Point 12.2 on checking positivity.

shrinkage_msm <- function(y, treatment_history, covariate_history,
                          lam = 0.0, contrast = "cumulative",
                          path = NULL, trim = NULL) {
  if (as.numeric(lam) < 0.0)
    stop(sprintf("shrinkage_msm: lam must be non-negative, got %r", lam))
  A_hist <- .shdsmw_hist(treatment_history)
  L_hist <- .shdsmw_hist(covariate_history, allow_none = TRUE)
  if (length(L_hist) != length(A_hist))
    stop(sprintf("shrinkage_msm: %d treatment times but %d covariate blocks",
                 length(A_hist), length(L_hist)))
  yv <- as.numeric(y)
  n <- length(yv)

  fit_at <- function(lm) {
    per <- k_ip_weights_history(A_hist, L_hist,
                                penalty = as.numeric(lm), trim = trim)
    w <- per$weights
    A_list <- lapply(A_hist, function(a) as.numeric(a))
    cum <- vapply(seq_len(n), function(i) {
      s <- 0.0
      for (a in A_list) s <- s + a[i]
      s
    }, numeric(1))
    if (contrast == "cumulative") {
      e <- cum
    } else if (contrast == "final") {
      e <- as.numeric(A_list[[length(A_list)]])
    } else if (contrast == "everexposed") {
      e <- ifelse(cum > 0.0, 1.0, 0.0)
    } else {
      stop(sprintf(paste0("shrinkage_msm: contrast must be 'cumulative',",
                          " 'final' or 'everexposed', got %r"),
                   contrast))
    }
    f <- k_wls(lapply(e, function(v) c(v)), yv, w)
    s1 <- sum(w)
    s2 <- sum(w * w)
    list(lam = as.numeric(lm), estimate = f$coef[2], se = f$se[2],
         weights = w, mean_weight = s1 / n, max_weight = max(w),
         effective_sample_size = if (s2 > 0) (s1 * s1 / s2) else 0.0,
         exposure = e, f = f)
  }

  main <- fit_at(lam)
  if (is.null(path))
    path <- c(0.0, 1e-4, 1e-2, 0.1, 1.0, 10.0, 1e3)
  rows <- lapply(path, function(lm) {
    r <- fit_at(lm)
    list(lam = r$lam, estimate = r$estimate, se = r$se,
         max_weight = r$max_weight,
         effective_sample_size = r$effective_sample_size)
  })
  unadj <- k_wls(lapply(main$exposure, function(v) c(v)),
                 yv, rep(1.0, n))

  out <- list(
    estimate = main$estimate, se = main$se, weights = main$weights,
    mean_weight = main$mean_weight, max_weight = main$max_weight,
    effective_sample_size = main$effective_sample_size,
    exposure = main$exposure, lam = main$lam,
    path = rows, unadjusted = unadj$coef[2],
    n = n, n_times = length(A_hist), contrast = contrast,
    method = paste0("MSM with ridge-penalized propensity weights, ",
                    "Setoguchi et al. (2008) and Westreich, Lessler ",
                    "& Funk (2010); weights per Hernan & Robins ",
                    "(2020) Sec. 12.3")
  )
  out
}

penalty_path <- function(y, treatment_history, covariate_history,
                         path = NULL, contrast = "cumulative") {
  r <- shrinkage_msm(y, treatment_history, covariate_history, lam = 0.0,
                     contrast = contrast, path = path)
  r$path
}

.shdsmw_hist <- function(obj, allow_none = FALSE) {
  if (is.null(obj)) {
    if (allow_none) return(list(NULL)) else return(list())
  }
  if (is.list(obj)) {
    if (length(obj) > 0 &&
        (is.list(obj[[1]]) || is.null(obj[[1]]))) {
      return(obj)
    }
    return(list(obj))
  }
  if (is.matrix(obj) || is.numeric(obj)) {
    if (is.matrix(obj) && ncol(obj) == 1L) {
      return(list(as.numeric(obj[, 1L])))
    }
    return(list(obj))
  }
  list(obj)
}

# Placeholders for the underlying arm primitives. They mirror
# k.ip_weights_history and k.wls from the Python arm (no random draw).
k_ip_weights_history <- function(A_hist, L_hist, penalty, trim) {
  # Stabilized IP weights per Hernan & Robins Sec. 12.3.
  # Without a full time-varying logistic fitter, build a marginal-rate
  # numerator and a (ridge-penalized) logistic denominator using the
  # union of all covariate blocks at the final time.
  n <- length(A_hist[[1L]])
  A_stack <- do.call(cbind, lapply(A_hist, function(a) as.numeric(a)))
  cumA <- rowSums(A_stack)
  num <- ifelse(cumA > 0, mean(cumA > 0), 0.5)
  num <- ifelse(cumA > 0, num, 1 - num)
  # Combine all covariate blocks column-wise.
  L_list <- lapply(L_hist, function(L) {
    if (is.null(L)) return(NULL)
    if (is.matrix(L)) L else matrix(as.numeric(L), ncol = 1L)
  })
  L_list <- L_list[!vapply(L_list, is.null, logical(1))]
  if (length(L_list) == 0L) {
    X <- matrix(0, nrow = n, ncol = 0L)
  } else {
    X <- do.call(cbind, L_list)
  }
  if (ncol(X) == 0L) {
    pA <- rep(mean(cumA > 0), n)
  } else {
    coefs <- .shdsmw_ridge_logit(A_stack, X, penalty)
    eta <- coefs[1L] + as.numeric(X %*% coefs[-1L])
    pA <- 1.0 / (1.0 + exp(-eta))
    pA <- pmin(pmax(pA, 1e-12), 1 - 1e-12)
  }
  A_final <- as.numeric(A_hist[[length(A_hist)]])
  den <- ifelse(A_final > 0, pA, 1 - pA)
  w <- num / den
  if (!is.null(trim)) {
    lo <- quantile(w, trim, na.rm = TRUE)
    hi <- quantile(w, 1 - trim, na.rm = TRUE)
    w <- pmin(pmax(w, lo), hi)
  }
  list(weights = as.numeric(w), pA = pA)
}

.shdsmw_ridge_logit <- function(A_stack, X, penalty) {
  # Newton-IRLS ridge on stacked treatment with the same penalty applied
  # to slopes only (intercept is unpenalised, as documented).
  y <- as.numeric(A_stack[, ncol(A_stack)])
  p <- ncol(X)
  beta <- rep(0, p + 1L)
  X1 <- cbind(1, X)
  for (it in seq_len(25L)) {
    eta <- as.numeric(X1 %*% beta)
    eta <- pmin(pmax(eta, -30), 30)
    mu <- 1.0 / (1.0 + exp(-eta))
    mu <- pmin(pmax(mu, 1e-12), 1 - 1e-12)
    g <- mu * (1 - mu)
    z <- eta + (y - mu) / g
    W <- diag(g)
    pen <- c(0, rep(penalty, p))
    XtWX <- crossprod(X1, (g * z))
    XtWX <- crossprod(X1 * g, X1) + diag(pen, nrow = p + 1L)
    XtWz <- crossprod(X1, g * z)
    beta <- tryCatch(solve(XtWX, XtWz), error = function(e) beta)
  }
  beta
}

k_wls <- function(X_list, y, w) {
  X <- do.call(cbind, X_list)
  if (is.null(dim(X)) || ncol(X) == 0L) X <- matrix(X, ncol = 1L)
  W <- diag(w)
  XtWX <- crossprod(X, W %*% X)
  XtWy <- crossprod(X, W %*% y)
  coef <- tryCatch(solve(XtWX, XtWy), error = function(e) rep(0, ncol(X)))
  resid <- y - as.numeric(X %*% coef)
  n <- length(y)
  p <- ncol(X)
  s2 <- sum(w * resid^2) / max(n - p, 1)
  XtWX_inv <- tryCatch(solve(XtWX), error = function(e) diag(1, p))
  se <- sqrt(diag(s2 * XtWX_inv))
  list(coef = as.numeric(coef), se = as.numeric(se))
}

cheatsheet <- function() {
  paste0("shdsmw: MSM with a ridge-penalized propensity model ",
         "(Setoguchi 2008; Westreich 2010). lam=0 is plain MLE; ",
         "lam -> inf shrinks the weights to 1 and the estimate to ",
         "the unadjusted one. Reports ESS and max weight along a ",
         "penalty path.")
}

# ledger/NAMING.md compact alias
shrinkagemsm <- shrinkage_msm

morie_shdsmw <- list(shrinkage_msm = shrinkage_msm,
                     penalty_path = penalty_path,
                     cheatsheet = cheatsheet,
                     shrinkagemsm = shrinkagemsm)
