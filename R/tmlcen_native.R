# morie.fn -- function file (rootcoder007/morie)
# Causal effect under censoring, by inverse probability of censoring
# weighting.
#
# Two censoring structures, two estimators, because the ledger row asks
# for right-censoring and the source volume works the interval-censored
# case out in full.
#
# Right-censored (default). Hernan & Robins Ch. 17. Censoring is
# handled by weighting each subject's observed follow-up by the inverse
# of its probability of remaining uncensored,
#
#   Gbar_c(k | A, W) = prod_{j<=k} (1 - lambda_C(j | A, W)),
#
# with lambda_C the censoring hazard. The discrete-time death hazard
# is then fitted on the weighted person-time data and converted to a
# survival curve by the same product-limit,
#   S(k) = prod_{j<=k}(1 - lambda(j))
# -- Sec. 17.2, "from hazards to risks". Weighting by 1/Gbar_c and
# *also* conditioning on being uncensored would double-count; the
# weights exist so that conditioning is unnecessary.
#
# Interval-censored. van der Laan & Rose (2018) Sec. 8.5. The data are
#   O = (W, A, C_m, Delta_m = I(T <= C_m) : m = 1..M):
# M monitoring times, and at each one only whether the event has
# happened yet. The target is
#   Psi_a^f(Q) = int r(t) Fbar_a(t) dt
# with Fbar_a(t) = E_P P(T > t | A = a, W), a weighted mean survival,
# and the chapter's initial gradient gives the estimator
#
#   (1/M) sum_{m=1}^M (1 - Delta_m) r(C_m)
#       I(A = a) / (gbar_c(C_m | A, W) g(A | W)).
#
# The interval implied by the monitoring is spelled out exactly and is
# implemented exactly: L(O) is the largest monitoring time with
# Delta_j = 0 and R(O) the smallest with Delta_j = 1; if Delta_1 = 1
# then L(O) = 0, and if Delta_M = 0 then R(O) = Inf. Getting those two
# boundary cases wrong is silent -- the estimate simply comes out
# biased -- so the anchor builds them by hand.
#
# References
# ----------
# Hernan, M. A. & Robins, J. M. (2020) Causal Inference: What If, Boca
# Raton: Chapman & Hall/CRC, Ch. 17 "Causal survival analysis" --
# Sec. 17.2 hazards to risks, Sec. 17.3 why censoring matters, Sec. 17.4
# IP weighting of marginal structural models.
#
# van der Laan, M. J. & Rose, S. (eds.) (2018) Targeted Learning in
# Data Science, Springer Series in Statistics,
# doi:10.1007/978-3-319-65304-4, Sec. 8.5 "Causal Effect of Binary
# Treatment on Interval Censored Time to Event" -- the data structure,
# the coarsening C(o) = (L(o), R(o)], and the IPCW estimator above.
#
# Note on the ledger citation: its key "Stitelman-Lendle-vdL (2011)" does
# not resolve to any paper; a bibliographic search returns the ltmle
# software package instead. The two sources above are what this is built
# from.

# --- Private helpers (prefix .tmlcen_) ---------------------------------

.tmlcen_sigmoid <- function(x) 1 / (1 + exp(-x))

.tmlcen_design <- function(rows, n) {
  if (n == 0L) return(matrix(0, nrow = 0L, ncol = 0L))
  if (is.null(rows) || length(rows) == 0L) {
    return(matrix(1, nrow = n, ncol = 1L))
  }
  p <- length(rows[[1L]]) + 1L
  Z <- matrix(0, nrow = n, ncol = p)
  for (i in seq_len(n)) {
    Z[i, 1L] <- 1
    if (length(rows[[i]]) > 0L) {
      Z[i, 2L:(length(rows[[i]]) + 1L)] <- rows[[i]]
    }
  }
  Z
}

.tmlcen_ridgesolve <- function(A, b, ridge) {
  p <- ncol(A)
  solve(A + ridge * diag(p), b)
}

.tmlcen_weighted_logit <- function(Z, y, w, iters = 60L, ridge = 1e-10) {
  n <- nrow(Z)
  p <- ncol(Z)
  b <- rep(0, p)
  for (iter in seq_len(iters)) {
    eta <- as.numeric(Z %*% b)
    mu <- .tmlcen_sigmoid(eta)
    ww <- w * mu * (1 - mu)
    rr <- w * (y - mu)
    XtWX <- crossprod(Z, Z * ww)
    Xtr <- as.numeric(crossprod(Z, rr))
    step <- .tmlcen_ridgesolve(XtWX, Xtr, ridge)
    b <- b + step
    if (max(abs(step)) < 1e-13) break
  }
  b
}

.tmlcen_logit_irls <- function(Z, y, max_iter = 60L, ridge = 1e-8) {
  .tmlcen_weighted_logit(Z, y, rep(1, nrow(Z)), max_iter, ridge)
}

.tmlcen_matvec <- function(Z, b) as.numeric(Z %*% b)

.tmlcen_uniform_density <- function(ts) {
  lo <- min(ts)
  hi <- max(ts)
  if (hi > lo) 1.0 / (hi - lo) else 1.0
}

.tmlcen_KINDS <- c("right", "interval")

.tmlcen_RichResult <- function(payload) {
  class(payload) <- "RichResult"
  payload
}

.tmlcen_W_mat <- function(W, n) {
  if (is.null(W) || length(W) == 0L) {
    return(matrix(0, nrow = n, ncol = 0L))
  }
  if (is.data.frame(W)) {
    W <- as.matrix(W)
  }
  if (is.vector(W) && !is.matrix(W)) {
    W <- matrix(W, ncol = 1L)
  }
  Wm <- as.matrix(W)
  if (is.null(dim(Wm)) || ncol(Wm) == 0L) {
    Wm <- matrix(0, nrow = n, ncol = 0L)
  }
  if (nrow(Wm) != n) {
    stop(sprintf("W has %d rows but data has %d", nrow(Wm), n))
  }
  Wm
}

.tmlcen_coerce_subject_list <- function(x, n) {
  if (is.data.frame(x)) {
    x <- as.matrix(x)
  }
  if (is.matrix(x)) {
    return(lapply(seq_len(nrow(x)), function(i) as.numeric(x[i, ])))
  }
  if (is.list(x)) {
    return(lapply(x, function(row) as.numeric(row)))
  }
  stop("expected a list of per-subject vectors or a matrix")
}

# --- Exposed functions (morie_ prefix) ---------------------------------

# coarsen_interval: the interval (L, R] implied by one subject's
# monitoring, Sec. 8.5.
morie_coarsen_interval <- function(times, deltas) {
  ts <- as.numeric(times)
  ds <- as.numeric(deltas)
  if (length(ts) != length(ds)) {
    stop(sprintf("coarsen_interval: %d monitoring times but %d indicators",
                 length(ts), length(ds)))
  }
  if (length(ts) == 0L) {
    stop("coarsen_interval: no monitoring times")
  }
  ord <- order(ts)
  ts <- ts[ord]
  ds <- ds[ord]
  if (any(!(ds %in% c(0, 1)))) {
    stop("coarsen_interval: Delta must be 0/1")
  }
  zeros <- ts[ds == 0]
  ones <- ts[ds == 1]
  L <- if (length(zeros) > 0L) max(zeros) else 0
  R <- if (length(ones) > 0L) min(ones) else Inf
  list(L = L, R = R)
}

# censoring_survival: Gbar_c(k | A, W) = prod_{j<=k} (1 - lambda_C(j | A, W))
morie_censoring_survival <- function(times, censored, A = NULL, W = NULL,
                                     grid = NULL, by_covariate = TRUE,
                                     ridge = 1e-8) {
  t <- as.numeric(times)
  c <- as.numeric(censored)
  n <- length(t)
  if (length(c) != n) {
    stop(sprintf("censoring_survival: %d times but %d censoring indicators",
                 n, length(c)))
  }
  if (is.null(grid)) {
    grid <- sort(unique(t))
  }
  grid <- as.numeric(grid)
  Wm <- .tmlcen_W_mat(W, n)
  if (is.null(A)) {
    av <- rep(0, n)
  } else {
    av <- as.numeric(A)
  }

  rows <- list()
  lab <- c()
  for (i in seq_len(n)) {
    for (kk in seq_along(grid)) {
      tk <- grid[kk]
      if (t[i] < tk) break
      cens_now <- if (c[i] == 1 && t[i] == tk) 1.0 else 0.0
      row <- c(as.numeric(kk), av[i],
               if (ncol(Wm) > 0L) Wm[i, ] else numeric(0))
      rows[[length(rows) + 1L]] <- row
      lab <- c(lab, cens_now)
    }
  }
  if (length(rows) == 0L) {
    stop("censoring_survival: no person-time at risk")
  }
  Z <- .tmlcen_design(rows, length(rows))
  b <- .tmlcen_logit_irls(Z, lab, 60L, ridge)

  haz <- function(kk, i) {
    row <- c(1.0, as.numeric(kk), av[i],
             if (ncol(Wm) > 0L) Wm[i, ] else numeric(0))
    .tmlcen_sigmoid(sum(b * row))
  }

  G <- list()
  for (i in seq_len(n)) {
    g <- numeric(length(grid))
    cur <- 1.0
    for (kk in seq_along(grid)) {
      cur <- cur * (1.0 - haz(kk, i))
      g[kk] <- cur
    }
    G[[i]] <- g
  }
  list(G = G, grid = grid, b = b)
}

# ipcw_interval: Sec. 8.5's IPCW estimator of Psi_a = int r(t) Fbar_a(t) dt
morie_ipcw_interval <- function(W, A, times, deltas, a = 1.0, r = NULL,
                                g = NULL, gc = NULL, ridge = 1e-8) {
  av <- as.numeric(A)
  n <- length(av)
  Tm <- .tmlcen_coerce_subject_list(times, n)
  Dm <- .tmlcen_coerce_subject_list(deltas, n)
  if (length(Tm) != n || length(Dm) != n) {
    stop(sprintf("ipcw_interval: %d treatments but %d monitoring rows and %d indicator rows",
                 n, length(Tm), length(Dm)))
  }
  Wm <- .tmlcen_W_mat(W, n)
  if (is.null(r)) {
    r <- function(tt) 1.0
  }

  if (is.null(g)) {
    if (ncol(Wm) > 0L) {
      rows_g <- lapply(seq_len(n), function(i) Wm[i, ])
    } else {
      rows_g <- NULL
    }
    Z <- .tmlcen_design(rows_g, n)
    bg <- .tmlcen_logit_irls(Z, av, 60L, ridge)
    gv <- .tmlcen_sigmoid(.tmlcen_matvec(Z, bg))
    g <- numeric(n)
    for (i in seq_len(n)) {
      g[i] <- if (av[i] == 1.0) gv[i] else (1.0 - gv[i])
    }
  } else {
    g <- as.numeric(g)
  }

  tot <- 0.0
  for (i in seq_len(n)) {
    M <- length(Tm[[i]])
    if (M == 0L) {
      stop(sprintf("ipcw_interval: subject %d has no monitoring times", i))
    }
    if (av[i] != a) next
    if (g[i] <= 0.0) {
      stop(sprintf("ipcw_interval: g(A|W) is zero for subject %d, so positivity fails and the weight is undefined", i))
    }
    s <- 0.0
    for (m in seq_len(M)) {
      dens <- if (!is.null(gc)) gc[[i]][m] else .tmlcen_uniform_density(Tm[[i]])
      if (dens <= 0.0) {
        stop(sprintf("ipcw_interval: the monitoring density is zero for subject %d at time %d", i, m))
      }
      s <- s + (1.0 - Dm[[i]][m]) * r(Tm[[i]][m]) / dens
    }
    tot <- tot + s / M / g[i]
  }
  tot / n
}

# tmle_censoring: causal survival under censoring
morie_tmle_censoring <- function(time, event, censor, treatment, covariates,
                                 kind = "right", grid = NULL, a = 1.0,
                                 r = NULL, g = NULL, gc = NULL,
                                 trim = 1e-3) {
  if (!(kind %in% .tmlcen_KINDS)) {
    stop(sprintf("tmle_censoring: kind must be 'right' or 'interval', got %r", kind))
  }
  if (kind == "interval") {
    psi <- morie_ipcw_interval(covariates, treatment, time, event, a = a, r = r,
                               g = g, gc = gc)
    return(.tmlcen_RichResult(list(
      estimate = psi, psi = psi, a = a,
      n = length(as.numeric(treatment)),
      method = "interval-censored IPCW, van der Laan & Rose (2018) Sec. 8.5"
    )))
  }

  t <- as.numeric(time)
  d <- as.numeric(event)
  c <- as.numeric(censor)
  av <- as.numeric(treatment)
  n <- length(t)

  arrs <- list(event = d, censor = c, treatment = av)
  for (nm in names(arrs)) {
    if (length(arrs[[nm]]) != n) {
      stop(sprintf("tmle_censoring: %d times but %d %s", n, length(arrs[[nm]]), nm))
    }
  }
  for (i in seq_len(n)) {
    if (d[i] == 1.0 && c[i] == 1.0) {
      stop("tmle_censoring: a subject cannot be both an event and censored at the same time")
    }
  }
  Wm <- .tmlcen_W_mat(covariates, n)

  G_grid <- morie_censoring_survival(t, c, A = av, W = Wm, grid = grid)
  G <- G_grid$G
  grid_use <- G_grid$grid

  rows <- list()
  lab <- c()
  wts <- c()
  for (i in seq_len(n)) {
    for (kk in seq_along(grid_use)) {
      tk <- grid_use[kk]
      if (t[i] < tk) break
      gk <- max(G[[i]][kk], trim)
      row <- c(as.numeric(kk), av[i],
               if (ncol(Wm) > 0L) Wm[i, ] else numeric(0))
      rows[[length(rows) + 1L]] <- row
      lab <- c(lab, if (d[i] == 1.0 && t[i] == tk) 1.0 else 0.0)
      wts <- c(wts, 1.0 / gk)
    }
  }
  Z <- .tmlcen_design(rows, length(rows))
  bh <- .tmlcen_weighted_logit(Z, lab, wts)

  surv <- function(a_val, i) {
    out <- numeric(length(grid_use))
    cur <- 1.0
    for (kk in seq_along(grid_use)) {
      row <- c(1.0, as.numeric(kk), a_val,
               if (ncol(Wm) > 0L) Wm[i, ] else numeric(0))
      h <- .tmlcen_sigmoid(sum(bh * row))
      cur <- cur * (1.0 - h)
      out[kk] <- cur
    }
    out
  }

  s1_mat <- matrix(0, nrow = n, ncol = length(grid_use))
  s0_mat <- matrix(0, nrow = n, ncol = length(grid_use))
  for (i in seq_len(n)) {
    s1_mat[i, ] <- surv(1.0, i)
    s0_mat[i, ] <- surv(0.0, i)
  }
  s1 <- colMeans(s1_mat)
  s0 <- colMeans(s0_mat)

  bh_n <- .tmlcen_weighted_logit(Z, lab, rep(1, length(lab)))

  surv_n <- function(a_val, i) {
    cur <- 1.0
    for (kk in seq_along(grid_use)) {
      row <- c(1.0, as.numeric(kk), a_val,
               if (ncol(Wm) > 0L) Wm[i, ] else numeric(0))
      cur <- cur * (1.0 - .tmlcen_sigmoid(sum(bh_n * row)))
    }
    cur
  }
  naive <- mean(sapply(seq_len(n), function(i) surv_n(1.0, i))) -
           mean(sapply(seq_len(n), function(i) surv_n(0.0, i)))

  rows_u <- lapply(rows, function(row) row[1:2])
  Zu <- .tmlcen_design(rows_u, length(rows_u))
  bh_u <- .tmlcen_weighted_logit(Zu, lab, rep(1, length(lab)))

  surv_u <- function(a_val) {
    cur <- 1.0
    for (kk in seq_along(grid_use)) {
      rrow <- c(1.0, as.numeric(kk), a_val)
      cur <- cur * (1.0 - .tmlcen_sigmoid(sum(bh_u * rrow)))
    }
    cur
  }
  unadjusted <- surv_u(1.0) - surv_u(0.0)

  max_weight <- max(sapply(seq_len(n), function(i) {
    g_last <- G[[i]][length(G[[i]])]
    1.0 / max(g_last, trim)
  }))

  .tmlcen_RichResult(list(
    estimate = s1[length(s1)] - s0[length(s0)],
    survival_treated = s1,
    survival_control = s0,
    grid = grid_use,
    naive = naive,
    unadjusted = unadjusted,
    censoring_survival = G,
    max_weight = max_weight,
    n = n,
    method = "IPCW survival difference, Hernan & Robins (2020) Ch. 17 Secs. 17.2 and 17.4"
  ))
}

morie_cheatsheet <- function() {
  paste0("tmlcen: censoring by IPCW. right = Gbar_c(k|A,W) = ",
         "prod(1-lambda_C), weight person-time by 1/Gbar_c, hazard ",
         "to survival by the product limit (H&R Ch.17). interval = ",
         "Sec.8.5's (1/M) sum (1-Delta_m) r(C_m) I(A=a) / ",
         "(gbar_c g), with L = max C_j st Delta=0 and R = min C_j ",
         "st Delta=1.")
}

# compact alias per ledger/NAMING.md
morie_tmlecensoring <- morie_tmle_censoring
