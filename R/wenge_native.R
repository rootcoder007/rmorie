# morie.fn -- function file (rootcoder007/morie)
# Inverse-odds weighting for causal mediation.
#
# Tchetgen Tchetgen & Shpitser (2012) give three representations of the
# mediation functional -- what Pearl calls the mediation formula --
#
#   theta_0 = \iint E(Y | E=1, M=m, X=x) *
#             f_{M|E,X}(m | E=0, x) * f_X(x) d(mu)(m,x),
#
# which is E(Y_{1,M_0}), the outcome had everyone been exposed but the
# mediator kept at the level it would have taken unexposed.  All three
# are implemented, because the paper's central structural point is that
# they are the same number on a nonparametric model and diverge only
# once parametric models are imposed:
#
#   "ym" - Strategy 1, the plug-in: fit the outcome regression and the
#          mediator density and integrate.
#   "ye" - Strategy 2, the outcome model reweighted to the unexposed.
#   "em" - Strategy 3, the inverse-odds form the module is named for,
#          P_n [ Y * I(E=1)/f(E|X) * f(M|E=0,X)/f(M|E,X) ].
#
# The natural direct and indirect effects follow, with delta_0 = E(Y_0):
#
#   NDE = theta_0 - delta_0,
#   NIE = E(Y_1) - theta_0.
#
# Assumptions, stated because they are not checkable from the data:
# consistency; sequential ignorability, which requires no exposure-
# induced confounder of the mediator-outcome relation; and positivity
# of both f(E|X) and f(M|E,X).  The positivity part IS checkable and
# is checked -- a zero density raises rather than producing a weight
# of infinity that quietly becomes a number.
#
# References
# ----------
# Tchetgen Tchetgen, E. J. & Shpitser, I. (2012) "Semiparametric theory
# for causal mediation analysis: efficiency bounds, multiple robustness
# and sensitivity analysis", The Annals of Statistics 40(3), 1816-1845,
# doi:10.1214/12-AOS990; arXiv:1210.4654.  Equation (2) and the three
# strategies of Sec. 3.
#
# Imai, K., Keele, L. & Tingley, D. (2010) "A general approach to causal
# mediation analysis", Psychological Methods 15(4), 309-334,
# doi:10.1037/a0020761 -- the sequential ignorability condition the
# paper adopts.
#
# Pearl, J. (2001) "Direct and indirect effects", Proceedings of the
# Seventeenth Conference on Uncertainty in Artificial Intelligence,
# 411-420 -- natural direct and indirect effects.

# Strategy constants
.wenge_STRATEGIES <- c("em", "ye", "ym", "all")

# Helper: cell key from a row vector
.wenge_cell_key <- function(row) {
  v <- round(as.numeric(row), 12)
  paste(format(v, nsmall = 12, scientific = FALSE, trim = FALSE),
        collapse = ",")
}

# Helper: convert to numeric vector
.wenge_vec <- function(x) {
  if (is.null(x)) return(numeric(0))
  if (is.matrix(x)) return(as.numeric(x))
  as.numeric(unlist(x))
}

# Helper: convert to matrix (ncol >= 1)
.wenge_mat <- function(x) {
  if (is.null(x)) return(matrix(0, nrow = 0, ncol = 1))
  if (is.matrix(x)) return(x)
  m <- as.matrix(x)
  if (is.null(dim(m))) m <- matrix(m, ncol = 1)
  m
}

# Helper: design matrix with intercept column
.wenge_design <- function(Z, n) {
  Z <- .wenge_mat(Z)
  if (nrow(Z) == 0) return(matrix(1, nrow = n, ncol = 1))
  cbind(1, Z)
}

# Helper: sigmoid
.wenge_sigmoid <- function(v) {
  1 / (1 + exp(-v))
}

# Helper: matrix-vector product
.wenge_matvec <- function(A, b) {
  as.numeric(A %*% b)
}

# Helper: least squares with ridge (A'A + rI) x = A'b
.wenge_lstsq <- function(A, b, ridge) {
  AtA <- crossprod(A)
  p <- ncol(AtA)
  AtA <- AtA + diag(ridge, p)
  Atb <- crossprod(A, b)
  solve(AtA, Atb)
}

# Helper: IRLS for logistic regression
.wenge_logit_irls <- function(Z, y, max_iter, ridge) {
  Z <- .wenge_mat(Z)
  y <- .wenge_vec(y)
  n <- nrow(Z)
  p <- ncol(Z)

  beta <- rep(0, p)
  for (iter in seq_len(max_iter)) {
    eta <- as.numeric(Z %*% beta)
    mu <- .wenge_sigmoid(eta)
    w <- mu * (1 - mu)
    w <- pmax(w, 1e-10)
    z <- eta + (y - mu) / w

    WZ <- Z * w
    A <- crossprod(Z, WZ) + diag(ridge, p)
    Wz <- w * z
    b <- as.numeric(crossprod(Z, Wz))

    beta_new <- solve(A, b)
    if (max(abs(beta_new - beta)) < 1e-8) {
      beta <- beta_new
      break
    }
    beta <- beta_new
  }
  beta
}

# Helper: density at (wrapper for fm)
.wenge_density_at <- function(fm, i, mkey, e) {
  fm(i, e, mkey)
}

# Saturated nonparametric models for f(E|X) and f(M|E,X)
.wenge_saturated_models <- function(ev, Mm, Xm) {
  n <- length(ev)
  xk <- lapply(seq_len(nrow(Xm)),
               function(i) round(as.numeric(Xm[i, ]), 12))
  mk <- lapply(seq_len(nrow(Mm)),
               function(i) round(as.numeric(Mm[i, ]), 12))

  nx <- list()
  nx1 <- list()
  ne <- list()
  nem <- list()

  for (i in seq_len(n)) {
    xk_i <- .wenge_cell_key(xk[[i]])
    mk_i <- .wenge_cell_key(mk[[i]])

    nx[[xk_i]] <- if (is.null(nx[[xk_i]])) 1L else nx[[xk_i]] + 1L

    if (ev[i] == 1.0) {
      nx1[[xk_i]] <- if (is.null(nx1[[xk_i]])) 1L else nx1[[xk_i]] + 1L
    }

    e_x_key <- paste(ev[i], xk_i, sep = ",")
    ne[[e_x_key]] <- if (is.null(ne[[e_x_key]])) 1L else ne[[e_x_key]] + 1L

    m_e_x_key <- paste(mk_i, ev[i], xk_i, sep = ",")
    nem[[m_e_x_key]] <- if (is.null(nem[[m_e_x_key]])) 1L
                       else nem[[m_e_x_key]] + 1L
  }

  fe1 <- function(i) {
    xk_i <- .wenge_cell_key(xk[[i]])
    if (is.null(nx1[[xk_i]])) 0.0 else nx1[[xk_i]] / nx[[xk_i]]
  }

  fm <- function(i, e, mkey = NULL) {
    xk_i <- .wenge_cell_key(xk[[i]])
    mk_i <- if (is.null(mkey)) .wenge_cell_key(mk[[i]])
            else .wenge_cell_key(mkey)

    m_e_x_key <- paste(mk_i, e, xk_i, sep = ",")
    e_x_key <- paste(e, xk_i, sep = ",")

    den <- if (is.null(ne[[e_x_key]])) 0 else ne[[e_x_key]]
    if (den == 0) return(0.0)
    num <- if (is.null(nem[[m_e_x_key]])) 0 else nem[[m_e_x_key]]
    num / den
  }

  list(fe1 = fe1, fm = fm)
}

# Saturated nonparametric E(Y | E, M, X)
.wenge_saturated_outcome <- function(yv, ev, Mm, Xm) {
  n <- length(yv)
  xk <- lapply(seq_len(nrow(Xm)),
               function(i) round(as.numeric(Xm[i, ]), 12))
  mk <- lapply(seq_len(nrow(Mm)),
               function(i) round(as.numeric(Mm[i, ]), 12))

  tot <- list()
  cnt <- list()

  for (i in seq_len(n)) {
    key <- paste(ev[i], .wenge_cell_key(mk[[i]]),
                 .wenge_cell_key(xk[[i]]), sep = ",")
    tot[[key]] <- if (is.null(tot[[key]])) yv[i] else tot[[key]] + yv[i]
    cnt[[key]] <- if (is.null(cnt[[key]])) 1L else cnt[[key]] + 1L
  }

  ey <- function(i, e, mkey = NULL) {
    mk_i <- if (is.null(mkey)) mk[[i]] else mkey
    key <- paste(e, .wenge_cell_key(mk_i),
                 .wenge_cell_key(xk[[i]]), sep = ",")
    c <- if (is.null(cnt[[key]])) 0 else cnt[[key]]
    if (c == 0) 0.0 else tot[[key]] / c
  }
  ey
}

# Parametric models: logistic E|X, Gaussian M|E,X, linear Y|E,M,X
.wenge_parametric_models <- function(yv, ev, Mm, Xm, ridge) {
  n <- length(yv)

  # Logistic regression for E|X
  Ze <- .wenge_design(Xm, n)
  be <- .wenge_logit_irls(Ze, ev, 60, ridge)
  pe <- .wenge_sigmoid(as.numeric(Ze %*% be))

  # Linear regression for M|E,X
  m1 <- Mm[, 1]
  Zm <- cbind(ev, Xm)
  Zm_design <- .wenge_design(Zm, n)
  bm <- .wenge_lstsq(Zm_design, m1, ridge)

  Zm0 <- cbind(0, Xm)
  Zm1 <- cbind(1, Xm)
  mu0 <- as.numeric(.wenge_design(Zm0, n) %*% bm)
  mu1 <- as.numeric(.wenge_design(Zm1, n) %*% bm)

  muo <- ifelse(ev == 1.0, mu1, mu0)
  resid <- m1 - muo
  s2 <- sum(resid * resid) / max(1, n - length(bm))
  if (s2 <= 0.0) {
    stop("mediation_functional: the mediator model has zero residual variance")
  }

  # Linear regression for Y|E,M,X
  Zy <- cbind(ev, m1, Xm)
  Zy_design <- .wenge_design(Zy, n)
  by <- .wenge_lstsq(Zy_design, yv, ridge)

  fe1 <- function(i) pe[i]

  fm <- function(i, e, mkey = NULL) {
    mval <- if (is.null(mkey)) m1[i] else as.numeric(mkey)[1]
    mu <- if (e == 1.0) mu1[i] else mu0[i]
    r <- mval - mu
    exp(-0.5 * r * r / s2) / sqrt(2.0 * pi * s2)
  }

  ey <- function(i, e, mkey = NULL) {
    mval <- if (is.null(mkey)) m1[i] else as.numeric(mkey)[1]
    row <- c(1.0, e, mval, as.numeric(Xm[i, ]))
    sum(by * row)
  }

  list(fe1 = fe1, fm = fm, ey = ey)
}

# Mediation functional: theta_0 = E(Y_{1, M_0}) by one of three strategies
.wenge_mediation_functional <- function(Y, E, M, X, strategy = "em",
                                        saturated = TRUE, ridge = 1e-8) {
  if (!(strategy %in% .wenge_STRATEGIES)) {
    stop(sprintf("mediation_functional: strategy must be one of %s, got %s",
                 paste(.wenge_STRATEGIES, collapse = ", "), strategy))
  }

  yv <- .wenge_vec(Y)
  ev <- .wenge_vec(E)
  n <- length(yv)

  if (is.null(M)) {
    Mm <- matrix(0, nrow = n, ncol = 1)
  } else {
    Mm <- .wenge_mat(M)
    if (nrow(Mm) != n) {
      stop(sprintf("mediation_functional: Y has %d rows but M has %d",
                   n, nrow(Mm)))
    }
  }

  if (is.null(X)) {
    Xm <- matrix(0, nrow = n, ncol = 1)
  } else {
    Xm <- .wenge_mat(X)
    if (nrow(Xm) != n) {
      stop(sprintf("mediation_functional: Y has %d rows but X has %d",
                   n, nrow(Xm)))
    }
  }

  if (any(ev != 0.0 & ev != 1.0)) {
    stop("mediation_functional: E must be binary 0/1; the mediation "
         "formula of eq. (2) is defined for a binary exposure")
  }

  if (saturated) {
    sat <- .wenge_saturated_models(ev, Mm, Xm)
    fe1 <- sat$fe1
    fm <- sat$fm
    ey <- .wenge_saturated_outcome(yv, ev, Mm, Xm)
  } else {
    par <- .wenge_parametric_models(yv, ev, Mm, Xm, ridge)
    fe1 <- par$fe1
    fm <- par$fm
    ey <- par$ey
  }

  for (i in seq_len(n)) {
    p <- fe1(i)
    if (p <= 0.0 || p >= 1.0) {
      stop(sprintf("mediation_functional: f(E|X) is %g at observation %d, "
                   "so positivity fails in the sample and the functional "
                   "is not identified there", p, i))
    }
  }

  out <- list()

  if (strategy %in% c("ye", "all")) {
    tot <- 0.0
    for (i in seq_len(n)) {
      if (ev[i] == 0.0) {
        tot <- tot + ey(i, 1.0) / (1.0 - fe1(i))
      }
    }
    out$ye <- tot / n
  }

  if (strategy %in% c("em", "all")) {
    tot <- 0.0
    for (i in seq_len(n)) {
      if (ev[i] == 1.0) {
        d1 <- fm(i, 1.0)
        d0 <- fm(i, 0.0)
        if (d1 <= 0.0) {
          stop(sprintf("mediation_functional: f(M|E,X) is zero at "
                       "observation %d, so the inverse-odds weight is "
                       "undefined", i))
        }
        tot <- tot + yv[i] * (d0 / d1) / fe1(i)
      }
    }
    out$em <- tot / n
  }

  if (strategy %in% c("ym", "all")) {
    # Support of M: unique cell keys
    support <- unique(lapply(seq_len(nrow(Mm)),
                             function(i) round(as.numeric(Mm[i, ]), 12)))
    tot <- 0.0
    for (i in seq_len(n)) {
      s <- 0.0
      for (mkey in support) {
        w <- .wenge_density_at(fm, i, mkey, 0.0)
        if (w > 0.0) {
          s <- s + w * ey(i, 1.0, mkey)
        }
      }
      tot <- tot + s
    }
    out$ym <- tot / n
  }

  if (strategy != "all") return(out[[strategy]])
  out
}

# Main entry point: natural direct and indirect effects via inverse-odds
# weighting.  Argument order is the stub's: X is the binary exposure
# (the paper's E), M the mediator, C the pre-exposure confounders (the
# paper's X), Y the outcome.
morie_wenge <- function(X, M, C, Y, strategy = "em", saturated = TRUE) {
  ev <- .wenge_vec(X)
  yv <- .wenge_vec(Y)
  n <- length(yv)

  theta <- .wenge_mediation_functional(Y, X, M, C,
                                       strategy = strategy,
                                       saturated = saturated)

  if (is.list(theta) && !is.null(names(theta))) {
    thetas <- theta
  } else {
    thetas <- list()
    thetas[[strategy]] <- theta
  }

  # Prefer the "em" estimate as the point summary if available
  if (!is.null(thetas$em)) {
    point <- thetas$em
  } else {
    point <- thetas[[1]]
  }

  # E(Y_1) and E(Y_0) by the same nonparametric standardisation
  Mm_for_est <- if (is.null(M)) matrix(0, nrow = n, ncol = 1)
                else .wenge_mat(M)
  Xm_for_est <- if (is.null(C)) matrix(0, nrow = n, ncol = 1)
                else .wenge_mat(C)

  if (saturated) {
    sat <- .wenge_saturated_models(ev, Mm_for_est, Xm_for_est)
    fe1 <- sat$fe1
  } else {
    par <- .wenge_parametric_models(yv, ev, Mm_for_est, Xm_for_est, 1e-8)
    fe1 <- par$fe1
  }

  ey1 <- 0.0
  ey0 <- 0.0
  for (i in seq_len(n)) {
    ey1 <- ey1 + yv[i] * ev[i] / fe1(i)
    ey0 <- ey0 + yv[i] * (1.0 - ev[i]) / (1.0 - fe1(i))
  }
  ey1 <- ey1 / n
  ey0 <- ey0 / n

  out <- list(
    estimate  = ey1 - point,        # natural indirect effect
    nie       = ey1 - point,
    nde       = point - ey0,
    theta     = point,
    ey1       = ey1,
    ey0       = ey0,
    total     = ey1 - ey0,
    n         = n,
    saturated = as.logical(saturated),
    strategy  = strategy,
    method    = sprintf("natural direct and indirect effects via the "
                        "mediation functional, Tchetgen Tchetgen & "
                        "Shpitser (2012) strategy '%s'", strategy)
  )

  for (kk in names(thetas)) {
    out[[paste0("theta_", kk)]] <- thetas[[kk]]
  }

  out
}
