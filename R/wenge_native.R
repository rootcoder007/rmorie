# morie.fn -- function file (rootcoder007/morie)
# Inverse-odds weighting for causal mediation.
#
# Tchetgen Tchetgen & Shpitser (2012) give three representations of the
# mediation functional (Pearl's mediation formula)
#   theta_0 = int E(Y | E=1, M=m, X=x) f_{M|E,X}(m | E=0, x) f_X(x)
#             d mu(m, x),
# which is E(Y_{1,M_0}), the outcome had everyone been exposed but the
# mediator kept at the level it would have taken unexposed. All three
# are implemented, because they are the SAME number on a nonparametric
# model and diverge only once parametric models are imposed:
#   "ym"  Strategy 1, the plug-in: fit the outcome regression and the
#         mediator density and integrate.
#   "ye"  Strategy 2, P_n[ I(E=0)/f_{E|X}(0|X) Ehat(Y | E=1, M, X) ].
#   "em"  Strategy 3, the inverse-odds form:
#         P_n[ Y I(E=1)/f_{E|X}(E|X)
#              f_{M|E,X}(M|E=0,X) / f_{M|E,X}(M|E,X) ].
#
# On a saturated model the three agree, are all efficient, and share a
# common influence function.
#
# NDE = theta_0 - E(Y_0), NIE = E(Y_1) - theta_0.
#
# Assumptions: consistency; sequential ignorability (no exposure-induced
# confounder of the mediator-outcome relation); positivity of f_{E|X}
# and f_{M|E,X}. The positivity part is checkable and is checked -- a
# zero density raises rather than producing an infinite weight.
#
# References
# ----------
# Tchetgen Tchetgen, E. J. & Shpitser, I. (2012) "Semiparametric theory
# for causal mediation analysis", The Annals of Statistics 40(3),
# 1816-1845, doi:10.1214/12-AOS990; arXiv:1210.4654. Equation (2), Sec.
# 3.
#
# Imai, K., Keele, L. & Tingley, D. (2010) "A general approach to
# causal mediation analysis", Psychological Methods 15(4), 309-334,
# doi:10.1037/a0020761.
#
# Pearl, J. (2001) "Direct and indirect effects", UAI 17, 411-420.

.wenge_STRATEGIES <- c("em", "ye", "ym", "all")

#' .wenge_key
#'
#' A step of the wenge_native implementation. Called by \code{.wenge_saturated_models}, \code{.wenge_saturated_outcome}, \code{morie_wenge_mediation_functional}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param row Coerced to numeric by the body, with \code{as.numeric}.
#' @return A character value.
#' @export
.wenge_key <- function(row) {
  paste(sprintf("%.12g", round(as.numeric(row), 12)), collapse="|")
}

#' Nonparametric cell estimates of f(E|X) and f(M|E,X)
#'
#' A step of the wenge_native implementation. Called by \code{morie_wenge_mediation_functional}, \code{morie_wenge_weight_based_mediation}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param ev A vector; its length is taken and its elements indexed.
#' @param Mm A matrix; indexed by row and column.
#' @param Xm A matrix; indexed by row and column.
#' @return A list with \code{fe1}, \code{fm}.
#' @export
.wenge_saturated_models <- function(ev, Mm, Xm) {
  # Nonparametric cell estimates of f(E|X) and f(M|E,X).
  n <- length(ev)
  xk <- vapply(seq_len(n), function(i) .wenge_key(Xm[i, ]), character(1))
  mk <- vapply(seq_len(n), function(i) .wenge_key(Mm[i, ]), character(1))
  nx <- list()
  nx1 <- list()
  ne <- list()
  nem <- list()
  add <- function(lst, key) {
    lst[[key]] <- (if (is.null(lst[[key]])) 0L else lst[[key]]) + 1L
    lst
  }
  for (i in seq_len(n)) {
    nx <- add(nx, xk[i])
    if (ev[i] == 1.0) {
      nx1 <- add(nx1, xk[i])
    }
    ekey <- paste(ev[i], xk[i], sep="#")
    ne <- add(ne, ekey)
    nem <- add(nem, paste(mk[i], ekey, sep="#"))
  }
  fe1 <- function(i) {
    num <- nx1[[xk[i]]]
    (if (is.null(num)) 0L else num) / nx[[xk[i]]]
  }
  fm <- function(i, e, mkey=NULL) {
    mkstr <- if (is.null(mkey)) mk[i] else .wenge_key(mkey)
    ekey <- paste(e, xk[i], sep="#")
    den <- ne[[ekey]]
    if (is.null(den) || den == 0L) {
      return(0.0)
    }
    num <- nem[[paste(mkstr, ekey, sep="#")]]
    (if (is.null(num)) 0L else num) / den
  }
  list(fe1=fe1, fm=fm)
}

#' Nonparametric E(Y | E, M, X) as a cell mean
#'
#' A step of the wenge_native implementation. Called by \code{morie_wenge_mediation_functional}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param yv A vector; its length is taken and its elements indexed.
#' @param ev A vector; indexed elementwise.
#' @param Mm A matrix; indexed by row and column.
#' @param Xm A matrix; indexed by row and column.
#' @return The value of \code{function}.
#' @export
.wenge_saturated_outcome <- function(yv, ev, Mm, Xm) {
  # Nonparametric E(Y | E, M, X) as a cell mean.
  n <- length(yv)
  xk <- vapply(seq_len(n), function(i) .wenge_key(Xm[i, ]), character(1))
  mk <- vapply(seq_len(n), function(i) .wenge_key(Mm[i, ]), character(1))
  tot <- list()
  cnt <- list()
  for (i in seq_len(n)) {
    key <- paste(ev[i], mk[i], xk[i], sep="#")
    tot[[key]] <- (if (is.null(tot[[key]])) 0.0 else tot[[key]]) + yv[i]
    cnt[[key]] <- (if (is.null(cnt[[key]])) 0L else cnt[[key]]) + 1L
  }
  function(i, e, mkey=NULL) {
    mkstr <- if (is.null(mkey)) mk[i] else .wenge_key(mkey)
    key <- paste(e, mkstr, xk[i], sep="#")
    c <- cnt[[key]]
    if (is.null(c) || c == 0L) 0.0 else tot[[key]] / c
  }
}

#' Logistic f(E|X), Gaussian f(M|E,X), linear E(Y|E,M,X)
#'
#' A step of the wenge_native implementation. Called by \code{morie_wenge_mediation_functional}, \code{morie_wenge_weight_based_mediation}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param yv A vector; its length is taken.
#' @param ev Numeric; combined arithmetically in the body.
#' @param Mm A matrix; indexed by row and column.
#' @param Xm A matrix; indexed by row and column.
#' @param ridge Passed to \code{.s03logit}.
#' @return A list with \code{fe1}, \code{fm}, \code{ey}.
#' @export
.wenge_parametric_models <- function(yv, ev, Mm, Xm, ridge) {
  # Logistic f(E|X), Gaussian f(M|E,X), linear E(Y|E,M,X).
  n <- length(yv)
  Ze <- .s03design(Xm, n)
  be <- .s03logit(Ze, ev, 60L, ridge)
  pe <- vapply(as.numeric(.s03matvec(Ze, be)), .s03sigmoid, numeric(1))
  m1 <- Mm[, 1L]
  Zm <- cbind(ev, Xm)
  bm <- .s03lstsq(.s03design(Zm, n), m1, ridge)
  mu0 <- as.numeric(.s03matvec(.s03design(cbind(0.0 * ev, Xm), n), bm))
  mu1 <- as.numeric(.s03matvec(.s03design(cbind(1.0 + 0.0 * ev, Xm), n), bm))
  muo <- ifelse(ev == 1.0, mu1, mu0)
  resid <- m1 - muo
  s2 <- sum(resid ^ 2) / max(1L, n - length(bm))
  if (s2 <= 0.0) {
    stop("mediation_functional: the mediator model has zero residual variance")
  }
  Zy <- cbind(ev, m1, Xm)
  by <- .s03lstsq(.s03design(Zy, n), yv, ridge)
  fe1 <- function(i) pe[i]
  fm <- function(i, e, mkey=NULL) {
    mval <- if (is.null(mkey)) m1[i] else as.numeric(mkey)[1L]
    mu <- if (e == 1.0) mu1[i] else mu0[i]
    r <- mval - mu
    exp(-0.5 * r * r / s2) / sqrt(2.0 * pi * s2)
  }
  ey <- function(i, e, mkey=NULL) {
    mval <- if (is.null(mkey)) m1[i] else as.numeric(mkey)[1L]
    row <- c(1.0, e, mval, Xm[i, ])
    sum(by * row)
  }
  list(fe1=fe1, fm=fm, ey=ey)
}

#' morie_wenge_mediation_functional
#'
#' A step of the wenge_native implementation. Called by \code{morie_wenge_weight_based_mediation}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param Y Passed to \code{.s03vec}.
#' @param E Passed to \code{.s03vec}.
#' @param M Optional; may be \code{NULL}. Passed to \code{.s03mat}.
#' @param X Optional; may be \code{NULL}. Passed to \code{.s03mat}.
#' @param strategy One of \code{"all"}, \code{"em"}, \code{"ye"}, \code{"ym"}. Defaults to \code{"em"}.
#' @param saturated A flag; the body branches on it. Defaults to \code{TRUE}.
#' @param ridge Passed to \code{.wenge_parametric_models}. Defaults to \code{1e-08}.
#' @return The value of \code{out}, as built in the body.
#' @export
morie_wenge_mediation_functional <- function(Y, E, M, X, strategy="em",
                                             saturated=TRUE, ridge=1e-8) {
  # theta_0 = E(Y_{1, M_0}) by one of the paper's three strategies.
  # saturated=TRUE estimates every conditional nonparametrically by
  # cell means; FALSE uses logistic and linear working models.
  if (!(strategy %in% .wenge_STRATEGIES)) {
    stop(sprintf("mediation_functional: strategy must be one of %s, got %s",
                 paste(.wenge_STRATEGIES, collapse=", "), strategy))
  }
  yv <- .s03vec(Y)
  ev <- .s03vec(E)
  n <- length(yv)
  Mm <- if (!is.null(M)) .s03mat(M) else matrix(0.0, n, 1L)
  Xm <- if (!is.null(X)) .s03mat(X) else matrix(0.0, n, 1L)
  if (length(ev) != n || nrow(Mm) != n || nrow(Xm) != n) {
    stop("mediation_functional: Y, E, M and X must have the same length")
  }
  if (any(!(ev %in% c(0.0, 1.0)))) {
    stop(paste0("mediation_functional: E must be binary 0/1; the mediation ",
                "formula of eq. (2) is defined for a binary exposure"))
  }
  if (saturated) {
    sm <- .wenge_saturated_models(ev, Mm, Xm)
    fe1 <- sm$fe1
    fm <- sm$fm
    ey <- .wenge_saturated_outcome(yv, ev, Mm, Xm)
  } else {
    pm <- .wenge_parametric_models(yv, ev, Mm, Xm, ridge)
    fe1 <- pm$fe1
    fm <- pm$fm
    ey <- pm$ey
  }
  for (i in seq_len(n)) {
    p <- fe1(i)
    if (p <= 0.0 || p >= 1.0) {
      stop(sprintf(paste0("mediation_functional: f(E|X) is %g at observation ",
                          "%d, so positivity fails in the sample and the ",
                          "functional is not identified there"), p, i))
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
          stop(sprintf(paste0("mediation_functional: f(M|E,X) is zero at ",
                              "observation %d, so the inverse-odds weight ",
                              "is undefined"), i))
        }
        tot <- tot + yv[i] * (d0 / d1) / fe1(i)
      }
    }
    out$em <- tot / n
  }
  if (strategy %in% c("ym", "all")) {
    # plug-in: average over the observed X of the mediator-density-
    # weighted outcome regression, with M ranging over its support
    seen <- character(0)
    support <- list()
    for (i in seq_len(n)) {
      key <- .wenge_key(Mm[i, ])
      if (!(key %in% seen)) {
        seen <- c(seen, key)
        support <- c(support, list(Mm[i, ]))
      }
    }
    ord <- order(seen)
    support <- support[ord]
    tot <- 0.0
    for (i in seq_len(n)) {
      s <- 0.0
      for (mrow in support) {
        w <- fm(i, 0.0, mrow)
        if (w > 0.0) {
          s <- s + w * ey(i, 1.0, mrow)
        }
      }
      tot <- tot + s
    }
    out$ym <- tot / n
  }
  if (strategy != "all") {
    return(out[[strategy]])
  }
  out
}

#' morie_wenge_weight_based_mediation
#'
#' A step of the wenge_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X Passed to \code{.s03vec}.
#' @param M Optional; may be \code{NULL}. Passed to \code{morie_wenge_mediation_functional}.
#' @param C Optional; may be \code{NULL}. Passed to \code{morie_wenge_mediation_functional}.
#' @param Y Passed to \code{.s03vec}.
#' @param strategy Passed to \code{morie_wenge_mediation_functional}. Defaults to \code{"em"}.
#' @param saturated A flag; the body branches on it. Defaults to \code{TRUE}.
#' @return The value of \code{out}, as built in the body.
#' @export
morie_wenge_weight_based_mediation <- function(X, M, C, Y, strategy="em",
                                               saturated=TRUE) {
  # Natural direct and indirect effects by inverse-odds weighting. The
  # argument order is the stub's: X is the binary exposure (the paper's
  # E), M the mediator, C the pre-exposure confounders (the paper's X),
  # Y the outcome.
  ev <- .s03vec(X)
  yv <- .s03vec(Y)
  n <- length(yv)
  theta <- morie_wenge_mediation_functional(yv, ev, M, C, strategy=strategy,
                                            saturated=saturated)
  thetas <- if (is.list(theta)) theta else stats::setNames(list(theta),
                                                           strategy)
  point <- if (!is.null(thetas$em)) thetas$em else thetas[[1L]]
  Mm <- if (!is.null(M)) .s03mat(M) else matrix(0.0, n, 1L)
  Cm <- if (!is.null(C)) .s03mat(C) else matrix(0.0, n, 1L)
  if (saturated) {
    fe1 <- .wenge_saturated_models(ev, Mm, Cm)$fe1
  } else {
    fe1 <- .wenge_parametric_models(yv, ev, Mm, Cm, 1e-8)$fe1
  }
  ey1 <- sum(vapply(seq_len(n),
                    function(i) yv[i] * ev[i] / fe1(i), numeric(1))) / n
  ey0 <- sum(vapply(seq_len(n),
                    function(i) yv[i] * (1.0 - ev[i]) / (1.0 - fe1(i)),
                    numeric(1))) / n
  out <- list(
    estimate=ey1 - point, nie=ey1 - point, nde=point - ey0,
    theta=point, ey1=ey1, ey0=ey0, total=ey1 - ey0, n=n,
    saturated=isTRUE(saturated), strategy=strategy,
    method=sprintf(paste0("natural direct and indirect effects via the ",
                          "mediation functional, Tchetgen Tchetgen & ",
                          "Shpitser (2012) strategy %s"), strategy))
  for (kk in names(thetas)) {
    out[[paste0("theta_", kk)]] <- thetas[[kk]]
  }
  out
}

#' morie_wenge_cheatsheet
#'
#' A step of the wenge_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
morie_wenge_cheatsheet <- function() {
  paste0(
    "wenge: mediation functional theta = E(Y_1,M_0) three ways ",
    "(Tchetgen Tchetgen-Shpitser 2012). em = inverse-odds ",
    "Y I(E=1)/f(E|X) * f(M|E=0,X)/f(M|E,X); ye = outcome model ",
    "reweighted to the unexposed; ym = plug-in. Identical on a ",
    "saturated model. NDE = theta - E(Y_0), NIE = E(Y_1) - theta."
  )
}

# compact alias per ledger/NAMING.md
morie_wenge_weightbasedmediation <- morie_wenge_weight_based_mediation

#' @export
morie_wenge <- morie_wenge_weight_based_mediation
