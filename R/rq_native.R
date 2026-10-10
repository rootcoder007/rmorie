# SPDX-License-Identifier: AGPL-3.0-or-later
# Linear quantile regression, native.
# Source: Koenker, R. and Bassett, G. (1978), Regression quantiles,
# Econometrica 46, 33-50 (the estimator); Portnoy, S. and Koenker, R.
# (1997), The Gaussian hare and the Laplacian tortoise, Statistical
# Science 12(4), 279-300 (the Frisch-Newton primal-dual interior point
# algorithm with the Mehrotra predictor-corrector step); Koenker, R.
# (2005), Quantile Regression, Cambridge University Press, Sec. 3.4
# (standard errors: Hendricks-Koenker sandwich "nid", the iid sparsity
# estimate "iid", Powell's kernel sandwich "ker", the xy-pair bootstrap
# "boot"), and Hall, P. and Sheather, S. (1988), JRSS B 50, 381-391
# (bandwidth).
#
# The interior point iteration follows, step for step, the Fortran
# routine lpfnb of the quantreg package (rq.fit.fnb): the same starting
# point, the same affine-scaling predictor, the same Mehrotra corrector
# with centring mu = mu0 (g / mu0)^3 / (2n), the same step-length rule
# (0.99995 of the distance to the boundary, capped at 1) and the same
# duality-gap stopping rule (gap < 1e-6).  Each iteration forms and
# Cholesky-factors the p x p matrix X' D X, so the cost is O(n p^2) per
# iteration and the number of iterations grows only slowly with n.
#
# "br" returns an exact vertex (basic) solution, as the Barrodale-Roberts
# simplex of quantreg's rq.fit.br does: the interior point solution is
# purified onto the p observations with the smallest absolute residuals
# and then Barrodale-Roberts pivots are taken until the dual certificate
# v in [tau - 1, tau]^p holds.  When the solution is unique both methods
# reach the same point (the interior point one to within its 1e-6 gap).

# Rho (check) loss.
.rqn_rho <- function(u, tau) sum(u * (tau - (u < 0)))


#' Frisch-Newton interior point solver for one regression quantile
#'
#' Internal: the primal-dual predictor-corrector of Portnoy and Koenker
#' (1997) in the formulation of quantreg's \code{lpfnb}.
#'
#' @param X Numeric design matrix (n x p, full column rank).
#' @param y Numeric response of length n.
#' @param tau Quantile in (0, 1).
#' @param beta Step-length damping factor.
#' @param eps Duality-gap tolerance.
#' @param maxit Iteration limit.
#' @return A list with \code{coefficients}, \code{residuals} and
#'   \code{nit} (iterations, corrector steps, n).
#' @keywords internal
.rqn_fnb <- function(X, y, tau, beta = 0.99995, eps = 1e-6, maxit = 500L) {
  if (tau < eps || tau > 1 - eps)
    stop("the Frisch-Newton method needs tau in (0, 1).", call. = FALSE)
  storage.mode(X) <- "double"
  # the iterations run in C++ (src/morie_rq_fnb.cpp): the same steps, without R's per-step
  # allocation of a dozen length-n vectors
  r <- tryCatch(.rqn_fnb_impl(X, as.double(y), tau, beta, eps, as.integer(maxit)),
                error = function(e) stop(conditionMessage(e), call. = FALSE))
  yv <- r$yv
  gap <- r$gap
  it <- r$it
  ncor <- r$ncor
  n <- nrow(X)
  if (gap > eps)
    warning(sprintf("Frisch-Newton stopped at the iteration limit with duality gap %.3g.", gap),
            call. = FALSE)
  coef <- -drop(yv)
  list(coefficients = coef, residuals = drop(y - X %*% coef),
       nit = c(it, ncor, n), gap = gap)
}

#' Basic solution and its dual certificate
#'
#' Internal: for the basis \code{h} computes the interpolating
#' coefficients, the residuals and the dual vector
#' \eqn{v = -X_h^{-T} \sum_{i \notin h} x_i \psi_\tau(r_i)}; the basis is
#' optimal when every \eqn{v_j \in [\tau - 1, \tau]}.
#'
#' @param X Design matrix.
#' @param y Response.
#' @param tau Quantile.
#' @param h Integer basis of length p.
#' @param tolz Residuals with absolute value at most \code{tolz} count as zero.
#' @param rref Optional reference residuals whose signs decide
#'   \eqn{\psi} at zero residuals (default: treated as positive).
#' @return A list with \code{XHi}, \code{coefficients}, \code{residuals},
#'   \code{v} and \code{optimal}, or NULL for a singular basis.
#' @keywords internal
.rqn_cert <- function(X, y, tau, h, tolz, rref = NULL) {
  XHi <- tryCatch(solve(X[h, , drop = FALSE]), error = function(e) NULL)
  if (is.null(XHi)) return(NULL)
  bh <- drop(XHi %*% y[h])
  r <- drop(y - X %*% bh)
  r[h] <- 0
  neg <- r < -tolz
  if (!is.null(rref)) {
    z <- abs(r) <= tolz
    neg[z] <- rref[z] < 0
  }
  psi <- tau - neg
  psi[h] <- 0
  v <- -drop(crossprod(XHi, drop(crossprod(X, psi))))
  ok <- all(v >= tau - 1 - 1e-10 & v <= tau + 1e-10)
  list(XHi = XHi, coefficients = bh, residuals = r, v = v, optimal = ok)
}

#' Barrodale-Roberts pivots from a starting basis
#'
#' Internal: exchanges basic observations along the edge of steepest
#' violation of the dual box, with an exact line search over the
#' breakpoints of the piecewise-linear objective, until the basis is
#' certified optimal.
#'
#' @param X Design matrix.
#' @param y Response.
#' @param tau Quantile.
#' @param h Starting basis.
#' @param tolz Zero tolerance for residuals.
#' @param maxpiv Pivot limit.
#' @return A list with \code{basis}, \code{cert} (from \code{.rqn_cert}),
#'   \code{pivots}, \code{optimal} and \code{why}.
#' @keywords internal
.rqn_pivot <- function(X, y, tau, h, tolz, maxpiv) {
  piv <- 0L
  why <- "pivot limit"
  repeat {
    ct <- .rqn_cert(X, y, tau, h, tolz)
    if (is.null(ct)) {
      why <- "singular basis"
      break
    }
    if (ct$optimal)
      return(list(basis = h, cert = ct, pivots = piv, optimal = TRUE, why = ""))
    if (piv >= maxpiv) break
    piv <- piv + 1L
    v <- ct$v
    r <- ct$residuals
    # Leave the basis at the most violated position j.
    j <- which.max(pmax(tau - 1 - v, v - tau, 0))
    sgn <- if (v[j] < tau - 1) 1 else -1
    slope <- if (sgn > 0) v[j] + 1 - tau else tau - v[j]
    a <- drop(X %*% (sgn * ct$XHi[, j]))
    a[h] <- 0
    cand <- which((r >= -tolz & a > 0) | (r < -tolz & a < 0))
    cand <- cand[!(cand %in% h)]
    if (!length(cand)) {
      why <- "unbounded direction"
      break
    }
    tt <- pmax(r[cand] / a[cand], 0)
    o <- order(tt)
    k <- which(slope + cumsum(abs(a[cand][o])) >= 0)[1L]
    if (is.na(k)) {
      why <- "unbounded direction"
      break
    }
    h[j] <- cand[o][k]
  }
  list(basis = h, cert = NULL, pivots = piv, optimal = FALSE, why = why)
}

#' Exact vertex solution by purification and Barrodale-Roberts pivots
#'
#' Internal: moves a (near-)optimal coefficient vector onto an optimal
#' basic solution, i.e. one interpolating p observations, and verifies
#' it with the dual certificate.  If the pivots stall on a degenerate
#' problem (many zero residuals), they are rerun on a response perturbed
#' by a deterministic amount far below the data's resolution, which
#' removes the ties; the resulting basis is then certified on the
#' original data, the signs of the perturbed residuals choosing
#' \eqn{\psi} at the tied observations (a valid choice in
#' \eqn{[\tau - 1, \tau]}).
#'
#' @param X Numeric design matrix.
#' @param y Numeric response.
#' @param tau Quantile in (0, 1).
#' @param b0 Starting coefficients (the interior point solution).
#' @param maxpiv Pivot limit.
#' @return A list with \code{coefficients}, \code{residuals},
#'   \code{basis}, \code{dual}, \code{pivots}, \code{optimal},
#'   \code{nonunique} and \code{why}.
#' @keywords internal
.rqn_vertex <- function(X, y, tau, b0, maxpiv = NULL) {
  n <- nrow(X)
  p <- ncol(X)
  if (is.null(maxpiv)) maxpiv <- 50L * p + 50L
  r0 <- drop(y - X %*% b0)
  ord <- order(abs(r0))
  m <- min(n, max(3L * p, p + 10L))
  repeat {
    cand <- ord[seq_len(m)]
    qq <- qr(t(X[cand, , drop = FALSE]), tol = 1e-9)
    if (qq$rank >= p || m == n) break
    m <- min(n, 4L * m)
  }
  if (qq$rank < p) stop("singular design matrix.", call. = FALSE)
  h0 <- cand[qq$pivot[seq_len(p)]]
  scl <- max(abs(y), 1)
  tolz <- 1e-12 * scl
  done <- function(ct, h, piv) {
    v <- ct$v
    list(coefficients = ct$coefficients, residuals = ct$residuals, basis = h,
         dual = v, pivots = piv, optimal = TRUE,
         nonunique = any(abs(v - tau) < 1e-10 | abs(v - (tau - 1)) < 1e-10),
         why = "")
  }
  pv <- .rqn_pivot(X, y, tau, h0, tolz, maxpiv)
  if (pv$optimal) return(done(pv$cert, pv$basis, pv$pivots))
  why <- pv$why
  piv <- pv$pivots
  xi <- ((seq_len(n) * 0.6180339887498949) %% 1) - 0.5
  for (dl in c(1e-9, 1e-11, 1e-7)) {
    yp <- y + dl * scl * xi
    pp <- .rqn_pivot(X, yp, tau, h0, 0, maxpiv)
    piv <- piv + pp$pivots
    if (!pp$optimal) next
    ct <- .rqn_cert(X, y, tau, pp$basis, tolz, rref = pp$cert$residuals)
    if (!is.null(ct) && ct$optimal) return(done(ct, pp$basis, piv))
  }
  list(coefficients = b0, residuals = r0, basis = NULL, dual = NULL,
       pivots = piv, optimal = FALSE, nonunique = NA, why = why)
}

#' Merge identical observations into weighted rows
#'
#' Internal: rows of \code{cbind(X, y)} that are exactly equal are
#' replaced by one row multiplied by their count, which leaves the check
#' loss unchanged.  Candidate ties are found by a fixed linear key and
#' then confirmed element by element.
#'
#' @param X Design matrix.
#' @param y Response.
#' @return A list with \code{X}, \code{y} and \code{count}.
#' @keywords internal
.rqn_aggregate <- function(X, y) {
  Z <- cbind(X, y)
  kv <- sqrt(c(2, 3, 5, 7, 11, 13, 17, 19, 23, 29)[(seq_len(ncol(Z)) - 1L) %% 10L + 1L]) +
    seq_len(ncol(Z)) / 7
  key <- drop(Z %*% kv)
  if (!anyDuplicated(key)) return(list(X = X, y = y, count = rep(1, nrow(X))))
  g <- match(key, key)
  same <- rowSums(Z != Z[g, , drop = FALSE]) == 0
  g[!same] <- seq_along(g)[!same]
  first <- which(g == seq_along(g))
  cnt <- tabulate(g, nbins = length(g))[first]
  list(X = X[first, , drop = FALSE] * cnt, y = y[first] * cnt, count = cnt)
}

#' Fit one regression quantile by the chosen method
#'
#' @param X Design matrix.
#' @param y Response.
#' @param tau Quantile.
#' @param method \code{"fn"} or \code{"br"}.
#' @return A list as from \code{.rqn_fnb}, plus \code{vertex} for "br".
#' @keywords internal
.rqn_fit1 <- function(X, y, tau, method) {
  if (method == "br") {
    # Identical observations are merged (row times multiplicity; the
    # check loss is positively homogeneous) so that tied copies of a
    # basic observation cannot make the vertex step degenerate.
    ag <- .rqn_aggregate(X, y)
    f <- .rqn_fnb(ag$X, ag$y, tau)
    v <- .rqn_vertex(ag$X, ag$y, tau, f$coefficients)
    if (!v$optimal)
      warning("the simplex finish did not certify a vertex; returning the interior point solution.",
              call. = FALSE)
    f$coefficients <- v$coefficients
    f$residuals <- drop(y - X %*% v$coefficients)
    f$vertex <- v
    f$nit[3] <- nrow(X)
    return(f)
  }
  .rqn_fnb(X, y, tau)
}

#' Bandwidth for the sparsity estimate (Hall-Sheather or Bofinger)
#'
#' @param tau Quantile.
#' @param n Sample size.
#' @param hs Logical; Hall-Sheather (TRUE) or Bofinger (FALSE).
#' @param alpha Level used by the Hall-Sheather rule.
#' @return The bandwidth on the probability scale.
#' @keywords internal
.rqn_bandwidth <- function(tau, n, hs = TRUE, alpha = 0.05) {
  x0 <- stats::qnorm(tau)
  f0 <- stats::dnorm(x0)
  if (hs)
    n^(-1 / 3) * stats::qnorm(1 - alpha / 2)^(2 / 3) * ((1.5 * f0^2) / (2 * x0^2 + 1))^(1 / 3)
  else n^-0.2 * ((4.5 * f0^4) / (2 * x0^2 + 1)^2)^0.2
}

#' Covariance of a regression quantile estimate
#'
#' @param X Design matrix (weighted if weights were given).
#' @param y Response (weighted likewise).
#' @param coef Fitted coefficients.
#' @param resid Residuals.
#' @param tau Quantile.
#' @param se \code{"nid"}, \code{"iid"}, \code{"ker"} or \code{"boot"}.
#' @param method Fitting method used for the auxiliary fits.
#' @param hs Hall-Sheather (TRUE) or Bofinger (FALSE) bandwidth.
#' @param R Bootstrap replications.
#' @param seed Optional bootstrap seed.
#' @return A list with \code{cov} and \code{scale} (the density or
#'   sparsity summary, NA for the bootstrap) and \code{nonpos} (the count
#'   of non-positive local densities, "nid" only).
#' @keywords internal
.rqn_cov <- function(X, y, coef, resid, tau, se, method, hs, R, seed) {
  n <- nrow(X)
  p <- ncol(X)
  eps <- .Machine$double.eps^(1 / 2)
  nonpos <- 0L
  scale <- NA_real_
  if (se == "iid") {
    xxinv <- chol2inv(chol(crossprod(X)))
    pz <- sum(abs(resid) < eps)
    h <- max(p + 1, ceiling(n * .rqn_bandwidth(tau, n, hs = hs)))
    ir <- (pz + 1):(h + pz + 1)
    ord.resid <- sort(resid[order(abs(resid))][ir])
    xt <- ir / (n - p)
    sp <- .rqn_fit1(cbind(1, xt), ord.resid, 0.5, "br")$coefficients[2]
    cov <- sp^2 * xxinv * tau * (1 - tau)
    scale <- 1 / sp
  } else if (se == "nid") {
    h <- .rqn_bandwidth(tau, n, hs = hs)
    while ((tau - h < 0) || (tau + h > 1)) h <- h / 2
    bhi <- .rqn_fit1(X, y, tau + h, method)$coefficients
    blo <- .rqn_fit1(X, y, tau - h, method)$coefficients
    dyhat <- drop(X %*% (bhi - blo))
    nonpos <- sum(dyhat <= 0)
    if (nonpos > 0) warning(paste(nonpos, "non-positive fis"), call. = FALSE)
    f <- pmax(0, (2 * h) / (dyhat - eps))
    fxxinv <- chol2inv(chol(crossprod(X * sqrt(f))))
    cov <- tau * (1 - tau) * fxxinv %*% crossprod(X) %*% fxxinv
    scale <- mean(f)
  } else if (se == "ker") {
    h <- .rqn_bandwidth(tau, n, hs = hs)
    while ((tau - h < 0) || (tau + h > 1)) h <- h / 2
    uhat <- drop(y - X %*% coef)
    qs <- stats::quantile(uhat, c(0.25, 0.75), names = FALSE)
    h <- (stats::qnorm(tau + h) - stats::qnorm(tau - h)) *
      min(sqrt(stats::var(uhat)), (qs[2] - qs[1]) / 1.34)
    f <- stats::dnorm(uhat / h) / h
    fxxinv <- chol2inv(chol(crossprod(X * sqrt(f))))
    cov <- tau * (1 - tau) * fxxinv %*% crossprod(X) %*% fxxinv
    scale <- mean(f)
  } else if (se == "boot") {
    if (!is.null(seed)) {
      had <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
      if (had) old <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
      on.exit(if (had) assign(".Random.seed", old, envir = globalenv())
              else rm(".Random.seed", envir = globalenv()), add = TRUE)
      set.seed(seed)
    }
    U <- matrix(sample(n, n * R, replace = TRUE), n, R)
    B <- matrix(NA_real_, R, p)
    for (k in seq_len(R)) {
      idx <- U[, k]
      fk <- tryCatch(suppressWarnings(.rqn_fit1(X[idx, , drop = FALSE], y[idx], tau, "br")),
                     error = function(e) NULL)
      if (!is.null(fk)) B[k, ] <- fk$coefficients
    }
    ok <- stats::complete.cases(B)
    if (sum(ok) < R) warning(sprintf("%d of %d bootstrap designs were singular and dropped.",
                                     R - sum(ok), R), call. = FALSE)
    cov <- stats::cov(B[ok, , drop = FALSE])
  } else stop("unknown se type.", call. = FALSE)
  list(cov = cov, scale = scale, nonpos = nonpos)
}

#' Quantile regression (Frisch-Newton interior point, native)
#'
#' Fits the linear conditional quantile model
#' \eqn{Q_{y|x}(\tau) = x'\beta(\tau)} by minimising the check loss
#' \eqn{\sum_i \rho_\tau(y_i - x_i'\beta)} (Koenker and Bassett 1978).
#' \code{method = "fn"} is the Frisch-Newton primal-dual interior point
#' method of Portnoy and Koenker (1997), O(n p^2) per iteration and fast
#' for n in the hundreds of thousands; \code{method = "br"} finishes the
#' interior point solution with Barrodale-Roberts simplex pivots so that
#' it returns an exact vertex (basic) solution.  Standard errors are
#' computed as in Koenker (2005, Sec. 3.4): the Hendricks-Koenker
#' sandwich with the Hall-Sheather bandwidth (\code{"nid"}), the iid
#' sparsity estimate (\code{"iid"}), Powell's kernel sandwich
#' (\code{"ker"}) or the xy-pair bootstrap (\code{"boot"}).
#'
#' @param formula A model formula, e.g. \code{y ~ x1 + x2}.
#' @param data A data frame holding the variables of \code{formula}.
#' @param tau Quantile(s) in (0, 1); a vector fits each one.
#' @param method \code{"fn"} (Frisch-Newton interior point, default) or
#'   \code{"br"} (exact vertex solution).
#' @param se Standard error type: \code{"nid"} (default),
#'   \code{"iid"}, \code{"ker"} or \code{"boot"}.
#' @param weights Optional non-negative case weights, multiplying rows
#'   of the design and the response (as quantreg's \code{rq.wfit}).
#' @param hs Logical; Hall-Sheather bandwidth (TRUE, default) or
#'   Bofinger (FALSE) for "nid", "iid" and "ker".
#' @param R Number of bootstrap replications for \code{se = "boot"}.
#' @param seed Optional integer seed for the bootstrap (the caller's
#'   random number stream is restored afterwards).
#' @param ... Unused; accepted for call compatibility.
#' @return An object of class \code{"morie_rq"}: \code{coefficients}
#'   (vector, or p x K matrix for K quantiles), \code{coef_table} (list
#'   of Value / Std. Error / t value / Pr(>|t|) tables, one per tau),
#'   \code{vcov} (list of covariance matrices), \code{residuals},
#'   \code{fitted.values}, \code{rho} (check-loss objective), \code{tau},
#'   \code{method}, \code{se}, \code{nit}, \code{n}, \code{p},
#'   \code{rdf}, \code{terms}, \code{xlevels}, \code{call}.
#' @references Koenker, R. and Bassett, G. (1978). Regression quantiles.
#'   Econometrica 46, 33-50.
#'
#'   Portnoy, S. and Koenker, R. (1997). The Gaussian hare and the
#'   Laplacian tortoise: computability of squared-error versus
#'   absolute-error estimators. Statistical Science 12(4), 279-300.
#'
#'   Koenker, R. (2005). Quantile Regression. Cambridge University Press.
#'
#'   Hall, P. and Sheather, S. J. (1988). On the distribution of a
#'   studentized quantile. JRSS B 50, 381-391.
#' @examples
#' set.seed(1)
#' d <- data.frame(x = runif(200))
#' d$y <- 1 + 2 * d$x + rnorm(200) * (1 + d$x)
#' fit <- morie_rq(y ~ x, d, tau = c(0.25, 0.5))
#' fit
#' summary(fit)
#' @export
morie_rq <- function(formula, data, tau = 0.5, method = c("fn", "br"),
                     se = c("nid", "iid", "ker", "boot"), weights = NULL,
                     hs = TRUE, R = 200L, seed = NULL, ...) {
  method <- match.arg(method)
  se <- match.arg(se)
  cl <- match.call()
  tau <- sort(unique(as.numeric(tau)))
  if (!length(tau) || any(!is.finite(tau)) || any(tau <= 0 | tau >= 1))
    stop("tau must lie strictly between 0 and 1.", call. = FALSE)
  mf <- stats::model.frame(formula, data, na.action = stats::na.omit,
                           drop.unused.levels = TRUE)
  mt <- attr(mf, "terms")
  y <- as.numeric(stats::model.response(mf))
  X <- stats::model.matrix(mt, mf)
  vnames <- colnames(X)
  n <- nrow(X)
  p <- ncol(X)
  if (n <= p) stop("need more observations than coefficients.", call. = FALSE)
  if (!is.null(weights)) {
    weights <- as.numeric(weights)
    na <- attr(mf, "na.action")
    if (length(na)) weights <- weights[-na]
    if (length(weights) != n || any(!is.finite(weights)) || any(weights < 0))
      stop("weights must be finite, non-negative, one per observation.", call. = FALSE)
  }
  Xw <- if (is.null(weights)) X else X * weights
  yw <- if (is.null(weights)) y else y * weights
  K <- length(tau)
  coef <- matrix(NA_real_, p, K, dimnames = list(vnames, paste("tau=", format(round(tau, 3)))))
  resid <- matrix(NA_real_, n, K)
  tabs <- vector("list", K)
  covs <- vector("list", K)
  nit <- matrix(NA_integer_, 3, K)
  rho <- numeric(K)
  scale <- numeric(K)
  rdf <- n - p
  for (k in seq_len(K)) {
    f <- .rqn_fit1(Xw, yw, tau[k], method)
    b <- f$coefficients
    coef[, k] <- b
    resid[, k] <- drop(y - X %*% b)
    rho[k] <- .rqn_rho(resid[, k], tau[k])
    nit[, k] <- as.integer(f$nit)
    cv <- .rqn_cov(Xw, yw, b, drop(yw - Xw %*% b), tau[k], se, method, hs, R, seed)
    V <- cv$cov
    dimnames(V) <- list(vnames, vnames)
    covs[[k]] <- V
    scale[k] <- cv$scale
    sev <- sqrt(diag(V))
    tv <- b / sev
    tab <- cbind(b, sev, tv, if (rdf > 0) 2 * (1 - stats::pt(abs(tv), rdf)) else NA)
    dimnames(tab) <- list(vnames, c("Value", "Std. Error", "t value", "Pr(>|t|)"))
    tabs[[k]] <- tab
  }
  names(tabs) <- names(covs) <- colnames(coef)
  fitted <- y - resid
  if (K == 1L) {
    coef <- coef[, 1]
    resid <- resid[, 1]
    fitted <- fitted[, 1]
  }
  structure(list(coefficients = coef, coef_table = tabs, vcov = covs,
                 residuals = resid, fitted.values = fitted, rho = rho,
                 tau = tau, method = method, se = se, nit = nit, scale = scale,
                 n = n, p = p, rdf = rdf, weights = weights, terms = mt,
                 xlevels = stats::.getXlevels(mt, mf), call = cl),
            class = "morie_rq")
}

#' @rdname morie_rq
#' @param x,object A \code{"morie_rq"} fit.
#' @export
print.morie_rq <- function(x, ...) {
  cat("Quantile regression (morie native, method ", x$method, ")\n", sep = "")
  cat("Call: ", paste(deparse(x$call), collapse = " "), "\n", sep = "")
  cat("n =", x$n, " tau =", paste(format(x$tau), collapse = ", "), "\n\nCoefficients:\n")
  print(x$coefficients)
  invisible(x)
}

#' @rdname morie_rq
#' @export
summary.morie_rq <- function(object, ...) {
  structure(list(call = object$call, tau = object$tau, se = object$se,
                 method = object$method, coefficients = object$coef_table,
                 cov = object$vcov, rdf = object$rdf, n = object$n,
                 rho = object$rho, scale = object$scale),
            class = "summary.morie_rq")
}

#' @rdname morie_rq
#' @export
print.summary.morie_rq <- function(x, ...) {
  cat("Quantile regression (morie native), standard errors:", x$se, "\n")
  cat("Call: ", paste(deparse(x$call), collapse = " "), "\n", sep = "")
  for (k in seq_along(x$tau)) {
    cat("\ntau: ", format(x$tau[k]), "   (check loss ", format(x$rho[k]), ")\n", sep = "")
    stats::printCoefmat(x$coefficients[[k]], P.values = TRUE, has.Pvalue = TRUE)
  }
  cat("\nResidual df:", x$rdf, "\n")
  invisible(x)
}

#' @rdname morie_rq
#' @export
coef.morie_rq <- function(object, ...) object$coefficients

#' @rdname morie_rq
#' @export
vcov.morie_rq <- function(object, ...) {
  if (length(object$vcov) == 1L) object$vcov[[1L]] else object$vcov
}

#' @rdname morie_rq
#' @export
fitted.morie_rq <- function(object, ...) object$fitted.values

#' @rdname morie_rq
#' @export
residuals.morie_rq <- function(object, ...) object$residuals

#' @rdname morie_rq
#' @param newdata Optional data frame for \code{predict}.
#' @export
predict.morie_rq <- function(object, newdata = NULL, ...) {
  if (is.null(newdata)) return(object$fitted.values)
  tt <- stats::delete.response(object$terms)
  m <- stats::model.frame(tt, newdata, na.action = stats::na.pass, xlev = object$xlevels)
  Xn <- stats::model.matrix(tt, m)
  out <- Xn %*% object$coefficients
  if (ncol(out) == 1L) drop(out) else out
}

#' @rdname morie_rq
#' @export
logLik.morie_rq <- function(object, ...) {
  # Asymmetric-Laplace log likelihood at the scale MLE, as quantreg's logLik.rq.
  n <- object$n
  val <- n * (log(object$tau * (1 - object$tau)) - 1 - log(object$rho / n))
  structure(val, nobs = n, df = object$p, class = "logLik")
}
