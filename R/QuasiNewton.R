.qn_num_grad <- function(f, x) {
  vapply(seq_along(x), function(i) {
    h <- 1e-6 * max(1, abs(x[i]))
    xp <- x
    xm <- x
    xp[i] <- xp[i] + h
    xm[i] <- xm[i] - h
    (f(xp) - f(xm)) / (2 * h)
  }, 0)
}

.qn_cubic_min <- function(a, fa, da, b, fb, db) {
  d1 <- da + db - 3 * (fa - fb) / (a - b)
  rad <- d1^2 - da * db
  if (rad < 0) return(0.5 * (a + b))
  d2 <- sign(b - a) * sqrt(rad)
  if (b == a) d2 <- sqrt(rad)
  den <- db - da + 2 * d2
  if (den == 0) return(0.5 * (a + b))
  t <- b - (b - a) * (db + d2 - d1) / den
  lo <- min(a, b)
  hi <- max(a, b)
  if (!(t >= lo + 0.1 * (hi - lo) && t <= hi - 0.1 * (hi - lo))) return(0.5 * (a + b))
  t
}

.qn_wolfe <- function(phi, a1 = 1, a_max = NULL, c1 = 1e-4, c2 = 0.9, max_eval = 40) {
  p0 <- phi(0)
  f0 <- p0[1]
  d0 <- p0[2]
  if (d0 >= 0) return(c(0, f0, d0, 1))
  ap <- 0
  fprev <- f0
  dprev <- d0
  a <- if (is.null(a_max)) a1 else min(a1, a_max)
  n <- 1
  zoom <- function(lo, flo, dlo, hi, fhi, dhi, n) {
    while (n < max_eval) {
      a <- .qn_cubic_min(lo, flo, dlo, hi, fhi, dhi)
      pa <- phi(a)
      n <- n + 1
      if (pa[1] > f0 + c1 * a * d0 || pa[1] >= flo) {
        hi <- a
        fhi <- pa[1]
        dhi <- pa[2]
      } else {
        if (abs(pa[2]) <= -c2 * d0) return(c(a, pa[1], pa[2], n))
        if (pa[2] * (hi - lo) >= 0) {
          hi <- lo
          fhi <- flo
          dhi <- dlo
        }
        lo <- a
        flo <- pa[1]
        dlo <- pa[2]
      }
      if (abs(hi - lo) < 1e-16 * max(1, abs(lo))) break
    }
    c(lo, flo, dlo, n)
  }
  while (n < max_eval) {
    pa <- phi(a)
    n <- n + 1
    if (pa[1] > f0 + c1 * a * d0 || (n > 2 && pa[1] >= fprev)) return(zoom(ap, fprev, dprev, a, pa[1], pa[2], n))
    if (abs(pa[2]) <= -c2 * d0) return(c(a, pa[1], pa[2], n))
    if (pa[2] >= 0) return(zoom(a, pa[1], pa[2], ap, fprev, dprev, n))
    if (!is.null(a_max) && a >= a_max) return(c(a, pa[1], pa[2], n))
    ap <- a
    fprev <- pa[1]
    dprev <- pa[2]
    a <- if (is.null(a_max)) 2 * a else min(2 * a, a_max)
  }
  c(ap, fprev, dprev, n)
}

#' BFGS minimisation
#'
#' BFGS in inverse-Hessian form with a strong-Wolfe line search (c1 = 1e-4,
#' c2 = 0.9), H0 = I rescaled by y's / y'y before the first update; stops when
#' max |g_i| <= gtol.
#'
#' @param f Objective.
#' @param x0 Starting point.
#' @param grad Gradient function; central differences when NULL.
#' @param gtol Gradient tolerance.
#' @param max_iter Iteration cap.
#' @return list(x, fun, grad, n_iter, n_fev, converged).
#' @references Nocedal, J. and Wright, S. J. (2006). Numerical Optimization,
#'   2nd ed., Algorithm 6.1.
#' @examples
#' BfgsMinimize(function(x) (1 - x[1])^2 + 100 * (x[2] - x[1]^2)^2, c(-1.2, 1))$x
#' @export
BfgsMinimize <- function(f, x0, grad = NULL, gtol = 1e-8, max_iter = 1000) {
  gr <- if (is.null(grad)) function(x) .qn_num_grad(f, x) else grad
  x <- as.numeric(x0)
  n <- length(x)
  fx <- as.numeric(f(x))
  g <- as.numeric(gr(x))
  H <- diag(n)
  nfev <- 1
  it <- 0
  conv <- max(abs(g)) <= gtol
  while (!conv && it < max_iter) {
    p <- -as.vector(H %*% g)
    if (sum(p * g) >= 0) {
      H <- diag(n)
      p <- -g
    }
    cache <- new.env()
    phi <- function(a) {
      xa <- x + a * p
      fa <- as.numeric(f(xa))
      ga <- as.numeric(gr(xa))
      cache$x <- xa
      cache$f <- fa
      cache$g <- ga
      cache$a <- a
      c(fa, sum(ga * p))
    }
    ls <- .qn_wolfe(phi)
    nfev <- nfev + ls[4]
    if (ls[1] == 0) break
    if (cache$a != ls[1]) invisible(phi(ls[1]))
    s <- cache$x - x
    y <- cache$g - g
    sy <- sum(s * y)
    it <- it + 1
    if (sy > 1e-12 * max(1, sum(y * y))) {
      if (it == 1) H <- H * (sy / sum(y * y))
      rho <- 1 / sy
      Hy <- as.vector(H %*% y)
      H <- H - rho * (outer(Hy, s) + outer(s, Hy)) + (rho^2 * sum(y * Hy) + rho) * outer(s, s)
    }
    stalled <- abs(fx - cache$f) <= 1e-15 * max(1, abs(fx)) && max(abs(s)) <= 1e-15 * max(1, max(abs(cache$x)))
    x <- cache$x
    fx <- cache$f
    g <- cache$g
    conv <- max(abs(g)) <= gtol
    if (stalled) break
  }
  list(x = x, fun = fx, grad = g, n_iter = it, n_fev = nfev, converged = conv)
}

#' Nelder-Mead simplex minimisation
#'
#' Reflection 1, expansion 2, contraction 1/2 and shrink 1/2 with the ordering
#' and acceptance rules of Lagarias et al. (1998); the initial simplex is x0 plus
#' step_i e_i, step_i = 0.1 max(1, |x0_i|). Stops when the simplex diameter
#' about the best vertex is at most xtol and the value spread at most ftol.
#'
#' @param f Objective.
#' @param x0 Starting point.
#' @param step Initial edge lengths (scalar or vector); NULL for the default.
#' @param xtol,ftol Tolerances.
#' @param max_iter Iteration cap (default 200 n).
#' @return list(x, fun, n_iter, n_fev, converged, simplex).
#' @references Nelder, J. A. and Mead, R. (1965). A simplex method for function
#'   minimization. Computer Journal 7, 308-313.
#'   Lagarias, J. C., Reeds, J. A., Wright, M. H. and Wright, P. E. (1998).
#'   SIAM Journal on Optimization 9, 112-147.
#' @examples
#' NelderMead(function(x) (x[1] - 1)^2 + (x[2] + 2)^2, c(0, 0))$x
#' @export
NelderMead <- function(f, x0, step = NULL, xtol = 1e-10, ftol = 1e-10, max_iter = NULL) {
  x0 <- as.numeric(x0)
  n <- length(x0)
  if (n == 0) stop("x0 must be non-empty", call. = FALSE)
  st <- if (is.null(step)) 0.1 * pmax(1, abs(x0)) else rep_len(as.numeric(step), n)
  max_iter <- if (is.null(max_iter)) 200 * n else as.integer(max_iter)
  simp <- rbind(x0, t(vapply(seq_len(n), function(i) x0 + st[i] * (seq_len(n) == i), numeric(n))))
  dimnames(simp) <- NULL
  fv <- apply(simp, 1, function(v) as.numeric(f(v)))
  nfev <- n + 1
  it <- 0
  conv <- FALSE
  while (it < max_iter) {
    o <- order(fv, seq_len(n + 1))
    simp <- simp[o, , drop = FALSE]
    fv <- fv[o]
    if (max(abs(sweep(simp[-1, , drop = FALSE], 2, simp[1, ]))) <= xtol && max(abs(fv - fv[1])) <= ftol) {
      conv <- TRUE
      break
    }
    it <- it + 1
    cen <- colSums(simp[1:n, , drop = FALSE]) / n
    w <- simp[n + 1, ]
    xr <- 2 * cen - w
    fr <- as.numeric(f(xr))
    nfev <- nfev + 1
    if (fr < fv[1]) {
      xe <- 3 * cen - 2 * w
      fe <- as.numeric(f(xe))
      nfev <- nfev + 1
      if (fe < fr) {
        simp[n + 1, ] <- xe
        fv[n + 1] <- fe
      } else {
        simp[n + 1, ] <- xr
        fv[n + 1] <- fr
      }
      next
    }
    if (fr < fv[n]) {
      simp[n + 1, ] <- xr
      fv[n + 1] <- fr
      next
    }
    if (fr < fv[n + 1]) {
      xc <- cen + 0.5 * (xr - cen)
      fc <- as.numeric(f(xc))
      nfev <- nfev + 1
      if (fc <= fr) {
        simp[n + 1, ] <- xc
        fv[n + 1] <- fc
        next
      }
    } else {
      xc <- cen + 0.5 * (w - cen)
      fc <- as.numeric(f(xc))
      nfev <- nfev + 1
      if (fc < fv[n + 1]) {
        simp[n + 1, ] <- xc
        fv[n + 1] <- fc
        next
      }
    }
    for (i in 2:(n + 1)) {
      simp[i, ] <- simp[1, ] + 0.5 * (simp[i, ] - simp[1, ])
      fv[i] <- as.numeric(f(simp[i, ]))
    }
    nfev <- nfev + n
  }
  o <- order(fv, seq_len(n + 1))
  simp <- simp[o, , drop = FALSE]
  list(x = simp[1, ], fun = fv[o][1], n_iter = it, n_fev = nfev, converged = conv, simplex = simp)
}

.lb_compact <- function(S, Y, theta) {
  k <- length(S)
  if (k == 0) return(NULL)
  Sm <- do.call(cbind, S)
  Ym <- do.call(cbind, Y)
  SY <- crossprod(Sm, Ym)
  L <- SY
  L[upper.tri(L, diag = TRUE)] <- 0
  K <- rbind(cbind(-diag(diag(SY), k), t(L)), cbind(L, theta * crossprod(Sm)))
  list(W = cbind(Ym, theta * Sm), M = solve(K))
}

.lb_cauchy <- function(x, g, lo, hi, theta, cm) {
  n <- length(x)
  t <- rep(Inf, n)
  t[g < 0] <- ((x - hi) / g)[g < 0]
  t[g > 0] <- ((x - lo) / g)[g > 0]
  d <- ifelse(t == 0, 0, -g)
  k2 <- if (is.null(cm)) 0 else nrow(cm$M)
  p <- if (k2) as.vector(crossprod(cm$W, d)) else numeric(0)
  cc <- numeric(k2)
  fp <- -sum(d * d)
  fpp <- -theta * fp - (if (k2) sum(p * (cm$M %*% p)) else 0)
  xc <- x
  if (fp >= 0) return(list(xc = xc, c = cc))
  dtm <- if (fpp > 0) -fp / fpp else Inf
  idx <- which(t > 0 & is.finite(t))
  idx <- idx[order(t[idx], idx)]
  told <- 0
  q <- 0
  while (q < length(idx)) {
    b <- idx[q + 1]
    dt <- t[b] - told
    if (dtm < dt) break
    xc[b] <- if (d[b] > 0) hi[b] else lo[b]
    zb <- xc[b] - x[b]
    cc <- cc + dt * p
    gb <- g[b]
    if (k2) {
      wb <- cm$W[b, ]
      Mwb <- as.vector(cm$M %*% wb)
      fp <- fp + dt * fpp + gb^2 + theta * gb * zb - gb * sum(Mwb * cc)
      fpp <- fpp - theta * gb^2 - 2 * gb * sum(Mwb * p) - gb^2 * sum(wb * Mwb)
      p <- p + gb * wb
    } else {
      fp <- fp + dt * fpp + gb^2 + theta * gb * zb
      fpp <- fpp - theta * gb^2
    }
    d[b] <- 0
    told <- t[b]
    q <- q + 1
    if (fpp <= 0) {
      dtm <- if (fp >= 0) 0 else Inf
      if (fp >= 0) break
    } else {
      dtm <- -fp / fpp
    }
  }
  if (q < length(idx) || any(is.infinite(t) & d != 0)) {
    dtm <- max(dtm, 0)
    if (is.infinite(dtm)) dtm <- 0
    told <- told + dtm
    xc[d != 0] <- x[d != 0] + told * d[d != 0]
    cc <- cc + dtm * p
  }
  list(xc = xc, c = cc)
}

.lb_subspace <- function(x, g, xc, cc, lo, hi, theta, cm) {
  Z <- which(lo < xc & xc < hi)
  if (!length(Z)) return(xc)
  if (is.null(cm)) {
    r <- g[Z] + theta * (xc[Z] - x[Z])
    du <- -r / theta
  } else {
    r <- g[Z] + theta * (xc[Z] - x[Z]) - as.vector(cm$W[Z, , drop = FALSE] %*% (cm$M %*% cc))
    WZ <- cm$W[Z, , drop = FALSE]
    v <- as.vector(cm$M %*% crossprod(WZ, r))
    N <- diag(ncol(WZ)) - cm$M %*% crossprod(WZ) / theta
    v <- solve(N, v)
    du <- -r / theta - as.vector(WZ %*% v) / theta^2
  }
  alpha <- 1
  up <- du > 0
  dn <- du < 0
  if (any(up)) alpha <- min(alpha, ((hi[Z] - xc[Z]) / du)[up])
  if (any(dn)) alpha <- min(alpha, ((lo[Z] - xc[Z]) / du)[dn])
  xb <- xc
  xb[Z] <- xc[Z] + alpha * du
  xb
}

#' L-BFGS-B bound-constrained minimisation
#'
#' Byrd, Lu, Nocedal and Zhu (1995): generalized Cauchy point of the compact
#' limited-memory model B = theta I - W M W' (Algorithm CP), direct primal
#' subspace minimisation over the free variables backtracked into the box, and a
#' strong-Wolfe line search with maximal step 1. Pairs are kept when
#' s'y > eps y'y (at most m). Stops when the projected gradient is at most
#' pgtol or the relative reduction of f is at most factr times machine epsilon,
#' the conventions of \code{optim(method = "L-BFGS-B")}.
#'
#' @param f Objective.
#' @param x0 Starting point (projected onto the box).
#' @param grad Gradient function; central differences when NULL.
#' @param lower,upper Bounds (default unbounded).
#' @param m Number of correction pairs.
#' @param pgtol,factr Tolerances.
#' @param max_iter Iteration cap.
#' @return list(x, fun, grad, projected_gradient, n_iter, n_fev, converged,
#'   message).
#' @references Byrd, R. H., Lu, P., Nocedal, J. and Zhu, C. (1995). A limited
#'   memory algorithm for bound constrained optimization. SIAM Journal on
#'   Scientific Computing 16, 1190-1208.
#' @examples
#' rb <- function(x) (1 - x[1])^2 + 100 * (x[2] - x[1]^2)^2
#' LbfgsbMinimize(rb, c(-1.2, 1), lower = c(-2, -2), upper = c(0.5, 2))$x
#' @export
LbfgsbMinimize <- function(f, x0, grad = NULL, lower = NULL, upper = NULL, m = 10,
                           pgtol = 1e-8, factr = 1e7, max_iter = 1000) {
  eps <- .Machine$double.eps
  x <- as.numeric(x0)
  n <- length(x)
  lo <- if (is.null(lower)) rep(-Inf, n) else rep_len(as.numeric(lower), n)
  hi <- if (is.null(upper)) rep(Inf, n) else rep_len(as.numeric(upper), n)
  if (any(lo > hi)) stop("lower must not exceed upper", call. = FALSE)
  x <- pmin(pmax(x, lo), hi)
  gr <- if (is.null(grad)) function(z) .qn_num_grad(f, z) else grad
  fx <- as.numeric(f(x))
  g <- as.numeric(gr(x))
  nfev <- 1
  pgn <- function(x, g) max(abs(pmin(pmax(x - g, lo), hi) - x))
  S <- list()
  Y <- list()
  theta <- 1
  it <- 0
  msg <- "max_iter reached"
  conv <- pgn(x, g) <= pgtol
  if (conv) msg <- "projected gradient below pgtol"
  while (!conv && it < max_iter) {
    cm <- .lb_compact(S, Y, theta)
    cp <- .lb_cauchy(x, g, lo, hi, theta, cm)
    xb <- .lb_subspace(x, g, cp$xc, cp$c, lo, hi, theta, cm)
    d <- xb - x
    if (sum(d * g) >= 0) {
      if (length(S)) {
        S <- list()
        Y <- list()
        theta <- 1
        next
      }
      msg <- "no descent direction"
      break
    }
    cache <- new.env()
    phi <- function(a) {
      xa <- pmin(pmax(x + a * d, lo), hi)
      fa <- as.numeric(f(xa))
      ga <- as.numeric(gr(xa))
      cache$x <- xa
      cache$f <- fa
      cache$g <- ga
      cache$a <- a
      c(fa, sum(ga * d))
    }
    a1 <- if (it == 0 && !length(S)) min(1, 1 / sqrt(sum(d * d))) else 1
    ls <- .qn_wolfe(phi, a1 = a1, a_max = 1)
    nfev <- nfev + ls[4]
    if (ls[1] == 0) {
      if (length(S)) {
        S <- list()
        Y <- list()
        theta <- 1
        next
      }
      msg <- "line search failed"
      break
    }
    if (cache$a != ls[1]) invisible(phi(ls[1]))
    it <- it + 1
    s <- cache$x - x
    y <- cache$g - g
    sy <- sum(s * y)
    yy <- sum(y * y)
    if (sy > eps * yy) {
      S[[length(S) + 1]] <- s
      Y[[length(Y) + 1]] <- y
      if (length(S) > m) {
        S <- S[-1]
        Y <- Y[-1]
      }
      theta <- yy / sy
    }
    rel <- (fx - cache$f) / max(abs(fx), abs(cache$f), 1)
    x <- cache$x
    fx <- cache$f
    g <- cache$g
    if (pgn(x, g) <= pgtol) {
      conv <- TRUE
      msg <- "projected gradient below pgtol"
    } else if (rel <= factr * eps) {
      conv <- TRUE
      msg <- "relative reduction of f below factr * epsmch"
    }
  }
  list(x = x, fun = fx, grad = g, projected_gradient = pgn(x, g), n_iter = it, n_fev = nfev,
       converged = conv, message = msg)
}
