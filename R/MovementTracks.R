#' Animal movement and spatial networks
#'
#' \code{TrackSteps}: step decomposition as \code{adehabitatLT::as.ltraj}.
#' \code{MovementNetwork}: grid-cell movement network. \code{CorePeriphery}:
#' MINRES continuous core-periphery. \code{NetworkRobustness}: largest
#' component under targeted or random node removal and the robustness R.
#' \code{MovementHmm}: gamma / von Mises hidden Markov model fitted by EM
#' with Viterbi states. \code{CrwKalman}: Kalman-filtered correlated random
#' walk with measurement error. Identical to the Python arm
#' \code{morie.fn.movetrack}.
#'
#' @param x,y Coordinates.
#' @param t Times (default 0, 1, ...).
#' @param resolution Cell size.
#' @param origin Grid origin.
#' @param A Adjacency matrix.
#' @param tol,maxit Convergence tolerance and iteration limit.
#' @param strategy "degree" (targeted) or "random".
#' @param seed Philox seed.
#' @param step,angle Step lengths and turning angles (NA when undefined).
#' @param n_states Number of states.
#' @param shape,scale,mu,kappa Starting emission parameters.
#' @param gamma,sigma,tau CRW persistence, process and measurement sd (NULL: estimate).
#' @param cycles Coordinate-search cycles.
#' @return List.
#' @references Calenge, C., Dray, S. and Royer-Carenzi, M. (2009). The
#'   concept of animals' trajectories from a data analysis perspective.
#'   Ecological Informatics 4, 34-41.
#'
#'   Boyd, J. P., Fitzgerald, W. J., Mahutga, M. C. and Smith, D. A. (2010).
#'   Computing continuous core/periphery structures for social relations data
#'   with MINRES/SVD. Social Networks 32, 125-137.
#'
#'   Schneider, C. M. et al. (2011). Mitigation of malicious attacks on
#'   networks. PNAS 108, 3838-3841.
#'
#'   Michelot, T., Langrock, R. and Patterson, T. A. (2016). moveHMM.
#'   Methods in Ecology and Evolution 7, 1308-1315.
#'
#'   Jonsen, I. D., Mills Flemming, J. and Myers, R. A. (2005). Robust
#'   state-space modeling of animal movement data. Ecology 86, 2874-2880.
#' @examples
#' TrackSteps(c(0, 1, 2, 2, 3), c(0, 0, 1, 2, 2))$rel_angle
#' NetworkRobustness(rbind(c(0, 1, 1, 1), c(1, 0, 0, 0), c(1, 0, 0, 0), c(1, 0, 0, 0)))$R
#' @export
TrackSteps <- function(x, y, t = NULL) {
  n <- length(x)
  if (is.null(t)) t <- seq_len(n) - 1
  dx <- c(diff(x), NA)
  dy <- c(diff(y), NA)
  dist <- sqrt(dx^2 + dy^2)
  dt <- c(diff(t), NA)
  r2 <- (x - x[1])^2 + (y - y[1])^2
  ab <- ifelse(!is.na(dist) & dist > 0, atan2(dy, dx), NA)
  rel <- rep(NA_real_, n)
  if (n > 2) {
    for (i in 2:(n - 1)) {
      if (is.na(ab[i]) || is.na(ab[i - 1])) next
      d <- ab[i] - ab[i - 1]
      d <- (d + pi) %% (2 * pi) - pi
      if (d == -pi) d <- pi
      rel[i] <- d
    }
  }
  list(dx = dx, dy = dy, dist = dist, dt = dt, R2n = r2, abs_angle = ab, rel_angle = rel)
}

#' @rdname TrackSteps
#' @export
MovementNetwork <- function(x, y, resolution, origin = c(0, 0)) {
  cx <- as.integer(floor((x - origin[1]) / resolution))
  cy <- as.integer(floor((y - origin[2]) / resolution))
  key <- paste(cx, cy)
  nodes_key <- unique(key)
  first <- match(nodes_key, key)
  nodes <- cbind(cx[first], cy[first])
  id <- match(key, nodes_key)
  n <- length(id)
  E <- matrix(0L, 0, 3)
  if (n > 1) {
    fr <- id[-n]
    to <- id[-1]
    mv <- fr != to
    if (any(mv)) {
      pr <- paste(fr[mv], to[mv])
      tab <- table(pr)
      ij <- do.call(rbind, lapply(strsplit(names(tab), " "), as.integer))
      E <- cbind(ij - 1L, as.integer(tab))
      E <- E[order(E[, 1], E[, 2]), , drop = FALSE]
    }
  }
  m <- nrow(nodes)
  list(nodes = nodes, edges = E,
       out_degree = vapply(seq_len(m) - 1L, function(k) sum(E[, 1] == k), 1L),
       in_degree = vapply(seq_len(m) - 1L, function(k) sum(E[, 2] == k), 1L),
       strength = vapply(seq_len(m) - 1L, function(k) sum(E[E[, 1] == k | E[, 2] == k, 3]), 0),
       visits = as.integer(tabulate(id, m)))
}

.mt_ss <- function(v) {
  s <- 0
  for (a in v) s <- s + a
  s
}

#' @rdname TrackSteps
#' @export
CorePeriphery <- function(A, tol = 1e-12, maxit = 10000L) {
  M <- as.matrix(A) + 0
  n <- nrow(M)
  M0 <- M
  diag(M0) <- 0
  cc <- vapply(seq_len(n), function(i) .mt_ss(M0[i, ]), 0)
  s <- sqrt(.mt_ss(cc^2))
  if (s == 0) s <- 1
  cc <- cc / s
  it <- 0L
  for (it in seq_len(maxit)) {
    sq <- .mt_ss(cc^2)
    new <- vapply(seq_len(n), function(i) .mt_ss(M0[i, -i] * cc[-i]) / (sq - cc[i]^2), 0)
    new <- 0.5 * (new + cc)
    diff <- max(abs(new - cc))
    cc <- new
    if (diff < tol) break
  }
  off <- row(M) != col(M)
  a <- t(M)[t(off)]
  b <- t(outer(cc, cc))[t(off)]
  ma <- .mt_ss(a) / length(a)
  mb <- .mt_ss(b) / length(b)
  fit <- .mt_ss((a - ma) * (b - mb)) / sqrt(.mt_ss((a - ma)^2) * .mt_ss((b - mb)^2))
  list(coreness = cc, fit = fit, iterations = it)
}

.mt_lcc <- function(adj, alive) {
  seen <- rep(FALSE, length(adj))
  best <- 0
  for (s in seq_along(adj)) {
    if (!alive[s] || seen[s]) next
    stack <- s
    seen[s] <- TRUE
    size <- 0
    while (length(stack)) {
      v <- stack[length(stack)]
      stack <- stack[-length(stack)]
      size <- size + 1
      for (w in adj[[v]]) {
        if (alive[w] && !seen[w]) {
          seen[w] <- TRUE
          stack <- c(stack, w)
        }
      }
    }
    best <- max(best, size)
  }
  best
}

#' @rdname TrackSteps
#' @export
NetworkRobustness <- function(A, strategy = "degree", seed = 1) {
  M <- as.matrix(A)
  n <- nrow(M)
  adj <- lapply(seq_len(n), function(i) setdiff(which(M[i, ] != 0), i))
  alive <- rep(TRUE, n)
  if (strategy == "random") {
    u <- .morie_random_uniform(n, seed = seed)
    ord <- order(u, seq_len(n))
  } else if (strategy != "degree") {
    stop("strategy must be 'degree' or 'random'")
  }
  curve <- numeric(n)
  for (q in seq_len(n)) {
    if (strategy == "degree") {
      cand <- which(alive)
      dg <- vapply(cand, function(i) sum(alive[adj[[i]]]), 0)
      k <- cand[order(-dg, cand)[1]]
    } else {
      k <- ord[q]
    }
    alive[k] <- FALSE
    curve[q] <- .mt_lcc(adj, alive) / n
  }
  list(R = .mt_ss(curve) / n, curve = curve)
}

.mt_trigamma <- function(x) {
  r <- 0
  while (x < 6) {
    r <- r + 1 / (x * x)
    x <- x + 1
  }
  f <- 1 / (x * x)
  r + 1 / x + f / 2 + f / x * (1 / 6 - f * (1 / 30 - f * (1 / 42 - f / 30)))
}

.mt_bratio <- function(k) {
  h <- k / 2
  t0 <- 1
  t1 <- 1
  s0 <- 1
  s1 <- 1
  for (m in 1:1999) {
    t0 <- t0 * h * h / (m * m)
    t1 <- t1 * h * h / (m * (m + 1))
    s0 <- s0 + t0
    s1 <- s1 + t1
    if (t0 < 1e-17 * s0 && t1 < 1e-17 * s1) break
  }
  h * s1 / s0
}

.mt_kappa <- function(Rbar) {
  if (Rbar <= 1e-12) return(0)
  k <- if (Rbar < 0.999) Rbar * (2 - Rbar^2) / (1 - Rbar^2) else 500
  k <- min(k, 500)
  for (it in 1:100) {
    a <- .mt_bratio(k)
    da <- if (k > 0) 1 - a / k - a * a else 0.5
    st <- (a - Rbar) / da
    k <- min(max(k - st, 1e-8), 500)
    if (abs(st) < 1e-13 * max(1, k)) break
  }
  k
}

.mt_logi0 <- function(k) {
  h <- k / 2
  t <- 1
  s <- 1
  for (m in 1:1999) {
    t <- t * h * h / (m * m)
    s <- s + t
    if (t < 1e-17 * s) break
  }
  log(s)
}

#' @rdname TrackSteps
#' @export
MovementHmm <- function(step, angle, n_states = 2L, shape = NULL, scale = NULL, mu = NULL, kappa = NULL,
                        maxit = 200L, tol = 1e-10) {
  S <- as.numeric(step)
  An <- as.numeric(angle)
  T <- length(S)
  K <- n_states
  sh <- if (is.null(shape)) rep(2, K) else as.numeric(shape)
  sc <- if (is.null(scale)) (seq_len(K)) * .mt_ss(S) / T / K else as.numeric(scale)
  mu_ <- if (is.null(mu)) ifelse(seq_len(K) == 1, pi, 0) else as.numeric(mu)
  ka <- if (is.null(kappa)) rep(1, K) else as.numeric(kappa)
  G <- matrix(0.1 / (K - 1), K, K)
  diag(G) <- 0.9
  delta <- rep(1 / K, K)
  emis <- function() {
    out <- matrix(0, T, K)
    for (t in seq_len(T)) {
      for (k in seq_len(K)) {
        lp <- (sh[k] - 1) * log(S[t]) - S[t] / sc[k] - lgamma(sh[k]) - sh[k] * log(sc[k])
        if (!is.na(An[t])) lp <- lp + ka[k] * cos(An[t] - mu_[k]) - log(2 * pi) - .mt_logi0(ka[k])
        out[t, k] <- exp(lp)
      }
    }
    out
  }
  lls <- numeric(0)
  for (iter in seq_len(maxit)) {
    P <- emis()
    al <- matrix(0, T, K)
    cs <- numeric(T)
    for (t in seq_len(T)) {
      a <- if (t == 1) delta * P[1, ] else vapply(seq_len(K), function(k) .mt_ss(al[t - 1, ] * G[, k]) * P[t, k], 0)
      cs[t] <- .mt_ss(a)
      al[t, ] <- a / cs[t]
    }
    be <- matrix(1, T, K)
    if (T > 1) {
      for (t in (T - 1):1) {
        be[t, ] <- vapply(seq_len(K), function(k) .mt_ss(G[k, ] * P[t + 1, ] * be[t + 1, ]), 0) / cs[t + 1]
      }
    }
    gam <- al * be
    lls <- c(lls, .mt_ss(log(cs)))
    xi <- matrix(0, K, K)
    for (t in seq_len(T - 1)) {
      for (i in seq_len(K)) for (j in seq_len(K)) xi[i, j] <- xi[i, j] + al[t, i] * G[i, j] * P[t + 1, j] * be[t + 1, j] / cs[t + 1]
    }
    G <- xi / vapply(seq_len(K), function(i) .mt_ss(xi[i, ]), 0)
    delta <- gam[1, ]
    for (k in seq_len(K)) {
      w <- gam[, k]
      sw <- .mt_ss(w)
      m <- .mt_ss(w * S) / sw
      s <- log(m) - .mt_ss(w * log(S)) / sw
      kk <- (3 - s + sqrt((s - 3)^2 + 24 * s)) / (12 * s)
      for (it in 1:100) {
        f <- log(kk) - .s03digamma(kk) - s
        st <- f / (1 / kk - .mt_trigamma(kk))
        kk <- max(kk - st, 1e-8)
        if (abs(st) < 1e-13 * kk) break
      }
      sh[k] <- kk
      sc[k] <- m / kk
      ok <- !is.na(An)
      C <- .mt_ss(w[ok] * cos(An[ok]))
      Sn <- .mt_ss(w[ok] * sin(An[ok]))
      mu_[k] <- atan2(Sn, C)
      ka[k] <- .mt_kappa(sqrt(C^2 + Sn^2) / .mt_ss(w[ok]))
    }
    nl <- length(lls)
    if (nl > 1 && abs(lls[nl] - lls[nl - 1]) < tol * (abs(lls[nl]) + 1)) break
  }
  P <- emis()
  V <- matrix(0, T, K)
  V[1, ] <- log(delta + 1e-300) + log(P[1, ] + 1e-300)
  back <- matrix(0L, T, K)
  if (T > 1) {
    for (t in 2:T) {
      for (k in seq_len(K)) {
        sc_ <- V[t - 1, ] + log(G[, k] + 1e-300)
        j <- which.max(sc_)
        V[t, k] <- sc_[j] + log(P[t, k] + 1e-300)
        back[t, k] <- j
      }
    }
  }
  st <- integer(T)
  st[T] <- which.max(V[T, ])
  if (T > 1) for (t in (T - 1):1) st[t] <- back[t + 1, st[t + 1]]
  list(shape = sh, scale = sc, mu = mu_, kappa = ka, transition = G, loglik = lls, states = st - 1L)
}

.mt_kf <- function(y, g, s2, t2) {
  n <- length(y)
  x <- c(y[1], 0)
  P <- matrix(c(t2 + 1e6, 0, 0, s2 / max(1 - g * g, 1e-12)), 2)
  ll <- 0
  xs <- matrix(0, n, 2)
  xp_l <- matrix(0, n, 2)
  Ps <- vector("list", n)
  Pp_l <- vector("list", n)
  for (t in seq_len(n)) {
    if (t > 1) {
      xp <- c(x[1] + g * x[2], g * x[2])
      p00 <- P[1, 1] + 2 * g * P[1, 2] + g * g * P[2, 2] + s2
      p01 <- g * P[1, 2] + g * g * P[2, 2] + s2
      Pp <- matrix(c(p00, p01, p01, g * g * P[2, 2] + s2), 2)
    } else {
      xp <- x
      Pp <- P
    }
    v <- y[t] - xp[1]
    F <- Pp[1, 1] + t2
    if (t > 1) ll <- ll - 0.5 * (log(2 * pi * F) + v * v / F)
    K0 <- Pp[1, 1] / F
    K1 <- Pp[2, 1] / F
    x <- c(xp[1] + K0 * v, xp[2] + K1 * v)
    P <- matrix(c(Pp[1, 1] - K0 * Pp[1, 1], Pp[2, 1] - K1 * Pp[1, 1], Pp[1, 2] - K0 * Pp[1, 2], Pp[2, 2] - K1 * Pp[1, 2]), 2)
    xs[t, ] <- x
    Ps[[t]] <- P
    xp_l[t, ] <- xp
    Pp_l[[t]] <- Pp
  }
  sm <- xs
  if (n > 1) {
    for (t in (n - 1):1) {
      Pp <- Pp_l[[t + 1]]
      det <- Pp[1, 1] * Pp[2, 2] - Pp[1, 2] * Pp[2, 1]
      inv <- matrix(c(Pp[2, 2], -Pp[2, 1], -Pp[1, 2], Pp[1, 1]), 2) / det
      Fm <- matrix(c(1, 0, g, g), 2)
      J <- Ps[[t]] %*% t(Fm) %*% inv
      sm[t, ] <- xs[t, ] + as.vector(J %*% (sm[t + 1, ] - xp_l[t + 1, ]))
    }
  }
  list(ll = ll, smooth = sm[, 1])
}

#' @rdname TrackSteps
#' @export
CrwKalman <- function(x, y, gamma = NULL, sigma = NULL, tau = NULL, cycles = 20L) {
  ll <- function(g, s, t) .mt_kf(x, g, s * s, t * t)$ll + .mt_kf(y, g, s * s, t * t)$ll
  steps <- sqrt(diff(x)^2 + diff(y)^2)
  sd <- sqrt(.mt_ss(steps^2) / max(length(steps), 1))
  if (sd == 0) sd <- 1
  g <- if (is.null(gamma)) 0.5 else gamma
  ls <- if (is.null(sigma)) log(sd / 2) else log(sigma)
  lt <- if (is.null(tau)) log(sd / 10) else log(tau)
  gold <- function(f, lo, hi) {
    gr <- (sqrt(5) - 1) / 2
    a1 <- hi - gr * (hi - lo)
    a2 <- lo + gr * (hi - lo)
    f1 <- f(a1)
    f2 <- f(a2)
    for (it in 1:200) {
      if (f1 >= f2) {
        hi <- a2
        a2 <- a1
        f2 <- f1
        a1 <- hi - gr * (hi - lo)
        f1 <- f(a1)
      } else {
        lo <- a1
        a1 <- a2
        f1 <- f2
        a2 <- lo + gr * (hi - lo)
        f2 <- f(a2)
      }
      if (hi - lo < 1e-10) break
    }
    0.5 * (lo + hi)
  }
  if (is.null(gamma) || is.null(sigma) || is.null(tau)) {
    for (cyc in seq_len(cycles)) {
      if (is.null(gamma)) g <- gold(function(v) ll(v, exp(ls), exp(lt)), 1e-6, 0.999)
      if (is.null(sigma)) ls <- gold(function(v) ll(g, exp(v), exp(lt)), ls - 5, ls + 5)
      if (is.null(tau)) lt <- gold(function(v) ll(g, exp(ls), exp(v)), lt - 8, lt + 5)
    }
  }
  s <- exp(ls)
  t <- exp(lt)
  kx <- .mt_kf(x, g, s * s, t * t)
  ky <- .mt_kf(y, g, s * s, t * t)
  list(gamma = g, sigma = s, tau = t, loglik = kx$ll + ky$ll, x_smooth = kx$smooth, y_smooth = ky$smooth)
}
