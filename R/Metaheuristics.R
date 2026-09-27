.mh_rand <- function(seed) {
  e <- new.env()
  e$seed <- seed
  e$ub <- numeric(0)
  e$ui <- 0L
  e$us <- -1
  e$nb <- numeric(0)
  e$ni <- 0L
  e$ns <- 0
  e
}

.mh_u <- function(e) {
  if (e$ui >= length(e$ub)) {
    e$us <- e$us + 2
    e$ub <- .morie_random_uniform(1024, seed = e$seed, stream = e$us)
    e$ui <- 0L
  }
  e$ui <- e$ui + 1L
  e$ub[e$ui]
}

.mh_n <- function(e) {
  if (e$ni >= length(e$nb)) {
    e$ns <- e$ns + 2
    e$nb <- .morie_random_normal(1024, seed = e$seed, stream = e$ns)
    e$ni <- 0L
  }
  e$ni <- e$ni + 1L
  e$nb[e$ni]
}

.mh_idx <- function(e, k) min(floor(.mh_u(e) * k), k - 1) + 1

.mh_other <- function(e, k, i) {
  j <- min(floor(.mh_u(e) * (k - 1)), k - 2)
  if (j >= i - 1) j + 2 else j + 1
}

.mh_sum <- function(x) {
  s <- 0
  for (v in x) s <- s + v
  s
}

.mh_colmean <- function(X) {
  vapply(seq_len(ncol(X)), function(j) .mh_sum(X[, j]) / nrow(X), 0)
}

.mh_setup <- function(f, bounds, n_pop, e) {
  B <- if (is.matrix(bounds)) bounds else do.call(rbind, lapply(bounds, as.numeric))
  lo <- as.numeric(B[, 1])
  hi <- as.numeric(B[, 2])
  if (!length(lo) || any(lo >= hi)) stop("bounds must be (low, high) pairs with low < high", call. = FALSE)
  if (as.integer(n_pop) < 2) stop("n_pop must be at least 2", call. = FALSE)
  n <- as.integer(n_pop)
  d <- length(lo)
  X <- matrix(0, n, d)
  for (i in seq_len(n)) for (j in seq_len(d)) X[i, j] <- lo[j] + .mh_u(e) * (hi[j] - lo[j])
  Fv <- vapply(seq_len(n), function(i) as.numeric(f(X[i, ])), 0)
  list(lo = lo, hi = hi, X = X, F = Fv, n = n, d = d)
}

.mh_clip <- function(x, lo, hi) pmin(pmax(x, lo), hi)

.mh_levy <- function(e, d) {
  out <- numeric(d)
  for (j in seq_len(d)) {
    u <- .mh_n(e) * 0.6965745025576967
    v <- .mh_n(e)
    out[j] <- u / abs(v)^(1 / 1.5)
  }
  out
}

.mh_out <- function(method, x, fx, hist, nfev) {
  list(x = as.numeric(x), fun = fx, estimate = fx, history = hist, n_fev = nfev, method = method)
}

#' Artificial bee colony
#'
#' Employed bees perturb one coordinate, v_ij = x_ij + phi (x_ij - x_kj),
#' phi ~ U(-1, 1), keeping v when it is no worse; onlookers repeat this for
#' sources drawn with probability proportional to fit = 1 / (1 + f) (1 + |f| for
#' f < 0); the most-tried source beyond \code{limit} failures is re-drawn (scout).
#' Draws come from the morie Philox streams in the same order as the Python arm.
#'
#' @param f Objective to minimise.
#' @param bounds List of c(low, high) pairs or a two-column matrix.
#' @param limit Abandonment limit (default n_pop times the dimension).
#' @param n_pop Population size.
#' @param max_iter Iterations.
#' @param seed Philox seed.
#' @return list(x, fun, estimate, history, n_fev, method).
#' @references Karaboga, D. and Basturk, B. (2007). A powerful and efficient
#'   algorithm for numerical function optimization: artificial bee colony (ABC)
#'   algorithm. Journal of Global Optimization 39, 459-471.
#' @examples
#' ArtificialBeeColony(function(x) sum(x^2), list(c(-5, 5), c(-5, 5)), max_iter = 50)$fun
#' @export
ArtificialBeeColony <- function(f, bounds, limit = NULL, n_pop = 20, max_iter = 200, seed = 0) {
  e <- .mh_rand(seed)
  s <- .mh_setup(f, bounds, n_pop, e)
  lo <- s$lo
  hi <- s$hi
  X <- s$X
  Fv <- s$F
  n <- s$n
  d <- s$d
  nfev <- n
  lim <- if (is.null(limit)) n * d else as.integer(limit)
  trial <- integer(n)
  fit <- function(v) if (v >= 0) 1 / (1 + v) else 1 + abs(v)
  probe <- function(i) {
    k <- .mh_other(e, n, i)
    j <- .mh_idx(e, d)
    phi <- 2 * .mh_u(e) - 1
    v <- X[i, ]
    v[j] <- X[i, j] + phi * (X[i, j] - X[k, j])
    v <- .mh_clip(v, lo, hi)
    fv <- as.numeric(f(v))
    nfev <<- nfev + 1
    if (fv <= Fv[i]) {
      X[i, ] <<- v
      Fv[i] <<- fv
      trial[i] <<- 0L
    } else {
      trial[i] <<- trial[i] + 1L
    }
  }
  b <- which.min(Fv)
  bx <- X[b, ]
  bf <- Fv[b]
  hist <- numeric(0)
  for (it in seq_len(max_iter)) {
    for (i in seq_len(n)) probe(i)
    fits <- vapply(Fv, fit, 0)
    tot <- .mh_sum(fits)
    t <- 0
    i <- 1
    while (t < n) {
      if (.mh_u(e) < fits[i] / tot) {
        probe(i)
        t <- t + 1
      }
      i <- i %% n + 1
    }
    b <- which.min(Fv)
    if (Fv[b] < bf) {
      bx <- X[b, ]
      bf <- Fv[b]
    }
    sc <- which.max(trial)
    if (trial[sc] > lim) {
      for (j in seq_len(d)) X[sc, j] <- lo[j] + .mh_u(e) * (hi[j] - lo[j])
      Fv[sc] <- as.numeric(f(X[sc, ]))
      nfev <- nfev + 1
      trial[sc] <- 0L
      if (Fv[sc] < bf) {
        bx <- X[sc, ]
        bf <- Fv[sc]
      }
    }
    hist <- c(hist, bf)
  }
  .mh_out("ABC (Karaboga and Basturk 2007)", bx, bf, hist, nfev)
}

#' Ant colony optimization for continuous domains (ACO_R)
#'
#' The archive keeps the n_pop best solutions, sorted; each ant picks a guide l
#' with probability proportional to w_l = exp(-(l - 1)^2 / (2 q^2 k^2)) and
#' samples every coordinate from N(s_li, sigma_i^2), sigma_i = xi times the mean
#' absolute distance from s_l to the archive; the archive keeps the best k.
#'
#' @inheritParams ArtificialBeeColony
#' @param n_ants New solutions per iteration (default n_pop).
#' @param q Locality of the rank weights.
#' @param xi Evaporation rate.
#' @return list(x, fun, estimate, history, n_fev, method).
#' @references Socha, K. and Dorigo, M. (2008). Ant colony optimization for
#'   continuous domains. European Journal of Operational Research 185,
#'   1155-1173.
#' @examples
#' AntColonyContinuous(function(x) sum(x^2), list(c(-5, 5), c(-5, 5)), max_iter = 50)$fun
#' @export
AntColonyContinuous <- function(f, bounds, n_ants = NULL, q = 0.2, xi = 0.85, n_pop = 20, max_iter = 200, seed = 0) {
  e <- .mh_rand(seed)
  s <- .mh_setup(f, bounds, n_pop, e)
  lo <- s$lo
  hi <- s$hi
  n <- s$n
  d <- s$d
  nfev <- n
  m <- if (is.null(n_ants)) n else as.integer(n_ants)
  o <- order(s$F, seq_len(n))
  A <- s$X[o, , drop = FALSE]
  AF <- s$F[o]
  w <- exp(-((0:(n - 1))^2) / (2 * q * q * n * n)) / (q * n * sqrt(2 * pi))
  wt <- .mh_sum(w)
  hist <- numeric(0)
  for (it in seq_len(max_iter)) {
    NX <- matrix(0, m, d)
    NF <- numeric(m)
    for (a in seq_len(m)) {
      r <- .mh_u(e) * wt
      acc <- 0
      g <- n
      for (l in seq_len(n)) {
        acc <- acc + w[l]
        if (r < acc) {
          g <- l
          break
        }
      }
      x <- numeric(d)
      for (j in seq_len(d)) {
        sg <- 0
        for (k in seq_len(n)) sg <- sg + abs(A[k, j] - A[g, j])
        x[j] <- A[g, j] + xi * sg / (n - 1) * .mh_n(e)
      }
      x <- .mh_clip(x, lo, hi)
      NX[a, ] <- x
      NF[a] <- as.numeric(f(x))
      nfev <- nfev + 1
    }
    P <- rbind(A, NX)
    PF <- c(AF, NF)
    o <- order(PF, seq_along(PF))[seq_len(n)]
    A <- P[o, , drop = FALSE]
    AF <- PF[o]
    hist <- c(hist, AF[1])
  }
  .mh_out("ACO_R (Socha and Dorigo 2008)", A[1, ], AF[1], hist, nfev)
}

#' Bat algorithm
#'
#' Frequency f_i = fmin + (fmax - fmin) beta, velocity v_i <- v_i + (x_i - x*)
#' f_i, position x_i + v_i; with probability 1 - r_i a local walk x* + eps
#' mean(A). A move is accepted when no worse and a uniform draw is below the
#' loudness A_i, which then decays (A <- alpha A) while r_i = r0 (1 - exp(-gamma t)).
#'
#' @inheritParams ArtificialBeeColony
#' @param fmin,fmax Frequency range.
#' @param A0,r0 Initial loudness and pulse rate.
#' @param alpha,gamma Loudness decay and pulse growth.
#' @return list(x, fun, estimate, history, n_fev, method).
#' @references Yang, X.-S. (2010). A new metaheuristic bat-inspired algorithm.
#'   Nature Inspired Cooperative Strategies for Optimization (NICSO 2010),
#'   Studies in Computational Intelligence 284, 65-74.
#' @examples
#' BatAlgorithm(function(x) sum(x^2), list(c(-5, 5), c(-5, 5)), max_iter = 50)$fun
#' @export
BatAlgorithm <- function(f, bounds, fmin = 0, fmax = 2, A0 = 1, r0 = 0.5, alpha = 0.9, gamma = 0.9,
                         n_pop = 20, max_iter = 200, seed = 0) {
  e <- .mh_rand(seed)
  s <- .mh_setup(f, bounds, n_pop, e)
  lo <- s$lo
  hi <- s$hi
  X <- s$X
  Fv <- s$F
  n <- s$n
  d <- s$d
  nfev <- n
  V <- matrix(0, n, d)
  Aud <- rep(A0, n)
  R <- rep(r0, n)
  b <- which.min(Fv)
  bx <- X[b, ]
  bf <- Fv[b]
  hist <- numeric(0)
  for (t in seq_len(max_iter)) {
    for (i in seq_len(n)) {
      fr <- fmin + (fmax - fmin) * .mh_u(e)
      V[i, ] <- V[i, ] + (X[i, ] - bx) * fr
      x <- .mh_clip(X[i, ] + V[i, ], lo, hi)
      if (.mh_u(e) > R[i]) {
        am <- .mh_sum(Aud) / n
        x <- numeric(d)
        for (j in seq_len(d)) x[j] <- bx[j] + (2 * .mh_u(e) - 1) * am
        x <- .mh_clip(x, lo, hi)
      }
      fx <- as.numeric(f(x))
      nfev <- nfev + 1
      if (fx <= Fv[i] && .mh_u(e) < Aud[i]) {
        X[i, ] <- x
        Fv[i] <- fx
        Aud[i] <- Aud[i] * alpha
        R[i] <- r0 * (1 - exp(-gamma * t))
      }
      if (fx <= bf) {
        bx <- x
        bf <- fx
      }
    }
    hist <- c(hist, bf)
  }
  .mh_out("Bat algorithm (Yang 2010)", bx, bf, hist, nfev)
}

#' Cuckoo search via Levy flights
#'
#' Each nest proposes x_i + step L (x_i - x*) with a Mantegna Levy vector L
#' (beta = 3/2) and replaces nest i when better; then each coordinate is, with
#' probability pa, moved by r (x_p1 - x_p2) for two random nests, kept when better.
#'
#' @inheritParams ArtificialBeeColony
#' @param pa Abandoned fraction.
#' @param step Levy step scale.
#' @return list(x, fun, estimate, history, n_fev, method).
#' @references Yang, X.-S. and Deb, S. (2009). Cuckoo search via Levy flights.
#'   World Congress on Nature and Biologically Inspired Computing (NaBIC 2009),
#'   210-214.
#' @examples
#' CuckooSearch(function(x) sum(x^2), list(c(-5, 5), c(-5, 5)), max_iter = 50)$fun
#' @export
CuckooSearch <- function(f, bounds, pa = 0.25, step = 0.01, n_pop = 20, max_iter = 200, seed = 0) {
  e <- .mh_rand(seed)
  s <- .mh_setup(f, bounds, n_pop, e)
  lo <- s$lo
  hi <- s$hi
  X <- s$X
  Fv <- s$F
  n <- s$n
  d <- s$d
  nfev <- n
  b <- which.min(Fv)
  bx <- X[b, ]
  bf <- Fv[b]
  hist <- numeric(0)
  for (it in seq_len(max_iter)) {
    for (i in seq_len(n)) {
      L <- .mh_levy(e, d)
      x <- .mh_clip(X[i, ] + step * L * (X[i, ] - bx), lo, hi)
      fx <- as.numeric(f(x))
      nfev <- nfev + 1
      if (fx < Fv[i]) {
        X[i, ] <- x
        Fv[i] <- fx
      }
    }
    P1 <- vapply(seq_len(n), function(i) .mh_idx(e, n), 0)
    P2 <- vapply(seq_len(n), function(i) .mh_idx(e, n), 0)
    for (i in seq_len(n)) {
      r <- .mh_u(e)
      x <- X[i, ]
      for (j in seq_len(d)) if (.mh_u(e) < pa) x[j] <- X[i, j] + r * (X[P1[i], j] - X[P2[i], j])
      x <- .mh_clip(x, lo, hi)
      fx <- as.numeric(f(x))
      nfev <- nfev + 1
      if (fx < Fv[i]) {
        X[i, ] <- x
        Fv[i] <- fx
      }
    }
    b <- which.min(Fv)
    if (Fv[b] < bf) {
      bx <- X[b, ]
      bf <- Fv[b]
    }
    hist <- c(hist, bf)
  }
  .mh_out("Cuckoo search via Levy flights (Yang and Deb 2009)", bx, bf, hist, nfev)
}

#' Firefly algorithm
#'
#' For every pair with f_j < f_i, firefly i moves by beta0 exp(-gamma r^2)
#' (x_j - x_i) + alpha (u - 1/2)(hi - lo), r measured on the unit-scaled box;
#' alpha decays by delta each iteration.
#'
#' @inheritParams ArtificialBeeColony
#' @param alpha Initial randomisation weight.
#' @param beta0,gamma Attractiveness parameters.
#' @param delta Decay of alpha.
#' @return list(x, fun, estimate, history, n_fev, method).
#' @references Yang, X.-S. (2009). Firefly algorithms for multimodal
#'   optimization. Stochastic Algorithms: Foundations and Applications (SAGA
#'   2009), LNCS 5792, 169-178.
#' @examples
#' FireflyAlgorithm(function(x) sum(x^2), list(c(-5, 5), c(-5, 5)), n_pop = 10, max_iter = 20)$fun
#' @export
FireflyAlgorithm <- function(f, bounds, alpha = 0.5, beta0 = 1, gamma = 1, delta = 0.97,
                             n_pop = 20, max_iter = 200, seed = 0) {
  e <- .mh_rand(seed)
  s <- .mh_setup(f, bounds, n_pop, e)
  lo <- s$lo
  hi <- s$hi
  X <- s$X
  Fv <- s$F
  n <- s$n
  d <- s$d
  nfev <- n
  b <- which.min(Fv)
  bx <- X[b, ]
  bf <- Fv[b]
  a <- alpha
  hist <- numeric(0)
  for (it in seq_len(max_iter)) {
    for (i in seq_len(n)) {
      for (k in seq_len(n)) {
        if (Fv[k] < Fv[i]) {
          r2 <- 0
          for (j in seq_len(d)) {
            z <- (X[i, j] - X[k, j]) / (hi[j] - lo[j])
            r2 <- r2 + z * z
          }
          beta <- beta0 * exp(-gamma * r2)
          x <- numeric(d)
          for (j in seq_len(d)) x[j] <- X[i, j] + beta * (X[k, j] - X[i, j]) + a * (.mh_u(e) - 0.5) * (hi[j] - lo[j])
          X[i, ] <- .mh_clip(x, lo, hi)
          Fv[i] <- as.numeric(f(X[i, ]))
          nfev <- nfev + 1
          if (Fv[i] < bf) {
            bx <- X[i, ]
            bf <- Fv[i]
          }
        }
      }
    }
    a <- a * delta
    hist <- c(hist, bf)
  }
  .mh_out("Firefly algorithm (Yang 2009)", bx, bf, hist, nfev)
}

#' Grey wolf optimizer
#'
#' With a falling from 2 to 0, each wolf moves to the mean over the leaders
#' alpha, beta, delta of X_l - A |C X_l - X|, A = 2 a r1 - a, C = 2 r2 per
#' coordinate.
#'
#' @inheritParams ArtificialBeeColony
#' @return list(x, fun, estimate, history, n_fev, method).
#' @references Mirjalili, S., Mirjalili, S. M. and Lewis, A. (2014). Grey wolf
#'   optimizer. Advances in Engineering Software 69, 46-61.
#' @examples
#' GreyWolfOptimizer(function(x) sum(x^2), list(c(-5, 5), c(-5, 5)), max_iter = 50)$fun
#' @export
GreyWolfOptimizer <- function(f, bounds, n_pop = 20, max_iter = 200, seed = 0) {
  e <- .mh_rand(seed)
  s <- .mh_setup(f, bounds, n_pop, e)
  lo <- s$lo
  hi <- s$hi
  X <- s$X
  Fv <- s$F
  n <- s$n
  d <- s$d
  nfev <- n
  o <- order(Fv, seq_len(n))
  idx <- o[pmin(1:3, n)]
  L <- X[idx, , drop = FALSE]
  LF <- Fv[idx]
  hist <- numeric(0)
  Tn <- max_iter
  for (t in 0:(Tn - 1)) {
    a <- 2 - 2 * t / Tn
    for (i in seq_len(n)) {
      x <- numeric(d)
      for (j in seq_len(d)) {
        sm <- 0
        for (q in 1:3) {
          A <- 2 * a * .mh_u(e) - a
          C <- 2 * .mh_u(e)
          sm <- sm + L[q, j] - A * abs(C * L[q, j] - X[i, j])
        }
        x[j] <- sm / 3
      }
      X[i, ] <- .mh_clip(x, lo, hi)
      Fv[i] <- as.numeric(f(X[i, ]))
      nfev <- nfev + 1
    }
    for (i in seq_len(n)) {
      if (Fv[i] < LF[1]) {
        L <- rbind(X[i, ], L[1, ], L[2, ])
        LF <- c(Fv[i], LF[1], LF[2])
      } else if (Fv[i] < LF[2]) {
        L <- rbind(L[1, ], X[i, ], L[2, ])
        LF <- c(LF[1], Fv[i], LF[2])
      } else if (Fv[i] < LF[3]) {
        L[3, ] <- X[i, ]
        LF[3] <- Fv[i]
      }
    }
    hist <- c(hist, LF[1])
  }
  .mh_out("GWO (Mirjalili, Mirjalili and Lewis 2014)", L[1, ], LF[1], hist, nfev)
}

#' Harris hawks optimization
#'
#' Escaping energy E = 2 E0 (1 - t / T), E0 ~ U(-1, 1). Exploration for
#' |E| >= 1 perches on a random hawk or relative to the rabbit and the mean;
#' exploitation uses soft or hard besiege, with or without progressive rapid
#' dives built from Levy flights.
#'
#' @inheritParams ArtificialBeeColony
#' @return list(x, fun, estimate, history, n_fev, method).
#' @references Heidari, A. A., Mirjalili, S., Faris, H., Aljarah, I., Mafarja,
#'   M. and Chen, H. (2019). Harris hawks optimization: algorithm and
#'   applications. Future Generation Computer Systems 97, 849-872.
#' @examples
#' HarrisHawksOptimizer(function(x) sum(x^2), list(c(-5, 5), c(-5, 5)), max_iter = 50)$fun
#' @export
HarrisHawksOptimizer <- function(f, bounds, n_pop = 20, max_iter = 200, seed = 0) {
  e <- .mh_rand(seed)
  s <- .mh_setup(f, bounds, n_pop, e)
  lo <- s$lo
  hi <- s$hi
  X <- s$X
  Fv <- s$F
  n <- s$n
  d <- s$d
  nfev <- n
  b <- which.min(Fv)
  bx <- X[b, ]
  bf <- Fv[b]
  hist <- numeric(0)
  Tn <- max_iter
  for (t in 0:(Tn - 1)) {
    for (i in seq_len(n)) {
      E0 <- 2 * .mh_u(e) - 1
      E <- 2 * E0 * (1 - t / Tn)
      if (abs(E) >= 1) {
        q <- .mh_u(e)
        if (q >= 0.5) {
          k <- .mh_idx(e, n)
          r1 <- .mh_u(e)
          r2 <- .mh_u(e)
          x <- X[k, ] - r1 * abs(X[k, ] - 2 * r2 * X[i, ])
        } else {
          r3 <- .mh_u(e)
          r4 <- .mh_u(e)
          x <- (bx - .mh_colmean(X)) - r3 * (lo + r4 * (hi - lo))
        }
        X[i, ] <- .mh_clip(x, lo, hi)
        Fv[i] <- as.numeric(f(X[i, ]))
        nfev <- nfev + 1
      } else {
        r <- .mh_u(e)
        J <- 2 * (1 - .mh_u(e))
        if (r >= 0.5 && abs(E) >= 0.5) {
          X[i, ] <- .mh_clip((bx - X[i, ]) - E * abs(J * bx - X[i, ]), lo, hi)
          Fv[i] <- as.numeric(f(X[i, ]))
          nfev <- nfev + 1
        } else if (r >= 0.5) {
          X[i, ] <- .mh_clip(bx - E * abs(bx - X[i, ]), lo, hi)
          Fv[i] <- as.numeric(f(X[i, ]))
          nfev <- nfev + 1
        } else {
          base <- if (abs(E) >= 0.5) X[i, ] else .mh_colmean(X)
          Y <- .mh_clip(bx - E * abs(J * bx - base), lo, hi)
          fy <- as.numeric(f(Y))
          nfev <- nfev + 1
          Lv <- .mh_levy(e, d)
          S <- vapply(seq_len(d), function(j) .mh_u(e), 0)
          Z <- .mh_clip(Y + S * Lv, lo, hi)
          fz <- as.numeric(f(Z))
          nfev <- nfev + 1
          if (fy < Fv[i]) {
            X[i, ] <- Y
            Fv[i] <- fy
          } else if (fz < Fv[i]) {
            X[i, ] <- Z
            Fv[i] <- fz
          }
        }
      }
      if (Fv[i] < bf) {
        bx <- X[i, ]
        bf <- Fv[i]
      }
    }
    hist <- c(hist, bf)
  }
  .mh_out("HHO (Heidari et al. 2019)", bx, bf, hist, nfev)
}

#' Jaya algorithm
#'
#' Each candidate proposes x + r1 (x_best - |x|) - r2 (x_worst - |x|), r1, r2
#' uniform per coordinate, and keeps it when better.
#'
#' @inheritParams ArtificialBeeColony
#' @return list(x, fun, estimate, history, n_fev, method).
#' @references Rao, R. V. (2016). Jaya: a simple and new optimization algorithm
#'   for solving constrained and unconstrained optimization problems.
#'   International Journal of Industrial Engineering Computations 7, 19-34.
#' @examples
#' JayaAlgorithm(function(x) sum(x^2), list(c(-5, 5), c(-5, 5)), max_iter = 50)$fun
#' @export
JayaAlgorithm <- function(f, bounds, n_pop = 20, max_iter = 200, seed = 0) {
  e <- .mh_rand(seed)
  s <- .mh_setup(f, bounds, n_pop, e)
  lo <- s$lo
  hi <- s$hi
  X <- s$X
  Fv <- s$F
  n <- s$n
  d <- s$d
  nfev <- n
  hist <- numeric(0)
  for (it in seq_len(max_iter)) {
    xb <- X[which.min(Fv), ]
    xw <- X[which.max(Fv), ]
    for (i in seq_len(n)) {
      x <- numeric(d)
      for (j in seq_len(d)) {
        r1 <- .mh_u(e)
        r2 <- .mh_u(e)
        x[j] <- X[i, j] + r1 * (xb[j] - abs(X[i, j])) - r2 * (xw[j] - abs(X[i, j]))
      }
      x <- .mh_clip(x, lo, hi)
      fx <- as.numeric(f(x))
      nfev <- nfev + 1
      if (fx < Fv[i]) {
        X[i, ] <- x
        Fv[i] <- fx
      }
    }
    hist <- c(hist, min(Fv))
  }
  b <- which.min(Fv)
  .mh_out("Jaya (Rao 2016)", X[b, ], Fv[b], hist, nfev)
}

#' Moth-flame optimization
#'
#' Flames are the best n solutions so far; their number falls as
#' round(n - t (n - 1) / T). Moth i spirals around flame min(i, count):
#' M = D exp(b s) cos(2 pi s) + F, D = |F - M|, s ~ U(r, 1), r from -1 to -2.
#'
#' @inheritParams ArtificialBeeColony
#' @param b Spiral shape.
#' @return list(x, fun, estimate, history, n_fev, method).
#' @references Mirjalili, S. (2015). Moth-flame optimization algorithm: a novel
#'   nature-inspired heuristic paradigm. Knowledge-Based Systems 89, 228-249.
#' @examples
#' MothFlameOptimizer(function(x) sum(x^2), list(c(-5, 5), c(-5, 5)), max_iter = 50)$fun
#' @export
MothFlameOptimizer <- function(f, bounds, b = 1, n_pop = 20, max_iter = 200, seed = 0) {
  e <- .mh_rand(seed)
  s <- .mh_setup(f, bounds, n_pop, e)
  lo <- s$lo
  hi <- s$hi
  X <- s$X
  Fv <- s$F
  n <- s$n
  d <- s$d
  nfev <- n
  o <- order(Fv, seq_len(n))
  FL <- X[o, , drop = FALSE]
  FF <- Fv[o]
  hist <- numeric(0)
  Tn <- max_iter
  for (t in seq_len(Tn)) {
    nf <- round(n - t * (n - 1) / Tn)
    r <- -1 - t / Tn
    for (i in seq_len(n)) {
      fl <- FL[min(i, nf), ]
      x <- numeric(d)
      for (j in seq_len(d)) {
        sv <- (r - 1) * .mh_u(e) + 1
        D <- abs(fl[j] - X[i, j])
        x[j] <- D * exp(b * sv) * cos(2 * pi * sv) + fl[j]
      }
      X[i, ] <- .mh_clip(x, lo, hi)
      Fv[i] <- as.numeric(f(X[i, ]))
      nfev <- nfev + 1
    }
    P <- rbind(FL, X)
    PF <- c(FF, Fv)
    o <- order(PF, seq_along(PF))[seq_len(n)]
    FL <- P[o, , drop = FALSE]
    FF <- PF[o]
    hist <- c(hist, FF[1])
  }
  .mh_out("MFO (Mirjalili 2015)", FL[1, ], FF[1], hist, nfev)
}

#' Sine cosine algorithm
#'
#' With r1 = a - t a / T, each coordinate moves x + r1 sin(r2) |r3 P - x| when
#' r4 < 1/2 and with cos(r2) otherwise; r2 ~ U(0, 2 pi), r3 ~ U(0, 2).
#'
#' @inheritParams ArtificialBeeColony
#' @param a Initial r1.
#' @return list(x, fun, estimate, history, n_fev, method).
#' @references Mirjalili, S. (2016). SCA: a sine cosine algorithm for solving
#'   optimization problems. Knowledge-Based Systems 96, 120-133.
#' @examples
#' SineCosineAlgorithm(function(x) sum(x^2), list(c(-5, 5), c(-5, 5)), max_iter = 50)$fun
#' @export
SineCosineAlgorithm <- function(f, bounds, a = 2, n_pop = 20, max_iter = 200, seed = 0) {
  e <- .mh_rand(seed)
  s <- .mh_setup(f, bounds, n_pop, e)
  lo <- s$lo
  hi <- s$hi
  X <- s$X
  Fv <- s$F
  n <- s$n
  d <- s$d
  nfev <- n
  b <- which.min(Fv)
  bx <- X[b, ]
  bf <- Fv[b]
  hist <- numeric(0)
  Tn <- max_iter
  for (t in 0:(Tn - 1)) {
    r1 <- a - t * a / Tn
    for (i in seq_len(n)) {
      x <- numeric(d)
      for (j in seq_len(d)) {
        r2 <- 2 * pi * .mh_u(e)
        r3 <- 2 * .mh_u(e)
        r4 <- .mh_u(e)
        tr <- if (r4 < 0.5) sin(r2) else cos(r2)
        x[j] <- X[i, j] + r1 * tr * abs(r3 * bx[j] - X[i, j])
      }
      X[i, ] <- .mh_clip(x, lo, hi)
      Fv[i] <- as.numeric(f(X[i, ]))
      nfev <- nfev + 1
      if (Fv[i] < bf) {
        bx <- X[i, ]
        bf <- Fv[i]
      }
    }
    hist <- c(hist, bf)
  }
  .mh_out("SCA (Mirjalili 2016)", bx, bf, hist, nfev)
}

#' Teaching-learning-based optimization
#'
#' Teacher phase x + r (x_teacher - T_F mean), T_F in 1, 2; learner phase with a
#' random partner k: x + r (x - x_k) if x is better, else x + r (x_k - x); each
#' proposal is kept when better.
#'
#' @inheritParams ArtificialBeeColony
#' @return list(x, fun, estimate, history, n_fev, method).
#' @references Rao, R. V., Savsani, V. J. and Vakharia, D. P. (2011).
#'   Teaching-learning-based optimization: a novel method for constrained
#'   mechanical design optimization problems. Computer-Aided Design 43, 303-315.
#' @examples
#' TeachingLearningOptimizer(function(x) sum(x^2), list(c(-5, 5), c(-5, 5)), max_iter = 50)$fun
#' @export
TeachingLearningOptimizer <- function(f, bounds, n_pop = 20, max_iter = 200, seed = 0) {
  e <- .mh_rand(seed)
  s <- .mh_setup(f, bounds, n_pop, e)
  lo <- s$lo
  hi <- s$hi
  X <- s$X
  Fv <- s$F
  n <- s$n
  d <- s$d
  nfev <- n
  hist <- numeric(0)
  for (it in seq_len(max_iter)) {
    for (i in seq_len(n)) {
      tch <- which.min(Fv)
      mn <- .mh_colmean(X)
      TF <- 1 + (if (.mh_u(e) >= 0.5) 1 else 0)
      x <- numeric(d)
      for (j in seq_len(d)) x[j] <- X[i, j] + .mh_u(e) * (X[tch, j] - TF * mn[j])
      x <- .mh_clip(x, lo, hi)
      fx <- as.numeric(f(x))
      nfev <- nfev + 1
      if (fx < Fv[i]) {
        X[i, ] <- x
        Fv[i] <- fx
      }
      k <- .mh_other(e, n, i)
      x <- numeric(d)
      if (Fv[i] < Fv[k]) {
        for (j in seq_len(d)) x[j] <- X[i, j] + .mh_u(e) * (X[i, j] - X[k, j])
      } else {
        for (j in seq_len(d)) x[j] <- X[i, j] + .mh_u(e) * (X[k, j] - X[i, j])
      }
      x <- .mh_clip(x, lo, hi)
      fx <- as.numeric(f(x))
      nfev <- nfev + 1
      if (fx < Fv[i]) {
        X[i, ] <- x
        Fv[i] <- fx
      }
    }
    hist <- c(hist, min(Fv))
  }
  b <- which.min(Fv)
  .mh_out("TLBO (Rao, Savsani and Vakharia 2011)", X[b, ], Fv[b], hist, nfev)
}

#' Whale optimization algorithm
#'
#' With a falling from 2 to 0, A = 2 a r - a, C = 2 r: for p < 1/2 a whale
#' encircles the best (|A| < 1) or a random whale by X_ref - A |C X_ref - X|;
#' otherwise it follows the spiral |X* - X| exp(b l) cos(2 pi l) + X*.
#'
#' @inheritParams ArtificialBeeColony
#' @param b Spiral shape.
#' @return list(x, fun, estimate, history, n_fev, method).
#' @references Mirjalili, S. and Lewis, A. (2016). The whale optimization
#'   algorithm. Advances in Engineering Software 95, 51-67.
#' @examples
#' WhaleOptimization(function(x) sum(x^2), list(c(-5, 5), c(-5, 5)), max_iter = 50)$fun
#' @export
WhaleOptimization <- function(f, bounds, b = 1, n_pop = 20, max_iter = 200, seed = 0) {
  e <- .mh_rand(seed)
  s <- .mh_setup(f, bounds, n_pop, e)
  lo <- s$lo
  hi <- s$hi
  X <- s$X
  Fv <- s$F
  n <- s$n
  d <- s$d
  nfev <- n
  bi <- which.min(Fv)
  bx <- X[bi, ]
  bf <- Fv[bi]
  hist <- numeric(0)
  Tn <- max_iter
  for (t in 0:(Tn - 1)) {
    a <- 2 - 2 * t / Tn
    for (i in seq_len(n)) {
      A <- 2 * a * .mh_u(e) - a
      C <- 2 * .mh_u(e)
      p <- .mh_u(e)
      l <- 2 * .mh_u(e) - 1
      if (p < 0.5) {
        ref <- if (abs(A) < 1) bx else X[.mh_idx(e, n), ]
        x <- ref - A * abs(C * ref - X[i, ])
      } else {
        x <- abs(bx - X[i, ]) * exp(b * l) * cos(2 * pi * l) + bx
      }
      X[i, ] <- .mh_clip(x, lo, hi)
      Fv[i] <- as.numeric(f(X[i, ]))
      nfev <- nfev + 1
    }
    for (i in seq_len(n)) {
      if (Fv[i] < bf) {
        bx <- X[i, ]
        bf <- Fv[i]
      }
    }
    hist <- c(hist, bf)
  }
  .mh_out("WOA (Mirjalili and Lewis 2016)", bx, bf, hist, nfev)
}
