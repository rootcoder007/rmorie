.ma_mulmod <- function(a, b, n) {
  r <- 0
  a <- a %% n
  while (b > 0) {
    if (b %% 2 == 1) r <- (r + a) %% n
    a <- (a + a) %% n
    b <- b %/% 2
  }
  r
}

.ma_powmod <- function(a, d, n) {
  r <- 1
  a <- a %% n
  while (d > 0) {
    if (d %% 2 == 1) r <- .ma_mulmod(r, a, n)
    a <- .ma_mulmod(a, a, n)
    d <- d %/% 2
  }
  r
}

.ma_gcd <- function(a, b) {
  while (b > 0) {
    t <- a %% b
    a <- b
    b <- t
  }
  a
}

.ma_is_prime <- function(n) {
  if (n < 2) return(FALSE)
  sp <- c(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37)
  for (p in sp) if (n %% p == 0) return(n == p)
  d <- n - 1
  s <- 0
  while (d %% 2 == 0) {
    d <- d / 2
    s <- s + 1
  }
  for (a in sp) {
    x <- .ma_powmod(a, d, n)
    if (x == 1 || x == n - 1) next
    comp <- TRUE
    for (r in seq_len(s - 1)) {
      x <- .ma_mulmod(x, x, n)
      if (x == n - 1) {
        comp <- FALSE
        break
      }
    }
    if (comp) return(FALSE)
  }
  TRUE
}

.ma_rho <- function(n, cc) {
  x <- 2
  y <- 2
  d <- 1
  while (d == 1) {
    x <- (.ma_mulmod(x, x, n) + cc) %% n
    y <- (.ma_mulmod(y, y, n) + cc) %% n
    y <- (.ma_mulmod(y, y, n) + cc) %% n
    d <- .ma_gcd(abs(x - y), n)
  }
  d
}

#' Numerical and discrete algorithms
#'
#' \code{PollardRho}: prime factorisation with Pollard's rho (exact for
#' n below 2^52). \code{LegendrePolynomials}: Bonnet recurrence.
#' \code{ResolutionRefutation}: propositional resolution saturation.
#' \code{PolarTransform}: recursive polar transform (PolarQuant) and its
#' inverse. \code{PanterDiteBound}: high-rate Gaussian quantisation
#' distortion. \code{LogitProportion}: logit proportions and variances.
#' \code{McStandardError}: Geyer initial-sequence Monte Carlo standard error.
#' Identical to the Python arm \code{morie.fn.mathalg}.
#'
#' @param n Integer to factor, or sample sizes.
#' @param x Values (or, for the inverse transform, a list of radius and
#'   angles).
#' @param degree Largest degree.
#' @param normalized Orthonormal scaling.
#' @param clauses List of integer vectors (DIMACS literals).
#' @param max_clauses Clause limit.
#' @param inverse Apply the inverse transform.
#' @param bits Bits per value.
#' @param sigma2 Source variance.
#' @param events Event counts.
#' @param draws Chain draws.
#' @return A vector, matrix or list.
#' @references Pollard, J. M. (1975). A Monte Carlo method for factorization.
#'   BIT 15, 331-334.
#'
#'   Robinson, J. A. (1965). A machine-oriented logic based on the resolution
#'   principle. Journal of the ACM 12, 23-41.
#'
#'   Panter, P. F. and Dite, W. (1951). Quantization distortion in pulse-count
#'   modulation with nonuniform spacing of levels. Proceedings of the IRE 39,
#'   44-48.
#'
#'   Geyer, C. J. (1992). Practical Markov chain Monte Carlo. Statistical
#'   Science 7, 473-483.
#' @examples
#' PollardRho(8051)
#' LegendrePolynomials(0.5, 3)
#' @export
PollardRho <- function(n) {
  if (n < 2 || n >= 2^52) stop("n must satisfy 2 <= n < 2^52")
  out <- numeric(0)
  for (p in c(2, 3, 5, 7, 11, 13)) {
    while (n %% p == 0) {
      out <- c(out, p)
      n <- n %/% p
    }
  }
  stack <- if (n > 1) n else numeric(0)
  while (length(stack)) {
    m <- stack[length(stack)]
    stack <- stack[-length(stack)]
    if (.ma_is_prime(m)) {
      out <- c(out, m)
      next
    }
    cc <- 1
    d <- .ma_rho(m, cc)
    while (d == m) {
      cc <- cc + 1
      d <- .ma_rho(m, cc)
    }
    stack <- c(stack, d, m %/% d)
  }
  sort(out)
}

#' @rdname PollardRho
#' @export
LegendrePolynomials <- function(x, degree, normalized = FALSE) {
  t(vapply(x, function(v) {
    p <- c(1, v)[seq_len(min(degree + 1, 2))]
    for (k in seq_len(degree - 1)) p <- c(p, ((2 * k + 1) * v * p[k + 1] - k * p[k]) / (k + 1))
    if (normalized) p <- p * sqrt((2 * (seq_along(p) - 1) + 1) / 2)
    p
  }, numeric(degree + 1)))
}

#' @rdname PollardRho
#' @export
ResolutionRefutation <- function(clauses, max_clauses = 100000) {
  key <- function(c) paste(c, collapse = ",")
  cl <- list()
  seen <- character(0)
  for (c in clauses) {
    k <- sort(unique(as.integer(c)))
    if (!(key(k) %in% seen) && !any(-k %in% k)) {
      seen <- c(seen, key(k))
      cl[[length(cl) + 1]] <- k
    }
  }
  parents <- vector("list", length(cl))
  i <- 1
  while (i <= length(cl)) {
    for (j in seq_len(i - 1)) {
      a <- cl[[j]]
      b <- cl[[i]]
      for (lit in a) {
        if (!(-lit %in% b)) next
        r <- sort(unique(c(a[a != lit], b[b != -lit])))
        if (any(-r %in% r) || key(r) %in% seen) next
        seen <- c(seen, key(r))
        cl[[length(cl) + 1]] <- r
        parents[[length(cl)]] <- c(j - 1, i - 1, abs(lit))
        if (length(r) == 0) return(list(unsatisfiable = TRUE, clauses = cl, parents = parents))
        if (length(cl) > max_clauses) stop("clause limit reached")
      }
    }
    i <- i + 1
  }
  list(unsatisfiable = FALSE, clauses = cl, parents = parents)
}

#' @rdname PollardRho
#' @export
PolarTransform <- function(x, inverse = FALSE) {
  if (!inverse) {
    v <- as.numeric(x)
    d <- length(v)
    if (d < 2 || bitwAnd(d, d - 1) != 0) stop("length must be a power of 2, at least 2")
    angles <- numeric(0)
    while (length(v) > 1) {
      odd <- v[seq(1, length(v), 2)]
      even <- v[seq(2, length(v), 2)]
      angles <- c(angles, atan2(even, odd))
      v <- sqrt(odd^2 + even^2)
    }
    return(list(radius = v, angles = angles))
  }
  ang <- x[[2]]
  d <- length(ang) + 1
  sizes <- d / 2^(seq_len(log2(d)))
  ends <- cumsum(sizes)
  v <- x[[1]]
  for (b in rev(seq_along(sizes))) {
    blk <- ang[(ends[b] - sizes[b] + 1):ends[b]]
    v <- as.vector(rbind(v * cos(blk), v * sin(blk)))
  }
  v
}

#' @rdname PollardRho
#' @export
PanterDiteBound <- function(bits, sigma2 = 1) {
  mse <- sqrt(3) * pi / 2 * sigma2 * 2^(-2 * bits)
  list(mse = mse, snr_db = 10 * log10(sigma2 / mse))
}

#' @rdname PollardRho
#' @export
LogitProportion <- function(events, n) list(yi = log(events / (n - events)), vi = 1 / events + 1 / (n - events))

#' @rdname PollardRho
#' @export
McStandardError <- function(draws) {
  n <- length(draws)
  x <- draws - sum(draws) / n
  gam <- function(k) sum(x[seq_len(n - k)] * x[(1 + k):n]) / n
  g0 <- gam(0)
  big <- numeric(0)
  for (i in seq_len(n %/% 2) - 1) {
    g <- gam(2 * i) + gam(2 * i + 1)
    if (g <= 0) break
    big <- c(big, g)
  }
  var_pos <- -g0 + 2 * sum(big)
  if (length(big) > 1) big <- cummin(big)
  var_dec <- -g0 + 2 * sum(big)
  list(mcse = sqrt(var_dec / n), ess = n * g0 / var_dec, gamma0 = g0, var_pos = var_pos, var_dec = var_dec)
}
