# morie.fn -- function file (rootcoder007/morie)
# Stahel-Donoho outlyingness and the estimator built on it.
#
# **The idea.** A multivariate outlier need not be extreme in any
# coordinate -- it can sit inside every marginal range and still be far
# from the data cloud, in a direction nobody thought to look. So look
# in *every* direction:
#
# .. math:: r_i = \sup_{\|a\| = 1}
#           \frac{|a'x_i - \mathrm{med}_j(a'x_j)|}
#                {\mathrm{MAD}_j(a'x_j)}.
#
# Each direction reduces the problem to one dimension, where the median
# and the median absolute deviation give a robust standardisation. The
# worst such standardised distance is the outlyingness. Downweighting
# by :math:`w(r_i)` and taking the weighted mean and covariance gives
# an affine-equivariant estimator with the highest possible breakdown
# point.
#
# References
# ----------
# Maronna, R. A. & Yohai, V. J. (1995) "The behavior of the
# Stahel-Donoho robust multivariate estimator", *Journal of the
# American Statistical Association* 90(429), 330-341,
# doi:10.1080/01621459.1995.10476517.

morie_stahdo_DIRECTIONS <- c("subsample", "random")

.stahdo_median <- function(v) {
  v <- as.numeric(v)
  s <- sort(v)
  n <- length(s)
  if (n == 0L) stop("stahdo: the median of nothing is undefined")
  m <- n %/% 2L
  if (n %% 2L == 1L) {
    return(s[m + 1L])
  } else {
    return(0.5 * (s[m] + s[m + 1L]))
  }
}

.stahdo_mad <- function(v, consistent = TRUE) {
  v <- as.numeric(v)
  m <- .stahdo_median(v)
  d <- .stahdo_median(abs(v - m))
  if (consistent) {
    return(d * 1.4826)
  } else {
    return(d)
  }
}

.stahdo_prep <- function(X) {
  if (is.data.frame(X)) {
    M <- as.matrix(X)
    storage.mode(M) <- "double"
  } else if (is.matrix(X)) {
    M <- X
    storage.mode(M) <- "double"
  } else if (is.list(X)) {
    lens <- vapply(X, length, integer(1L))
    if (length(lens) == 0L) stop("stahdo: the data matrix is empty")
    if (any(lens != lens[1L])) stop("stahdo: the data matrix is ragged")
    M <- do.call(rbind, lapply(X, as.numeric))
  } else {
    stop("stahdo: X must be a matrix, data.frame, or list of vectors")
  }
  n <- nrow(M)
  if (n < 3L) stop("stahdo: need at least three observations")
  p <- ncol(M)
  if (p == 0L) stop("stahdo: the data matrix is ragged or empty")
  list(M = M, n = n, p = p)
}

.stahdo_combn <- function(n, p) {
  if (p == 0L) return(list(integer(0L)))
  if (p > n) return(list())
  total <- choose(n, p)
  result <- vector("list", total)
  combo <- seq_len(p)
  k <- 1L
  repeat {
    result[[k]] <- combo
    k <- k + 1L
    if (k > total) break
    i <- p
    while (i >= 1L && combo[i] == n - p + i) {
      i <- i - 1L
    }
    if (i < 1L) break
    combo[i] <- combo[i] + 1L
    if (i < p) {
      for (j in (i + 1L):p) {
        combo[j] <- combo[j - 1L] + 1L
      }
    }
  }
  result
}

.stahdo_null_vector <- function(rows, p) {
  nr <- nrow(rows)
  nc <- ncol(rows)
  if (nr == 0L) {
    v <- rep(0, nc)
    v[1L] <- 1
    return(v)
  }
  A <- rows
  piv_cols <- integer(0L)
  rr <- 0L
  for (c in seq_len(nc)) {
    piv <- NULL
    if (rr < nr) {
      for (i in (rr + 1L):nr) {
        if (abs(A[i, c]) > 1e-10) {
          piv <- i
          break
        }
      }
    }
    if (is.null(piv)) next
    if ((rr + 1L) != piv) {
      temp <- A[rr + 1L, , drop = FALSE]
      A[rr + 1L, ] <- A[piv, ]
      A[piv, ] <- temp
    }
    f <- A[rr + 1L, c]
    A[rr + 1L, ] <- A[rr + 1L, ] / f
    for (i in seq_len(nr)) {
      if (i != (rr + 1L) && abs(A[i, c]) > 0) {
        g <- A[i, c]
        A[i, ] <- A[i, ] - g * A[rr + 1L, ]
      }
    }
    piv_cols <- c(piv_cols, c)
    rr <- rr + 1L
  }
  free <- setdiff(seq_len(nc), piv_cols)
  if (length(free) == 0L) return(NULL)
  fc <- free[1L]
  v <- rep(0.0, nc)
  v[fc] <- 1.0
  for (i in seq_along(piv_cols)) {
    c_idx <- piv_cols[i]
    v[c_idx] <- -A[i, fc]
  }
  nrm <- sqrt(sum(v * v))
  if (nrm < 1e-12) return(NULL)
  v / nrm
}

.stahdo_subsample_dirs <- function(M, n, p, n_dirs, seed) {
  total <- 1L
  for (i in 0L:(p - 1L)) {
    total <- total * (n - i) %/% (i + 1L)
  }
  if (total <= max(as.integer(n_dirs), 1L)) {
    combos <- .stahdo_combn(n, p)
    exhaustive <- TRUE
  } else {
    e <- .ghc_rng(as.integer(seed))
    seen <- character(0L)
    combos <- list()
    while (length(combos) < as.integer(n_dirs)) {
      u <- .ghc_unif(e, p * 3L)
      idx <- sort(unique((floor(u * n) %% n) + 1L))
      if (length(idx) < p) next
      idx <- idx[seq_len(p)]
      key <- paste(idx, collapse = ",")
      if (key %in% seen) next
      seen <- c(seen, key)
      combos[[length(combos) + 1L]] <- idx
    }
    exhaustive <- FALSE
  }
  dirs <- list()
  for (idx in combos) {
    base <- M[idx[1L], ]
    rows <- M[idx[-1L], , drop = FALSE] -
            matrix(base, nrow = p - 1L, ncol = p, byrow = TRUE)
    a <- .stahdo_null_vector(rows, p)
    if (!is.null(a)) {
      dirs[[length(dirs) + 1L]] <- a
    }
  }
  if (length(dirs) == 0L) {
    stop("stahdo: every sampled subset was degenerate -- ",
         "the data may lie in a lower-dimensional subspace")
  }
  list(dirs = dirs, exhaustive = exhaustive)
}

.stahdo_random_dirs <- function(p, n_dirs, seed) {
  e <- .ghc_rng(as.integer(seed))
  out <- list()
  while (length(out) < as.integer(n_dirs)) {
    u <- .ghc_unif(e, p)
    v <- u * 2.0 - 1.0
    nrm <- sqrt(sum(v * v))
    if (nrm > 1e-9) {
      out[[length(out) + 1L]] <- v / nrm
    }
  }
  out
}

.stahdo_outlyingness <- function(X, directions = "subsample",
                                 n_directions = 500, seed = 1) {
  if (!(directions %in% morie_stahdo_DIRECTIONS)) {
    stop("stahdo: directions must be one of ",
         paste(morie_stahdo_DIRECTIONS, collapse = ", "),
         ", got ", directions)
  }
  prep <- .stahdo_prep(X)
  M <- prep$M
  n <- prep$n
  p <- prep$p
  if (p == 1L) {
    dirs <- list(c(1.0))
    exhaustive <- TRUE
  } else if (directions == "subsample") {
    res <- .stahdo_subsample_dirs(M, n, p, n_directions, seed)
    dirs <- res$dirs
    exhaustive <- res$exhaustive
  } else {
    dirs <- .stahdo_random_dirs(p, n_directions, seed)
    exhaustive <- FALSE
  }
  r <- rep(0.0, n)
  used <- 0L
  for (a in dirs) {
    proj <- as.numeric(M %*% a)
    s <- .stahdo_mad(proj)
    if (s <= 1e-12) next
    used <- used + 1L
    m <- .stahdo_median(proj)
    d <- abs(proj - m) / s
    r <- pmax(r, d)
  }
  if (used == 0L) {
    stop("stahdo: every searched direction has zero MAD, ",
         "so no outlyingness is defined")
  }
  list(outlyingness = r, n_directions = length(dirs),
       n_used = used, exhaustive = exhaustive,
       directions = directions)
}

.stahdo_weight <- function(r, cutoff) {
  c <- as.numeric(cutoff)
  ifelse(r <= c, 1.0, (c / r)^2)
}

.stahdo_chi2_cdf <- function(x, k) {
  if (x <= 0) return(0.0)
  a <- k / 2.0
  z <- x / 2.0
  if (z < a + 1.0) {
    term <- 1.0 / a
    s <- term
    for (i in 1:499) {
      term <- term * z / (a + i)
      s <- s + term
      if (abs(term) < 1e-15 * abs(s)) break
    }
    return(s * exp(-z + a * log(z) - lgamma(a)))
  }
  b <- z + 1.0 - a
  cc <- 1e300
  d <- 1.0 / (z + 1.0 - a)
  h <- d
  for (i in 1:499) {
    an <- -i * (i - a)
    b <- b + 2.0
    d <- an * d + b
    if (abs(d) < 1e-300) d <- 1e-300
    cc <- b + an / cc
    if (abs(cc) < 1e-300) cc <- 1e-300
    d <- 1.0 / d
    delta <- d * cc
    h <- h * delta
    if (abs(delta - 1.0) < 1e-15) break
  }
  q <- exp(-z + a * log(z) - lgamma(a)) * h
  1.0 - q
}

.stahdo_chi2_median <- function(p) {
  lo <- 0.0
  hi <- 100.0 + 10.0 * p
  for (i in 1:200) {
    mid <- 0.5 * (lo + hi)
    if (.stahdo_chi2_cdf(mid, p) < 0.5) {
      lo <- mid
    } else {
      hi <- mid
    }
  }
  0.5 * (lo + hi)
}

.stahdo_stahel_donoho <- function(X, directions = "subsample",
                                  n_directions = 500, seed = 1,
                                  cutoff = NULL) {
  prep <- .stahdo_prep(X)
  M <- prep$M
  n <- prep$n
  p <- prep$p
  o <- .stahdo_outlyingness(X, directions, n_directions, seed)
  r <- o$outlyingness
  c <- if (is.null(cutoff)) sqrt(.stahdo_chi2_median(p)) else as.numeric(cutoff)
  if (c <= 0) stop("stahdo: the cutoff must be positive")
  w <- .stahdo_weight(r, c)
  sw <- sum(w)
  if (sw <= 0) {
    stop("stahdo: every observation was downweighted to zero")
  }
  loc <- colSums(M * w) / sw
  Xc <- M - matrix(loc, nrow = n, ncol = p, byrow = TRUE)
  cov <- crossprod(Xc * w, Xc) / sw
  list(
    estimate = loc, location = loc, scatter = cov,
    outlyingness = r, weights = w, cutoff = c,
    n_directions = o$n_directions, n_used = o$n_used,
    exhaustive = o$exhaustive, directions = directions,
    n_downweighted = sum(w < 1.0),
    n = n, p = p,
    method = sprintf("Stahel-Donoho estimator (Maronna & Yohai 1995) with %s directions", directions)
  )
}

morie_stahdo <- function(X, directions = "subsample",
                        n_directions = 500, seed = 1,
                        cutoff = NULL) {
  .stahdo_stahel_donoho(X, directions, n_directions, seed, cutoff)
}
