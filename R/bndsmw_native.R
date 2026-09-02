# Conditional moment inequalities via instrument functions: the
# CvM statistic. Andrews & Shi (2013) Econometrica 81(2), 609-666.
# A conditional inequality is equivalent to the unconditional family
# E[m(W,theta) g(X)] >= 0 for ALL non-negative g. T_n is the CvM
# integral of S over Q. GMS critical values push moments slack by
# more than kappa_n to +infinity.

.bndsmw_GHC_EPS <- 1e-12
.S_FORMS <- c("sum", "qlr", "max")

#' morie_hypercube_instruments
#'
#' A step of the bndsmw_native implementation. Called by \code{morie_confidence_set}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A matrix; passed to \code{as.matrix}.
#' @param n_levels Coerced to integer by the body, with \code{as.integer}. Defaults to \code{3L}.
#' @return A list with \code{instruments}, \code{n_instruments}, \code{n_levels}, \code{note}.
#' @export
morie_hypercube_instruments <- function(X, n_levels = 3L) {
  Xm <- as.matrix(X)
  storage.mode(Xm) <- "double"
  n <- nrow(Xm)
  if (n == 0L) stop("bndsmw: no observations")
  d <- ncol(Xm)
  lo <- apply(Xm, 2, min)
  hi <- apply(Xm, 2, max)
  span <- pmax(hi - lo, .bndsmw_GHC_EPS)
  G <- list()
  for (lev in seq_len(as.integer(n_levels) - 1L)) {
    cells <- 2 ^ lev
    total <- cells ^ d
    for (c_ in seq_len(total) - 1L) {
      idx <- integer(d)
      rem <- c_
      for (kk in seq_len(d)) { idx[kk] <- rem %% cells
      rem <- rem %/% cells }
      g <- numeric(n)
      for (i in seq_len(n)) {
        inside <- TRUE
        for (j in seq_len(d)) {
          u <- (Xm[i, j] - lo[j]) / span[j]
          hi_ <- if (idx[j] == cells - 1L) 1.0 + 1e-12 else 1.0
          if (!(idx[j] / cells <= u && u < hi_)) { inside <- FALSE
          break }
        }
        g[i] <- if (inside) 1 else 0
      }
      if (sum(g) > 0) G[[length(G) + 1L]] <- g
    }
  }
  list(instruments = G, n_instruments = length(G),
       n_levels = as.integer(n_levels),
       note = "non-negative indicator weights; the conditional inequality is equivalent to the unconditional family holding for ALL of them")
}

#' morie_weighted_moments
#'
#' A step of the bndsmw_native implementation. Called by \code{morie_cvm_statistic}, \code{morie_gms_critical_value}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param m A matrix; passed to \code{as.matrix}.
#' @param g Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{mean}, \code{sd}, \code{n}.
#' @export
morie_weighted_moments <- function(m, g) {
  M <- as.matrix(m)
  storage.mode(M) <- "double"
  n <- nrow(M)
  if (n < 2L) stop("bndsmw: need at least 2 observations")
  gv <- as.numeric(g)
  if (length(gv) != n)
    stop("bndsmw: ", length(gv), " weights for ", n, " observations")
  if (any(gv < 0))
    stop("bndsmw: instrument weights must be non-negative")
  J <- ncol(M)
  means <- numeric(J)
  sds <- numeric(J)
  for (j in seq_len(J)) {
    v <- M[, j] * gv
    mu <- mean(v)
    var <- sum((v - mu)^2) / (n - 1L)
    means[j] <- mu
    sds[j] <- sqrt(max(var, 0))
  }
  list(mean = means, sd = sds, n = n)
}

#' morie_S_function
#'
#' A step of the bndsmw_native implementation. Called by \code{morie_cvm_statistic}, \code{morie_gms_critical_value}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param std_moments Coerced to numeric by the body, with \code{as.numeric}.
#' @param form One of \code{"max"}, \code{"sum"}. Defaults to \code{"sum"}.
#' @param n_equality Coerced to integer by the body, with \code{as.integer}. Defaults to \code{0L}.
#' @return A numeric value.
#' @export
morie_S_function <- function(std_moments, form = "sum", n_equality = 0L) {
  if (!(form %in% .S_FORMS))
    stop("bndsmw: form must be one of ", paste(.S_FORMS, collapse = ", "))
  v <- as.numeric(std_moments)
  J <- length(v)
  ineq <- v[seq_len(J - as.integer(n_equality))]
  eq <- if (J - as.integer(n_equality) > 0L) v[(J - as.integer(n_equality) + 1L):J] else numeric(0)
  neg <- pmin(ineq, 0)
  s <- if (form == "sum") sum(neg^2)
       else if (form == "max") max(c(neg^2, 0))
       else sum(neg^2)
  s + sum(eq^2)
}

#' morie_cvm_statistic
#'
#' A step of the bndsmw_native implementation. Called by \code{morie_confidence_set}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param m Passed to \code{morie_weighted_moments}.
#' @param instruments A list; the body reads \code{$instruments} from it.
#' @param form Passed to \code{morie_S_function}. Defaults to \code{"sum"}.
#' @param n_equality Passed to \code{morie_S_function}. Defaults to \code{0L}.
#' @param weights Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{statistic}, \code{per_instrument}, \code{form}, \code{n_instruments}, \code{method}.
#' @export
morie_cvm_statistic <- function(m, instruments, form = "sum",
                                n_equality = 0L, weights = NULL) {
  G <- if (is.list(instruments) && !is.null(instruments$instruments))
    instruments$instruments else instruments
  if (length(G) == 0L) stop("bndsmw: the instrument class is empty")
  q <- if (is.null(weights)) rep(1 / length(G), length(G))
       else as.numeric(weights)
  if (length(q) != length(G))
    stop("bndsmw: ", length(q), " measure weights for ", length(G), " instruments")
  if (abs(sum(q) - 1) > 1e-6)
    stop("bndsmw: the measure Q must sum to 1, got ", sum(q))
  tot <- 0
  parts <- numeric(length(G))
  for (a in seq_along(G)) {
    g <- G[[a]]
    wm <- morie_weighted_moments(m, g)
    n <- wm$n
    std <- sqrt(n) * wm$mean / pmax(wm$sd, .bndsmw_GHC_EPS)
    s <- morie_S_function(std, form = form, n_equality = n_equality)
    parts[a] <- s
    tot <- tot + q[a] * s
  }
  list(statistic = tot, per_instrument = parts, form = form,
       n_instruments = length(G),
       method = "Cramer-von Mises: integral of S over Q (Andrews & Shi, Sec. 1)")
}

#' morie_gms_critical_value
#'
#' A step of the bndsmw_native implementation. Called by \code{morie_confidence_set}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param m A matrix; passed to \code{as.matrix}.
#' @param instruments A list; the body reads \code{$instruments} from it.
#' @param form Passed to \code{morie_S_function}. Defaults to \code{"sum"}.
#' @param n_equality Passed to \code{morie_S_function}. Defaults to \code{0L}.
#' @param level Numeric; combined arithmetically in the body. Defaults to \code{0.95}.
#' @param reps Coerced to integer by the body, with \code{as.integer}. Defaults to \code{200L}.
#' @param seed Passed to \code{.ghc_rng}. Defaults to \code{0}.
#' @param kappa Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{critical_value}, \code{kappa}, \code{reps}, \code{level}, \code{method}.
#' @export
morie_gms_critical_value <- function(m, instruments, form = "sum",
                                     n_equality = 0L, level = 0.95,
                                     reps = 200L, seed = 0, kappa = NULL) {
  M <- as.matrix(m)
  storage.mode(M) <- "double"
  n <- nrow(M)
  G <- if (is.list(instruments) && !is.null(instruments$instruments))
    instruments$instruments else instruments
  kap <- if (is.null(kappa)) sqrt(log(max(n, 3))) else as.numeric(kappa)
  e <- .ghc_rng(seed)
  draws <- numeric(as.integer(reps))
  n_int <- as.integer(reps)
  for (b in seq_len(n_int)) {
    idx <- (.ghc_unif(e, n) * n) %/% 1
    idx <- idx %% n + 1L
    Mb <- M[idx, , drop = FALSE]
    tot <- 0
    for (g in G) {
      gb <- g[idx]
      wm <- morie_weighted_moments(Mb, gb)
      wm0 <- morie_weighted_moments(M, g)
      std <- numeric(length(wm$mean))
      for (j in seq_along(wm$mean)) {
        sd <- max(wm0$sd[j], .bndsmw_GHC_EPS)
        xi <- sqrt(n) * wm0$mean[j] / sd
        centred <- sqrt(n) * (wm$mean[j] - wm0$mean[j]) / sd
        std[j] <- centred + (if (xi <= kap) 0 else 1e6)
      }
      tot <- tot + morie_S_function(std, form = form,
                                    n_equality = n_equality) / length(G)
    }
    draws[b] <- tot
  }
  draws <- sort(draws)
  qv <- draws[min(length(draws), as.integer(level * length(draws)))]
  list(critical_value = qv, kappa = kap, reps = n_int,
       level = as.numeric(level),
       method = "GMS bootstrap (Andrews & Soares 2010, extended to infinitely many moments)")
}

#' morie_confidence_set
#'
#' A step of the bndsmw_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param moment_fn Accepted by the signature and not used anywhere in the body.
#' @param theta_grid See Usage.
#' @param X Passed to \code{morie_hypercube_instruments}.
#' @param form Passed to \code{morie_cvm_statistic}. Defaults to \code{"sum"}.
#' @param n_equality Passed to \code{morie_cvm_statistic}. Defaults to \code{0L}.
#' @param level Passed to \code{morie_gms_critical_value}. Defaults to \code{0.95}.
#' @param n_levels Passed to \code{morie_hypercube_instruments}. Defaults to \code{2L}.
#' @param reps Passed to \code{morie_gms_critical_value}. Defaults to \code{100L}.
#' @param seed Passed to \code{morie_gms_critical_value}. Defaults to \code{0}.
#' @return A list with \code{estimate}, \code{set}, \code{n_in_set}, \code{bounds}, \code{statistics}, \code{form}, \code{level}, \code{n_instruments}, \code{method}.
#' @export
morie_confidence_set <- function(moment_fn, theta_grid, X, form = "sum",
                                 n_equality = 0L, level = 0.95,
                                 n_levels = 2L, reps = 100L, seed = 0) {
  inst <- morie_hypercube_instruments(X, n_levels = n_levels)
  keep <- list()
  stats <- list()
  for (th in theta_grid) {
    m <- moment_fn(th)
    t <- morie_cvm_statistic(m, inst, form = form, n_equality = n_equality)
    c <- morie_gms_critical_value(m, inst, form = form,
                                  n_equality = n_equality, level = level,
                                  reps = reps, seed = seed)
    stats[[as.character(th)]] <- c(t$statistic, c$critical_value)
    if (t$statistic <= c$critical_value)
      keep[[length(keep) + 1L]] <- th
  }
  list(estimate = keep, set = keep, n_in_set = length(keep),
       bounds = if (length(keep) > 0L) c(min(unlist(keep)), max(unlist(keep))) else NULL,
       statistics = stats, form = form, level = as.numeric(level),
       n_instruments = inst$n_instruments,
       method = "CvM test with GMS critical values, inverted over the grid; Andrews & Shi")
}

morie_simulatedweightbound <- morie_confidence_set
morie_bound_simul_weights <- morie_confidence_set

# house entry point: the package exports one morie_<module>
morie_bndsmw <- morie_confidence_set
