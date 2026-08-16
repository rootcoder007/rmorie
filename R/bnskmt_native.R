# bnskmt_native.R
# Base R port of conditional moment inequalities in the Kolmogorov-
# Smirnov form (Andrews & Shi 2013, Econometrica 81(2), 609-666).
# Companion to bndsmw: same construction, supremum over g instead of
# integral over Q, GMS critical values (Andrews & Soares 2010).

.bnskmt_EPS <- 1e-12

#' S_function
#'
#' A step of the bnskmt_native implementation. Called by \code{cvm_statistic}, \code{ks_critical_value}, \code{ks_statistic}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param std_moments Coerced to numeric by the body, with \code{as.numeric}.
#' @param form One of \code{"max"}, \code{"sum"}. Defaults to \code{"sum"}.
#' @param n_equality Coerced to integer by the body, with \code{as.integer}. Defaults to \code{0L}.
#' @return A numeric value.
#' @export
S_function <- function(std_moments, form = "sum", n_equality = 0L) {
  v <- as.numeric(std_moments); J <- length(v); ne <- as.integer(n_equality)
  ineq <- v[seq_len(J - ne)]
  # guard: with no equality moments the colon slice DESCENDS through
  # v[J + 1] and poisons the statistic with NA
  eq <- if (ne > 0L) v[(J - ne + 1L):J] else numeric(0)
  neg <- pmin(ineq, 0.0)
  s <- if (form == "sum") sum(neg * neg)
       else if (form == "max") max(c(neg * neg, 0.0))
       else sum(neg * neg)
  s + sum(eq * eq)
}

#' weighted_moments
#'
#' A step of the bnskmt_native implementation. Called by \code{cvm_statistic}, \code{ks_critical_value}, \code{ks_statistic}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param m A matrix; the body checks with \code{is.matrix}.
#' @param g Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{mean}, \code{sd}, \code{n}.
#' @export
weighted_moments <- function(m, g) {
  if (is.matrix(m)) M <- split(m, row(m)) else M <- m
  M <- lapply(M, as.numeric); n <- length(M)
  if (n < 2L) stop("bnskmt: need at least 2 observations")
  gv <- as.numeric(g)
  if (length(gv) != n) stop(sprintf("bnskmt: %d weights for %d observations", length(gv), n))
  if (any(gv < 0.0)) stop("bnskmt: instrument weights must be non-negative")
  J <- length(M[[1L]]); means <- numeric(J); sds <- numeric(J)
  for (j in seq_len(J)) {
    v <- sapply(seq_len(n), function(i) M[[i]][j] * gv[i])
    mu <- mean(v); var <- sum((v - mu) ^ 2) / (n - 1L)
    means[j] <- mu; sds[j] <- sqrt(max(var, 0.0))
  }
  list(mean = means, sd = sds, n = n)
}

#' hypercube_instruments
#'
#' A step of the bnskmt_native implementation. Called by \code{ks_confidence_set}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A matrix; the body checks with \code{is.matrix}.
#' @param n_levels Coerced to integer by the body, with \code{as.integer}. Defaults to \code{3L}.
#' @return A list with \code{instruments}, \code{n_instruments}, \code{n_levels}.
#' @export
hypercube_instruments <- function(X, n_levels = 3L) {
  if (!is.list(X) && !is.matrix(X)) stop("bnskmt: X must be a matrix or list of rows")
  if (is.matrix(X)) Xm <- split(X, row(X)) else Xm <- X
  Xm <- lapply(Xm, as.numeric); n <- length(Xm)
  if (n == 0L) stop("bnskmt: no observations")
  d <- length(Xm[[1L]])
  lo <- sapply(seq_len(d), function(j) min(sapply(Xm, function(r) r[j])))
  hi <- sapply(seq_len(d), function(j) max(sapply(Xm, function(r) r[j])))
  span <- pmax(hi - lo, .bnskmt_EPS)
  G <- list()
  for (lev in seq_len(as.integer(n_levels) - 1L)) {
    cells <- 2 ^ lev
    for (c in 0:(cells ^ d - 1L)) {
      idx <- integer(d); rem <- c
      for (jj in seq_len(d)) { idx[jj] <- rem %/% cells ^ ((d - jj) %% d) %% cells
                               rem <- rem - (rem %/% cells ^ ((d - jj) %% d)) * cells ^ ((d - jj) %% d) }
      g <- numeric(n)
      for (i in seq_len(n)) {
        inside <- TRUE
        for (j in seq_len(d)) {
          pos <- (Xm[[i]][j] - lo[j]) / span[j]
          edge <- if (idx[j] == cells - 1L) 1e-12 else 0.0
          if (!(idx[j] / cells <= pos && pos < (idx[j] + 1L) / cells + edge))
            { inside <- FALSE; break }
        }
        g[i] <- if (inside) 1.0 else 0.0
      }
      if (sum(g) > 0) G[[length(G) + 1L]] <- g
    }
  }
  list(instruments = G, n_instruments = length(G), n_levels = as.integer(n_levels))
}

#' ks_statistic
#'
#' A step of the bnskmt_native implementation. Called by \code{compare_forms}, \code{ks_confidence_set}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param m Passed to \code{weighted_moments}.
#' @param instruments A list; the body reads \code{$instruments} from it.
#' @param form Carried through into a list the body builds. Defaults to \code{"sum"}.
#' @param n_equality Passed to \code{S_function}. Defaults to \code{0L}.
#' @return A list with \code{statistic}, \code{argmax}, \code{per_instrument}, \code{form}, \code{n_instruments}, \code{method}.
#' @export
ks_statistic <- function(m, instruments, form = "sum", n_equality = 0L) {
  G <- if (is.list(instruments) && !is.null(instruments$instruments))
    instruments$instruments else instruments
  if (length(G) == 0L) stop("bnskmt: the instrument class is empty")
  best <- 0.0; arg <- NA_integer_; parts <- numeric(length(G))
  for (a in seq_along(G)) {
    wm <- weighted_moments(m, G[[a]])
    n <- wm$n
    std <- sapply(seq_along(wm$mean),
                  function(j) sqrt(n) * wm$mean[j] / max(wm$sd[j], .bnskmt_EPS))
    s <- S_function(std, form = form, n_equality = n_equality)
    parts[a] <- s
    if (s > best) { best <- s; arg <- a }
  }
  list(statistic = best, argmax = arg, per_instrument = parts,
       form = form, n_instruments = length(G),
       method = "Kolmogorov-Smirnov: supremum of S over G (Andrews & Shi, Sec. 1)")
}

#' ks_critical_value
#'
#' A step of the bnskmt_native implementation. Called by \code{ks_confidence_set}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param m A matrix; the body checks with \code{is.matrix}.
#' @param instruments A list; the body reads \code{$instruments} from it.
#' @param form Passed to \code{S_function}. Defaults to \code{"sum"}.
#' @param n_equality Passed to \code{S_function}. Defaults to \code{0L}.
#' @param level Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.95}.
#' @param reps Coerced to integer by the body, with \code{as.integer}. Defaults to \code{200L}.
#' @param seed Coerced to integer by the body, with \code{as.integer}. Defaults to \code{0L}.
#' @param kappa Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{critical_value}, \code{kappa}, \code{reps}, \code{level}.
#' @export
ks_critical_value <- function(m, instruments, form = "sum",
                              n_equality = 0L, level = 0.95,
                              reps = 200L, seed = 0L, kappa = NULL) {
  if (is.matrix(m)) M <- split(m, row(m)) else M <- m
  M <- lapply(M, as.numeric); n <- length(M)
  G <- if (is.list(instruments) && !is.null(instruments$instruments))
    instruments$instruments else instruments
  kap <- if (is.null(kappa)) sqrt(log(max(n, 3L))) else as.numeric(kappa)
  set.seed(as.integer(seed))
  draws <- numeric(as.integer(reps))
  for (r in seq_along(draws)) {
    idx <- sample.int(n, n, replace = TRUE)
    Mb <- M[idx]
    best <- 0.0
    for (g in G) {
      gb <- g[idx]
      wm  <- weighted_moments(Mb, gb)
      wm0 <- weighted_moments(M,  g)
      std <- sapply(seq_along(wm$mean), function(j) {
        sd <- max(wm0$sd[j], .bnskmt_EPS)
        xi  <- sqrt(n) * wm0$mean[j] / sd
        centred <- sqrt(n) * (wm$mean[j] - wm0$mean[j]) / sd
        centred + if (xi <= kap) 0.0 else 1e6
      })
      s <- S_function(std, form = form, n_equality = n_equality)
      if (s > best) best <- s
    }
    draws[r] <- best
  }
  draws <- sort(draws)
  q <- draws[min(length(draws), max(1L, as.integer(as.numeric(level) * length(draws))))]
  list(critical_value = q, kappa = kap, reps = as.integer(reps),
       level = as.numeric(level))
}

#' ks_confidence_set
#'
#' A step of the bnskmt_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param moment_fn Accepted by the signature and not used anywhere in the body.
#' @param theta_grid See Usage.
#' @param X Passed to \code{hypercube_instruments}.
#' @param form Carried through into a list the body builds. Defaults to \code{"sum"}.
#' @param n_equality Passed to \code{ks_statistic}. Defaults to \code{0L}.
#' @param level Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.95}.
#' @param n_levels Passed to \code{hypercube_instruments}. Defaults to \code{2L}.
#' @param reps Passed to \code{ks_critical_value}. Defaults to \code{100L}.
#' @param seed Passed to \code{ks_critical_value}. Defaults to \code{0L}.
#' @return A list with \code{estimate}, \code{set}, \code{n_in_set}, \code{bounds}, \code{statistics}, \code{form}, \code{level}, \code{n_instruments}, \code{method}.
#' @export
ks_confidence_set <- function(moment_fn, theta_grid, X, form = "sum",
                              n_equality = 0L, level = 0.95,
                              n_levels = 2L, reps = 100L, seed = 0L) {
  inst <- hypercube_instruments(X, n_levels = n_levels)
  keep <- c(); stats <- list()
  for (th in theta_grid) {
    m <- moment_fn(th)
    t <- ks_statistic(m, inst, form = form, n_equality = n_equality)
    cv <- ks_critical_value(m, inst, form = form, n_equality = n_equality,
                            level = level, reps = reps, seed = seed)
    stats[[as.character(th)]] <- c(t$statistic, cv$critical_value)
    if (t$statistic <= cv$critical_value) keep <- c(keep, th)
  }
  list(estimate = keep, set = keep, n_in_set = length(keep),
       bounds = if (length(keep) > 0L) c(min(keep), max(keep)) else NULL,
       statistics = stats, form = form, level = as.numeric(level),
       n_instruments = inst$n_instruments,
       method = "KS test with GMS critical values, inverted over the grid; Andrews & Shi")
}

#' cvm_statistic
#'
#' A step of the bnskmt_native implementation. Called by \code{compare_forms}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param m Passed to \code{weighted_moments}.
#' @param instruments A list; the body reads \code{$instruments} from it.
#' @param form Carried through into a list the body builds. Defaults to \code{"sum"}.
#' @param n_equality Passed to \code{S_function}. Defaults to \code{0L}.
#' @param weights Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{statistic}, \code{per_instrument}, \code{form}, \code{n_instruments}, \code{method}.
#' @export
cvm_statistic <- function(m, instruments, form = "sum", n_equality = 0L,
                          weights = NULL) {
  G <- if (is.list(instruments) && !is.null(instruments$instruments))
    instruments$instruments else instruments
  if (length(G) == 0L) stop("bnskmt: the instrument class is empty")
  q <- if (is.null(weights)) rep(1.0 / length(G), length(G)) else as.numeric(weights)
  if (length(q) != length(G))
    stop(sprintf("bnskmt: %d measure weights for %d instruments", length(q), length(G)))
  if (abs(sum(q) - 1.0) > 1e-6)
    stop(sprintf("bnskmt: the measure Q must sum to 1, got %.6f", sum(q)))
  tot <- 0.0; parts <- numeric(length(G))
  for (a in seq_along(G)) {
    wm <- weighted_moments(m, G[[a]])
    n <- wm$n
    std <- sapply(seq_along(wm$mean),
                  function(j) sqrt(n) * wm$mean[j] / max(wm$sd[j], .bnskmt_EPS))
    s <- S_function(std, form = form, n_equality = n_equality)
    parts[a] <- s; tot <- tot + q[a] * s
  }
  list(statistic = tot, per_instrument = parts, form = form,
       n_instruments = length(G),
       method = "Cramer-von Mises: integral of S over Q (Andrews & Shi, Sec. 1)")
}

#' compare_forms
#'
#' A step of the bnskmt_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param m Passed to \code{cvm_statistic}.
#' @param instruments Passed to \code{cvm_statistic}.
#' @param form Passed to \code{cvm_statistic}. Defaults to \code{"sum"}.
#' @param n_equality Passed to \code{cvm_statistic}. Defaults to \code{0L}.
#' @return A list with \code{cvm}, \code{ks}, \code{ratio_ks_over_cvm}, \code{argmax_instrument}, \code{note}.
#' @export
compare_forms <- function(m, instruments, form = "sum", n_equality = 0L) {
  cv <- cvm_statistic(m, instruments, form = form, n_equality = n_equality)
  ks <- ks_statistic(m, instruments, form = form, n_equality = n_equality)
  list(cvm = cv$statistic, ks = ks$statistic,
       ratio_ks_over_cvm = ks$statistic / max(cv$statistic, .bnskmt_EPS),
       argmax_instrument = ks$argmax,
       note = "KS is driven by the single worst instrument, CvM by the average; a concentrated violation favours KS and a diffuse one favours CvM")
}

#' .bnskmt_cheatsheet
#'
#' A step of the bnskmt_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
.bnskmt_cheatsheet <- function() {
  paste0(
    "bnskmt: conditional moment inequalities, KS form. Same ",
    "construction as bndsmw -- conditional inequality becomes ",
    "E[m g(X)] >= 0 for all non-negative g -- but the family is ",
    "collapsed by a SUPREMUM over g rather than an integral against ",
    "Q. KS therefore reacts to a violation concentrated on ONE ",
    "region of X, where CvM averages it away; a diffuse violation ",
    "reverses that. Adding instruments can only RAISE the supremum, ",
    "so truncation is conservative."
  )
}

kernelmomentbound  <- ks_confidence_set
bound_kernel_moment <- ks_confidence_set

# house entry point: the package exports one morie_<module>
morie_bnskmt <- ks_confidence_set
