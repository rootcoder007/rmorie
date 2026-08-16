# Polya tree prior: a tailfree process on densities.
# Sources: Muller, P. & Quintana, F. A. (2004) "Nonparametric Bayesian
# Data Analysis", Statistical Science 19(1), 95-110, Sec. 2.3
# (Polya trees as a generalisation of the DP; the c m^2 rule for
# absolute continuity; the partition-as-model view); Lavine, M.
# (1992, 1994), Annals of Statistics, the proposal and parameter
# development; Ferguson, T. S. (1974), Annals of Statistics, the
# tailfree framework of which the Polya tree and the DP are
# instances.
#
# Native implementation mirroring Python morie.fn.poltrx exactly: the
# same level-parameter rules, the same Gamma-Beta sampling, the same
# accumulated-product set probability, the same piecewise-constant
# density, and the same error conditions. Random numbers come from
# the shared .ghc_rng / .ghc_unif helpers so both arms produce the
# same stream.

.POLTRX_RULES <- c("m_squared", "constant", "linear")
.POLTRX_EPS <- 1e-12

#' Polya tree level parameter
#'
#' Computes \code{alpha_eps} at level \code{m} under a stated rule.
#' \code{"m_squared"} (\code{c m^2}) is the choice that buys absolute
#' continuity of the limit; \code{"constant"} reproduces the
#' DP-like, atomic behaviour; \code{"linear"} sits between them.
#'
#' @param level Level index \code{m}, an integer \code{>= 1}.
#' @param c Positive scaling constant.
#' @param rule One of \code{"m_squared"}, \code{"constant"},
#'   \code{"linear"}.
#' @return A list with \code{alpha}, \code{level} and \code{rule}.
#' @references Muller, P. & Quintana, F. A. (2004). Nonparametric
#'   Bayesian Data Analysis. Statistical Science, 19(1), 95-110.
#' @export
poltrx_level_parameters <- function(level, c = 1.0, rule = "m_squared") {
  m <- as.integer(level)
  if (m < 1L) stop("poltrx: levels are numbered from 1")
  cc <- as.numeric(c)
  if (cc <= 0) stop("poltrx: c must be positive")
  rule <- as.character(rule)
  a <- if (rule == "m_squared") {
    cc * m * m
  } else if (rule == "constant") {
    cc
  } else if (rule == "linear") {
    cc * m
  } else {
    stop(sprintf("poltrx: rule must be one of %s, got %s",
                 paste(.POLTRX_RULES, collapse = ", "),
                 paste0("'", rule, "'")))
  }
  list(alpha = a, level = m, rule = rule)
}

#' Continuity regime implied by a parameter rule
#'
#' Names the consequence of a parameter rule for the realised draw.
#'
#' @param rule One of \code{"m_squared"}, \code{"constant"},
#'   \code{"linear"}.
#' @return A list with \code{rule}, \code{draws} and \code{reason}.
#' @references Muller, P. & Quintana, F. A. (2004). Sec. 2.3.
#' @export
poltrx_continuity_regime <- function(rule) {
  rule <- as.character(rule)
  if (!(rule %in% .POLTRX_RULES))
    stop(sprintf("poltrx: rule must be one of %s, got %s",
                 paste(.POLTRX_RULES, collapse = ", "),
                 paste0("'", rule, "'")))
  tab <- list(
    m_squared = list(draws = "absolutely continuous",
                     reason = paste("branch probabilities concentrate",
                                    "near 1/2 fast enough that the",
                                    "limit has a density")),
    constant  = list(draws = "discrete, DP-like",
                     reason = paste("the DP is the special case; draws",
                                    "are atomic")),
    linear    = list(draws = "borderline",
                     reason = paste("between the two; growth is not",
                                    "fast enough to guarantee a density"))
  )
  entry <- tab[[rule]]
  list(rule = rule, draws = entry$draws, reason = entry$reason)
}

#' Partition index of a point
#'
#' Returns the binary string, which IS the address in the tree.
#'
#' @param x Point at which to evaluate.
#' @param level Level \code{m}, integer \code{>= 1}.
#' @param lo Lower bound of the partitioned interval.
#' @param hi Upper bound of the partitioned interval.
#' @return A list with \code{epsilon}, \code{interval} and
#'   \code{level}.
#' @references Muller, P. & Quintana, F. A. (2004). Sec. 2.3.
#' @export
poltrx_partition_index <- function(x, level, lo = 0.0, hi = 1.0) {
  m <- as.integer(level)
  a <- as.numeric(lo); b <- as.numeric(hi)
  v <- as.numeric(x)
  if (!(a <= v && v <= b))
    stop(sprintf("poltrx: x = %s lies outside the partitioned interval [%s, %s]",
                 format(v), format(a), format(b)))
  bits <- integer(m)
  for (i in seq_len(m)) {
    mid <- 0.5 * (a + b)
    if (v < mid) { bits[i] <- 0L; b <- mid } else { bits[i] <- 1L; a <- mid }
  }
  list(epsilon = bits, interval = c(a, b), level = m)
}

#' Draw a truncated Polya tree
#'
#' Draws the branch probabilities \code{Y_eps ~ Beta(a, a)} down to
#' a stated level by two Gamma draws per node. Truncated and honest
#' about it: the partition depth is part of the model.
#'
#' @param levels Truncation level \code{M}, integer \code{>= 1}.
#' @param c Positive scaling constant.
#' @param rule One of \code{"m_squared"}, \code{"constant"},
#'   \code{"linear"}.
#' @param rng Optional generator environment; if \code{NULL} a fresh
#'   one is built from \code{seed}.
#' @param seed Seed for the generator shared with the Python arm.
#' @return A list with \code{Y}, \code{levels}, \code{rule}, \code{c},
#'   \code{n_nodes} and \code{note}.
#' @references Muller, P. & Quintana, F. A. (2004). Sec. 2.3;
#'   Lavine, M. (1992, 1994).
#' @export
poltrx_finite_tree <- function(levels, c = 1.0, rule = "m_squared",
                               rng = NULL, seed = 0) {
  M <- as.integer(levels)
  if (M < 1L) stop("poltrx: at least one level is needed")
  e <- if (!is.null(rng)) rng else .ghc_rng(seed)
  Y <- vector("list", 2^M - 1L + 1L)  # placeholder; will index by name
  # Build keys in the same order as the Python arm: for each m in
  # 1..M, idx in 0..2^(m-1)-1, eps = zfill(bin(idx), m-1) when m>1.
  keys <- character(0)
  for (m in seq_len(M)) {
    a <- poltrx_level_parameters(m, c, rule)$alpha
    n_at_m <- 2L ^ (m - 1L)
    for (idx in seq_len(n_at_m) - 1L) {
      eps <- if (m == 1L) integer(0) else {
        bits <- intToBits(idx)[seq_len(m - 1L)]
        as.integer(bits)
      }
      k <- paste0("(", paste(eps, collapse = ","), ")")
      keys <- c(keys, k)
      g0 <- poltrx_gamma(e, a)
      g1 <- poltrx_gamma(e, a)
      Y[[k]] <- if ((g0 + g1) > .POLTRX_EPS) g0 / (g0 + g1) else 0.5
    }
  }
  Y <- Y[keys]
  names(Y) <- keys
  list(Y = Y, levels = M, rule = as.character(rule), c = as.numeric(c),
       n_nodes = length(Y),
       note = sprintf("truncated at level %d; the partition depth is part of the model, not an approximation to hide",
                      M))
}

# Marsaglia-Tsang Gamma sampler, shape >= 1, matching the Python arm.
# Strict: the SAME uniform draws in the SAME order as the Python
# implementation, so both arms agree on every variate.
#' Marsaglia-Tsang Gamma sampler, shape >= 1, matching the Python arm
#'
#' Strict: the SAME uniform draws in the SAME order as the Python
#' implementation, so both arms agree on every variate.
#'
#' @param e Passed to \code{.ghc_unif}.
#' @param shape Numeric; combined arithmetically in the body.
#' @return The value of \code{repeat}.
#' @export
poltrx_gamma <- function(e, shape) {
  if (shape < 1) {
    # Python: u = max(r.uniform(), 1e-15); gamma(shape+1) * u**(1/shape)
    u <- max(.ghc_unif(e, 1L), 1e-15)
    return(poltrx_gamma(e, shape + 1) * u^(1 / shape))
  }
  d <- shape - 1/3
  cc <- 1 / sqrt(9 * d)
  repeat {
    u1raw <- .ghc_unif(e, 1L)
    u2raw <- .ghc_unif(e, 1L)
    u1 <- min(max(u1raw, 1e-12), 1 - 1e-12)
    u2 <- min(max(u2raw, 1e-12), 1 - 1e-12)
    z <- sqrt(-2 * log(u1)) * cos(2 * pi * u2)
    v <- (1 + cc * z)^3
    if (v <= 0) next
    u <- max(.ghc_unif(e, 1L), 1e-15)
    if (log(u) < (0.5 * z * z + d - d * v + d * log(v)))
      return(d * v)
  }
}

# Convert a list key of the form "(0,1,1)" back to an integer vector.
#' Convert a list key of the form "(0,1,1)" back to an integer vector
#'
#' A step of the poltrx_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param key Compared against \code{"()"}.
#' @return The value of \code{as.integer}.
#' @export
poltrx_eps_from_key <- function(key) {
  if (key == "()") return(integer(0))
  as.integer(strsplit(sub("^\\(|\\)$", "", key), ",")[[1]])
}

#' Set probability in a finite Polya tree
#'
#' Accumulated product of branch probabilities down to
#' \code{B_epsilon}.
#'
#' @param epsilon Binary string, an integer vector of 0s and 1s.
#' @param tree A tree as returned by \code{poltrx_finite_tree}.
#' @return A list with \code{probability}, \code{epsilon} and
#'   \code{level}.
#' @references Muller, P. & Quintana, F. A. (2004). Sec. 2.3.
#' @export
poltrx_set_probability <- function(epsilon, tree) {
  eps <- as.integer(epsilon)
  if (length(eps) > tree$levels)
    stop(sprintf("poltrx: the tree was truncated at level %d, so it says nothing about level %d",
                 tree$levels, length(eps)))
  p <- 1
  for (m in seq_along(eps)) {
    parent <- eps[seq_len(m - 1L)]
    pk <- paste0("(", paste(parent, collapse = ","), ")")
    y <- tree$Y[[pk]]
    p <- p * (if (eps[m] == 0L) y else 1 - y)
  }
  list(probability = p, epsilon = eps, level = length(eps))
}

#' Piecewise-constant density implied by a finite Polya tree
#'
#' The probabilities at a level sum to 1 by construction, whatever
#' the branch draws were.
#'
#' @param tree A tree as returned by \code{poltrx_finite_tree}.
#' @param level Optional override of the level; defaults to the
#'   tree's truncation level.
#' @param lo Lower bound of the display interval.
#' @param hi Upper bound of the display interval.
#' @return A list with \code{estimate}, \code{density},
#'   \code{probabilities}, \code{edges}, \code{level}, \code{total},
#'   \code{method} and \code{note}.
#' @references Muller, P. & Quintana, F. A. (2004). Sec. 2.3, after
#'   Lavine (1992, 1994).
#' @export
poltrx_tree_density <- function(tree, level = NULL, lo = 0.0, hi = 1.0) {
  M <- if (is.null(level)) as.integer(tree$levels) else as.integer(level)
  n <- 2L ^ M
  a <- as.numeric(lo); b <- as.numeric(hi)
  width <- (b - a) / n
  probs <- numeric(n)
  dens   <- numeric(n)
  edges  <- vector("list", n)
  for (idx in seq_len(n) - 1L) {
    bits <- intToBits(idx)[seq_len(M)]
    eps <- as.integer(bits)
    pk <- paste0("(", paste(eps, collapse = ","), ")")
    p <- poltrx_set_probability(eps, tree)$probability
    probs[idx + 1L] <- p
    dens[idx + 1L]   <- p / width
    edges[[idx + 1L]] <- c(a + idx * width, a + (idx + 1L) * width)
  }
  list(estimate = dens, density = dens, probabilities = probs,
       edges = edges, level = M, total = sum(probs),
       method = "Polya tree; Muller & Quintana (2004) Sec. 2.3, after Lavine (1992, 1994)",
       note = "the probabilities at a level sum to 1 by construction, whatever the branch draws were")
}

#' Polya tree (morie.fn.poltrx) -- main entry point
#'
#' A thin wrapper that mirrors the Python module's public surface.
#' The main entry point builds a finite tree, the level parameter
#' rule and the implied density in one call, and reports the same
#' payload the Python arm returns.
#'
#' @param levels Truncation level \code{M}, integer \code{>= 1}.
#' @param c Positive scaling constant.
#' @param rule One of \code{"m_squared"}, \code{"constant"},
#'   \code{"linear"}.
#' @param seed Seed for the generator shared with the Python arm.
#' @return A named list with \code{level_parameters},
#'   \code{continuity_regime}, \code{tree}, \code{density} and
#'   \code{method}.
#' @references Muller, P. & Quintana, F. A. (2004). Nonparametric
#'   Bayesian Data Analysis. Statistical Science, 19(1), 95-110.
#' @export
morie_poltrx <- function(levels, c = 1.0, rule = "m_squared", seed = 0) {
  lp <- poltrx_level_parameters(levels, c, rule)
  cr <- poltrx_continuity_regime(lp$rule)
  tr <- poltrx_finite_tree(levels, c, rule, seed = as.integer(seed))
  dn <- poltrx_tree_density(tr)
  list(level_parameters = lp,
       continuity_regime = cr,
       tree = tr,
       density = dn,
       method = "Polya tree; Muller & Quintana (2004) Sec. 2.3, after Lavine (1992, 1994)")
}
