# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Partial identification: bounds, not points.
#
# R mirror of morie.fn.{bndest,bndvar,bndnln,bndcvx,bndpl}.
#
# Partial identification is about what the data alone can say, so
# most of what these functions return is governed by identities
# rather than approximations: the Manski bound width is EXACTLY the
# support width times the missing share, the no-assumption ATE
# interval always contains zero, the Imbens-Manski critical value
# interpolates between the one- and two-sided normal quantiles and
# attains both limits, and the CHT criterion is exactly zero on the
# identified set. The tests assert the identities.

# ---------------------------------------------------------------
# A small dense two-phase simplex with Bland's rule, written here
# because base R has no linear programming and this package does not
# take dependencies for what eighty lines can do. Solves
#   min c'x  s.t.  A_ub x <= b_ub,  A_eq x = b_eq,  l <= x <= u.
# Bland's rule (always the lowest-index eligible column and row)
# trades speed for a cycling-freedom guarantee, which is the right
# trade at the problem sizes identification bounds produce.
#' .bnd_simplex
#'
#' A small dense two-phase simplex with Bland\'s rule, written here
#' because base R has no linear programming and this package does not
#' take dependencies for what eighty lines can do. Solves min c\'x s.t.
#' A_ub x <= b_ub, A_eq x = b_eq, l <= x <= u. Bland\'s rule (always the
#' lowest-index eligible column and row) trades speed for a
#' cycling-freedom guarantee, which is the right trade at the problem
#' sizes identification bounds produce.
#'
#' @param cv A vector; its length is taken.
#' @param A_ub Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.
#' @param b_ub Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @param A_eq Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.
#' @param b_eq Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @param lower See Usage.
#' @param upper A vector; indexed elementwise.
#' @return A list with \code{status}, \code{x}, \code{fun}.
#' @export
.bnd_simplex <- function(cv, A_ub = NULL, b_ub = NULL, A_eq = NULL,
                         b_eq = NULL, lower, upper) {
  k <- length(cv)
  # shift to x = l + z, z >= 0; finite uppers become inequality rows
  shift <- ifelse(is.finite(lower), lower, 0)
  if (any(!is.finite(lower))) {
    stop("free (unbounded-below) variables are not supported here.",
      call. = FALSE
    )
  }
  Au <- if (is.null(A_ub)) matrix(0, 0L, k) else as.matrix(A_ub)
  bu <- if (is.null(b_ub)) {
    numeric(0)
  } else {
    as.numeric(b_ub) -
      as.numeric(Au %*% shift)
  }
  Ae <- if (is.null(A_eq)) matrix(0, 0L, k) else as.matrix(A_eq)
  be <- if (is.null(b_eq)) {
    numeric(0)
  } else {
    as.numeric(b_eq) -
      as.numeric(Ae %*% shift)
  }
  ub_rows <- which(is.finite(upper))
  for (i in ub_rows) {
    r <- numeric(k)
    r[i] <- 1
    Au <- rbind(Au, r)
    bu <- c(bu, upper[i] - shift[i])
  }
  m_u <- nrow(Au)
  m_e <- nrow(Ae)
  # standard form: [A_ub | I; A_eq | 0] z+ = b, all vars >= 0
  n_s <- m_u
  A <- rbind(
    cbind(Au, if (n_s) diag(n_s) else matrix(0, 0L, 0L)),
    cbind(Ae, matrix(0, m_e, n_s))
  )
  b <- c(bu, be)
  neg <- b < 0
  A[neg, ] <- -A[neg, , drop = FALSE]
  b[neg] <- -b[neg]
  m <- nrow(A)
  n <- ncol(A)
  if (m == 0L) {
    # no rows at all: the problem is separable over the box, and each
    # coordinate goes to whichever end its cost points at
    z <- ifelse(cv >= 0, 0, upper - shift)
    if (any(cv < 0 & !is.finite(upper))) {
      return(list(status = 3L))
    }
    return(list(status = 0L, x = z + shift, fun = sum(cv * (z + shift))))
  }
  # phase 1: artificials
  A1 <- cbind(A, diag(m))
  basis <- n + seq_len(m)
  obj1 <- c(numeric(n), rep(1, m))
  run <- function(A, b, basis, obj, ban = integer(0)) {
    m <- nrow(A)
    n <- ncol(A)
    B <- A[, basis, drop = FALSE]
    for (it in seq_len(5000L)) {
      xb <- solve(B, b)
      y <- solve(t(B), obj[basis])
      red <- obj - as.numeric(t(A) %*% y)
      red[basis] <- 0
      red[ban] <- Inf
      ent <- which(red < -1e-9)
      if (!length(ent)) {
        return(list(basis = basis, x = xb, status = 0L))
      }
      j <- min(ent) # Bland
      d <- solve(B, A[, j])
      pos <- which(d > 1e-9)
      if (!length(pos)) {
        return(list(status = 3L))
      } # unbounded
      ratio <- xb[pos] / d[pos]
      leave_candidates <- pos[abs(ratio - min(ratio)) < 1e-12]
      i <- leave_candidates[which.min(basis[leave_candidates])] # Bland
      basis[i] <- j
      B <- A[, basis, drop = FALSE]
    }
    list(status = 4L) # iteration cap: numerical trouble
  }
  r1 <- run(A1, b, basis, obj1)
  if (r1$status != 0L || sum(obj1[r1$basis] * r1$x) > 1e-7) {
    return(list(status = 2L)) # infeasible
  }
  # drive any artificial still basic out (degenerate); ban artificials
  basis <- r1$basis
  obj <- c(cv, numeric(n_s), rep(0, m))
  r2 <- run(A1, b, basis, obj, ban = n + seq_len(m))
  if (r2$status != 0L) {
    return(list(status = r2$status))
  }
  x <- numeric(n + m)
  x[r2$basis] <- r2$x
  z <- x[seq_len(k)]
  list(
    status = 0L, x = z + shift,
    fun = sum(cv * (z + shift)) - 0
  ) # objective on the original scale
}


#' Manski worst-case bounds on a partially observed mean
#'
#' With `P(obs)` the share of outcomes seen and `\[K0, K1\]` the
#' outcome's logical support, `E\[Y\]` lies in
#' `\[E\[Y|obs\] P(obs) + K0 (1-P(obs)), E\[Y|obs\] P(obs) + K1 (1-P(obs))\]`.
#' The width is EXACTLY `(K1 - K0)(1 - P(obs))`. Nothing about the
#' missing data is assumed -- that is the point, and the reason the
#' bounds are wide: they are what the data alone say before any
#' missing-at-random or selection assumption is spent.
#'
#' With `treatment` supplied the same construction bounds each
#' potential-outcome mean and differences them. The no-assumption ATE
#' interval always has width exactly `K1 - K0` and therefore ALWAYS
#' contains zero: worst-case bounds never sign an effect on their
#' own, and bounds that do have smuggled in an assumption.
#'
#' @param y outcome; unobserved entries are ignored.
#' @param observed logical, whether the outcome was seen.
#' @param support the logical range `c(K0, K1)` -- the one assumption.
#' @param treatment optional binary treatment; switches to ATE bounds.
#' @return list: lower, upper, width, p_observed, identified, or the
#'   ate_* keys plus contains_zero, n, method.
#' @references Manski (1989), *J. Human Resources* 24:343-360;
#'   Manski (1990), *AER P&P* 80:319-323; Manski and Tamer (2002),
#'   *Econometrica* 70:519-546.
#' @examples
#' y <- stats::runif(100)
#' morie_bnd_manski(y, y > 0.2, c(0, 1))$width
#' @export
morie_bnd_manski <- function(y, observed, support, treatment = NULL) {
  yv <- as.numeric(y)
  k0 <- as.numeric(support[1L])
  k1 <- as.numeric(support[2L])
  if (!(k0 < k1)) {
    stop(sprintf("the support must satisfy K0 < K1, got [%g, %g].", k0, k1),
      call. = FALSE
    )
  }
  n <- length(yv)
  one_mean <- function(seen) {
    p <- mean(seen)
    m <- 0
    if (p > 0) {
      ys <- yv[seen]
      if (any(ys < k0 - 1e-12) || any(ys > k1 + 1e-12)) {
        stop(paste(
          "an observed outcome lies outside the declared support;",
          "the support is the one assumption here, so violating it",
          "voids the bounds."
        ), call. = FALSE)
      }
      m <- mean(ys)
    }
    c(m * p + k0 * (1 - p), m * p + k1 * (1 - p), p)
  }
  if (is.null(treatment)) {
    obs <- as.logical(observed)
    if (length(obs) != n) {
      stop(sprintf("observed has %d entries for %d.", length(obs), n),
        call. = FALSE
      )
    }
    r <- one_mean(obs)
    return(list(
      lower = r[1L], upper = r[2L], width = r[2L] - r[1L],
      p_observed = r[3L], identified = r[3L] == 1,
      width_identity = "(K1 - K0)(1 - P(obs)) exactly",
      n = n,
      method = "Manski worst-case bounds on a partially observed mean"
    ))
  }
  Tv <- as.numeric(treatment)
  if (length(Tv) != n) {
    stop(sprintf("treatment has %d entries for %d.", length(Tv), n),
      call. = FALSE
    )
  }
  if (!all(Tv %in% c(0, 1))) {
    stop("treatment must be binary 0/1.", call. = FALSE)
  }
  r1 <- one_mean(Tv == 1)
  r0 <- one_mean(Tv == 0)
  ate_lo <- r1[1L] - r0[2L]
  ate_hi <- r1[2L] - r0[1L]
  list(
    ate_lower = ate_lo, ate_upper = ate_hi, ate_width = ate_hi - ate_lo,
    y1_bounds = c(r1[1L], r1[2L]), y0_bounds = c(r0[1L], r0[2L]),
    p_treated = r1[3L], contains_zero = ate_lo <= 0 && 0 <= ate_hi,
    width_identity = paste(
      "the ATE bounds always have width exactly",
      "K1 - K0, so they always contain zero"
    ),
    identified = FALSE, n = n,
    method = "Manski (1990) worst-case bounds on the average treatment effect"
  )
}


#' Imbens-Manski confidence interval for a partially identified parameter
#'
#' `\[lower - c s_l/sqrt(n), upper + c s_u/sqrt(n)\]` with `c` solving
#' `pnorm(c + sqrt(n) delta / max(s_l, s_u)) - pnorm(-c) = 1 - alpha`.
#'
#' The construction exists because covering the identified SET and
#' covering the true PARAMETER are different targets. The parameter
#' can only be missed at one end at a time, so a wide identified set
#' needs only the one-sided `z`; as the set collapses to a point the
#' problem becomes point identification and the two-sided `z` is what
#' comes out. `c` interpolates and attains both limits exactly.
#' Stoye (2009) showed the interpolation needs `delta`'s own sampling
#' error negligible or the bound superefficient; the output says so.
#'
#' @param lower_hat,upper_hat estimated bounds.
#' @param se_lower,se_upper their standard deviations on the
#'   `sqrt(n)` scale, i.e. `sqrt(n) * se(bound)`.
#' @param n sample size.
#' @param alpha miss probability.
#' @return list: ci, c, z_one_sided, z_two_sided, delta, covers,
#'   stoye_caveat, n, method.
#' @references Imbens and Manski (2004), *Econometrica* 72:1845-1857,
#'   Eq. (6); Stoye (2009), *Econometrica* 77:1299-1315.
#' @examples
#' morie_bnd_imbens_manski(0.2, 0.8, 1, 1, 400)$c
#' @export
morie_bnd_imbens_manski <- function(lower_hat, upper_hat, se_lower,
                                    se_upper, n, alpha = 0.05) {
  tl <- as.numeric(lower_hat)
  tu <- as.numeric(upper_hat)
  sl <- as.numeric(se_lower)
  su <- as.numeric(se_upper)
  nn <- as.integer(n)
  a <- as.numeric(alpha)
  if (tu < tl) {
    stop(sprintf(
      "upper_hat must be at least lower_hat, got [%g, %g].",
      tl, tu
    ), call. = FALSE)
  }
  if (sl <= 0 || su <= 0) {
    stop("both standard deviations must be positive.", call. = FALSE)
  }
  if (is.na(nn) || nn < 2L) stop("n must be at least 2.", call. = FALSE)
  if (a <= 0 || a >= 1) {
    stop(sprintf("alpha must lie in (0, 1), got %g.", a), call. = FALSE)
  }
  delta <- tu - tl
  shift <- sqrt(nn) * delta / max(sl, su)
  z1 <- stats::qnorm(1 - a)
  z2 <- stats::qnorm(1 - a / 2)
  gap <- function(cc) stats::pnorm(cc + shift) - stats::pnorm(-cc) - (1 - a)
  cval <- if (gap(z1) >= 0) {
    z1
  } else if (gap(z2) <= 0) {
    z2
  } else {
    stats::uniroot(gap, c(z1, z2), tol = 1e-12)$root
  }
  list(
    ci = c(tl - cval * sl / sqrt(nn), tu + cval * su / sqrt(nn)),
    c = cval, z_one_sided = z1, z_two_sided = z2, delta = delta,
    covers = paste(
      "the TRUE PARAMETER at 1 - alpha; a set-covering",
      "interval would use the two-sided z throughout"
    ),
    stoye_caveat = paste(
      "the interpolation presumes delta's own sampling",
      "error is negligible or the bound superefficient",
      "(Stoye 2009)"
    ),
    n = nn,
    method = "Imbens-Manski (2004) Eq. (6) confidence interval"
  )
}


#' Moment-inequality criterion and confidence set
#'
#' Chernozhukov, Hong and Tamer (2007). The sample criterion
#' `Q_n(theta) = sum_j \[sqrt(n) gbar_j / sd_j\]_+^2` counts only
#' VIOLATED inequalities, so deep inside the identified set -- where
#' every sample moment is negative -- it is EXACTLY zero. The
#' confidence region is the level set of `Q_n` under a multiplier
#' bootstrap of the RECENTRED moments: the null sits at the boundary
#' of the binding inequalities, not at the sample slack, and skipping
#' the recentring makes the cutoff grow with slack.
#'
#' @param data observations, rows passed to `g`.
#' @param g `function(data, theta)` returning an (n, J) matrix
#'   oriented so the model says `E\[g_j\] <= 0`.
#' @param theta_grid candidate parameter values.
#' @param alpha miss probability.
#' @param B multiplier-bootstrap draws.
#' @param seed bootstrap seed.
#' @return list: theta_grid, criterion, critical_value,
#'   in_confidence_set, set_estimate, confidence_set_bounds, n, J,
#'   method.
#' @references Chernozhukov, Hong and Tamer (2007), *Econometrica*
#'   75:1243-1284; Andrews and Soares (2010), *Econometrica*
#'   78:119-157.
#' @examples
#' d <- cbind(stats::rnorm(50, 1), stats::rnorm(50, 3))
#' g <- function(d, th) cbind(d[, 1] - th, th - d[, 2])
#' morie_bnd_moment_inequality(d, g, c(2))$criterion
#' @export
morie_bnd_moment_inequality <- function(data, g, theta_grid, alpha = 0.05,
                                        B = 500, seed = 0) {
  d <- as.matrix(data)
  storage.mode(d) <- "double"
  n <- nrow(d)
  if (n < 10L) {
    stop(sprintf("need at least 10 observations, got %d.", n), call. = FALSE)
  }
  a <- as.numeric(alpha)
  if (a <= 0 || a >= 1) {
    stop(sprintf("alpha must lie in (0, 1), got %g.", a), call. = FALSE)
  }
  grid <- as.numeric(theta_grid)
  old <- if (exists(".Random.seed", envir = globalenv())) {
    get(".Random.seed", envir = globalenv())
  } else {
    NULL
  }
  set.seed(seed)
  on.exit(if (!is.null(old)) assign(".Random.seed", old, envir = globalenv()))

  Q <- numeric(length(grid))
  crit <- numeric(length(grid))
  J <- NULL
  for (i in seq_along(grid)) {
    G <- as.matrix(g(d, grid[i]))
    if (nrow(G) != n) G <- t(G)
    if (nrow(G) != n) {
      stop("g must return one row per observation.", call. = FALSE)
    }
    J <- ncol(G)
    gbar <- colMeans(G)
    sdv <- apply(G, 2L, stats::sd)
    sdv[sdv <= 0] <- 1
    tt <- sqrt(n) * gbar / sdv
    Q[i] <- sum(pmax(tt, 0)^2)
    Z <- sweep(sweep(G, 2L, gbar), 2L, sdv, "/")
    mult <- matrix(stats::rnorm(as.integer(B) * n), as.integer(B), n)
    boot_t <- (mult %*% Z) / sqrt(n)
    binding <- tt > -sqrt(2 * log(log(max(n, 3))))
    bq <- if (any(binding)) {
      rowSums(pmax(boot_t[, binding, drop = FALSE], 0)^2)
    } else {
      numeric(as.integer(B))
    }
    crit[i] <- stats::quantile(bq, 1 - a, names = FALSE, type = 7L)
  }
  inset <- Q <= crit
  argmin <- which(Q <= min(Q) + 1e-12)
  cs <- if (any(inset)) c(min(grid[inset]), max(grid[inset])) else NULL
  list(
    theta_grid = grid, criterion = Q, critical_value = crit,
    in_confidence_set = inset, set_estimate = grid[argmin],
    confidence_set_bounds = cs,
    positive_part_note = paste(
      "only VIOLATED inequalities enter Q_n;",
      "deep inside the identified set every",
      "sample moment is negative and Q_n is",
      "exactly zero"
    ),
    n = n, J = J,
    method = paste(
      "Chernozhukov-Hong-Tamer criterion-function confidence",
      "region for moment inequalities"
    )
  )
}


#' Sharp linear-programming bounds on a linear target
#'
#' The computational engine of Mogstad, Santos and Torgovitsky
#' (2018): when the object of interest is a linear functional `c'x`
#' and everything the data and assumptions say about `x` is linear,
#' the identified set for the target is
#' `\[min c'x, max c'x\]` over the feasible polytope -- and both ends
#' are linear programmes. SHARPNESS is the point: the feasible set is
#' convex, so every value between the optima is attained, and the
#' interval IS the identified set for the target.
#'
#' Infeasibility is informative rather than an error: an empty
#' feasible set means the maintained assumptions contradict the data
#' moments -- a specification REJECTION.
#'
#' Solved by a self-contained two-phase simplex with Bland's rule
#' (base R has no linear programming); at identification-bound
#' problem sizes the anti-cycling guarantee is worth more than speed.
#'
#' @param c target functional's coefficients.
#' @param A_ub,b_ub inequality restrictions `A_ub x <= b_ub`.
#' @param A_eq,b_eq equality restrictions (typically data moments).
#' @param bounds list of `c(lo, hi)` per coordinate; `\[0, 1\]` each
#'   when `NULL` -- the natural range for response probabilities.
#' @return list: lower, upper, width, argmin, argmax, feasible,
#'   bounded, sharp, k, n_inequalities, n_equalities, method.
#' @references Mogstad, Santos and Torgovitsky (2018), *Econometrica*
#'   86:1589-1619, Prop. 2 and Sec. 4.
#' @examples
#' morie_bnd_lp(c(1, 1), A_eq = rbind(c(1, 2)), b_eq = 1)$lower
#' @export
morie_bnd_lp <- function(c, A_ub = NULL, b_ub = NULL, A_eq = NULL,
                         b_eq = NULL, bounds = NULL) {
  cv <- as.numeric(c)
  k <- length(cv)
  if (k < 1L) {
    stop("the target functional needs at least one coefficient.",
      call. = FALSE
    )
  }
  bx <- if (is.null(bounds)) rep(list(c(0, 1)), k) else bounds
  if (length(bx) != k) {
    stop(sprintf("bounds has %d pairs for %d coordinates.", length(bx), k),
      call. = FALSE
    )
  }
  lower <- vapply(bx, function(p) as.numeric(p[1L]), numeric(1))
  upper <- vapply(bx, function(p) {
    u <- p[2L]
    if (is.null(u) || is.na(u)) Inf else as.numeric(u)
  }, numeric(1))
  n_ub <- if (is.null(A_ub)) 0L else nrow(as.matrix(A_ub))
  n_eq <- if (is.null(A_eq)) 0L else nrow(as.matrix(A_eq))
  if (n_ub && (ncol(as.matrix(A_ub)) != k ||
    length(as.numeric(b_ub)) != n_ub)) {
    stop("A_ub and b_ub have inconsistent shapes.", call. = FALSE)
  }
  if (n_eq && (ncol(as.matrix(A_eq)) != k ||
    length(as.numeric(b_eq)) != n_eq)) {
    stop("A_eq and b_eq have inconsistent shapes.", call. = FALSE)
  }
  lo <- .bnd_simplex(cv, A_ub, b_ub, A_eq, b_eq, lower, upper)
  hi <- .bnd_simplex(-cv, A_ub, b_ub, A_eq, b_eq, lower, upper)
  infeasible <- lo$status == 2L || hi$status == 2L
  unbounded <- lo$status == 3L || hi$status == 3L
  list(
    lower = if (lo$status == 0L) lo$fun else if (lo$status == 3L) -Inf else NA_real_,
    upper = if (hi$status == 0L) -hi$fun else if (hi$status == 3L) Inf else NA_real_,
    width = if (lo$status == 0L && hi$status == 0L) {
      -hi$fun - lo$fun
    } else {
      NA_real_
    },
    argmin = if (lo$status == 0L) lo$x else NULL,
    argmax = if (hi$status == 0L) hi$x else NULL,
    feasible = !infeasible, bounded = !unbounded, sharp = TRUE,
    sharpness_note = paste(
      "the interval IS the identified set for the",
      "target: the feasible set is convex, so every",
      "value between the optima is attained"
    ),
    infeasibility_note = paste(
      "an empty feasible set is a specification",
      "REJECTION -- the maintained assumptions",
      "contradict the data moments"
    ),
    k = k, n_inequalities = n_ub, n_equalities = n_eq,
    method = "Sharp LP bounds on a linear target (Mogstad-Santos-Torgovitsky 2018)"
  )
}


#' Polya-tree posterior mean density
#'
#' Lavine (1992). On the level-`m` dyadic partition the prior puts
#' independent `Beta(alpha_m, alpha_m)` splits at every node;
#' conjugacy updates each to `Beta(alpha_m + n_L, alpha_m + n_R)`,
#' and the posterior mean density multiplies the expected splits
#' along the path to each cell.
#'
#' The canonical `alpha_m = alpha m^2` is what makes the prior put
#' probability one on ABSOLUTELY CONTINUOUS distributions (Kraft
#' 1964); a CONSTANT `alpha_m` behaves like a Dirichlet process,
#' whose realisations are discrete. Larger `alpha` smooths toward the
#' uniform base measure, smaller `alpha` follows the empirical
#' histogram, and both limits are tested.
#'
#' @param y sample.
#' @param grid evaluation points; data-driven when `NULL`.
#' @param tree_depth partition levels; resolution `2^tree_depth`.
#' @param alpha the scale in `alpha_m = alpha m^2`.
#' @param lo,hi support of the base measure; padded data range
#'   when `NULL`.
#' @return list: grid, density, mass, tree_depth, alpha, alpha_rule,
#'   n, method.
#' @references Lavine (1992), *Annals of Statistics* 20:1222-1235;
#'   Ferguson (1974), *Annals of Statistics* 2:615-629; Kraft (1964),
#'   *J. Applied Probability* 1:385-388; Hanson (2006), *JASA*
#'   101:1548-1565.
#' @examples
#' morie_bnd_polya_tree(stats::rnorm(100), grid = c(-1, 0, 1))$density
#' @export
morie_bnd_polya_tree <- function(y, grid = NULL, tree_depth = 6L,
                                 alpha = 1, lo = NULL, hi = NULL) {
  yv <- as.numeric(y)
  n <- length(yv)
  if (n < 2L) {
    stop(sprintf("need at least 2 observations, got %d.", n), call. = FALSE)
  }
  depth <- as.integer(tree_depth)
  if (is.na(depth) || depth < 1L || depth > 16L) {
    stop(sprintf("tree_depth must lie in 1..16, got %s.", format(tree_depth)),
      call. = FALSE
    )
  }
  av <- as.numeric(alpha)
  if (av <= 0) {
    stop(sprintf("alpha must be positive, got %g.", av), call. = FALSE)
  }
  if (is.null(lo)) lo <- min(yv) - 0.05 * (max(yv) - min(yv) + 1e-12)
  if (is.null(hi)) hi <- max(yv) + 0.05 * (max(yv) - min(yv) + 1e-12)
  lo <- as.numeric(lo)
  hi <- as.numeric(hi)
  if (!(lo < hi)) stop("lo must be below hi.", call. = FALSE)
  if (is.null(grid)) grid <- seq(lo, hi, length.out = 256L)
  g <- as.numeric(grid)
  # posterior expected mass of each level-`depth` dyadic cell:
  # product of E[Beta splits] down the path, computed iteratively
  ncell <- 2L^depth
  probs <- rep(1, ncell)
  for (m in seq_len(depth)) {
    am <- av * m^2
    n_here <- 2L^m
    width_here <- (hi - lo) / n_here
    idx <- floor((yv - lo) / width_here)
    idx <- pmin(pmax(idx, 0), n_here - 1L)
    counts <- tabulate(idx + 1L, nbins = n_here)
    nl <- counts[seq(1L, n_here, by = 2L)]
    nr <- counts[seq(2L, n_here, by = 2L)]
    p_left <- (am + nl) / (2 * am + nl + nr)
    rep_each <- ncell / n_here
    split <- as.numeric(rbind(p_left, 1 - p_left)) # interleave L, R
    probs <- probs * rep(split, each = rep_each)
  }
  cellw <- (hi - lo) / ncell
  dens_cells <- probs / cellw
  gi <- floor((g - lo) / cellw)
  inside <- gi >= 0 & gi < ncell
  dens <- numeric(length(g))
  dens[inside] <- dens_cells[gi[inside] + 1L]
  mass <- if (length(g) > 2L && all(diff(g) > 0)) {
    sum(diff(g) * (dens[-1L] + dens[-length(dens)])) / 2
  } else {
    NULL
  }
  list(
    grid = g, density = dens, mass = mass,
    tree_depth = depth, alpha = av,
    alpha_rule = paste(
      "alpha_m = alpha m^2, the Kraft rule that makes",
      "the prior sit on absolutely continuous",
      "distributions; a CONSTANT alpha_m gives a",
      "Dirichlet-process-like tree with discrete",
      "realisations"
    ),
    n = n,
    method = "Polya tree posterior mean density (Lavine 1992)"
  )
}
