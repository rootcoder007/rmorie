# tail2 batch, tranche 2 -- R mirror of the Python modules
#   morie/fn/oddsrt.py   Cornfield (1951) odds ratio, Woolf (1955) interval
#   morie/fn/ctta1c.py   Cronbach (1951) coefficient alpha
#   morie/fn/cttamx.py   Cronbach (1951) alpha with each item deleted
#   morie/fn/qnscl.py    Rousseeuw & Croux (1993) Qn
#   morie/fn/snscl.py    Rousseeuw & Croux (1993) Sn
#   morie/fn/aitbcp.py   Bray & Curtis (1957) dissimilarity
#   morie/fn/convdv.py   Csiszar (1967) f-divergence
#   morie/fn/meplt.py    Davison & Smith (1990) mean excess
#   morie/fn/grubbs.py   Grubbs (1969) single-outlier test
#   morie/fn/dixon.py    Dixon (1953) Q ratio
#
# Byte-identical between r-package/morie/R and r-morie-oss/R.
#
# Sources actually consulted are named in each function; where a paper
# could not be obtained the docstring of the Python module says so in
# the same words.  Qn and Sn were checked against robustbase::Qn and
# robustbase::Sn (Rousseeuw is an author of that package) and agree to
# 4e-16 and exactly, respectively.  Grubbs' p-value is transcribed from
# outliers::qgrubbs.  Every routine below is closed form or runs a FIXED
# number of steps, because an early exit on one language arm and not the
# other silently breaks Py<->R parity.

# ---- Cornfield (1951) odds ratio ------------------------------------

#' Cross-product odds ratio for a 2 x 2 table, with a Woolf interval
#' @param a,b,c,d cell counts: exposed cases, exposed controls,
#'   unexposed cases, unexposed controls
#' @param conf_level two-sided confidence level for the Woolf interval
#' @param correction constant added to every cell (0.5 = Haldane-Anscombe)
#' @return list(estimate, log_estimate, se_log, ci_lower, ci_upper, z,
#'   p_value, n)
#' @export
#' @examples
#' OddsRat(10, 20, 15, 25)
OddsRat <- function(a, b, c, d, conf_level = 0.95, correction = 0) {
  if (!(conf_level > 0 && conf_level < 1))
    stop("conf_level must lie strictly between 0 and 1")
  aa <- a + correction; bb <- b + correction
  cc <- c + correction; dd <- d + correction
  if (any(c(aa, bb, cc, dd) < 0))
    stop("2 x 2 cell counts must be non-negative")
  n <- a + b + c + d
  est <- if (bb == 0 || cc == 0) Inf else (aa * dd) / (bb * cc)
  if (aa == 0 || bb == 0 || cc == 0 || dd == 0)
    return(list(estimate = est, log_estimate = NaN, se_log = NaN,
                ci_lower = NaN, ci_upper = NaN, z = NaN, p_value = NaN,
                n = n, conf_level = conf_level,
                method = paste("Cornfield (1951) cross-product odds ratio;",
                               "a zero cell leaves the Woolf variance undefined")))
  log_or <- log(est)
  se <- sqrt(1/aa + 1/bb + 1/cc + 1/dd)
  zq <- qnorm(0.5 + 0.5 * conf_level)
  z <- log_or / se
  p <- 2 * (1 - pnorm(abs(z)))
  list(estimate = est, log_estimate = log_or, se_log = se,
       ci_lower = exp(log_or - zq * se), ci_upper = exp(log_or + zq * se),
       z = z, p_value = p, n = n, conf_level = conf_level,
       method = paste("Cornfield (1951) cross-product odds ratio,",
                      "Woolf (1955) log-scale variance 1/a+1/b+1/c+1/d"))
}

# ---- Cronbach (1951) alpha ------------------------------------------

#' .t2_alpha_on
#'
#' A step of the tail2_t02 implementation. Called by \code{CttAlphaMax}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A matrix; indexed by row and column.
#' @param cols A vector; its length is taken.
#' @return A numeric value.
#' @export
.t2_alpha_on <- function(X, cols) {
  k <- length(cols)
  if (k < 2L) stop("alpha needs at least two items")
  n <- nrow(X)
  sv <- 0
  for (j in cols) {
    v <- X[, j]
    m <- sum(v) / n
    sv <- sv + sum((v - m)^2) / (n - 1)
  }
  tot <- numeric(n)
  for (i in seq_len(n)) tot[i] <- sum(X[i, cols])
  m <- sum(tot) / n
  tv <- sum((tot - m)^2) / (n - 1)
  if (tv <= 0) stop("total score has zero variance; alpha is undefined")
  (k / (k - 1)) * (1 - sv / tv)
}

#' .t2_table
#'
#' A step of the tail2_t02 implementation. Called by \code{CttAlpha}, \code{CttAlphaMax}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A matrix; passed to \code{nrow}.
#' @return The value of \code{X}, as built in the body.
#' @export
.t2_table <- function(X) {
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  if (nrow(X) < 2L) stop("alpha needs at least two persons")
  X
}

#' Cronbach (1951) coefficient alpha
#' @param X n persons x k items score matrix
#' @return list(alpha, estimate, item_var, sum_item_var, total_var,
#'   n_items, n)
#' @export
#' @examples
#' M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
#' CttAlpha(M)
CttAlpha <- function(X) {
  X <- .t2_table(X)
  n <- nrow(X); k <- ncol(X)
  if (k < 2L) stop("alpha needs at least two items")
  iv <- numeric(k)
  for (j in seq_len(k)) {
    v <- X[, j]; m <- sum(v) / n
    iv[j] <- sum((v - m)^2) / (n - 1)
  }
  sv <- sum(iv)
  tot <- numeric(n)
  for (i in seq_len(n)) tot[i] <- sum(X[i, ])
  m <- sum(tot) / n
  tv <- sum((tot - m)^2) / (n - 1)
  if (tv <= 0) stop("total score has zero variance; alpha is undefined")
  alpha <- (k / (k - 1)) * (1 - sv / tv)
  list(alpha = alpha, estimate = alpha, item_var = iv, sum_item_var = sv,
       total_var = tv, n_items = k, n = n,
       method = "Cronbach (1951) alpha = k/(k-1) (1 - sum V_i / V_t)")
}

#' Cronbach (1951) alpha with each item deleted in turn
#' @param X n persons x k items score matrix, k >= 3
#' @return list(alpha_full, alpha_dropped, delta, max_alpha, argmax_alpha,
#'   estimate, n_items, n)
#' @export
#' @examples
#' M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
#' CttAlphaMax(M)
CttAlphaMax <- function(X) {
  X <- .t2_table(X)
  n <- nrow(X); k <- ncol(X)
  if (k < 3L) stop("alpha-if-item-deleted needs at least three items")
  full <- .t2_alpha_on(X, seq_len(k))
  drop <- numeric(k)
  for (j in seq_len(k)) drop[j] <- .t2_alpha_on(X, setdiff(seq_len(k), j))
  best <- 1L
  for (j in seq_len(k)) if (drop[j] > drop[best]) best <- j
  list(alpha_full = full, alpha_dropped = drop, delta = drop - full,
       max_alpha = drop[best], argmax_alpha = best - 1L,
       estimate = drop[best], n_items = k, n = n,
       method = "Cronbach (1951) alpha recomputed with each item deleted")
}

# ---- Rousseeuw & Croux (1993) Qn and Sn -----------------------------
# Definitions and both correction blocks are quoted verbatim from
# robustbase R/qnsn.R; see the Python docstrings for the quotations.

.T2_QN_SMALL <- c(.399356, .99365, .51321, .84401, .61220,
                  .85877, .66993, .87344, .72014, .88906, .75743)
.T2_SN_SMALL <- c(0.743, 1.851, 0.954, 1.351, 0.993, 1.198, 1.005, 1.131)

#' .t2_qn_finite_c
#'
#' A step of the tail2_t02 implementation. Called by \code{QnScale}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.t2_qn_finite_c <- function(n) {
  inner <- if (n %% 2L)  1.60188 + (-2.1284 - 5.172 / n) / n
           else          3.67561 + ( 1.9654 + (6.987 - 77 / n) / n) / n
  inner / n + 1
}

#' Rousseeuw & Croux (1993) Qn robust scale
#' @param y numeric sample, length >= 2
#' @param constant consistency constant, 1/(sqrt(2) qnorm(5/8)) = 2.21914
#' @param finite_corr apply the robustbase finite-sample bias correction
#' @return list(estimate, raw, k, h, n_pairs, correction, constant, n)
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' QnScale(V)
QnScale <- function(y, constant = 2.21914, finite_corr = TRUE) {
  x <- as.numeric(y)
  n <- length(x)
  if (n < 2L) stop("Qn needs at least two observations")
  h <- n %/% 2L + 1L
  k <- (h * (h - 1L)) %/% 2L          # %/% binds tighter than *; parens matter
  d <- numeric((n * (n - 1L)) %/% 2L)
  p <- 0L
  for (i in seq_len(n - 1L)) for (j in (i + 1L):n) {
    p <- p + 1L
    d[p] <- abs(x[i] - x[j])
  }
  d <- sort(d)
  raw <- d[k]
  corr <- if (!finite_corr) 1
          else if (n <= 12L) .T2_QN_SMALL[n - 1L]
          else 1 / .t2_qn_finite_c(n)
  list(estimate = constant * raw * corr, raw = raw, k = k, h = h,
       n_pairs = (n * (n - 1L)) %/% 2L, correction = corr,
       constant = constant, n = n,
       method = "Rousseeuw & Croux (1993) Qn, robustbase qnsn.R definition")
}

#' Rousseeuw & Croux (1993) Sn robust scale
#' @param y numeric sample, length >= 2
#' @param constant consistency constant, 1.1926
#' @param finite_corr apply the robustbase finite-sample bias correction
#' @return list(estimate, raw, inner, correction, constant, n)
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' SnScale(V)
SnScale <- function(y, constant = 1.1926, finite_corr = TRUE) {
  x <- as.numeric(y)
  n <- length(x)
  if (n < 2L) stop("Sn needs at least two observations")
  inner <- numeric(n)
  for (i in seq_len(n)) {
    row <- sort(abs(x[i] - x))
    inner[i] <- row[n %/% 2L + 1L]          # himed
  }
  srt <- sort(inner)
  raw <- srt[(n + 1L) %/% 2L]               # lomed
  corr <- if (!finite_corr) 1
          else if (n <= 9L) .T2_SN_SMALL[n - 1L]
          else if (n %% 2L) n / (n - 0.9)
          else 1
  list(estimate = constant * raw * corr, raw = raw, inner = inner,
       correction = corr, constant = constant, n = n,
       method = "Rousseeuw & Croux (1993) Sn, robustbase qnsn.R definition")
}

# ---- Bray & Curtis (1957) -------------------------------------------

#' Bray & Curtis (1957) dissimilarity between two non-negative vectors
#' @param x,y non-negative parts of equal length
#' @param close divide each vector by its own total first
#' @return list(bc, estimate, numerator, denominator, similarity, n)
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' BrayCurt(V, V)
BrayCurt <- function(x, y, close = TRUE) {
  a <- as.numeric(x); b <- as.numeric(y)
  n <- length(a)
  if (n == 0L || length(b) != n)
    stop("x and y must be non-empty and of equal length")
  if (any(a < 0) || any(b < 0))
    stop("Bray-Curtis is defined for non-negative parts")
  if (close) {
    sa <- sum(a); sb <- sum(b)
    if (sa <= 0 || sb <= 0)
      stop("a composition cannot be closed to a zero total")
    a <- a / sa; b <- b / sb
  }
  num <- sum(abs(a - b))
  den <- sum(a + b)
  if (den <= 0) stop("both vectors are zero; Bray-Curtis is undefined")
  bc <- num / den
  list(bc = bc, estimate = bc, numerator = num, denominator = den,
       similarity = 1 - bc, closed = close, n = n,
       method = paste("Bray & Curtis (1957) dissimilarity,",
                      "sum|x-y| / sum(x+y); a dissimilarity, not a metric"))
}

# ---- Csiszar (1967) f-divergence ------------------------------------

#' .t2_named_f
#'
#' A step of the tail2_t02 implementation. Called by \code{FDiverg}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param key Passed to \code{switch}.
#' @return The value of \code{switch}.
#' @export
.t2_named_f <- function(key) switch(key,
  kl        = function(t) if (t > 0) t * log(t) else 0,
  rkl       = function(t) if (t > 0) -log(t) else Inf,
  tv        = function(t) 0.5 * abs(t - 1),
  chi2      = function(t) (t - 1)^2,
  hellinger = function(t) (sqrt(t) - 1)^2,
  js        = function(t) (if (t > 0) t * log(t) else 0) -
                          (t + 1) * log((t + 1) / 2),
  stop(sprintf("unknown generator '%s'", key)))

#' .t2_named_inf
#'
#' A step of the tail2_t02 implementation. Called by \code{FDiverg}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param key Passed to \code{switch}.
#' @return The value of \code{switch}.
#' @export
.t2_named_inf <- function(key) switch(key,
  kl = Inf, rkl = 0, tv = 0.5, chi2 = Inf, hellinger = 1, js = log(2),
  stop(sprintf("unknown generator '%s'", key)))

#' Csiszar (1967) f-divergence D_f(p || q) = sum q f(p/q)
#' @param p,q non-negative weights of equal length
#' @param f a convex generator with f(1) = 0, or one of "kl", "rkl",
#'   "tv", "chi2", "hellinger", "js"
#' @param f_inf recession constant charged against mass p places where
#'   q is zero; defaults to the exact value for a named generator
#' @param normalise divide p and q by their totals first
#' @return list(divergence, estimate, terms, support, generator, f_inf, n)
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' FDiverg(V, V)
FDiverg <- function(p, q, f = "kl", f_inf = NULL, normalise = TRUE) {
  a <- as.numeric(p); b <- as.numeric(q)
  n <- length(a)
  if (n == 0L || length(b) != n)
    stop("p and q must be non-empty and of equal length")
  if (any(a < 0) || any(b < 0)) stop("p and q must be non-negative")
  if (is.character(f)) {
    key <- tolower(f)
    fn <- .t2_named_f(key)
    if (is.null(f_inf)) f_inf <- .t2_named_inf(key)
    nm <- key
  } else {
    fn <- f
    if (is.null(f_inf)) f_inf <- Inf
    if (abs(fn(1)) > 1e-12) stop("generator must satisfy f(1) = 0")
    nm <- "callable"
  }
  if (normalise) {
    sa <- sum(a); sb <- sum(b)
    if (sa <= 0 || sb <= 0)
      stop("p and q must each carry positive total mass")
    a <- a / sa; b <- b / sb
  }
  terms <- numeric(n)
  for (i in seq_len(n)) {
    terms[i] <- if (b[i] > 0) fn(a[i] / b[i]) * b[i]
                else if (a[i] > 0) a[i] * f_inf
                else 0
  }
  list(divergence = sum(terms), estimate = sum(terms), terms = terms,
       support = sum(terms != 0), generator = nm, f_inf = f_inf, n = n,
       method = "Csiszar (1967) f-divergence, sum q f(p/q)")
}

# ---- Davison & Smith (1990) mean excess -----------------------------

#' Davison & Smith (1990) empirical mean excess over a threshold grid
#' @param x numeric sample
#' @param u_grid thresholds; default the sorted distinct values of x with
#'   the largest dropped
#' @param conf_level level for the pointwise normal limits
#' @return list(u, e, se, sd_excess, ci_lower, ci_upper, n_exceed, n)
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' MeanExc(V)
MeanExc <- function(x, u_grid = NULL, conf_level = 0.95) {
  xs <- as.numeric(x)
  n <- length(xs)
  if (n < 2L) stop("mean excess needs at least two observations")
  if (!(conf_level > 0 && conf_level < 1))
    stop("conf_level must lie strictly between 0 and 1")
  if (is.null(u_grid)) {
    uniq <- sort(unique(xs))
    if (length(uniq) < 2L)
      stop("x is constant; no threshold leaves an exceedance")
    grid <- uniq[-length(uniq)]
  } else {
    grid <- as.numeric(u_grid)
    if (length(grid) == 0L) stop("u_grid must be non-empty")
  }
  zq <- qnorm(0.5 + 0.5 * conf_level)
  ng <- length(grid)
  e <- rep(NaN, ng); se <- rep(NaN, ng); sd <- rep(NaN, ng)
  lo <- rep(NaN, ng); hi <- rep(NaN, ng); cnt <- integer(ng)
  for (g in seq_len(ng)) {
    u <- grid[g]
    ex <- xs[xs > u] - u
    m <- length(ex)
    cnt[g] <- m
    if (m == 0L) next
    mu <- sum(ex) / m
    e[g] <- mu
    if (m < 2L) next
    sd[g] <- sqrt(sum((ex - mu)^2) / (m - 1))
    se[g] <- sd[g] / sqrt(m)
    lo[g] <- mu - zq * se[g]
    hi[g] <- mu + zq * se[g]
  }
  list(u = grid, e = e, se = se, sd_excess = sd, ci_lower = lo,
       ci_upper = hi, n_exceed = cnt, conf_level = conf_level, n = n,
       method = paste("Davison & Smith (1990) mean excess e(u)=E[X-u|X>u];",
                      "linear in u above a generalised Pareto threshold"))
}

# ---- Grubbs (1969) --------------------------------------------------
# Statistic and p-value transcribed from outliers::grubbs.test and
# outliers::qgrubbs(..., rev = TRUE), type 10.

#' Grubbs (1969) test that the most extreme observation is an outlier
#' @param x numeric sample, length >= 3
#' @param alpha level at which the critical value is reported
#' @param opposite test the end NOT selected by the maximum deviation
#' @return list(statistic, p_value, critical_value, reject, outlier,
#'   index, side, mean, sd, n)
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' GrubbsT(V)
GrubbsT <- function(x, alpha = 0.05, opposite = FALSE) {
  xs <- as.numeric(x)
  n <- length(xs)
  if (n < 3L) stop("Grubbs' test needs at least three observations")
  if (!(alpha > 0 && alpha < 1))
    stop("alpha must lie strictly between 0 and 1")
  m <- sum(xs) / n
  sd <- sqrt(sum((xs - m)^2) / (n - 1))
  if (sd <= 0) stop("x is constant; Grubbs' statistic is undefined")
  imax <- 1L; imin <- 1L
  for (i in seq_len(n)) {
    if (xs[i] > xs[imax]) imax <- i
    if (xs[i] < xs[imin]) imin <- i
  }
  take_high <- (xs[imax] - m) >= (m - xs[imin])
  if (opposite) take_high <- !take_high
  idx <- if (take_high) imax else imin
  side <- if (take_high) "max" else "min"
  g <- abs(xs[idx] - m) / sd
  den <- g * g * n - (n - 1)^2
  if (den == 0) {
    p <- 0
  } else {
    s <- (g * g * n * (2 - n)) / den
    if (s < 0) s <- 0
    p <- n * (1 - pt(sqrt(s), n - 2))
    if (p > 1) p <- 1
    if (p < 0) p <- 0
  }
  ta <- qt(alpha / n, n - 2)
  ta2 <- ta * ta
  crit <- ((n - 1) / sqrt(n)) * sqrt(ta2 / (n - 2 + ta2))
  list(statistic = g, p_value = p, critical_value = crit,
       reject = g > crit, outlier = xs[idx], index = idx - 1L,
       side = side, mean = m, sd = sd, alpha = alpha, n = n,
       method = paste("Grubbs (1969) single-outlier test,",
                      "outliers::grubbs.test type 10;",
                      "p = n (1 - pt(t, n-2)), one-sided"))
}

# ---- Dixon (1953) ---------------------------------------------------
# Ratios transcribed from outliers::dixon.test.  No p-value: the null
# distribution is a stored table there, not a formula.

.T2_DIX_NUM <- c("10" = 1L, "11" = 1L, "12" = 1L,
                 "20" = 2L, "21" = 2L, "22" = 2L)
.T2_DIX_DEN <- c("10" = 0L, "11" = 1L, "12" = 2L,
                 "20" = 0L, "21" = 1L, "22" = 2L)
.T2_DIX_MIN <- c("10" = 3L, "11" = 4L, "12" = 5L,
                 "20" = 4L, "21" = 5L, "22" = 6L)

#' Dixon (1953) ratio for the most extreme observation
#' @param x numeric sample
#' @param type one of 10, 11, 12, 20, 21, 22; 10 is Q = gap / range
#' @param opposite test the end NOT selected by the larger deviation
#' @return list(statistic, type, outlier, side, numerator, denominator, n)
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' DixonQ(V)
DixonQ <- function(x, type = 10, opposite = FALSE) {
  key <- as.character(as.integer(type))
  if (!(key %in% names(.T2_DIX_NUM)))
    stop("type must be one of 10, 11, 12, 20, 21, 22")
  xs <- sort(as.numeric(x))
  n <- length(xs)
  if (n < .T2_DIX_MIN[[key]])
    stop(sprintf("Dixon type %s needs at least %d observations",
                 key, .T2_DIX_MIN[[key]]))
  num_off <- .T2_DIX_NUM[[key]]
  den_off <- .T2_DIX_DEN[[key]]
  m <- sum(xs) / n
  take_high <- (xs[n] - m) >= (m - xs[1L])
  if (opposite) take_high <- !take_high
  if (take_high) {
    num <- xs[n] - xs[n - num_off]
    den <- xs[n] - xs[den_off + 1L]
    idx <- n; side <- "max"
  } else {
    num <- xs[num_off + 1L] - xs[1L]
    den <- xs[n - den_off] - xs[1L]
    idx <- 1L; side <- "min"
  }
  if (den == 0) stop("Dixon's denominator is zero; the ratio is undefined")
  list(statistic = num / den, type = as.integer(type),
       outlier = xs[idx], side = side, numerator = num,
       denominator = den, n = n,
       method = sprintf(paste("Dixon (1953) ratio type %d,",
                              "outliers::dixon.test; no p-value,",
                              "the null distribution is tabulated only"),
                        as.integer(type)))
}
