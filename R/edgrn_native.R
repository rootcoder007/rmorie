# morie.fn -- edgrn: edgeR-style NB counts, moderated dispersions, TMM, QL F-test.
# Native R implementation of /home/rootcoder/work/morie/src/morie/fn/edgrn.py.
# All algorithms translated verbatim from the Python spec; no external
# packages are used.

.edgrn_eps <- 1e-12

.edgrn_lgamma <- function(x) lgamma(x)

.edgrn_vec <- function(x) as.numeric(x)

.edgrn_nb_logpmf <- function(y, mu, phi) {
  if (phi <= .edgrn_eps) {
    return (-mu + y * log(max(mu, .edgrn_eps)) - .edgrn_lgamma(y + 1))
  }
  r <- 1 / phi
  (.edgrn_lgamma(y + r) - .edgrn_lgamma(r) - .edgrn_lgamma(y + 1)
   + r * log(r / (r + mu)) + y * log(mu / (r + mu)))
}

.edgrn_betainc <- function(a, b, x, iters = 300) {
  if (x <= 0) return(0)
  if (x >= 1) return(1)
  lbeta <- .edgrn_lgamma(a) + .edgrn_lgamma(b) - .edgrn_lgamma(a + b)
  front <- exp(a * log(x) + b * log(1 - x) - lbeta)
  if (x > (a + 1) / (a + b + 2)) {
    return(1 - .edgrn_betainc(b, a, 1 - x, iters))
  }
  f <- 1; cc <- 1; d <- 0
  for (i in 0:(iters - 1)) {
    m <- i %/% 2
    if (i == 0) {
      num <- 1
    } else if ((i %% 2) == 0) {
      num <- (m * (b - m) * x) / ((a + 2 * m - 1) * (a + 2 * m))
    } else {
      num <- -((a + m) * (a + b + m) * x) /
             ((a + 2 * m) * (a + 2 * m + 1))
    }
    d <- 1 + num * d
    if (abs(d) < 1e-30) d <- 1e-30
    d <- 1 / d
    cc <- 1 + num / cc
    if (abs(cc) < 1e-30) cc <- 1e-30
    f <- f * cc * d
    if (abs(1 - cc * d) < 1e-14) break
  }
  front * (f - 1) / a
}

.edgrn_chi2_sf <- function(x, df, iters = 400) {
  if (x <= 0) return(1)
  a <- 0.5 * df
  xx <- 0.5 * x
  if (xx < a + 1) {
    term <- 1 / a
    tot <- 1 / a
    n <- 0
    repeat {
      n <- n + 1
      if (n >= iters) break
      term <- term * xx / (a + n)
      tot <- tot + term
      if (abs(term) < abs(tot) * 1e-15) break
    }
    lower <- tot * exp(-xx + a * log(xx) - .edgrn_lgamma(a))
    return(max(0, min(1, 1 - lower)))
  }
  d <- 1 / (xx + 1 - a)
  f <- d
  cc <- 1e30
  for (i in 1:(iters - 1)) {
    an <- -i * (i - a)
    bb <- xx + 2 * i + 1 - a
    d <- an * d + bb
    if (abs(d) < 1e-30) d <- 1e-30
    cc <- bb + an / cc
    if (abs(cc) < 1e-30) cc <- 1e-30
    d <- 1 / d
    delta <- cc * d
    f <- f * delta
    if (abs(delta - 1) < 1e-15) break
  }
  upper <- f * exp(-xx + a * log(xx) - .edgrn_lgamma(a))
  max(0, min(1, upper))
}

edgrn_nb_variance <- function(mu, dispersion) {
  m <- as.numeric(mu); p <- as.numeric(dispersion)
  if (m < 0 || p < 0)
    stop("edgrn: the mean and dispersion must be non-negative")
  list(variance = m * (1 + m * p), poisson = m,
       biological = m * m * p, bcv = sqrt(p),
       note = "phi = 0 leaves the Poisson variance exactly; sqrt(phi) is the biological coefficient of variation")
}

edgrn_tmm_factor <- function(counts_sample, counts_reference,
                            trim_m = 0.3, trim_a = 0.05,
                            lib_sample = NULL, lib_reference = NULL) {
  y <- .edgrn_vec(counts_sample); r <- .edgrn_vec(counts_reference)
  if (length(y) != length(r))
    stop(sprintf("edgrn: %d genes in the sample but %d in the reference",
                 length(y), length(r)))
  Nk <- if (is.null(lib_sample)) sum(y) else as.numeric(lib_sample)
  Nr <- if (is.null(lib_reference)) sum(r) else as.numeric(lib_reference)
  if (Nk <= 0 || Nr <= 0)
    stop("edgrn: a library size is zero")
  Mv <- numeric(0); Av <- numeric(0); Wv <- numeric(0)
  for (g in seq_along(y)) {
    if (y[g] <= 0 || r[g] <= 0) next
    Mg <- log2(y[g] / Nk) - log2(r[g] / Nr)
    Ag <- 0.5 * (log2(y[g] / Nk) + log2(r[g] / Nr))
    w <- ((Nk - y[g]) / (Nk * y[g])) + ((Nr - r[g]) / (Nr * r[g]))
    Mv <- c(Mv, Mg); Av <- c(Av, Ag)
    Wv <- c(Wv, if (w > .edgrn_eps) 1 / w else 0)
  }
  if (!length(Mv))
    stop("edgrn: no gene is positive in both libraries, so no ratio can be formed")
  n <- length(Mv)
  tm <- as.numeric(trim_m); ta <- as.numeric(trim_a)
  if (tm < 0 || tm >= 0.5 || ta < 0 || ta >= 0.5)
    stop("edgrn: the trim fractions must lie in [0, 0.5)")
  om <- order(Mv); oa <- order(Av)
  cut_m <- as.integer(floor(n * tm))
  cut_a <- as.integer(floor(n * ta))
  keep <- intersect(om[(cut_m + 1):(n - cut_m)],
                    oa[(cut_a + 1):(n - cut_a)])
  if (!length(keep))
    stop("edgrn: the trimming removed every gene")
  num <- sum(Wv[keep] * Mv[keep])
  den <- sum(Wv[keep])
  log2f <- if (den > .edgrn_eps) num / den else 0
  list(factor = 2 ^ log2f, log2_factor = log2f,
       n_used = length(keep), n_genes = n,
       trimmed_m = 2 * cut_m, trimmed_a = 2 * cut_a,
       note = "the counts are NOT modified; this factor enters the model as an offset")
}

edgrn_effective_library_size <- function(library_size, factor) {
  N <- as.numeric(library_size); f <- as.numeric(factor)
  if (N <= 0 || f <= 0)
    stop("edgrn: the library size and factor must be positive")
  list(effective = N * f, offset = log(N * f),
       note = "an offset in the GLM, so the sampling properties of the counts survive")
}

edgrn_moderate_dispersion <- function(gene_dispersions, common = NULL,
                                      prior_df = 10.0, df_residual = 1.0) {
  p <- .edgrn_vec(gene_dispersions)
  if (!length(p))
    stop("edgrn: no dispersions given")
  if (any(p < 0))
    stop("edgrn: a dispersion cannot be negative")
  d0 <- as.numeric(prior_df); dg <- as.numeric(df_residual)
  if (d0 < 0 || dg <= 0)
    stop("edgrn: the degrees of freedom must be positive")
  pos <- p[p > .edgrn_eps]
  cval <- if (is.null(common)) {
    if (length(pos)) exp(sum(log(pos)) / length(pos)) else 0
  } else as.numeric(common)
  w <- d0 / (d0 + dg)
  out <- numeric(length(p))
  for (i in seq_along(p)) {
    v <- p[i]
    if (v <= .edgrn_eps || cval <= .edgrn_eps) {
      out[i] <- (1 - w) * v + w * cval
    } else {
      out[i] <- exp((1 - w) * log(v) + w * log(cval))
    }
  }
  list(dispersion = out, common = cval, shrinkage = w,
       prior_df = d0, df_residual = dg,
       note = "with few libraries per gene there are almost no degrees of freedom, so d0 dominates")
}

edgrn_exact_test <- function(count_a, count_b, lib_a, lib_b, dispersion) {
  ya <- as.numeric(count_a); yb <- as.numeric(count_b)
  Na <- as.numeric(lib_a); Nb <- as.numeric(lib_b)
  phi <- as.numeric(dispersion)
  if (min(ya, yb) < 0 || Na <= 0 || Nb <= 0)
    stop("edgrn: counts must be non-negative and library sizes positive")
  total <- ya + yb
  p_common <- total / (Na + Nb)
  obs <- .edgrn_nb_logpmf(ya, Na * p_common, phi) +
         .edgrn_nb_logpmf(yb, Nb * p_common, phi)
  num <- 0; den <- 0
  s_max <- as.integer(total)
  for (s in 0:s_max) {
    lp <- .edgrn_nb_logpmf(s, Na * p_common, phi) +
          .edgrn_nb_logpmf(total - s, Nb * p_common, phi)
    den <- den + exp(lp)
    if (lp <= obs + 1e-12) num <- num + exp(lp)
  }
  pv <- if (den > 0) min(1, num / den) else 1
  logFC <- log2((ya / Na + .edgrn_eps) / (yb / Nb + .edgrn_eps))
  list(p_value = pv, logFC = logFC, dispersion = phi,
       note = "conditional on the total count, as in Fisher's exact test")
}

edgrn_ql_f_test <- function(lrt, q, quasi_dispersion, df_residual,
                            df_prior = NULL) {
  L <- as.numeric(lrt); Q <- as.integer(q); P <- as.numeric(quasi_dispersion)
  d2 <- as.numeric(df_residual) + if (is.null(df_prior)) 0 else as.numeric(df_prior)
  if (Q < 1 || P <= 0 || d2 <= 0)
    stop("edgrn: need q >= 1 and positive dispersion and degrees of freedom")
  Fv <- L / (Q * P)
  x <- Q * Fv / (Q * Fv + d2)
  p <- if (x > 0) 1 - .edgrn_betainc(0.5 * Q, 0.5 * d2, x) else 1
  chisq <- .edgrn_chi2_sf(L, Q)
  list(estimate = Fv, F = Fv, df1 = Q, df2 = d2, p_value = p,
       lrt_p_value = chisq,
       method = "quasi-likelihood F-test with shrunken dispersion; Lund, Nettleton, McCarthy & Smyth (2012)",
       note = "finite df2 carries the uncertainty in the estimated dispersion into the test; the LRT treats it as known and is therefore more liberal")
}

edger <- edgrn_ql_f_test
edger_diff <- edgrn_ql_f_test
edgerdiff <- edgrn_ql_f_test

#' @rdname edgrn_tmm_factor
#' @export
morie_edgrn <- edgrn_tmm_factor

#' @rdname edgrn_tmm_factor
#' @export
morie_edgrn <- edgrn_tmm_factor

#' @rdname edgrn_tmm_factor
#' @export
morie_edgrn <- edgrn_tmm_factor

#' @rdname edgrn_tmm_factor
#' @export
morie_edgrn <- edgrn_tmm_factor

#' @rdname edgrn_tmm_factor
#' @export
morie_edgrn <- edgrn_tmm_factor

#' @rdname edgrn_tmm_factor
#' @export
morie_edgrn <- edgrn_tmm_factor

#' @rdname edgrn_tmm_factor
#' @export
morie_edgrn <- edgrn_tmm_factor

#' @rdname edgrn_tmm_factor
#' @export
morie_edgrn <- edgrn_tmm_factor

#' @rdname edgrn_tmm_factor
#' @export
morie_edgrn <- edgrn_tmm_factor

#' @rdname edgrn_tmm_factor
#' @export
morie_edgrn <- edgrn_tmm_factor

#' @rdname edgrn_tmm_factor
#' @export
morie_edgrn <- edgrn_tmm_factor

#' @rdname edgrn_tmm_factor
#' @export
morie_edgrn <- edgrn_tmm_factor

#' @rdname edgrn_tmm_factor
#' @export
morie_edgrn <- edgrn_tmm_factor

#' @rdname edgrn_tmm_factor
#' @export
morie_edgrn <- edgrn_tmm_factor

#' @rdname edgrn_tmm_factor
#' @export
morie_edgrn <- edgrn_tmm_factor

#' @rdname edgrn_tmm_factor
#' @export
morie_edgrn <- edgrn_tmm_factor
