# EMMAX: a variance component model for sample structure in GWAS.
# Sources: Kang, H. M., Sul, J. H., Service, S. K., Zaitlen, N. A.,
# Kong, S., Freimer, N. B., Sabatti, C. & Eskin, E. (2010) Variance
# component model to account for sample structure in genome-wide
# association studies, Nature Genetics 42(4), 348-354 -- equations 5
# (Gower centring), 6 (the variance component model) and 7 (the
# marker-level test), the three-step Online Methods procedure, the
# pseudo-heritability, the case-control Armitage-style adaptation,
# and the per-marker REML switch. Kang, H. M. et al. (2008) Efficient
# control of population structure in model organism association
# mapping, Genetics 178(3), 1709-1723 -- EMMA and the spectral
# decomposition that makes step 2 cheap.
#
# Native R port mirroring morie.fn.gwasem exactly. The Python arm
# uses numpy.linalg.eigh / solve / slogdet / inv; we use the same
# routines internally (see .gwasem_eigh, .gwasem_solve, etc.) so
# both arms produce numerically identical results. The incomplete-
# beta F upper tail is reproduced with the Lentz continued-fraction
# algorithm; the genomic-control null median is the chi-square 0.5
# quantile qchisq(0.5, 1) for df = 1, and the cubic Wilson-Hilferty
# approximation for df > 1.

#' IBS relatedness matrix
#'
#' Mean proportion of alleles shared identical by state across
#' markers, one of the matrices the paper names for step 1.
#'
#' @param genotypes n x m matrix of minor-allele counts.
#' @return n x n symmetric matrix.
#' @export
morie_gwasem_kinship <- function(genotypes) {
  G <- as.matrix(genotypes); storage.mode(G) <- "double"
  n <- nrow(G)
  if (n == 0L || ncol(G) == 0L)
    stop("gwasem: genotypes must be a non-empty individual x marker matrix")
  m <- ncol(G)
  S <- matrix(0, n, n)
  for (i in seq_len(n)) {
    S[i, i] <- 1.0
    if (i < n) for (k in (i + 1L):n) {
      d <- sum(abs(G[i, ] - G[k, ]))
      v <- 1.0 - d / (2.0 * m)
      S[i, k] <- S[k, i] <- v
    }
  }
  S
}

#' Gower centring (equation 5)
#'
#' Scales the relatedness matrix to sample variance 1 so that
#' \code{sigma_a^2} is on the scale of the phenotypic variance.
#'
#' @param S n x n matrix.
#' @return Centred matrix.
#' @export
morie_gwasem_gower <- function(S) {
  S <- as.matrix(S); storage.mode(S) <- "double"
  n <- nrow(S)
  if (n < 2L) stop("gwasem: need at least two individuals")
  rowmean <- rowMeans(S)
  total <- mean(rowmean)
  tr <- 0.0
  for (i in seq_len(n)) tr <- tr + S[i, i] - 2.0 * rowmean[i] + total
  if (abs(tr) < 1e-300)
    stop("gwasem: the relatedness matrix has zero centred trace; it carries no structure to normalise")
  f <- (n - 1.0) / tr
  S * f
}

#' .gwasem_eigh
#'
#' Part of the gwasem_native implementation; see the file header for the
#' source it follows.
#'
#' @param M See Usage.
#' @return A list with \code{values}, \code{vectors}.
#' @export
.gwasem_eigh <- function(M) {
  ee <- eigen(as.matrix(M), symmetric = TRUE)
  list(values = ee$values, vectors = ee$vectors)
}

#' .gwasem_solve
#'
#' Part of the gwasem_native implementation; see the file header for the
#' source it follows.
#'
#' @param A See Usage.
#' @param b See Usage.
#' @return A vector, from \code{as.numeric}.
#' @export
.gwasem_solve <- function(A, b) {
  as.numeric(solve(A, b))
}

#' .gwasem_inv
#'
#' Part of the gwasem_native implementation; see the file header for the
#' source it follows.
#'
#' @param A See Usage.
#' @return A matrix, from \code{solve}.
#' @export
.gwasem_inv <- function(A) solve(A)

#' .gwasem_slogdet
#'
#' Part of the gwasem_native implementation; see the file header for the
#' source it follows.
#'
#' @param M See Usage.
#' @return A list with \code{sign}, \code{logdet}.
#' @export
.gwasem_slogdet <- function(M) {
  v <- svd(M)
  prod(v$d)
  sign <- prod(sign(v$d))
  logdet <- sum(log(v$d))
  list(sign = sign, logdet = as.numeric(logdet))
}

#' .gwasem_loglik
#'
#' Part of the gwasem_native implementation; see the file header for the
#' source it follows.
#'
#' @param yt See Usage.
#' @param Xt See Usage.
#' @param d See Usage.
#' @param ml See Usage.
#' @return A numeric value.
#' @export
.gwasem_loglik <- function(yt, Xt, d, ml) {
  n <- length(yt); p <- ncol(Xt)
  M <- matrix(0, p, p)
  for (a in seq_len(p)) for (b in seq_len(p))
    M[a, b] <- sum(Xt[, a] * Xt[, b] / d)
  v <- numeric(p)
  for (a in seq_len(p)) v[a] <- sum(Xt[, a] * yt / d)
  ms <- .gwasem_slogdet(M)
  if (ms$sign <= 0) return(-Inf)
  beta <- .gwasem_solve(M, v)
  rss <- sum((yt - as.numeric(Xt %*% beta))^2 / d)
  if (rss <= 0) return(-Inf)
  logdetV <- sum(log(d))
  if (ml) return(-0.5 * (n * log(2 * pi * rss / n) + n + logdetV))
  df <- n - p
  -0.5 * (df * log(2 * pi * rss / df) + df + logdetV + ms$logdet)
}

#' .gwasem_reml_delta
#'
#' Part of the gwasem_native implementation; see the file header for the
#' source it follows.
#'
#' @param y See Usage.
#' @param X See Usage.
#' @param evals See Usage.
#' @param evecs See Usage.
#' @param ml Defaults to \code{FALSE}.
#' @param lo Defaults to \code{-10}.
#' @param hi Defaults to \code{10}.
#' @param n_grid Defaults to \code{100L}.
#' @param refine Defaults to \code{60L}.
#' @return A list with \code{delta}, \code{sigma_a2}, \code{sigma_e2}, \code{loglik}.
#' @export
.gwasem_reml_delta <- function(y, X, evals, evecs, ml = FALSE,
                                lo = -10, hi = 10, n_grid = 100L,
                                refine = 60L) {
  n <- length(y); p <- ncol(X)
  yt <- as.numeric(t(evecs) %*% y)
  Xt <- as.matrix(t(evecs) %*% X)
  loglik <- function(delta) {
    d <- evals + delta
    if (min(d) <= 1e-12) return(-Inf)
    .gwasem_loglik(yt, Xt, d, ml)
  }
  us <- seq(lo, hi, length.out = n_grid + 1L)
  ll <- vapply(us, function(u) loglik(exp(u)), numeric(1))
  best <- which.max(ll)
  a <- us[max(1L, best - 1L)]; b <- us[min(n_grid + 1L, best + 1L)]
  phi <- (sqrt(5) - 1) / 2
  c <- b - phi * (b - a); d <- a + phi * (b - a)
  fc <- loglik(exp(c)); fd <- loglik(exp(d))
  for (i in seq_len(refine)) {
    if (fc > fd) { b <- d; d <- c; fd <- fc; c <- b - phi * (b - a); fc <- loglik(exp(c)) }
    else { a <- c; c <- d; fc <- fd; d <- a + phi * (b - a); fd <- loglik(exp(d)) }
  }
  delta <- exp(0.5 * (a + b))
  d <- evals + delta
  M <- matrix(0, p, p)
  for (a in seq_len(p)) for (b in seq_len(p))
    M[a, b] <- sum(Xt[, a] * Xt[, b] / d)
  v <- numeric(p)
  for (a in seq_len(p)) v[a] <- sum(Xt[, a] * yt / d)
  beta <- .gwasem_solve(M, v)
  rss <- sum((yt - as.numeric(Xt %*% beta))^2 / d)
  df <- if (ml) n else n - p
  sigma_a2 <- rss / df
  list(delta = delta, sigma_a2 = sigma_a2, sigma_e2 = sigma_a2 * delta,
       loglik = loglik(delta))
}

#' REML variance component estimation (step 2)
#'
#' Estimates \code{sigma_a^2} and \code{sigma_e^2} in equation 6.
#' Returns the components, the pseudo-heritability, the restricted
#' log-likelihood, the null log-likelihood (at \code{sigma_a^2 = 0})
#' and the LRT statistic for \code{H_0: sigma_a^2 = 0}.
#'
#' @param y Phenotype vector.
#' @param kinship n x n relatedness matrix.
#' @param covariates Optional n x q covariate matrix.
#' @param ml Use ML rather than REML.
#' @return A list with the components and supporting quantities.
#' @export
morie_gwasem_reml <- function(y, kinship, covariates = NULL, ml = FALSE) {
  yv <- as.numeric(y)
  n <- length(yv)
  K <- morie_gwasem_gower(kinship)
  if (nrow(K) != n) stop("gwasem: the kinship matrix must be n x n")
  if (is.null(covariates)) {
    X <- matrix(1, n, 1)
  } else {
    X <- cbind(1, as.matrix(covariates))
    if (nrow(X) != n) stop("gwasem: one covariate row per individual")
  }
  ee <- .gwasem_eigh(K)
  shift <- if (min(ee$values) <= 0) -min(ee$values) + 1e-8 else 0
  evals <- ee$values + shift
  evecs <- ee$vectors
  fit <- .gwasem_reml_delta(yv, X, evals, evecs, ml)
  p <- ncol(X)
  M0 <- crossprod(X)
  v0 <- as.numeric(crossprod(X, yv))
  beta0 <- .gwasem_solve(M0, v0)
  rss0 <- sum((yv - as.numeric(X %*% beta0))^2)
  df0 <- if (ml) n else n - p
  ll0 <- -0.5 * (df0 * log(2 * pi * rss0 / df0) + df0)
  if (!ml) {
    ms <- .gwasem_slogdet(M0)
    ll0 <- ll0 - 0.5 * ms$logdet
  }
  ph <- if (fit$sigma_a2 + fit$sigma_e2 > 0)
    fit$sigma_a2 / (fit$sigma_a2 + fit$sigma_e2) else 0.0
  list(sigma_a2 = fit$sigma_a2, sigma_e2 = fit$sigma_e2,
       delta = fit$delta, pseudo_heritability = ph,
       loglik = fit$loglik, loglik_null = ll0,
       lrt = max(0, 2 * (fit$loglik - ll0)),
       evals = evals, evecs = evecs, kinship_normalized = K,
       shift = shift)
}

#' .gwasem_f_sf
#'
#' Part of the gwasem_native implementation; see the file header for the
#' source it follows.
#'
#' @param f See Usage.
#' @param df1 See Usage.
#' @param df2 See Usage.
#' @return One of two values, depending on the branch taken.
#' @export
.gwasem_f_sf <- function(f, df1, df2) {
  if (f <= 0) return(1.0)
  x <- df2 / (df2 + df1 * f)
  a <- 0.5 * df2; b <- 0.5 * df1
  log_beta <- lbeta(a, b) + a * log(x) + b * log(1 - x)
  cf <- function(a, b, x) {
    qab <- a + b; qap <- a + 1; qam <- a - 1
    c <- 1; d <- 1 - qab * x / qap
    if (abs(d) < 1e-300) d <- 1e-300
    d <- 1 / d; h <- d
    for (mm in seq_len(300L)) {
      m2 <- 2 * mm
      aa <- mm * (b - mm) * x / ((qam + m2) * (a + m2))
      d <- 1 + aa * d
      if (abs(d) < 1e-300) d <- 1e-300
      d <- 1 / d
      c <- 1 + aa / c
      if (abs(c) < 1e-300) c <- 1e-300
      h <- h * d * c
      aa <- -(a + mm) * (qab + mm) * x / ((a + m2) * (qap + m2))
      d <- 1 + aa * d
      if (abs(d) < 1e-300) d <- 1e-300
      d <- 1 / d
      c <- 1 + aa / c
      if (abs(c) < 1e-300) c <- 1e-300
      de <- d * c
      h <- h * de
      if (abs(de - 1) < 3e-16) break
    }
    h
  }
  if (x < (a + 1) / (a + b + 2)) exp(log_beta) * cf(a, b, x) / a
  else 1 - exp(log_beta) * cf(b, a, 1 - x) / b
}

#' .gwasem_norm_sf
#'
#' Part of the gwasem_native implementation; see the file header for the
#' source it follows.
#'
#' @param z See Usage.
#' @return The value of \code{pnorm}.
#' @export
.gwasem_norm_sf <- function(z) pnorm(abs(z), lower.tail = FALSE)

#' Genomic-control inflation factor
#'
#' Median observed chi-square divided by its null median. A
#' well-calibrated analysis sits at 1.
#'
#' @param stats Numeric vector of chi-square statistics.
#' @param df Degrees of freedom.
#' @return Scalar.
#' @export
morie_gwasem_gc <- function(stats, df = 1) {
  s <- sort(as.numeric(stats))
  if (length(s) == 0L) stop("gwasem: no statistics")
  n <- length(s)
  med <- if (n %% 2L) s[(n + 1L) %/% 2L] else 0.5 * (s[n %/% 2L] + s[n %/% 2L + 1L])
  null_med <- if (df == 1L) qchisq(0.5, 1) else
    df * (1 - 2 / (9 * df))^3
  med / null_med
}

#' EMMAX GWAS
#'
#' Three-step procedure: Gower-normalise \code{S} (eq.5), estimate
#' \code{sigma_a^2, sigma_e^2} once under the null (eq.6) and test
#' each marker with the fixed variance (eq.7). Case-control is the
#' 0/1 response as a quantitative trait, in the spirit of Armitage.
#'
#' @param y Phenotype vector.
#' @param genotypes n x m minor-allele count matrix.
#' @param kinship Optional n x n relatedness matrix; IBS when omitted.
#' @param covariates Optional n x q covariate matrix.
#' @param trait \code{"quantitative"} or \code{"binary"}.
#' @param test \code{"f"} or \code{"score"}.
#' @param ml Use ML rather than REML.
#' @param per_marker_reml Re-estimate variance components per marker.
#' @param min_maf Skip markers below this MAF.
#' @return A list with the per-marker statistics, the variance
#'   components, the genomic-control inflation factor and the
#'   skipped-marker indices.
#' @references Kang, H. M. et al. (2010). Nature Genetics 42(4),
#'   348-354.
#' @export
morie_gwasem <- function(y, genotypes, kinship = NULL, covariates = NULL,
                         trait = "quantitative", test = "f", ml = FALSE,
                         per_marker_reml = FALSE, min_maf = 0) {
  yv <- as.numeric(y)
  G <- as.matrix(genotypes); storage.mode(G) <- "double"
  n <- length(yv)
  if (n == 0L || nrow(G) != n)
    stop("gwasem: one genotype row per phenotype")
  m <- ncol(G)
  if (!(trait %in% c("quantitative", "binary")))
    stop("gwasem: trait must be 'quantitative' or 'binary'")
  if (trait == "binary" && any(!(yv %in% c(0, 1))))
    stop("gwasem: a binary trait must be coded 0/1")
  if (!(test %in% c("f", "score")))
    stop("gwasem: test must be 'f' or 'score'")

  K <- if (is.null(kinship)) morie_gwasem_kinship(G) else as.matrix(kinship)
  vc <- morie_gwasem_reml(yv, K, covariates, ml)
  evals <- vc$evals; evecs <- vc$evecs; delta <- vc$delta

  base <- if (is.null(covariates)) matrix(1, n, 1)
          else cbind(1, as.matrix(covariates))
  base_t <- as.matrix(t(evecs) %*% base)
  y_t <- as.numeric(t(evecs) %*% yv)

  beta <- numeric(m); se <- numeric(m)
  stat <- numeric(m); pval <- numeric(m); skipped <- integer(0)
  for (j in seq_len(m)) {
    col <- G[, j]
    p_hat <- sum(col) / (2.0 * n)
    if (min(p_hat, 1 - p_hat) < min_maf || max(col) == min(col)) {
      skipped <- c(skipped, j)
      beta[j] <- NA; se[j] <- NA; stat[j] <- 0; pval[j] <- 1
      next
    }
    if (per_marker_reml) {
      Xfull <- cbind(base, col)
      vcj <- morie_gwasem_reml(yv, K, covariates, ml)
      dj <- vcj$delta; ev <- vcj$evals
      rot <- as.matrix(t(vcj$evecs) %*% Xfull)
      yr <- as.numeric(t(vcj$evecs) %*% yv)
      d <- ev + dj
    } else {
      col_t <- as.numeric(t(evecs) %*% col)
      rot <- cbind(base_t, col_t)
      yr <- y_t; d <- evals + delta
    }
    p <- ncol(rot)
    M <- matrix(0, p, p)
    for (a in seq_len(p)) for (b in seq_len(p))
      M[a, b] <- sum(rot[, a] * rot[, b] / d)
    v <- numeric(p)
    for (a in seq_len(p)) v[a] <- sum(rot[, a] * yr / d)
    bb <- tryCatch(.gwasem_solve(M, v), error = function(e) NULL)
    inv <- tryCatch(.gwasem_inv(M), error = function(e) NULL)
    if (is.null(bb) || is.null(inv)) {
      skipped <- c(skipped, j)
      beta[j] <- NA; se[j] <- NA; stat[j] <- 0; pval[j] <- 1
      next
    }
    rss <- sum((yr - as.numeric(rot %*% bb))^2 / d)
    df <- n - p; s2 <- rss / df
    b_k <- bb[p]
    var_k <- s2 * inv[p, p]
    se_k <- sqrt(max(var_k, 0))
    beta[j] <- b_k; se[j] <- se_k
    if (test == "f") {
      f <- if (var_k > 0) b_k * b_k / var_k else 0
      stat[j] <- f; pval[j] <- .gwasem_f_sf(f, 1, df)
    } else {
      p0 <- p - 1
      M0 <- M[seq_len(p0), seq_len(p0), drop = FALSE]
      v0 <- v[seq_len(p0)]
      b0 <- .gwasem_solve(M0, v0)
      r0 <- yr - as.numeric(rot[, seq_len(p0), drop = FALSE] %*% b0)
      s20 <- sum(r0^2 / d) / (n - p0)
      vx <- numeric(p0)
      for (a in seq_len(p0)) vx[a] <- sum(rot[, a] * rot[, p] / d)
      cx <- .gwasem_solve(M0, vx)
      xres <- rot[, p] - as.numeric(rot[, seq_len(p0), drop = FALSE] %*% cx)
      num <- sum(xres * r0 / d)
      den <- sum(xres * xres / d) * s20
      chi <- if (den > 0) num^2 / den else 0
      stat[j] <- chi; pval[j] <- .gwasem_norm_sf(sqrt(max(chi, 0)))
    }
  }
  tested <- setdiff(seq_len(m), skipped)
  list(estimate = beta, beta = beta, se = se, stat = stat, pvalue = pval,
       variance_components = vc,
       pseudo_heritability = vc$pseudo_heritability,
       lambda_gc = if (length(tested) > 0)
         morie_gwasem_gc(stat[tested]) else NaN,
       skipped = skipped, n = n, n_markers = m, test = test, trait = trait,
       per_marker_reml = per_marker_reml,
       note = paste0("the variance components are estimated ONCE under ",
                     "the null (that is what makes it EMMAX rather than ",
                     "EMMA); per_marker_reml=TRUE restores the exact model"),
       method = "EMMAX variance component association (Kang et al. 2010)")
}
