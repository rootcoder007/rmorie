# SPDX-License-Identifier: AGPL-3.0-or-later
# R arm of varqc1 -- variant quality filtering: GATK hard filters and a
# VQSR-style Gaussian-mixture recalibration. Mirrors
# src/morie/fn/varqc1.py operation for operation, on the shared numerics
# in R/aaa_helpers_w3num.R and the generator in R/aaa_helpers_ghc_rng.R.
#
# A variant caller emits everything it can defend and leaves the
# deciding to you. Two ways of deciding are implemented, and they fail
# in opposite directions, which is why both are here.
#
# HARD FILTERING applies a fixed threshold to each annotation
# independently. Transparent, needs no training data, and what you use
# when there are too few variants to fit anything. Its weakness is that
# it treats the annotations as independent: a variant slightly over the
# line on two annotations at once is no worse off than one slightly over
# on a single annotation, even though the joint evidence against it is
# far stronger.
#
# The GATK recommendations are the defaults, and they differ between
# SNPs and indels because the annotations do:
#
#   SNP    QD < 2.0, QUAL < 30.0, SOR > 3.0, FS > 60.0, MQ < 40.0,
#          MQRankSum < -12.5, ReadPosRankSum < -8.0
#   INDEL  QD < 2.0, QUAL < 30.0, FS > 200.0, SOR > 10.0,
#          ReadPosRankSum < -20.0
#
# The indel thresholds on FS and SOR are far looser because indel calls
# carry more strand and position skew for reasons that are not artefacts
# -- applying the SNP numbers to indels throws away real variants, which
# is the single most common way this gets done wrong.
#
# VQSR fits the joint distribution instead. A Gaussian mixture is
# trained on variants believed good, a second on variants believed bad,
# and each variant is scored by the log ratio of the two densities --
# VQSLOD. Because it is a joint model it can accept a variant marginal
# on one annotation and excellent on the others, exactly the case hard
# filtering handles worst. Its weakness is the mirror image: it needs a
# training set, and a bad one produces a confident, wrong ranking.
#
# Tranches turn the score into a decision. Sort the TRAINING-POSITIVE
# variants by VQSLOD; the threshold retaining 99% of them defines the
# 99.0 tranche. A tranche is a statement about sensitivity to known
# variants, not about the number of calls kept, and reading it as the
# latter is a mistake worth naming.
#
# The mixture is fitted by EM with full or diagonal covariances.
# Diagonal is not merely cheaper: with a handful of training variants a
# full covariance is singular, and the module says so rather than
# inverting something it should not.
#
# References
#   Broad Institute, GATK Best Practices: "Hard-filtering germline short
#     variants," the source of the threshold values above.
#   DePristo, M.A. et al. (2011) "A framework for variation discovery
#     and genotyping using next-generation DNA sequencing data." Nature
#     Genetics 43(5), 491-498.
#   Van der Auwera, G.A. et al. (2013) "From FastQ data to
#     high-confidence variant calls." Current Protocols in
#     Bioinformatics 43, 11.10.1-11.10.33.
#   Dempster, A.P., Laird, N.M. and Rubin, D.B. (1977) JRSS B 39(1),
#     1-38.

.VARQC1_METHODS <- c("hard", "vqsr", "both")
.VARQC1_COVARIANCES <- c("full", "diagonal")

# Each entry is annotation, direction, cutoff. Direction "lt" means a
# variant FAILS when the value is below the cutoff.
.VARQC1_DEFAULTS <- list(
  snp = list(c("QD", "lt", "2"), c("QUAL", "lt", "30"), c("SOR", "gt", "3"),
             c("FS", "gt", "60"), c("MQ", "lt", "40"),
             c("MQRankSum", "lt", "-12.5"),
             c("ReadPosRankSum", "lt", "-8")),
  indel = list(c("QD", "lt", "2"), c("QUAL", "lt", "30"),
               c("FS", "gt", "200"), c("SOR", "gt", "10"),
               c("ReadPosRankSum", "lt", "-20")))

#' Apply each hard threshold independently
#'
#' A missing annotation does NOT fail its filter. A caller omits an
#' annotation when it could not be computed, and treating "not measured"
#' as "measured and bad" would discard exactly the variants with unusual
#' read support -- which is where real novel variation lives.
#'
#' @param records Annotation matrix, one row per variant.
#' @param fields Column names.
#' @param thresholds A list of annotation, direction, cutoff triples.
#' @return A list with one FILTER string per record and the counts.
#' @export
morie_varqc1_hard <- function(records, fields, thresholds) {
  n <- nrow(records)
  out <- character(n)
  for (i in seq_len(n)) {
    failed <- character(0)
    for (tr in thresholds) {
      ann <- tr[1]; direction <- tr[2]; cut <- as.numeric(tr[3])
      j <- match(ann, fields)
      if (is.na(j)) next
      v <- records[i, j]
      if (is.na(v)) next
      bad <- if (direction == "lt") v < cut else v > cut
      if (bad)
        failed <- c(failed, sprintf("%s%s%g", ann,
                                    if (direction == "lt") "<" else ">", cut))
    }
    out[i] <- if (length(failed)) paste(failed, collapse = ";") else "PASS"
  }
  lv <- sort(unique(out))
  cnt <- vapply(lv, function(k) sum(out == k), integer(1))
  names(cnt) <- lv
  list(filter = out, counts = cnt)
}

#' log sum_k w_k N(x; mu_k, L_k L_k'), from the Cholesky factors
#'
#' @param x The point.
#' @param weights Mixture weights.
#' @param means Component means, a list of vectors.
#' @param chols Component Cholesky factors, a list of matrices.
#' @return The log density.
#' @export
morie_varqc1_logpdf <- function(x, weights, means, chols) {
  d <- length(x)
  terms <- numeric(length(weights))
  for (k in seq_along(weights)) {
    L <- chols[[k]]
    z <- numeric(d)
    for (i in seq_len(d)) {
      acc <- if (i > 1L) .w3_dot(L[i, seq_len(i - 1L)], z[seq_len(i - 1L)]) else 0
      z[i] <- (x[i] - means[[k]][i] - acc) / L[i, i]
    }
    logdet <- 2 * .w3_csum(vapply(seq_len(d), function(i) log(L[i, i]),
                                  numeric(1)))
    q <- .w3_csum(z * z)
    terms[k] <- log(weights[k]) - 0.5 * q - 0.5 * logdet -
      0.5 * d * log(2 * pi)
  }
  .w3_logsumexp(terms)
}

#' EM for a Gaussian mixture, returning Cholesky factors
#'
#' Initialised by assigning point i to component i mod K -- a
#' deterministic split, not a random one, so the fit does not move with
#' the seed unless the seed is actually used. The seed is retained
#' because a jittered restart is the usual escape from a degenerate
#' component and it must be reproducible when it happens.
#'
#' @param X Training matrix.
#' @param n_components Number of components.
#' @param n_iter EM iterations.
#' @param seed Seed for the generator shared with the Python arm.
#' @param covariance "full" or "diagonal".
#' @param min_variance Floor on each variance.
#' @param jitter Added to the diagonal.
#' @param shrinkage Each component covariance is pulled towards the
#'   diagonal of the whole training set's covariance by this fraction.
#'   That is not cosmetic: with a handful of training variants in seven
#'   dimensions the per-component covariance is very nearly singular,
#'   its Cholesky has a diagonal entry near zero, and the quadratic form
#'   blows up -- turning a VQSLOD, a log RATIO that should sit in single
#'   digits, into something of order 1e9 and letting the last bits of
#'   the fit decide the answer. An absolute variance floor does not
#'   help, because it is a floor in the annotation's own units and every
#'   annotation has a different scale. Shrinking towards the data's own
#'   diagonal is scale-free and is the standard remedy.
#' @return A list with the weights, means, Cholesky factors and the
#'   log-likelihood trace.
#' @export
morie_varqc1_mixture <- function(X, n_components = 2L, n_iter = 50L, seed = 1,
                                 covariance = "full", min_variance = 1e-6,
                                 jitter = 1e-8, shrinkage = 0.05) {
  if (!(covariance %in% .VARQC1_COVARIANCES))
    stop("covariance must be one of ",
         paste(.VARQC1_COVARIANCES, collapse = ", "))
  n <- nrow(X); d <- ncol(X)
  K <- as.integer(n_components)
  if (K < 1L) stop("need at least one component")
  if (n < K * (d + 1L))
    stop("too few training variants for ", K, " components in ", d,
         " dimensions; use a diagonal covariance or fewer components")
  e <- .ghc_rng(seed)
  gmean <- vapply(seq_len(d), function(j) .w3_csum(X[, j]) / n, numeric(1))
  gvar <- vapply(seq_len(d), function(j)
    .w3_csum((X[, j] - gmean[j]) * (X[, j] - gmean[j])) / n, numeric(1))
  for (j in seq_len(d)) if (gvar[j] < min_variance) gvar[j] <- min_variance
  lam <- as.numeric(shrinkage)
  if (!(lam >= 0 && lam < 1)) stop("shrinkage must lie in [0, 1)")
  resp <- matrix(0, n, K)
  for (i in seq_len(n)) resp[i, ((i - 1L) %% K) + 1L] <- 1
  weights <- rep(1 / K, K)
  means <- lapply(seq_len(K), function(k) numeric(d))
  chols <- lapply(seq_len(K), function(k) matrix(0, d, d))
  ll_trace <- numeric(0)
  for (it in seq_len(as.integer(n_iter))) {
    for (k in seq_len(K)) {
      nk <- .w3_csum(resp[, k])
      if (nk <= 1e-12) {
        # A component that lost every point is restarted at a jittered
        # overall mean rather than left singular.
        nk <- 1e-12
        means[[k]] <- vapply(seq_len(d), function(j)
          .w3_csum(X[, j]) / n + 1e-3 * .ghc_norm(e, 1L), numeric(1))
        cov <- diag(gvar, d, d)
      } else {
        means[[k]] <- vapply(seq_len(d), function(j)
          .w3_csum(resp[, k] * X[, j]) / nk, numeric(1))
        cov <- matrix(0, d, d)
        for (a in seq_len(d)) for (b in seq_len(d)) {
          if (covariance == "diagonal" && a != b) next
          cov[a, b] <- .w3_csum(resp[, k] * (X[, a] - means[[k]][a]) *
                                  (X[, b] - means[[k]][b])) / nk
        }
        for (a in seq_len(d)) {
          for (b in seq_len(d)) cov[a, b] <- cov[a, b] * (1 - lam)
          cov[a, a] <- cov[a, a] + lam * gvar[a]
          if (cov[a, a] < min_variance) cov[a, a] <- min_variance
          cov[a, a] <- cov[a, a] + jitter
        }
      }
      weights[k] <- nk / n
      chols[[k]] <- .w3_chol(cov)
    }
    s <- .w3_csum(weights)
    weights <- weights / s
    ll <- 0
    for (i in seq_len(n)) {
      lp <- vapply(seq_len(K), function(k)
        morie_varqc1_logpdf(X[i, ], weights[k], means[k], chols[k]),
        numeric(1))
      tot <- .w3_logsumexp(lp)
      ll <- ll + tot
      resp[i, ] <- exp(lp - tot)
    }
    ll_trace <- c(ll_trace, ll)
  }
  list(weights = weights, means = means, chols = chols,
       loglik = ll_trace[length(ll_trace)], loglik_trace = ll_trace,
       covariance = covariance, n_components = K)
}

#' Filter variant records by hard thresholds, by VQSR, or by both
#'
#' @param vcf Annotation matrix, one row per variant.
#' @param thresholds Triples of annotation, direction and cutoff; the
#'   GATK recommendations for the mode when omitted.
#' @param fields Column names. Required.
#' @param mode "snp" or "indel", which selects the default thresholds.
#' @param method "hard", "vqsr" or "both".
#' @param positive Row indices of the training-positive set (one-based).
#' @param negative Row indices of the training-negative set.
#' @param n_components Mixture components.
#' @param n_iter EM iterations.
#' @param seed Seed for the generator shared with the Python arm.
#' @param covariance "full" or "diagonal".
#' @param tranches Target sensitivities to the training-positive set.
#' @param vqsr_fields Which annotations the mixture uses.
#' @param min_variance Floor on each variance.
#' @param shrinkage Pulls each component covariance towards the diagonal
#'   of the training set's own covariance, which is what keeps a
#'   seven-dimensional fit on twenty variants from being singular.
#' @return A list with per-record FILTER strings and counts, and for the
#'   VQSR routes the VQSLOD score, the tranche and the fitted mixtures.
#' @export
morie_varqc1 <- function(vcf, thresholds = NULL, fields = NULL, mode = "snp",
                         method = "hard", positive = NULL, negative = NULL,
                         n_components = 2L, n_iter = 50L, seed = 1,
                         covariance = "full",
                         tranches = c(90, 99, 99.9, 100),
                         vqsr_fields = NULL, min_variance = 1e-6,
                         shrinkage = 0.05) {
  if (!(method %in% .VARQC1_METHODS))
    stop("method must be one of ", paste(.VARQC1_METHODS, collapse = ", "))
  if (!(mode %in% names(.VARQC1_DEFAULTS))) stop("mode must be snp or indel")
  if (is.null(fields)) stop("fields (the column names) is required")
  fields <- as.character(fields)
  recs <- as.matrix(vcf)
  storage.mode(recs) <- "double"
  n <- nrow(recs)
  if (n < 1L) stop("no records")
  thr <- if (is.null(thresholds)) .VARQC1_DEFAULTS[[mode]] else
    lapply(thresholds, function(t) c(as.character(t[1]), as.character(t[2]),
                                     as.character(t[3])))

  hf <- morie_varqc1_hard(recs, fields, thr)
  res <- list(filter = hf$filter, counts = hf$counts, n = n, mode = mode,
              method = method,
              thresholds = lapply(thr, function(t) c(t[1], t[2], t[3])),
              n_pass_hard = sum(hf$filter == "PASS"), fields = fields,
              method_name = "variant quality filtering")

  if (method == "hard") {
    res$estimate <- res$n_pass_hard / n
    res$se <- sqrt(res$estimate * (1 - res$estimate) / n)
    return(res)
  }

  if (is.null(positive) || is.null(negative))
    stop("the VQSR routes need positive and negative training indices")
  use <- if (is.null(vqsr_fields)) fields else as.character(vqsr_fields)
  cols <- match(use, fields)
  for (i in seq_len(n)) for (cc in cols)
    if (is.na(recs[i, cc]))
      stop("record ", i - 1L, " is missing annotation ", fields[cc],
           ", which the mixture cannot use; drop the record or the annotation")
  X <- recs[, cols, drop = FALSE]
  good <- morie_varqc1_mixture(X[positive, , drop = FALSE], n_components,
                               n_iter, seed, covariance, min_variance,
                               shrinkage = shrinkage)
  bad <- morie_varqc1_mixture(X[negative, , drop = FALSE], n_components,
                              n_iter, seed, covariance, min_variance,
                              shrinkage = shrinkage)

  ln10 <- log(10)
  lod <- vapply(seq_len(n), function(i)
    (morie_varqc1_logpdf(X[i, ], good$weights, good$means, good$chols) -
       morie_varqc1_logpdf(X[i, ], bad$weights, bad$means, bad$chols)) / ln10,
    numeric(1))

  # Tranches: the VQSLOD cut retaining the stated percentage of the
  # TRAINING-POSITIVE variants. A tranche is a sensitivity to known
  # variants, not a count of calls kept.
  ps <- sort(lod[positive], decreasing = TRUE)
  m <- length(ps)
  tvals <- as.numeric(tranches)
  cutv <- numeric(length(tvals))
  for (j in seq_along(tvals)) {
    keep <- ceiling(tvals[j] / 100 * m)
    if (keep < 1) keep <- 1
    if (keep > m) keep <- m
    cutv[j] <- ps[keep]
  }
  o <- order(-cutv, tvals)
  tvals <- tvals[o]; cutv <- cutv[o]

  tranche <- character(n)
  for (i in seq_len(n)) {
    lab <- "FAIL"
    for (j in seq_along(tvals))
      if (lod[i] >= cutv[j]) { lab <- sprintf("%.1f", tvals[j]); break }
    tranche[i] <- lab
  }

  res$vqslod <- lod
  res$tranche <- tranche
  res$tranche_cuts <- lapply(seq_along(tvals), function(j) c(tvals[j], cutv[j]))
  res$good_model <- list(weights = good$weights, means = good$means,
                         loglik = good$loglik)
  res$bad_model <- list(weights = bad$weights, means = bad$means,
                        loglik = bad$loglik)
  res$good_loglik_trace <- good$loglik_trace
  res$bad_loglik_trace <- bad$loglik_trace
  res$covariance <- covariance
  res$vqsr_fields <- use

  if (method == "both") {
    combined <- character(n)
    for (i in seq_len(n))
      combined[i] <- if (hf$filter[i] != "PASS") hf$filter[i]
      else if (tranche[i] == "FAIL") "VQSRFail" else "PASS"
    res$filter <- combined
  } else {
    res$filter <- ifelse(tranche != "FAIL", "PASS", "VQSRFail")
  }
  lv <- sort(unique(res$filter))
  cnt <- vapply(lv, function(k) sum(res$filter == k), integer(1))
  names(cnt) <- lv
  res$counts <- cnt
  res$n_pass <- sum(res$filter == "PASS")
  res$estimate <- res$n_pass / n
  res$se <- sqrt(res$estimate * (1 - res$estimate) / n)
  res
}

#' One-line summary of the varqc1 module
#'
#' @return A character scalar.
#' @export
morie_varqc1_cheatsheet <- function()
  paste0("varqc1: variant quality filtering. methods ",
         paste(.VARQC1_METHODS, collapse = ", "), "; covariances ",
         paste(.VARQC1_COVARIANCES, collapse = ", "))
