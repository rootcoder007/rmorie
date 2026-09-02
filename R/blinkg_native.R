# SPDX-License-Identifier: AGPL-3.0-or-later
# R arm of blinkg -- BLINK, iterative fixed-effect GWAS with LD-filtered
# pseudo-QTNs. Mirrors src/morie/fn/blinkg.py operation for operation, on
# the shared numerics in R/aaa_helpers_w3num.R.
#
# A genome scan that tests one marker at a time against a null of no
# association has two well-known problems. Population structure and
# relatedness inflate the test, and a real signal leaks into every marker
# in linkage disequilibrium with it, so one causal variant produces a
# plateau of significance rather than a peak. The mixed-model answer puts
# a kinship random effect in the model and pays for it with a variance
# component estimated by restricted maximum likelihood at every step.
#
# BLINK is the fixed-effect answer. The relatedness that the random
# effect was carrying is instead absorbed by a handful of markers --
# pseudo-QTNs -- carried as covariates, and the variance component is
# replaced by a model-selection criterion. Two models alternate: the
# SCAN, which tests every marker in turn with the current pseudo-QTNs as
# covariates, dropping a marker from the covariate set exactly while it
# is itself under test because a variable cannot be its own control; and
# the SELECTION, which re-chooses the pseudo-QTNs from that scan.
#
# The selection is what gives BLINK its name. Markers are sorted by p
# value and the ones above a Bonferroni threshold are discarded. The most
# significant survivor is taken; every marker whose correlation with it
# exceeds a threshold -- 0.7 in the paper -- is dropped; the most
# significant of what remains is taken next; and so on. This replaces
# FarmCPU's fixed genomic bins, which is the older route and is kept here
# because a bin is the right tool when marker positions are known and
# correlation is not: it never drops a distant marker that happens to
# correlate by chance. Both are selectable and the choice travels in the
# result.
#
# How MANY of those markers to keep is then a model-selection question,
# and BLINK answers it with a criterion rather than a variance
# component: fit the first k of them, for k running from one to all of
# them, and take the k minimising BIC = 2(-log likelihood) + k log n.
#
# The whole thing iterates until the pseudo-QTN set stops changing. That
# it stops is not guaranteed by anything -- it is a fixed point of a
# discrete map -- so the number of iterations and whether it actually
# settled are both reported rather than assumed.
#
# References
#   Huang, M., Liu, X., Zhou, Y., Summers, R.M. and Zhang, Z. (2019)
#     "BLINK: a package for the next level of genome-wide association
#     studies with both individuals and markers in the millions."
#     GigaScience 8(2), giy154. doi:10.1093/gigascience/giy154. The LD
#     filter with its 0.7 threshold and the Bonferroni pre-filter at
#     alpha = 0.01, the BIC = 2(-LL) + k log n selection, and the
#     iteration to a stable pseudo-QTN set.
#   Liu, X., Huang, M., Fan, B., Buckler, E.S. and Zhang, Z. (2016)
#     "Iterative usage of fixed and random effect models for powerful
#     and efficient genome-wide association studies." PLoS Genetics
#     12(2), e1005767. FarmCPU, the bin-based predecessor.
#   Devlin, B. and Roeder, K. (1999) "Genomic control for association
#     studies." Biometrics 55(4), 997-1004.

.BLINKG_SELECTIONS <- c("ld", "bin")
.BLINKG_CRITERIA <- c("bic", "aic", "none")

# The paper's defaults: markers correlated above this with an already
# chosen pseudo-QTN are dropped, and the Bonferroni pre-filter runs at
# this alpha.
.BLINKG_LD_THRESHOLD <- 0.7
.BLINKG_ALPHA <- 0.01

# The median of a chi-square on one degree of freedom. Genomic control
# divides the observed median by this, so it is a constant of the method
# and not a fitted quantity.
.BLINKG_CHISQ1_MEDIAN <- 0.45493642311957283

# Pearson correlation, compensated, zero when either side is constant.
#' Pearson correlation, compensated, zero when either side is constant
#'
#' A step of the blinkg_native implementation. Called by \code{morie_blinkg_ld_filter}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a A vector; its length is taken.
#' @param b Numeric; passed to \code{.w3_csum}.
#' @return A numeric value.
#' @export
.blinkg_corr <- function(a, b) {
  n <- length(a)
  ma <- .w3_csum(a) / n
  mb <- .w3_csum(b) / n
  saa <- .w3_csum((a - ma) * (a - ma))
  sbb <- .w3_csum((b - mb) * (b - mb))
  if (saa <= 0 || sbb <= 0) return(0)
  sab <- .w3_csum((a - ma) * (b - mb))
  sab / sqrt(saa * sbb)
}

# Intercept, fixed covariates, then the given genotype columns.
#' Intercept, fixed covariates, then the given genotype columns
#'
#' A step of the blinkg_native implementation. Called by \code{morie_blinkg_scan}, \code{morie_blinkg_select}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n A count; the body uses it as \code{matrix(...)}.
#' @param covars A vector; its length is taken.
#' @param cols A vector; its length is taken.
#' @return The value of \code{d}, as built in the body.
#' @export
.blinkg_design <- function(n, covars, cols) {
  d <- matrix(1, n, 1L + length(covars) + length(cols))
  k <- 1L
  for (cc in covars) { k <- k + 1L
  d[, k] <- cc }
  for (cc in cols) { k <- k + 1L
  d[, k] <- cc }
  d
}

#' Test every marker with the current pseudo-QTNs as covariates
#'
#' A marker with no variation, or one whose design is rank deficient
#' once the covariates are in, gets a p value of NaN rather than a
#' fabricated number.
#'
#' @param y The phenotype.
#' @param geno A list of marker vectors, one value per individual.
#' @param covars A list of fixed covariate vectors, or NULL.
#' @param qtn Indices of the current pseudo-QTNs, one-based.
#' @return A list with the effect, its standard error, the t statistic
#'   and the two sided p value for each marker.
#' @export
morie_blinkg_scan <- function(y, geno, covars = NULL, qtn = integer(0)) {
  n <- length(y)
  m <- length(geno)
  if (is.null(covars)) covars <- list()
  qtn <- as.integer(qtn)
  beta <- numeric(m)
  se <- numeric(m)
  tt <- numeric(m)
  pv <- numeric(m)
  for (j in seq_len(m)) {
    # A pseudo-QTN cannot be its own control, so it comes out of the
    # covariate set exactly while it is the marker under test.
    keep <- qtn[qtn != j]
    cols <- c(lapply(keep, function(q) geno[[q]]), list(geno[[j]]))
    d <- .blinkg_design(n, covars, cols)
    p <- ncol(d)
    if (n <= p) {
      beta[j] <- NaN
      se[j] <- NaN
      tt[j] <- NaN
      pv[j] <- NaN
      next
    }
    fit <- try(.w3_ols(y, d), silent = TRUE)
    if (inherits(fit, "try-error")) {
      beta[j] <- NaN
      se[j] <- NaN
      tt[j] <- NaN
      pv[j] <- NaN
      next
    }
    b <- fit$beta[p]
    v <- fit$sigma2 * fit$xtx_inv[p, p]
    if (!isTRUE(v > 0) || is.nan(v)) {
      beta[j] <- b
      se[j] <- NaN
      tt[j] <- NaN
      pv[j] <- NaN
      next
    }
    s <- sqrt(v)
    beta[j] <- b
    se[j] <- s
    tt[j] <- b / s
    pv[j] <- 2 * .w3_t_sf(abs(b / s), fit$df)
  }
  list(beta = beta, se = se, t = tt, p = pv)
}

# Markers sorted by p value, ties broken by index. The tie break matters:
# on a small panel several markers can share a p value to the last bit,
# and a selection that depended on which one the sort happened to put
# first would not be reproducible.
#' Markers sorted by p value, ties broken by index. The tie break
#' matters:
#'
#' on a small panel several markers can share a p value to the last bit,
#' and a selection that depended on which one the sort happened to put
#' first would not be reproducible.
#'
#' @param pv A vector; indexed elementwise.
#' @return The value of \code{[}.
#' @export
.blinkg_order <- function(pv) {
  live <- which(!is.nan(pv))
  if (!length(live)) return(integer(0))
  live[order(pv[live], live)]
}

#' Keep the most significant marker, drop what correlates with it
#'
#' Walks the p-value ordering once. Each surviving marker is compared
#' with every marker already kept, and dropped if the absolute
#' correlation exceeds the threshold. This is BLINK's replacement for
#' FarmCPU's bins.
#'
#' @param geno A list of marker vectors.
#' @param order Marker indices in p-value order.
#' @param threshold Correlation above which a candidate is dropped.
#' @return The kept marker indices, most significant first.
#' @export
morie_blinkg_ld_filter <- function(geno, order,
                                   threshold = .BLINKG_LD_THRESHOLD) {
  kept <- integer(0)
  for (j in order) {
    ok <- TRUE
    for (q in kept)
      if (abs(.blinkg_corr(geno[[j]], geno[[q]])) > threshold) {
        ok <- FALSE
        break
      }
    if (ok) kept <- c(kept, j)
  }
  kept
}

#' One marker per genomic bin, the most significant in the bin
#'
#' FarmCPU's rule. It cannot drop a distant marker that correlates by
#' chance, and it cannot keep two real signals that fall in one bin --
#' which is the trade the LD filter is making.
#'
#' @param order Marker indices in p-value order.
#' @param positions Genomic positions, one per marker.
#' @param bin_size Bin width.
#' @return The kept marker indices, most significant first.
#' @export
morie_blinkg_bin_filter <- function(order, positions, bin_size) {
  if (bin_size <= 0) stop("the bin size must be positive")
  seen <- numeric(0)
  kept <- integer(0)
  for (j in order) {
    b <- floor(positions[j] / bin_size)
    if (b %in% seen) next
    seen <- c(seen, b)
    kept <- c(kept, j)
  }
  kept
}

# Gaussian log likelihood at the least-squares fit.
#' Gaussian log likelihood at the least-squares fit
#'
#' A step of the blinkg_native implementation. Called by \code{morie_blinkg_select}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param rss Numeric; combined arithmetically in the body.
#' @param n Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.blinkg_loglik <- function(rss, n) {
  if (rss <= 0) return(Inf)
  -0.5 * n * (log(2 * pi) + log(rss / n) + 1)
}

#' Choose how many of the ranked candidates to keep
#'
#' Fits the first k of them for k from one to all, scores each fit and
#' takes the best. The criterion counts the pseudo-QTNs, as the paper
#' writes it, and not the intercept or the fixed covariates -- those are
#' in every model being compared, so they cannot separate them.
#'
#' @param y The phenotype.
#' @param geno A list of marker vectors.
#' @param candidates Ranked candidate marker indices.
#' @param covars A list of fixed covariate vectors, or NULL.
#' @param criterion A member of the criterion list.
#' @return A list with the chosen indices, the score path and the chosen
#'   count.
#' @export
morie_blinkg_select <- function(y, geno, candidates, covars = NULL,
                                criterion = "bic") {
  if (!(criterion %in% .BLINKG_CRITERIA))
    stop("criterion must be one of ",
         paste(.BLINKG_CRITERIA, collapse = ", "))
  n <- length(y)
  if (is.null(covars)) covars <- list()
  if (criterion == "none" || !length(candidates))
    return(list(qtn = candidates, scores = numeric(0),
                k = length(candidates)))
  scores <- numeric(0)
  best <- NA_real_
  best_k <- 0L
  for (k in seq_along(candidates)) {
    cols <- lapply(candidates[seq_len(k)], function(q) geno[[q]])
    d <- .blinkg_design(n, covars, cols)
    if (n <= ncol(d)) {
      scores <- c(scores, Inf)
      next
    }
    fit <- try(.w3_ols(y, d), silent = TRUE)
    if (inherits(fit, "try-error")) {
      scores <- c(scores, Inf)
      next
    }
    ll <- .blinkg_loglik(fit$rss, n)
    pen <- if (criterion == "bic") log(as.numeric(n)) else 2
    s <- 2 * (-ll) + k * pen
    scores <- c(scores, s)
    if (is.na(best) || s < best) {
      best <- s
      best_k <- k
    }
  }
  list(qtn = if (best_k > 0L) candidates[seq_len(best_k)] else integer(0),
       scores = scores, k = best_k)
}

#' Iterate the scan and the pseudo-QTN selection to a fixed point
#'
#' @param y The phenotype.
#' @param geno A list of marker vectors, one value per individual.
#' @param positions Genomic positions, needed only by the bin selection.
#' @param covars Fixed covariates -- principal components, say -- carried
#'   in every model, or NULL.
#' @param selection A member of the selection list.
#' @param criterion A member of the criterion list.
#' @param ld_threshold Correlation above which a candidate is dropped.
#' @param alpha The Bonferroni level for the pre-filter.
#' @param bin_size Bin width for the bin selection.
#' @param max_iter Iteration cap. Reaching it is reported, not hidden.
#' @return A list with the final scan, the pseudo-QTNs and how they were
#'   chosen, the criterion path, the iteration count and whether the set
#'   settled, and the genomic inflation factor.
#' @export
morie_blinkg <- function(y, geno, positions = NULL, covars = NULL,
                         selection = "ld", criterion = "bic",
                         ld_threshold = .BLINKG_LD_THRESHOLD,
                         alpha = .BLINKG_ALPHA, bin_size = NULL,
                         max_iter = 10L) {
  if (!(selection %in% .BLINKG_SELECTIONS))
    stop("selection must be one of ",
         paste(.BLINKG_SELECTIONS, collapse = ", "))
  if (!(criterion %in% .BLINKG_CRITERIA))
    stop("criterion must be one of ",
         paste(.BLINKG_CRITERIA, collapse = ", "))
  ys <- as.numeric(y)
  n <- length(ys)
  if (n < 3L) stop("need at least three individuals")
  g <- lapply(geno, as.numeric)
  m <- length(g)
  if (m < 1L) stop("need at least one marker")
  if (any(vapply(g, length, integer(1)) != n))
    stop("every marker must have one value per individual")
  if (selection == "bin") {
    if (is.null(positions))
      stop("the bin selection needs marker positions")
    positions <- as.numeric(positions)
    if (length(positions) != m)
      stop("positions must have one entry per marker")
    if (is.null(bin_size)) stop("the bin selection needs a bin size")
  }
  cv <- if (is.null(covars)) list() else lapply(covars, as.numeric)
  thr <- as.numeric(alpha) / m

  qtn <- integer(0)
  scan <- NULL
  scores <- numeric(0)
  cand <- integer(0)
  it <- 0L
  converged <- FALSE
  for (it in seq_len(as.integer(max_iter))) {
    scan <- morie_blinkg_scan(ys, g, cv, qtn)
    o <- .blinkg_order(scan$p)
    o <- o[scan$p[o] < thr]
    cand <- if (selection == "ld")
      morie_blinkg_ld_filter(g, o, as.numeric(ld_threshold))
    else morie_blinkg_bin_filter(o, positions, as.numeric(bin_size))
    sel <- morie_blinkg_select(ys, g, cand, cv, criterion)
    scores <- sel$scores
    if (length(sel$qtn) == length(qtn) && all(sel$qtn == qtn)) {
      converged <- TRUE
      qtn <- sel$qtn
      break
    }
    qtn <- sel$qtn
  }
  if (!converged) {
    # One last scan so the reported p values belong to the reported
    # pseudo-QTN set rather than to the previous one.
    scan <- morie_blinkg_scan(ys, g, cv, qtn)
  }

  chi <- sort((scan$t * scan$t)[!is.nan(scan$t)], method = "radix")
  lam <- NaN
  if (length(chi)) {
    h <- length(chi) %/% 2L
    med <- if (length(chi) %% 2L == 1L) chi[h + 1L]
           else 0.5 * (chi[h] + chi[h + 1L])
    lam <- med / .BLINKG_CHISQ1_MEDIAN
  }

  sig <- which(!is.nan(scan$p) & scan$p < thr)
  live_p <- scan$p[!is.nan(scan$p)]
  live_se <- scan$se[!is.nan(scan$se)]
  # Marker indices are one-based inside this arm and zero-based on the
  # way out, because the reported index is the Python arm's index and a
  # reader comparing the two must see the same number.
  list(p = scan$p, beta = scan$beta, se = scan$se, t = scan$t,
       qtn = qtn - 1L, candidates = cand - 1L, criterion_path = scores,
       n_qtn = length(qtn), significant = sig - 1L,
       n_significant = length(sig), threshold = thr,
       lambda_gc = lam, iterations = it, converged = converged,
       estimate = if (length(live_p)) min(live_p) else NaN,
       se_min = if (length(live_se)) min(live_se) else NaN,
       n = n, m = m, selection = selection, criterion = criterion,
       ld_threshold = as.numeric(ld_threshold), alpha = as.numeric(alpha),
       method = "BLINK iterative fixed-effect GWAS")
}

#' One-line summary of the blinkg module
#'
#' @return A character scalar.
#' @export
morie_blinkg_cheatsheet <- function()
  paste0("blinkg: BLINK iterative fixed-effect GWAS. selections ",
         paste(.BLINKG_SELECTIONS, collapse = ", "), "; criteria ",
         paste(.BLINKG_CRITERIA, collapse = ", "))
