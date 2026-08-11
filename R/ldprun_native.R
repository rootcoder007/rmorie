# SPDX-License-Identifier: AGPL-3.0-or-later
#
# LD-based SNP pruning, PLINK --indep-pairwise scheme (Ldprun).
# Bit-identical mirror of src/morie/fn/ldprun.py.

#' LD-based variant pruning (PLINK indep-pairwise scheme)
#'
#' A window of \code{window} variants slides along the variant index in
#' increments of \code{step}. Within the current window the squared
#' Pearson correlation of genotype allele counts is computed for every
#' pair of still-kept variants, and pairs with r-squared above the
#' threshold are greedily pruned until no such pair remains (PLINK 1.9
#' LD documentation; phase is not considered).
#'
#' The published description leaves the pair-member choice
#' unspecified, so this implementation pins a deterministic rule,
#' identical in both language arms: offending pairs are scanned in
#' row-major index order (i ascending, then j), and of the first
#' offending pair the member with the LOWER minor allele frequency is
#' removed (the later variant on ties). Removal is permanent across
#' subsequent windows.
#'
#' @param G Genotype matrix, individuals by variants, coded 0/1/2; any
#'   other value is treated as missing (pairwise deletion within a
#'   pair, per-variant deletion for MAF).
#' @param window Window size in variant count.
#' @param step Window shift in variant count.
#' @param r2_threshold Pairwise r-squared above which one member of a
#'   pair is pruned.
#' @return List with \code{keep} (1-based kept variant indices),
#'   \code{drop} (removed), \code{estimate} (number kept), \code{maf},
#'   \code{n_variants}, \code{method}.
#' @references Purcell, S., Neale, B., et al. (2007). PLINK: a tool
#'   set for whole-genome association and population-based linkage
#'   analyses. American Journal of Human Genetics 81(3), 559-575
#'   (sec. Linkage disequilibrium based SNP pruning). PLINK 1.9 LD
#'   documentation, indep-pairwise, cog-genomics.org/plink/1.9/ld
#'   (fetched 2026-08-09).
#' @export
Ldprun <- function(G, window = 50L, step = 5L, r2_threshold = 0.5) {
  Gm <- as.matrix(G)
  storage.mode(Gm) <- "double"
  n <- nrow(Gm); m <- ncol(Gm)
  if (n == 0L) stop("empty genotype matrix", call. = FALSE)
  window <- as.integer(window); step <- as.integer(step)
  if (window < 2L || step < 1L) stop("need window >= 2 and step >= 1", call. = FALSE)
  thr <- as.numeric(r2_threshold)
  if (!(thr > 0 && thr <= 1)) stop("r2_threshold must be in (0, 1]", call. = FALSE)

  valid <- function(v) v %in% c(0, 1, 2)
  r2_geno <- function(x, y) {
    ok <- valid(x) & valid(y)
    a <- x[ok]; b <- y[ok]
    if (length(a) < 2L) return(NaN)
    mx <- mean(a); my <- mean(b)
    sxy <- sum((a - mx) * (b - my))
    sxx <- sum((a - mx)^2); syy <- sum((b - my)^2)
    if (sxx <= 0 || syy <= 0) return(NaN)
    (sxy * sxy) / (sxx * syy)
  }
  maf1 <- function(x) {
    obs <- x[valid(x)]
    if (length(obs) == 0L) return(0)
    p <- sum(obs) / (2 * length(obs))
    min(p, 1 - p)
  }
  mafs <- vapply(seq_len(m), function(j) maf1(Gm[, j]), numeric(1))
  removed <- rep(FALSE, m)
  start <- 0L
  repeat {
    end <- min(start + window, m)
    active <- setdiff(seq.int(start + 1L, end), which(removed))
    repeat {
      offender <- NULL
      na <- length(active)
      if (na >= 2L) {
        for (ai in seq_len(na - 1L)) {
          for (bi in seq.int(ai + 1L, na)) {
            i <- active[ai]; j <- active[bi]
            r2 <- r2_geno(Gm[, i], Gm[, j])
            if (!is.nan(r2) && r2 > thr) { offender <- c(i, j); break }
          }
          if (!is.null(offender)) break
        }
      }
      if (is.null(offender)) break
      i <- offender[1]; j <- offender[2]
      drop <- if (mafs[j] <= mafs[i]) j else i
      removed[drop] <- TRUE
      active <- active[active != drop]
    }
    if (end >= m) break
    start <- start + step
  }
  keep <- which(!removed)
  list(
    estimate = as.numeric(length(keep)),
    keep = as.integer(keep), drop = as.integer(which(removed)),
    maf = mafs, n_variants = as.integer(m),
    method = "LD pruning, PLINK --indep-pairwise (lower-MAF member dropped)")
}
