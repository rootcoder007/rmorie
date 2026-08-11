# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Extended haplotype homozygosity decay (Ehhdec). Bit-identical mirror
# of src/morie/fn/ehhdec.py.

.ehh_groups_hh <- function(keys) {
  cnt <- table(keys)
  sum(cnt * (cnt - 1))
}

.ehh_curve <- function(H, core, carriers) {
  L <- ncol(H)
  n <- length(carriers)
  denom <- n * (n - 1)
  out <- numeric(L)
  for (j in seq_len(L)) {
    lo <- min(j, core); hi <- max(j, core)
    keys <- apply(H[carriers, lo:hi, drop = FALSE], 1, paste, collapse = ",")
    out[j] <- .ehh_groups_hh(keys) / denom
  }
  out
}

#' Extended haplotype homozygosity (EHH) decay
#'
#' For the chromosomes carrying core allele a (n_a of them), EHH at
#' marker x is the probability that two distinct chromosomes drawn
#' from that set are identical at every SNP between the core and x
#' inclusive: \eqn{EHH_a(x) = \sum_k C(n_k, 2) / C(n_a, 2)} over the
#' identical-haplotype groups of sizes n_k (Sabeti et al. 2002, as
#' printed in Sabeti et al. 2007 Methods; rehh vignette eq. 3.1). The
#' site-wise curve over ALL N chromosomes (the EHH used by XP-EHH) is
#' also returned: \eqn{EHHS(x) = \sum_i C(n_i, 2) / C(N, 2)}.
#'
#' @param hap Phased 0/1 haplotype matrix, chromosomes by SNPs.
#' @param core Core SNP index (1-based).
#' @param positions Optional marker positions; index scale by default.
#' @return List with \code{estimate} (EHH of the derived/1 allele),
#'   \code{ehh1}, \code{ehh0}, \code{ehhs}, \code{positions},
#'   \code{core}, \code{n1}, \code{n0}, \code{n}, \code{method}.
#' @references Sabeti, P. C., Reich, D. E., et al. (2002). Nature
#'   419(6909), 832-837. Sabeti, P. C., Varilly, P., et al. (2007).
#'   Nature 449(7164), 913-918, Methods p. 6 (displayed EHH equation;
#'   fetched-wave3 PDF). Gautier, M. and Vitalis, R., rehh vignette
#'   sec. 3.1.1 eq. 3.1 (CRAN, fetched 2026-08-09).
#' @export
Ehhdec <- function(hap, core, positions = NULL) {
  H <- as.matrix(hap)
  storage.mode(H) <- "integer"
  N <- nrow(H); L <- ncol(H)
  if (N < 2L) stop("need at least 2 chromosomes", call. = FALSE)
  core <- as.integer(core)
  if (core < 1L || core > L) stop("core out of range", call. = FALSE)
  if (!all(H %in% c(0L, 1L))) {
    stop("haplotypes must be coded 0/1", call. = FALSE)
  }
  pos <- if (is.null(positions)) as.numeric(seq_len(L) - 1L) else {
    p <- as.numeric(positions)
    if (length(p) != L) stop("positions length mismatch", call. = FALSE)
    p
  }
  car1 <- which(H[, core] == 1L)
  car0 <- which(H[, core] == 0L)
  ehh1 <- if (length(car1) >= 2L) .ehh_curve(H, core, car1) else rep(NaN, L)
  ehh0 <- if (length(car0) >= 2L) .ehh_curve(H, core, car0) else rep(NaN, L)
  ehhs <- .ehh_curve(H, core, seq_len(N))
  list(
    estimate = ehh1, ehh1 = ehh1, ehh0 = ehh0, ehhs = ehhs,
    positions = pos, core = core,
    n1 = length(car1), n0 = length(car0), n = as.integer(N),
    method = "EHH decay (Sabeti 2002/2007), allele-wise and site-wise")
}
