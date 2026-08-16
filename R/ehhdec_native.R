# Extended haplotype homozygosity (EHH) decay.
# Sources: Sabeti, P. C. et al. (2002), Detecting recent positive
# selection in the human genome from haplotype structure, Nature 419,
# 832-837 (EHH defined as the probability that two randomly chosen
# chromosomes carrying the core allele are homozygous for the whole
# interval from the core out to a given site); Sabeti, P. C. et al.
# (2007), Genome-wide detection and characterization of positive
# selection in human populations, Nature 449, 913-918 (the site-wise
# EHHS over all chromosomes, used by XP-EHH).
#
# For an interval containing haplotype classes of sizes n_k among n_a
# carriers, EHH_a = sum_k n_k (n_k - 1) / [n_a (n_a - 1)], i.e. the
# ratio of homozygous ordered pairs.
#
# Native implementation mirroring Python morie.fn.ehhdec exactly,
# including the NaN curve returned when a core allele has fewer than
# two carriers.

#' .mor_ehh_curve
#'
#' Part of the ehhdec_native implementation; see the file header for the
#' source it follows.
#'
#' @param H See Usage.
#' @param core See Usage.
#' @param carriers See Usage.
#' @return The value of \code{out}, as built in the body.
#' @export
.mor_ehh_curve <- function(H, core, carriers) {
  L <- ncol(H)
  n <- length(carriers)
  denom <- n * (n - 1)
  out <- numeric(L)
  for (j in seq_len(L)) {
    lo <- min(j, core); hi <- max(j, core)
    keys <- apply(H[carriers, lo:hi, drop = FALSE], 1, paste, collapse = "")
    cnt <- table(keys)
    out[j] <- sum(cnt * (cnt - 1)) / denom
  }
  out
}

#' EHH decay around a core SNP
#'
#' Extended haplotype homozygosity as a function of distance from a
#' core site, computed separately for chromosomes carrying each core
#' allele (Sabeti et al. 2002) and for all chromosomes together, the
#' site-wise EHHS of Sabeti et al. (2007).
#'
#' @param hap Haplotype matrix, chromosomes by sites, coded 0/1.
#' @param core Core site index, 0-based.
#' @param positions Optional physical positions of the sites; default
#'   is the site index.
#' @return A list with \code{estimate} (= \code{ehh1}), \code{ehh1},
#'   \code{ehh0}, \code{ehhs}, \code{positions}, \code{core},
#'   \code{n1}, \code{n0}, \code{n}, \code{method}.
#' @references Sabeti, P. C. et al. (2002). Detecting recent positive
#'   selection in the human genome from haplotype structure. Nature,
#'   419, 832-837.
#' @export
morie_ehhdec <- function(hap, core, positions = NULL) {
  H <- as.matrix(hap)
  storage.mode(H) <- "integer"
  N <- nrow(H); L <- ncol(H)
  if (N < 2L) stop("need at least 2 chromosomes")
  core <- as.integer(core)
  if (!(core >= 0L && core < L)) stop("core out of range")
  if (!all(H %in% c(0L, 1L))) stop("haplotypes must be coded 0/1")
  if (is.null(positions)) {
    pos <- as.numeric(seq_len(L) - 1L)
  } else {
    pos <- as.numeric(positions)
    if (length(pos) != L) stop("positions length mismatch")
  }
  cc <- core + 1L
  car1 <- which(H[, cc] == 1L)
  car0 <- which(H[, cc] == 0L)
  ehh1 <- if (length(car1) >= 2L) .mor_ehh_curve(H, cc, car1) else rep(NaN, L)
  ehh0 <- if (length(car0) >= 2L) .mor_ehh_curve(H, cc, car0) else rep(NaN, L)
  ehhs <- .mor_ehh_curve(H, cc, seq_len(N))
  list(estimate = ehh1, ehh1 = ehh1, ehh0 = ehh0, ehhs = ehhs,
       positions = pos, core = core,
       n1 = length(car1), n0 = length(car0), n = N,
       method = "EHH decay (Sabeti 2002/2007), allele-wise and site-wise")
}

# Trapezoid integral of an EHH curve out from the core on one side,
# stopping once the curve drops below min_ehh.  `truncated` stays TRUE
# when the curve never dropped that far before the data ran out, which
# is exactly the case where the integral understates iHH.
#' Trapezoid integral of an EHH curve out from the core on one side,
#'
#' stopping once the curve drops below min_ehh.  `truncated` stays TRUE
#' when the curve never dropped that far before the data ran out, which
#' is exactly the case where the integral understates iHH.
#'
#' @param pos See Usage.
#' @param ehh See Usage.
#' @param core See Usage.
#' @param side See Usage.
#' @param min_ehh See Usage.
#' @return A list with \code{area}, \code{truncated}.
#' @export
.mor_ihh_one_side <- function(pos, ehh, core, side, min_ehh) {
  L <- length(pos)
  cc <- core + 1L
  idx <- if (side > 0) {
    if (cc + 1L <= L) seq.int(cc + 1L, L) else integer(0)
  } else {
    if (cc - 1L >= 1L) seq.int(cc - 1L, 1L) else integer(0)
  }
  area <- 0
  prev_p <- pos[cc]; prev_e <- ehh[cc]
  truncated <- TRUE
  for (j in idx) {
    area <- area + abs(pos[j] - prev_p) * 0.5 * (ehh[j] + prev_e)
    prev_p <- pos[j]; prev_e <- ehh[j]
    if (ehh[j] < min_ehh) { truncated <- FALSE; break }
  }
  list(area = area, truncated = truncated)
}
