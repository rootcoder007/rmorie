# morie.fn -- function file (rootcoder007/morie)
# IMPUTE2: genotype imputation across multiple reference panels.
#
# Imputation fills in untyped genotypes from reference haplotypes, and
# the panel bounds what is achievable: with HapMap as the sole
# reference the improvement possible is constrained by the panel itself,
# while expanding to thousands of chromosomes greatly increases accuracy
# at both rare and common SNPs.
#
# The hard part is that panels disagree about which SNPs they carry.
# Controls genotyped on several chip designs and densely typed
# sequencing samples cover different, overlapping marker sets. Merging
# them by *intersection* discards exactly the extra coverage that
# motivated merging.
#
# The framework separates two roles. SNPs typed in the study align
# the study haplotypes to the reference; SNPs missing from the study are
# the targets. A reference haplotype can be a template at one SNP and
# uninformative at another, so panels combine by role rather than by
# intersection -- the flexibility that lets panels typed on different
# SNP sets be used together.
#
# Underneath is the Li-Stephens copying model. A study haplotype is
# a mosaic of reference haplotypes; a hidden Markov chain switches
# template at a rate set by recombination and absorbs mismatch through a
# mutation parameter. The imputed dosage is a posterior mean, so it
# carries uncertainty -- conflicting templates give a middling dosage
# rather than a confident wrong call.
#
# Accuracy is measured on masked truth, never on the model's own
# posterior: a confident model can be confidently wrong.
#
# References
# ----------
# Howie, B. N., Donnelly, P. & Marchini, J. (2009) "A Flexible and
# Accurate Genotype Imputation Method for the Next Generation of
# Genome-Wide Association Studies", *PLoS Genetics* 5(6), e1000529,
# doi:10.1371/journal.pgen.1000529. The main innovation as a flexible
# modelling framework that increases accuracy and combines information
# across MULTIPLE REFERENCE PANELS while remaining computationally
# feasible; higher accuracy than other methods when HapMap provides the
# sole reference panel, with the panel size constraining the
# improvements possible; greatly enhanced accuracy from expanding the
# panel to thousands of chromosomes, outperforming other methods at both
# rare and common SNPs with error rates 15-20% lower than the closest
# competitor; and the practical advantages of this approach to
# integrating information across panels genotyped on different sets of
# SNPs.
#
# Li, N. & Stephens, M. (2003) "Modeling Linkage Disequilibrium and
# Identifying Recombination Hotspots Using Single-Nucleotide
# Polymorphism Data", *Genetics* 165(4), 2213-2233,
# doi:10.1093/genetics/165.4.2213. The copying model.
#
# Browning, S. R. & Browning, B. L. (2007) "Rapid and Accurate Haplotype
# Phasing and Missing-Data Inference for Whole-Genome Association
# Studies by Use of Localized Haplotype Clustering", *American Journal
# of Human Genetics* 81(5), 1084-1097, doi:10.1086/521987.

.impfun_eps <- 1e-12

#' .impfun_as_double_matrix
#'
#' A step of the impfun_native implementation. Called by \code{impute_dosage}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x A vector; its length is taken.
#' @return A matrix, from \code{matrix}.
#' @export
.impfun_as_double_matrix <- function(x) {
  if (is.matrix(x)) {
    storage.mode(x) <- "double"
    return(x)
  }
  if (is.list(x)) {
    if (length(x) == 0L) return(matrix(0, nrow = 0L, ncol = 0L))
    return(do.call(rbind, lapply(x, as.numeric)))
  }
  matrix(as.numeric(x), nrow = 1L)
}

#' .impfun_as_double_vec
#'
#' A step of the impfun_native implementation. Called by \code{concordance}, \code{info_score}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x A list; the body checks with \code{is.list}.
#' @return A vector, from \code{as.numeric}.
#' @export
.impfun_as_double_vec <- function(x) {
  if (is.list(x)) return(unlist(lapply(x, as.numeric)))
  as.numeric(x)
}

#' .impfun_as_int_list
#'
#' A step of the impfun_native implementation. Called by \code{copying_model}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x A matrix; indexed by row and column.
#' @return The value of \code{list}.
#' @export
.impfun_as_int_list <- function(x) {
  if (is.list(x)) return(lapply(x, as.integer))
  if (is.matrix(x)) {
    return(lapply(seq_len(nrow(x)), function(i) as.integer(x[i, ])))
  }
  list(as.integer(x))
}

#' merge_panels
#'
#' A step of the impfun_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param panels A vector; its length is taken and its elements indexed.
#' @param study_snps Passed to \code{unique}.
#' @return A list with \code{scaffold}, \code{targets}, \code{union}, \code{intersection}, \code{kept_by_union}, \code{kept_by_intersection}, \code{gain}, \code{note}.
#' @export
merge_panels <- function(panels, study_snps) {
  if (length(panels) == 0L) {
    stop("impfun: no reference panels given")
  }
  panel_names <- names(panels)
  if (is.null(panel_names) || any(!nzchar(panel_names))) {
    stop("impfun: every panel must have a name")
  }
  per <- list()
  all_snps <- NULL
  for (nm in panel_names) {
    snps <- unique(panels[[nm]])
    per[[nm]] <- snps
    all_snps <- if (is.null(all_snps)) snps else union(all_snps, snps)
  }
  inter <- per[[panel_names[1L]]]
  if (length(panel_names) > 1L) {
    for (nm in panel_names[-1L]) {
      inter <- intersect(inter, per[[nm]])
    }
  }
  study_chr <- unique(study_snps)
  scaffold <- sort(intersect(all_snps, study_chr))
  if (length(scaffold) == 0L) {
    stop("impfun: no SNP is typed in both the study and a panel; there is nothing to align against")
  }
  list(
    scaffold = scaffold,
    targets = sort(setdiff(all_snps, study_chr)),
    union = sort(all_snps),
    intersection = sort(inter),
    kept_by_union = length(all_snps),
    kept_by_intersection = length(inter),
    gain = length(all_snps) - length(inter),
    note = "intersection would discard the coverage that motivated adding the panel"
  )
}

#' copying_model
#'
#' A step of the impfun_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param study_hap Coerced to integer by the body, with \code{as.integer}.
#' @param reference_haps Passed to \code{.impfun_as_int_list}.
#' @param rho Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.001}.
#' @param theta Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.01}.
#' @return A list with \code{posterior}, \code{n_templates}, \code{n_sites}, \code{log_likelihood}.
#' @export
copying_model <- function(study_hap, reference_haps, rho = 0.001, theta = 0.01) {
  h <- as.integer(study_hap)
  R_list <- .impfun_as_int_list(reference_haps)
  K <- length(R_list)
  L <- length(h)
  if (K < 1L || L < 1L) {
    stop("impfun: need at least one reference haplotype and one site")
  }
  for (kk in seq_len(K)) {
    if (length(R_list[[kk]]) != L) {
      stop("impfun: a reference haplotype has the wrong length")
    }
  }
  r_ <- as.numeric(rho)
  t_ <- as.numeric(theta)
  if (!(r_ > 0 && r_ < 1) || !(t_ > 0 && t_ < 0.5)) {
    stop("impfun: rho must lie in (0,1) and theta in (0,0.5)")
  }

  emit <- function(kk, l) {
    if (R_list[[kk]][l] == h[l]) 1.0 - t_ else t_
  }

  F <- matrix(0, nrow = L, ncol = K)
  scale <- numeric(L)
  for (kk in seq_len(K)) {
    F[1L, kk] <- emit(kk, 1L) / K
  }
  s <- sum(F[1L, ])
  if (s == 0) s <- 1.0
  F[1L, ] <- F[1L, ] / s
  scale[1L] <- s
  if (L >= 2L) {
    for (l in 2L:L) {
      tot <- sum(F[l - 1L, ])
      for (kk in seq_len(K)) {
        F[l, kk] <- ((1.0 - r_) * F[l - 1L, kk] + r_ * tot / K) * emit(kk, l)
      }
      s <- sum(F[l, ])
      if (s == 0) s <- 1.0
      F[l, ] <- F[l, ] / s
      scale[l] <- s
    }
  }
  list(
    posterior = F,
    n_templates = K,
    n_sites = L,
    log_likelihood = sum(log(pmax(scale, .impfun_eps)))
  )
}

#' impute_dosage
#'
#' A step of the impfun_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param posterior Passed to \code{.impfun_as_double_matrix}.
#' @param reference_haps Passed to \code{.impfun_as_double_matrix}.
#' @param site Coerced to integer by the body, with \code{as.integer}.
#' @return A list with \code{dosage}, \code{allele_freq}, \code{certainty}, \code{note}.
#' @export
impute_dosage <- function(posterior, reference_haps, site) {
  P <- .impfun_as_double_matrix(posterior)
  R_mat <- .impfun_as_double_matrix(reference_haps)
  l <- as.integer(site)
  if (l < 1L || l > nrow(P)) {
    stop(sprintf("impfun: site %d is outside the region", l))
  }
  w <- P[l, ]
  tot <- sum(w)
  if (tot == 0) tot <- 1.0
  p1 <- sum(w * R_mat[, l]) / tot
  list(
    dosage = 2.0 * p1,
    allele_freq = p1,
    certainty = max(p1, 1.0 - p1),
    note = "conflicting templates give a middling dosage, which is the honest answer"
  )
}

#' info_score
#'
#' A step of the impfun_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param dosages Passed to \code{.impfun_as_double_vec}.
#' @return A list with \code{info}, \code{theta}, \code{note}.
#' @export
info_score <- function(dosages) {
  d <- .impfun_as_double_vec(dosages)
  n <- length(d)
  if (n < 2L) {
    stop("impfun: at least 2 individuals are needed")
  }
  theta <- sum(d) / (2.0 * n)
  if (theta <= .impfun_eps || theta >= 1.0 - .impfun_eps) {
    return(list(
      info = 1.0,
      theta = theta,
      note = "monomorphic: no information to lose"
    ))
  }
  m <- sum(d) / n
  var_d <- sum((d - m)^2) / n
  info <- var_d / (2.0 * theta * (1.0 - theta))
  info <- min(max(info, 0.0), 1.0)
  list(
    info = info,
    theta = theta,
    note = "filtering on info is how badly-imputed SNPs are excluded before testing"
  )
}

#' concordance
#'
#' A step of the impfun_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param imputed Passed to \code{.impfun_as_double_vec}.
#' @param truth Passed to \code{.impfun_as_double_vec}.
#' @return A list with \code{estimate}, \code{concordance}, \code{mean_absolute_error}, \code{n}, \code{method}.
#' @export
concordance <- function(imputed, truth) {
  a <- .impfun_as_double_vec(imputed)
  b <- .impfun_as_double_vec(truth)
  if (length(a) != length(b)) {
    stop(sprintf("impfun: %d imputed but %d true genotypes", length(a), length(b)))
  }
  ok <- sum(round(a) == round(b))
  list(
    estimate = ok / length(a),
    concordance = ok / length(a),
    mean_absolute_error = sum(abs(a - b)) / length(a),
    n = length(a),
    method = "IMPUTE2 evaluation on masked genotypes; Howie, Donnelly & Marchini (2009)"
  )
}

#' .impfun_cheatsheet
#'
#' A step of the impfun_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
.impfun_cheatsheet <- function() {
  "impfun: imputation is bounded by the REFERENCE PANEL, and panels disagree about which SNPs they carry -- merging by INTERSECTION discards the coverage that motivated merging. IMPUTE2 merges by ROLE: SNPs typed in the study align the haplotypes, the rest are targets. Underneath is Li-Stephens copying, the study haplotype as a MOSAIC of references switching at the recombination rate. Dosages carry uncertainty, and accuracy is measured on MASKED truth, because a confident model can be confidently wrong."
}

# compact alias per ledger/NAMING.md
impute2 <- copying_model

# public names resolved by fn/_lazy_map.json
genotype_imputation <- copying_model

# Main entry point for the impfun module (Li-Stephens copying model)
morie_impfun <- copying_model























