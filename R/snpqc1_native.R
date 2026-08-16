# morie.fn -- function file (rootcoder007/morie)
# GWAS quality control: the seven standard steps.
#
# Marees, A. T., de Kluiver, H., Stringer, S., Vorspan, F., Curis, E.,
# Marie-Claire, C., & Derks, E. M. (2018) "A tutorial on conducting
# genome-wide association studies: Quality control and statistical
# analysis", International Journal of Methods in Psychiatric Research
# 27(2), e1608.
#
# The tutorial's Table 1 lists seven QC steps with the thresholds it
# recommends, implemented here as the defaults:
#  1. Missingness, SNPs then individuals, relaxed (0.2) then stringent
#     (0.02); SNP filtering before individual filtering each pass.
#  2. Sex discrepancy from X-chromosome homozygosity (>0.8 male, <0.2
#     female).
#  3. Minor allele frequency (0.01 large / 0.05 moderate samples).
#  4. Hardy-Weinberg equilibrium (1e-10 cases, 1e-6 controls for binary
#     traits; 1e-6 for quantitative).
#  5. Heterozygosity (+-3 SD from the mean).
#  6. Relatedness (pi-hat 0.2 on LD-pruned autosomal SNPs).
#  7. Population stratification by MDS on the IBS matrix.
#
# The HWE p-value is the chi-square goodness-of-fit test or the exact
# conditional test (default), neither attributed to the tutorial.
#
# Relatedness has two routes. relatedness="pihat" (default) is PLINK's
# method-of-moments IBD estimator (Purcell et al. 2007, Am J Hum Genet
# 81(3), 559-575): conditional on IBD state Z the expected count of
# SNPs at IBS state I is inverted in order to give P(Z=0), P(Z=1),
# P(Z=2), with pi-hat = P(Z=2) + P(Z=1)/2 and the paper's bounding
# rules and ascertainment correction as printed. relatedness="kinship"
# is the genomic kinship from centred, scaled genotypes, on the same
# scale but a different estimator.
#
# Genotypes are counts of the minor allele, 0/1/2, with NA for a
# missing call.

.snpqc1_check <- function(genotypes) {
  G <- as.matrix(genotypes)
  storage.mode(G) <- "double"
  if (nrow(G) == 0L || ncol(G) == 0L) {
    stop(paste0("snpqc1: genotypes must be a non-empty individual x SNP ",
                "matrix"))
  }
  vals <- G[!is.na(G)]
  if (any(!(vals %in% c(0, 1, 2)))) {
    stop("snpqc1: genotypes must be 0, 1, 2 or NA")
  }
  list(G=G, n=nrow(G), m=ncol(G))
}

#' Per-SNP and per-individual call rates
#'
#' Part of the snpqc1_native implementation; see the file header for the
#' source it follows.
#'
#' @param genotypes See Usage.
#' @return A list with \code{per_snp}, \code{per_ind}.
#' @export
morie_snpqc1_call_rates <- function(genotypes) {
  # Per-SNP and per-individual call rates.
  ch <- .snpqc1_check(genotypes)
  G <- ch$G
  per_snp <- colSums(!is.na(G)) / ch$n
  per_ind <- rowSums(!is.na(G)) / ch$m
  list(per_snp=as.numeric(per_snp), per_ind=as.numeric(per_ind))
}

#' Minor allele frequency per SNP, over non-missing calls
#'
#' Part of the snpqc1_native implementation; see the file header for the
#' source it follows.
#'
#' @param genotypes See Usage.
#' @return The value of \code{out}, as built in the body.
#' @export
morie_snpqc1_maf <- function(genotypes) {
  # Minor allele frequency per SNP, over non-missing calls.
  ch <- .snpqc1_check(genotypes)
  G <- ch$G
  out <- numeric(ch$m)
  for (j in seq_len(ch$m)) {
    called <- G[!is.na(G[, j]), j]
    if (length(called) == 0L) {
      out[j] <- 0.0
      next
    }
    p <- sum(called) / (2.0 * length(called))
    out[j] <- min(p, 1.0 - p)
  }
  out
}

.snpqc1_log_fact <- function(n) {
  lgamma(n + 1.0)
}

#' morie_snpqc1_hwe_pvalue
#'
#' Part of the snpqc1_native implementation; see the file header for the
#' source it follows.
#'
#' @param n_hom_minor See Usage.
#' @param n_het See Usage.
#' @param n_hom_major See Usage.
#' @param test Defaults to \code{"exact"}.
#' @return A numeric value.
#' @export
morie_snpqc1_hwe_pvalue <- function(n_hom_minor, n_het, n_hom_major,
                                    test="exact") {
  # Hardy-Weinberg p-value for one SNP. "exact" is the conditional
  # test summing the probabilities of every table no more probable
  # than the observed one; "chisq" is the goodness-of-fit test.
  a <- as.integer(n_hom_minor)
  h <- as.integer(n_het)
  b <- as.integer(n_hom_major)
  if (min(a, h, b) < 0L) {
    stop("snpqc1: genotype counts must be non-negative")
  }
  n <- a + h + b
  if (n == 0L) {
    stop("snpqc1: no genotypes")
  }
  if (!(test %in% c("exact", "chisq"))) {
    stop("snpqc1: test must be 'exact' or 'chisq'")
  }
  n_minor <- 2L * a + h
  if (test == "chisq") {
    p <- n_minor / (2.0 * n)
    exp_ <- c(n * p * p, 2.0 * n * p * (1 - p), n * (1 - p) ^ 2)
    obs <- c(a, h, b)
    if (min(exp_) <= 0) {
      return(1.0)
    }
    chi <- sum((obs - exp_) ^ 2 / exp_)
    return(.snpqc1_erfc(sqrt(chi / 2.0)))
  }
  # exact conditional test
  n_major <- 2L * n - n_minor
  hets <- seq.int(n_minor %% 2L, min(n_minor, n_major), by=2L)
  lp <- numeric(length(hets))
  for (idx in seq_along(hets)) {
    het <- hets[idx]
    hom_a <- (n_minor - het) %/% 2L
    hom_b <- (n_major - het) %/% 2L
    lp[idx] <- (.snpqc1_log_fact(n) - .snpqc1_log_fact(hom_a) -
                .snpqc1_log_fact(het) - .snpqc1_log_fact(hom_b) +
                het * log(2.0))
  }
  lognorm <- max(lp)
  probs <- exp(lp - lognorm)
  tot <- sum(probs)
  obs_i <- which(hets == h)
  if (length(obs_i) == 0L) {
    stop(paste0("snpqc1: the observed heterozygote count is impossible ",
                "given the allele counts"))
  }
  thresh <- probs[obs_i] * (1.0 + 1e-9)
  p <- sum(probs[probs <= thresh]) / tot
  min(max(p, 0.0), 1.0)
}

.snpqc1_erfc <- function(x) {
  # complementary error function via pnorm: erfc(x) = 2*pnorm(-x*sqrt2)
  2.0 * stats::pnorm(-x * sqrt(2.0))
}

#' Per-individual heterozygosity rate over non-missing calls
#'
#' Part of the snpqc1_native implementation; see the file header for the
#' source it follows.
#'
#' @param genotypes See Usage.
#' @return The value of \code{out}, as built in the body.
#' @export
morie_snpqc1_heterozygosity <- function(genotypes) {
  # Per-individual heterozygosity rate over non-missing calls.
  ch <- .snpqc1_check(genotypes)
  G <- ch$G
  out <- numeric(ch$n)
  for (i in seq_len(ch$n)) {
    called <- G[i, !is.na(G[i, ])]
    out[i] <- if (length(called) > 0L) {
      sum(called == 1) / length(called)
    } else {
      0.0
    }
  }
  out
}

#' morie_snpqc1_sex_check
#'
#' Part of the snpqc1_native implementation; see the file header for the
#' source it follows.
#'
#' @param x_genotypes See Usage.
#' @param reported_sex Defaults to \code{NULL}.
#' @param male_min Defaults to \code{0.8}.
#' @param female_max Defaults to \code{0.2}.
#' @return The value of \code{res}, as built in the body.
#' @export
morie_snpqc1_sex_check <- function(x_genotypes, reported_sex=NULL,
                                   male_min=0.8, female_max=0.2) {
  # X-chromosome homozygosity F = (O - E)/(n - E) with the tutorial's
  # cutoffs. Males exceed male_min (0.8), females fall below
  # female_max (0.2). reported_sex (1 male, 2 female) turns the result
  # into a discrepancy list (0-based indices).
  ch <- .snpqc1_check(x_genotypes)
  G <- ch$G
  n <- ch$n
  m <- ch$m
  freqs <- morie_snpqc1_maf(G)
  out <- numeric(n)
  for (i in seq_len(n)) {
    obs_hom <- 0.0
    exp_hom <- 0.0
    for (j in seq_len(m)) {
      g <- G[i, j]
      if (is.na(g)) {
        next
      }
      p <- freqs[j]
      obs_hom <- obs_hom + (if (g != 1) 1.0 else 0.0)
      exp_hom <- exp_hom + (1.0 - 2.0 * p * (1.0 - p))
    }
    denom <- sum(!is.na(G[i, ])) - exp_hom
    out[i] <- if (abs(denom) > 1e-12) (obs_hom - exp_hom) / denom else NaN
  }
  called <- ifelse(out > male_min, 1L,
                   ifelse(out < female_max, 2L, 0L))
  res <- list(F=out, inferred_sex=called)
  if (!is.null(reported_sex)) {
    rep_ <- as.integer(reported_sex)
    if (length(rep_) != n) {
      stop("snpqc1: one reported sex per individual")
    }
    res$discrepant <- which(called != 0L & called != rep_) - 1L
    res$undetermined <- which(called == 0L) - 1L
  }
  res
}

#' PLINK\'s Table 1: P(I | Z) for one SNP. Returns a 3x3 matrix with
#'
#' rows Z=0,1,2 and columns I=0,1,2 (lower triangle zero).
#'
#' @param x_count See Usage.
#' @param y_count See Usage.
#' @param correction Defaults to \code{TRUE}.
#' @return The value of \code{rbind}.
#' @export
morie_snpqc1_ibs_given_ibd <- function(x_count, y_count, correction=TRUE) {
  # PLINK's Table 1: P(I | Z) for one SNP. Returns a 3x3 matrix with
  # rows Z=0,1,2 and columns I=0,1,2 (lower triangle zero).
  X <- as.numeric(x_count)
  Y <- as.numeric(y_count)
  T <- X + Y
  if (T <= 0) {
    stop("snpqc1: a SNP with no non-missing alleles")
  }
  p <- X / T
  q <- Y / T
  if (!correction || T < 5 || X < 4 || Y < 4) {
    # textbook forms; also the fallback when the corrected factors
    # would divide by a count too small to support them
    z0 <- c(2 * p * p * q * q,
            4 * p ^ 3 * q + 4 * p * q ^ 3,
            p ^ 4 + q ^ 4 + 4 * p * p * q * q)
    z1 <- c(0.0, 2 * p * q, 1.0 - 2 * p * q)
    return(rbind(z0, z1, c(0.0, 0.0, 1.0)))
  }
  t1 <- T / (T - 1.0)
  t2 <- T / (T - 2.0)
  t3 <- T / (T - 3.0)
  xa <- (X - 1.0) / X
  xb <- (X - 2.0) / X
  xc <- (X - 3.0) / X
  ya <- (Y - 1.0) / Y
  yb <- (Y - 2.0) / Y
  yc <- (Y - 3.0) / Y
  i0z0 <- 2 * p * p * q * q * xa * ya * t1 * t2 * t3
  i1z0 <- (4 * p ^ 3 * q * xa * xb * t1 * t2 * t3 +
           4 * p * q ^ 3 * ya * yb * t1 * t2 * t3)
  i2z0 <- (p ^ 4 * xa * xb * xc * t1 * t2 * t3 +
           q ^ 4 * ya * yb * yc * t1 * t2 * t3 +
           4 * p * p * q * q * xa * ya * t1 * t2 * t3)
  i1z1 <- (2 * p * p * q * xa * t1 * t2 + 2 * p * q * q * ya * t1 * t2)
  i2z1 <- (p ^ 3 * xa * xb * t1 * t2 + q ^ 3 * ya * yb * t1 * t2 +
           p * p * q * xa * t1 * t2 + p * q * q * ya * t1 * t2)
  rbind(c(i0z0, i1z0, i2z0), c(0.0, i1z1, i2z1), c(0.0, 0.0, 1.0))
}

#' PLINK\'s method-of-moments IBD estimates for every pair. Returns
#'
#' list(Z, pihat) where Z[[i]][[k]] is c(P(Z=0), P(Z=1), P(Z=2)) after
#' the paper\'s bounding rules and pihat[i, k] = P(Z=2) + P(Z=1)/2.
#'
#' @param genotypes See Usage.
#' @param correction Defaults to \code{TRUE}.
#' @return A list with \code{Z}, \code{pihat}.
#' @export
morie_snpqc1_ibd_moments <- function(genotypes, correction=TRUE) {
  # PLINK's method-of-moments IBD estimates for every pair. Returns
  # list(Z, pihat) where Z[[i]][[k]] is c(P(Z=0), P(Z=1), P(Z=2))
  # after the paper's bounding rules and pihat[i, k] = P(Z=2) +
  # P(Z=1)/2.
  ch <- .snpqc1_check(genotypes)
  G <- ch$G
  n <- ch$n
  m <- ch$m
  tables <- vector("list", m)
  for (j in seq_len(m)) {
    called <- G[!is.na(G[, j]), j]
    if (length(called) == 0L) {
      tables[[j]] <- NULL
      next
    }
    X <- sum(called)
    Y <- 2 * length(called) - X
    if (X <= 0 || Y <= 0) {
      tables[[j]] <- NULL
      next
    }
    tables[[j]] <- morie_snpqc1_ibs_given_ibd(X, Y, correction)
  }
  Z <- matrix(list(NULL), n, n)
  P <- matrix(0.0, n, n)
  for (i in seq_len(n)) {
    Z[[i, i]] <- c(0.0, 0.0, 1.0)
    P[i, i] <- 1.0
    for (k in seq.int(i + 1L, length.out=max(0L, n - i))) {
      obs <- c(0.0, 0.0, 0.0)
      exp_ <- matrix(0.0, 3, 3)
      for (j in seq_len(m)) {
        if (is.null(tables[[j]])) {
          next
        }
        gi <- G[i, j]
        gk <- G[k, j]
        if (is.na(gi) || is.na(gk)) {
          next
        }
        ibs <- 2 - abs(gi - gk)
        obs[ibs + 1L] <- obs[ibs + 1L] + 1.0
        exp_ <- exp_ + tables[[j]]
      }
      if (exp_[1, 1] <= 0) {
        Z[[i, k]] <- c(1.0, 0.0, 0.0)
        Z[[k, i]] <- c(1.0, 0.0, 0.0)
        next
      }
      z0 <- obs[1] / exp_[1, 1]
      z1 <- if (exp_[2, 2] > 0) (obs[2] - z0 * exp_[1, 2]) / exp_[2, 2] else 0.0
      z2 <- if (exp_[3, 3] > 0) {
        (obs[3] - z0 * exp_[1, 3] - z1 * exp_[2, 3]) / exp_[3, 3]
      } else {
        0.0
      }
      # the paper's bounding rules, as printed
      if (z0 > 1.0) {
        z0 <- 1.0
        z1 <- 0.0
        z2 <- 0.0
      } else if (z0 < 0.0) {
        z0 <- 0.0
        s <- z1 + z2
        if (s > 0) {
          z1 <- z1 / s
          z2 <- z2 / s
        } else {
          z1 <- 0.0
          z2 <- 1.0
        }
      }
      z1 <- max(z1, 0.0)
      z2 <- max(z2, 0.0)
      tot <- z0 + z1 + z2
      if (tot > 0) {
        z0 <- z0 / tot
        z1 <- z1 / tot
        z2 <- z2 / tot
      }
      Z[[i, k]] <- c(z0, z1, z2)
      Z[[k, i]] <- c(z0, z1, z2)
      P[i, k] <- z2 + 0.5 * z1
      P[k, i] <- z2 + 0.5 * z1
    }
  }
  list(Z=Z, pihat=P)
}

#' Just the pi-hat = P(Z=2) + P(Z=1)/2 matrix
#'
#' Part of the snpqc1_native implementation; see the file header for the
#' source it follows.
#'
#' @param genotypes See Usage.
#' @param correction Defaults to \code{TRUE}.
#' @return The value of \code{$}.
#' @export
morie_snpqc1_pihat_matrix <- function(genotypes, correction=TRUE) {
  # Just the pi-hat = P(Z=2) + P(Z=1)/2 matrix.
  morie_snpqc1_ibd_moments(genotypes, correction)$pihat
}

#' Genomic kinship from centred, scaled genotypes:
#'
#' K_ik = (1/M) sum_j (g_ij - 2p_j)(g_kj - 2p_j) / (2 p_j (1 - p_j)). On
#' the same scale as pi-hat but NOT PLINK\'s pi-hat.
#'
#' @param genotypes See Usage.
#' @return The value of \code{K}, as built in the body.
#' @export
morie_snpqc1_kinship_matrix <- function(genotypes) {
  # Genomic kinship from centred, scaled genotypes:
  # K_ik = (1/M) sum_j (g_ij - 2p_j)(g_kj - 2p_j) / (2 p_j (1 - p_j)).
  # On the same scale as pi-hat but NOT PLINK's pi-hat.
  ch <- .snpqc1_check(genotypes)
  G <- ch$G
  n <- ch$n
  m <- ch$m
  freqs <- numeric(m)
  for (j in seq_len(m)) {
    called <- G[!is.na(G[, j]), j]
    freqs[j] <- if (length(called) > 0L) {
      sum(called) / (2.0 * length(called))
    } else {
      0.0
    }
  }
  use <- which(freqs > 1e-6 & freqs < 1 - 1e-6)
  if (length(use) == 0L) {
    stop("snpqc1: no polymorphic SNP for kinship")
  }
  K <- matrix(0.0, n, n)
  for (i in seq_len(n)) {
    for (k in seq.int(i, n)) {
      tot <- 0.0
      cnt <- 0L
      for (j in use) {
        gi <- G[i, j]
        gk <- G[k, j]
        if (is.na(gi) || is.na(gk)) {
          next
        }
        p <- freqs[j]
        tot <- tot + ((gi - 2 * p) * (gk - 2 * p) / (2.0 * p * (1.0 - p)))
        cnt <- cnt + 1L
      }
      v <- if (cnt > 0L) tot / cnt else 0.0
      K[i, k] <- v
      K[k, i] <- v
    }
  }
  K
}

#' Window-based pruning: drop one of any pair with r^2 above the
#'
#' threshold. Returns kept SNP indices (1-based).
#'
#' @param genotypes See Usage.
#' @param window Defaults to \code{50}.
#' @param step Defaults to \code{5}.
#' @param r2 Defaults to \code{0.2}.
#' @return The value of \code{keep}, as built in the body.
#' @export
morie_snpqc1_ld_prune <- function(genotypes, window=50, step=5, r2=0.2) {
  # Window-based pruning: drop one of any pair with r^2 above the
  # threshold. Returns kept SNP indices (1-based).
  ch <- .snpqc1_check(genotypes)
  G <- ch$G
  n <- ch$n
  m <- ch$m
  keep <- seq_len(m)
  corr2 <- function(j, k) {
    ok <- !is.na(G[, j]) & !is.na(G[, k])
    if (sum(ok) < 3L) {
      return(0.0)
    }
    aj <- G[ok, j]
    bk <- G[ok, k]
    mj <- mean(aj)
    mk <- mean(bk)
    sj <- sum((aj - mj) ^ 2)
    sk <- sum((bk - mk) ^ 2)
    if (sj <= 0 || sk <= 0) {
      return(0.0)
    }
    c <- sum((aj - mj) * (bk - mk))
    c * c / (sj * sk)
  }
  start <- 1L
  while (start <= length(keep)) {
    block <- keep[start:min(start + window - 1L, length(keep))]
    drop <- integer(0)
    for (a in seq_along(block)) {
      if (block[a] %in% drop) {
        next
      }
      for (b in seq.int(a + 1L, length.out=max(0L, length(block) - a))) {
        if (block[b] %in% drop) {
          next
        }
        if (corr2(block[a], block[b]) > r2) {
          drop <- c(drop, block[b])
        }
      }
    }
    keep <- keep[!(keep %in% drop)]
    start <- start + step
  }
  keep
}

#' morie_snpqc1
#'
#' Part of the snpqc1_native implementation; see the file header for the
#' source it follows.
#'
#' @param genotypes See Usage.
#' @param phenotype Defaults to \code{NULL}.
#' @param trait Defaults to \code{"binary"}.
#' @param geno_relaxed Defaults to \code{0.2}.
#' @param mind_relaxed Defaults to \code{0.2}.
#' @param geno Defaults to \code{0.02}.
#' @param mind Defaults to \code{0.02}.
#' @param maf_threshold Defaults to \code{0.01}.
#' @param hwe_case Defaults to \code{1e-10}.
#' @param hwe_control Defaults to \code{1e-06}.
#' @param hwe_quantitative Defaults to \code{1e-06}.
#' @param het_sd Defaults to \code{3}.
#' @param pihat Defaults to \code{0.2}.
#' @param hwe_test Defaults to \code{"exact"}.
#' @param x_genotypes Defaults to \code{NULL}.
#' @param reported_sex Defaults to \code{NULL}.
#' @param relatedness Defaults to \code{"pihat"}.
#' @param ibd_correction Defaults to \code{TRUE}.
#' @return A list with \code{estimate}, \code{keep_snps}, \code{keep_individuals}, \code{removed}, \code{n_snps_kept}, \code{n_individuals_kept}, \code{call_rate_snp}, \code{call_rate_ind}, \code{maf}, \code{hwe_p}, \code{heterozygosity}, \code{relatedness_matrix}, \code{kinship}, \code{ibd_states}, \code{relatedness}, \code{pruned_snps}, \code{thresholds}, \code{trait}, \code{hwe_test}, \code{note}, \code{method}.
#' @export
morie_snpqc1 <- function(genotypes, phenotype=NULL, trait="binary",
                         geno_relaxed=0.2, mind_relaxed=0.2, geno=0.02,
                         mind=0.02, maf_threshold=0.01, hwe_case=1e-10,
                         hwe_control=1e-6, hwe_quantitative=1e-6,
                         het_sd=3.0, pihat=0.2, hwe_test="exact",
                         x_genotypes=NULL, reported_sex=NULL,
                         relatedness="pihat", ibd_correction=TRUE) {
  # Run the tutorial's QC steps and report what each one removes.
  # Defaults are the tutorial's own thresholds. Kept indices are
  # 0-based to match the Python. Returns a named list.
  ch <- .snpqc1_check(genotypes)
  G <- ch$G
  n <- ch$n
  m <- ch$m
  if (!(trait %in% c("binary", "quantitative"))) {
    stop("snpqc1: trait must be 'binary' or 'quantitative'")
  }
  for (nm in c("geno", "mind", "geno_relaxed", "mind_relaxed")) {
    v <- get(nm)
    if (!(v >= 0.0 && v <= 1.0)) {
      stop(sprintf("snpqc1: %s must lie in [0, 1]", nm))
    }
  }
  if (!(maf_threshold >= 0.0 && maf_threshold < 0.5)) {
    stop("snpqc1: maf_threshold must lie in [0, 0.5)")
  }
  snps <- seq_len(m)       # 1-based column indices into G
  inds <- seq_len(n)       # 1-based row indices into G
  removed <- list(geno_relaxed=integer(0), mind_relaxed=integer(0),
                  geno=integer(0), mind=integer(0), maf=integer(0),
                  hwe=integer(0), heterozygosity=integer(0),
                  relatedness=integer(0), sex=integer(0))
  sub <- function() G[inds, snps, drop=FALSE]

  # step 1, relaxed then stringent, SNPs before individuals each time
  passes <- list(list(geno_relaxed, mind_relaxed, "geno_relaxed", "mind_relaxed"),
                 list(geno, mind, "geno", "mind"))
  for (pass in passes) {
    gthr <- pass[[1L]]
    mthr <- pass[[2L]]
    gkey <- pass[[3L]]
    mkey <- pass[[4L]]
    cr <- morie_snpqc1_call_rates(sub())
    cr_snp <- cr$per_snp
    drop <- snps[(1.0 - cr_snp) > gthr]
    removed[[gkey]] <- drop - 1L
    snps <- snps[!(snps %in% drop)]
    if (length(snps) == 0L) {
      break
    }
    cr <- morie_snpqc1_call_rates(sub())
    cr_ind <- cr$per_ind
    dropi <- inds[(1.0 - cr_ind) > mthr]
    removed[[mkey]] <- dropi - 1L
    inds <- inds[!(inds %in% dropi)]
    if (length(inds) == 0L) {
      break
    }
  }
  if (length(snps) == 0L || length(inds) == 0L) {
    stop("snpqc1: missingness filtering removed everything")
  }

  # step 2, sex discrepancy
  if (!is.null(x_genotypes)) {
    Xg <- as.matrix(x_genotypes)
    storage.mode(Xg) <- "double"
    rs <- if (is.null(reported_sex)) NULL else as.integer(reported_sex)[inds]
    sx <- morie_snpqc1_sex_check(Xg[inds, , drop=FALSE], rs)
    if (!is.null(reported_sex)) {
      bad <- inds[sx$discrepant + 1L]
      removed[["sex"]] <- bad - 1L
      inds <- inds[!(inds %in% bad)]
    }
  }

  # step 3, MAF
  freqs <- morie_snpqc1_maf(sub())
  drop <- snps[freqs < maf_threshold]
  removed[["maf"]] <- drop - 1L
  snps <- snps[!(snps %in% drop)]
  if (length(snps) == 0L) {
    stop("snpqc1: the MAF filter removed every SNP")
  }

  # step 4, HWE
  pheno <- if (is.null(phenotype)) NULL else as.numeric(phenotype)[inds]
  hwe_p <- numeric(0)
  drop <- integer(0)
  counts <- function(rows, j) {
    a <- 0L; h <- 0L; b <- 0L
    for (i in rows) {
      g <- G[i, j]
      if (is.na(g)) {
        next
      }
      if (g == 1) {
        h <- h + 1L
      } else if (g == 2) {
        a <- a + 1L
      } else {
        b <- b + 1L
      }
    }
    c(a, h, b)
  }
  for (j in snps) {
    if (trait == "quantitative" || is.null(pheno)) {
      cc <- counts(inds, j)
      p <- morie_snpqc1_hwe_pvalue(cc[1], cc[2], cc[3], hwe_test)
      hwe_p <- c(hwe_p, p)
      if (p < hwe_quantitative) {
        drop <- c(drop, j)
      }
    } else {
      cases <- inds[pheno == 1]
      ctrls <- inds[pheno == 0]
      pc <- if (length(cases) > 0L) {
        cc <- counts(cases, j)
        morie_snpqc1_hwe_pvalue(cc[1], cc[2], cc[3], hwe_test)
      } else {
        1.0
      }
      pk <- if (length(ctrls) > 0L) {
        cc <- counts(ctrls, j)
        morie_snpqc1_hwe_pvalue(cc[1], cc[2], cc[3], hwe_test)
      } else {
        1.0
      }
      hwe_p <- c(hwe_p, min(pc, pk))
      if (pc < hwe_case || pk < hwe_control) {
        drop <- c(drop, j)
      }
    }
  }
  removed[["hwe"]] <- drop - 1L
  snps <- snps[!(snps %in% drop)]
  if (length(snps) == 0L) {
    stop("snpqc1: the HWE filter removed every SNP")
  }

  # step 5, heterozygosity, +- het_sd SD from the mean
  het <- morie_snpqc1_heterozygosity(sub())
  mean_ <- mean(het)
  var_ <- sum((het - mean_) ^ 2) / max(1L, length(het) - 1L)
  sd_ <- sqrt(var_)
  drop <- inds[sd_ > 0 & abs(het - mean_) > het_sd * sd_]
  removed[["heterozygosity"]] <- drop - 1L
  inds <- inds[!(inds %in% drop)]

  # step 6, relatedness on pruned SNPs
  if (!(relatedness %in% c("pihat", "kinship"))) {
    stop(paste0("snpqc1: relatedness must be 'pihat' (PLINK's ",
                "method-of-moments IBD) or 'kinship'"))
  }
  pruned <- morie_snpqc1_ld_prune(sub())  # indices into current snps
  pruned_geno <- G[inds, snps[pruned], drop=FALSE]
  if (relatedness == "pihat") {
    im <- morie_snpqc1_ibd_moments(pruned_geno, ibd_correction)
    Zstates <- im$Z
    K <- im$pihat
  } else {
    Zstates <- NULL
    K <- morie_snpqc1_kinship_matrix(pruned_geno)
  }
  drop <- integer(0)
  ni <- length(inds)
  for (a in seq_len(ni)) {
    for (b in seq.int(a + 1L, length.out=max(0L, ni - a))) {
      if (K[a, b] > pihat && !(inds[b] %in% drop)) {
        drop <- c(drop, inds[b])
      }
    }
  }
  removed[["relatedness"]] <- drop - 1L
  inds <- inds[!(inds %in% drop)]

  cr_all <- morie_snpqc1_call_rates(genotypes)
  list(
    estimate=snps - 1L,
    keep_snps=snps - 1L,
    keep_individuals=inds - 1L,
    removed=removed,
    n_snps_kept=length(snps),
    n_individuals_kept=length(inds),
    call_rate_snp=cr_all$per_snp,
    call_rate_ind=cr_all$per_ind,
    maf=freqs,
    hwe_p=hwe_p,
    heterozygosity=het,
    relatedness_matrix=K,
    kinship=K,
    ibd_states=Zstates,
    relatedness=relatedness,
    pruned_snps=snps[pruned] - 1L,
    thresholds=list(geno_relaxed=geno_relaxed, mind_relaxed=mind_relaxed,
                    geno=geno, mind=mind, maf=maf_threshold,
                    hwe_case=hwe_case, hwe_control=hwe_control,
                    hwe_quantitative=hwe_quantitative, het_sd=het_sd,
                    pihat=pihat),
    trait=trait,
    hwe_test=hwe_test,
    note=paste0(
      if (relatedness == "pihat") {
        paste0("relatedness by PLINK's method-of-moments IBD (Purcell ",
               "et al. 2007), pi-hat = P(Z=2) + P(Z=1)/2")
      } else {
        paste0("relatedness by genomic kinship, NOT PLINK's pi-hat; ",
               "pass relatedness='pihat' for the IBD estimator")
      },
      "; the 0.2 cutoff is the tutorial's"),
    method="GWAS quality control (Marees et al. 2018, Table 1)"
  )
}

#' morie_snpqc1_cheatsheet
#'
#' Part of the snpqc1_native implementation; see the file header for the
#' source it follows.
#'
#' @return A character value.
#' @export
morie_snpqc1_cheatsheet <- function() {
  paste0(
    "snpqc1: GWAS QC (Marees et al. 2018, Table 1). Seven steps ",
    "with the tutorial's own thresholds: missingness in TWO passes ",
    "(0.2 relaxed, then 0.02) and SNPs BEFORE individuals each ",
    "time; sex check on X homozygosity (>0.8 male, <0.2 female); ",
    "MAF 0.01 large samples / 0.05 moderate; HWE 1e-10 in cases ",
    "and 1e-6 in controls for binary traits, 1e-6 for ",
    "quantitative; heterozygosity +-3 SD from the mean; ",
    "relatedness above 0.2 after LD pruning. HWE by exact ",
    "conditional test or chi-square. Relatedness has TWO routes: ",
    "PLINK's method-of-moments IBD (Purcell 2007) giving ",
    "pi-hat = P(Z=2) + P(Z=1)/2 with the paper's bounding rules ",
    "and ascertainment correction, which is the default and the ",
    "statistic the 0.2 cutoff was written for, or a genomic ",
    "kinship on the same scale."
  )
}

# compact aliases
morie_snpqc1_snp_quality_control <- morie_snpqc1
morie_snpqc1_snp_qc <- morie_snpqc1
# public names resolved by fn/_lazy_map.json
morie_snpqc1_snpqc <- morie_snpqc1
