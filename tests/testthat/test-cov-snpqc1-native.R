# GWAS QC steps (Marees et al. 2018) and PLINK's IBD moments (Purcell et
# al. 2007), each recomputed from its formula in the test body.

sq_geno <- function() {
  set.seed(11)
  G <- matrix(sample(0:2, 12 * 8, replace = TRUE, prob = c(0.45, 0.4, 0.15)),
              12, 8)
  G[c(1, 4, 7), 3] <- NA
  G[2, 5] <- NA
  G
}

test_that("call rates and MAF follow their definitions", {
  G <- sq_geno()
  cr <- morie_snpqc1_call_rates(G)
  expect_equal(cr$per_snp, unname(colMeans(!is.na(G))), tolerance = 1e-12)
  expect_equal(cr$per_ind, unname(rowMeans(!is.na(G))), tolerance = 1e-12)
  p <- colSums(G, na.rm = TRUE) / (2 * colSums(!is.na(G)))
  expect_equal(morie_snpqc1_maf(G), unname(pmin(p, 1 - p)), tolerance = 1e-12)
  expect_equal(morie_snpqc1_maf(matrix(c(NA, NA, 2, 2), 2)), c(0, 0))
  expect_error(morie_snpqc1_maf(matrix(c(0, 3), 1)), "0, 1, 2")
  expect_error(morie_snpqc1_call_rates(matrix(numeric(0), 0, 2)), "non-empty")
})

test_that("heterozygosity is the share of called heterozygous genotypes", {
  G <- sq_geno()
  expect_equal(morie_snpqc1_heterozygosity(G),
               unname(rowSums(G == 1, na.rm = TRUE) / rowSums(!is.na(G))),
               tolerance = 1e-12)
})

sq_hwe_exact <- function(a, h, b) {
  n <- a + h + b
  na <- 2 * a + h
  nb <- 2 * n - na
  hets <- seq(na %% 2, min(na, nb), by = 2)
  pr <- vapply(hets, function(k) {
    exp(k * log(2) + lfactorial(n) + lfactorial(na) + lfactorial(nb) -
          lfactorial((na - k) / 2) - lfactorial(k) - lfactorial((nb - k) / 2) -
          lfactorial(2 * n))
  }, numeric(1))
  list(pr = pr, p = sum(pr[pr <= pr[hets == h] * (1 + 1e-9)]))
}

test_that("HWE exact and chi-square p-values match the Levene/Wigginton sums", {
  for (cc in list(c(3, 10, 20), c(8, 2, 15), c(0, 7, 5), c(10, 0, 10))) {
    ref <- sq_hwe_exact(cc[1], cc[2], cc[3])
    expect_equal(sum(ref$pr), 1, tolerance = 1e-12)
    expect_equal(morie_snpqc1_hwe_pvalue(cc[1], cc[2], cc[3]), ref$p,
                 tolerance = 1e-12)
    n <- sum(cc)
    p <- (2 * cc[1] + cc[2]) / (2 * n)
    e <- c(n * p^2, 2 * n * p * (1 - p), n * (1 - p)^2)
    chi <- sum((cc - e)^2 / e)
    expect_equal(morie_snpqc1_hwe_pvalue(cc[1], cc[2], cc[3], "chisq"),
                 stats::pchisq(chi, 1, lower.tail = FALSE), tolerance = 1e-12)
  }
  expect_identical(morie_snpqc1_hwe_pvalue(0, 0, 9, "chisq"), 1)
  expect_error(morie_snpqc1_hwe_pvalue(-1, 2, 3), "non-negative")
  expect_error(morie_snpqc1_hwe_pvalue(0, 0, 0), "no genotypes")
  expect_error(morie_snpqc1_hwe_pvalue(1, 2, 3, "lrt"), "exact")
})

test_that("sex check computes X homozygosity F and flags discrepancies", {
  X <- rbind(c(0, 2, 0, 2, 0, 2, 0, 0),
             c(1, 1, 0, 1, 2, 1, 1, 0),
             c(0, 2, 0, 2, 0, 1, 0, 0),
             c(1, 0, 1, 1, 0, 1, 1, 1))
  p <- colSums(X) / (2 * nrow(X))
  maf <- pmin(p, 1 - p)
  e <- sum(1 - 2 * maf * (1 - maf))
  Fref <- (rowSums(X != 1) - e) / (ncol(X) - e)
  s <- morie_snpqc1_sex_check(X, reported_sex = c(1, 1, 2, 2))
  expect_equal(s$F, Fref, tolerance = 1e-12)
  inf <- ifelse(Fref > 0.8, 1L, ifelse(Fref < 0.2, 2L, 0L))
  expect_identical(s$inferred_sex, inf)
  expect_identical(s$discrepant, which(inf != 0L & inf != c(1, 1, 2, 2)) - 1L)
  expect_identical(s$undetermined, which(inf == 0L) - 1L)
  expect_null(morie_snpqc1_sex_check(X)$discrepant)
  expect_error(morie_snpqc1_sex_check(X, reported_sex = 1:3), "one reported sex")
})

test_that("P(IBS | IBD) uses allele draws with and without replacement", {
  X <- 13
  Y <- 21
  T <- X + Y
  p <- X / T
  q <- Y / T
  u <- morie_snpqc1_ibs_given_ibd(X, Y, correction = FALSE)
  expect_equal(unname(u[1, ]), c(2 * p^2 * q^2, 4 * p^3 * q + 4 * p * q^3,
                                 p^4 + q^4 + 4 * p^2 * q^2), tolerance = 1e-12)
  expect_equal(unname(rowSums(u)), c(1, 1, 1), tolerance = 1e-12)
  # corrected: sampling four (Z=0) or three (Z=1) alleles without replacement
  ff <- function(n, k) prod(n - seq_len(k) + 1)
  c0 <- morie_snpqc1_ibs_given_ibd(X, Y)
  d4 <- ff(T, 4)
  d3 <- ff(T, 3)
  expect_equal(c0[1, 1], 2 * ff(X, 2) * ff(Y, 2) / d4, tolerance = 1e-12)
  expect_equal(c0[1, 2], 4 * (ff(X, 3) * Y + X * ff(Y, 3)) / d4, tolerance = 1e-12)
  expect_equal(c0[1, 3], (ff(X, 4) + ff(Y, 4) + 4 * ff(X, 2) * ff(Y, 2)) / d4,
               tolerance = 1e-12)
  expect_equal(c0[2, 2], 2 * (ff(X, 2) * Y + X * ff(Y, 2)) / d3, tolerance = 1e-12)
  expect_equal(c0[2, 3], (ff(X, 3) + ff(Y, 3) + ff(X, 2) * Y + X * ff(Y, 2)) / d3,
               tolerance = 1e-12)
  # small counts fall back to the textbook table
  expect_equal(morie_snpqc1_ibs_given_ibd(3, 9),
               morie_snpqc1_ibs_given_ibd(3, 9, correction = FALSE))
  expect_error(morie_snpqc1_ibs_given_ibd(0, 0), "no non-missing")
})

test_that("IBD moments invert the expected IBS counts; pihat = Z2 + Z1/2", {
  G <- sq_geno()[, -3]
  G[12, ] <- G[11, ]
  im <- morie_snpqc1_ibd_moments(G)
  expect_equal(im$Z[[11, 12]], c(0, 0, 1), tolerance = 1e-12)
  expect_equal(im$pihat[11, 12], 1, tolerance = 1e-12)
  expect_equal(diag(im$pihat), rep(1, 12))
  # recompute one pair from Purcell et al. (2007) eqs.
  i <- 1
  k <- 3
  E <- matrix(0, 3, 3)
  obs <- c(0, 0, 0)
  for (j in seq_len(ncol(G))) {
    if (is.na(G[i, j]) || is.na(G[k, j])) next
    cl <- G[!is.na(G[, j]), j]
    x <- sum(cl)
    y <- 2 * length(cl) - x
    if (x == 0 || y == 0) next
    E <- E + morie_snpqc1_ibs_given_ibd(x, y)
    ibs <- 2 - abs(G[i, j] - G[k, j])
    obs[ibs + 1] <- obs[ibs + 1] + 1
  }
  z0 <- obs[1] / E[1, 1]
  z1 <- (obs[2] - z0 * E[1, 2]) / E[2, 2]
  z2 <- (obs[3] - z0 * E[1, 3] - z1 * E[2, 3]) / E[3, 3]
  if (z0 > 1) {
    z <- c(1, 0, 0)
  } else {
    if (z0 < 0) {
      z0 <- 0
      s <- z1 + z2
      z1 <- z1 / s
      z2 <- z2 / s
    }
    z <- c(z0, max(z1, 0), max(z2, 0))
    z <- z / sum(z)
  }
  expect_equal(im$Z[[i, k]], z, tolerance = 1e-12)
  expect_equal(im$pihat[i, k], z[3] + z[2] / 2, tolerance = 1e-12)
  expect_equal(morie_snpqc1_pihat_matrix(G), im$pihat, tolerance = 1e-12)
  expect_equal(morie_snpqc1_pihat_matrix(G, correction = FALSE),
               morie_snpqc1_ibd_moments(G, FALSE)$pihat, tolerance = 1e-12)
})

test_that("kinship is the GRM of centred, scaled genotypes", {
  G <- sq_geno()
  G[2, 5] <- 1
  G <- G[, -3]
  p <- colSums(G) / (2 * nrow(G))
  Z <- sweep(G, 2, 2 * p) / rep(sqrt(2 * p * (1 - p)), each = nrow(G))
  expect_equal(morie_snpqc1_kinship_matrix(G), tcrossprod(Z) / ncol(G),
               tolerance = 1e-12)
  expect_error(morie_snpqc1_kinship_matrix(matrix(0, 3, 2)), "polymorphic")
})

test_that("LD pruning drops the later SNP of any pair above r2", {
  set.seed(3)
  a <- sample(0:2, 20, replace = TRUE)
  b <- sample(0:2, 20, replace = TRUE)
  cc <- sample(0:2, 20, replace = TRUE)
  G <- cbind(a, b, a, cc, 2 - b)
  r2 <- stats::cor(G)^2
  keep <- morie_snpqc1_ld_prune(G, r2 = 0.9)
  expect_identical(keep, c(1L, 2L, 4L))
  # a threshold above every observed r^2 keeps everything
  thr <- max(r2[c(1, 2, 4), c(1, 2, 4)][upper.tri(diag(3))]) + 1e-6
  expect_identical(morie_snpqc1_ld_prune(G[, c(1, 2, 4)], r2 = thr), 1:3)
})

test_that("morie_snpqc1 runs the tutorial steps in order", {
  G <- sq_geno()
  G[, 6] <- 0
  G[1:5, 1] <- NA
  G[12, ] <- G[11, ]
  pheno <- rep(0:1, 6)
  r <- morie_snpqc1(G, phenotype = pheno)
  expect_identical(r$removed$geno_relaxed,
                   which(colMeans(is.na(G)) > 0.2) - 1L)
  expect_true(5L %in% r$removed$maf)
  expect_true(11L %in% r$removed$relatedness)
  expect_false(11L %in% r$keep_individuals)
  expect_equal(r$call_rate_snp, unname(colMeans(!is.na(G))), tolerance = 1e-12)
  expect_identical(r$keep_snps, r$estimate)
  expect_identical(r$n_snps_kept, length(r$keep_snps))
  expect_identical(identical(morie_snpqc1_snpqc, morie_snpqc1), TRUE)
  rk <- morie_snpqc1(G, trait = "quantitative", relatedness = "kinship",
                     hwe_test = "chisq")
  expect_null(rk$ibd_states)
  expect_match(rk$note, "kinship")
  expect_error(morie_snpqc1(G, trait = "count"), "trait")
  expect_error(morie_snpqc1(G, geno = 2), "geno must lie")
  expect_error(morie_snpqc1(G, maf_threshold = 0.5), "maf_threshold")
  expect_error(morie_snpqc1(G, relatedness = "grm"), "relatedness")
})

test_that("the cheatsheet names the seven steps", {
  cs <- morie_snpqc1_cheatsheet()
  expect_length(cs, 1L)
  expect_match(cs, "Marees et al. 2018", fixed = TRUE)
})
