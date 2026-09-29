# Coverage for the molecular fingerprints (atmpair.R, ecfp4/6 and fcfp4,
# both the .t1 and the *_native twins): the atom invariants and the first
# Morgan layer are rebuilt from the hash definition, the bit vectors from
# the identifiers, and the two twin implementations are checked against
# each other.

fp_mix <- function(h, v) (h * 1000003 + (v %% 2147483647)) %% 2147483647
# 2-methylpropan-1-ol skeleton: C1-C2(-C3)-C4-O5, with a double bond C2=C3
fp_A <- matrix(0, 5, 5)
fp_A[1, 2] <- fp_A[2, 1] <- 1
fp_A[2, 3] <- fp_A[3, 2] <- 2
fp_A[2, 4] <- fp_A[4, 2] <- 1
fp_A[4, 5] <- fp_A[5, 4] <- 1
fp_Z <- c(6, 6, 6, 6, 8)
fp_H <- c(3, 0, 2, 2, 1)

inv_ref <- function(A, z, h, ring = rep(0, length(z))) {
  deg <- rowSums(A != 0)
  vapply(seq_along(z), function(i) {
    comps <- c(z[i], deg[i] + h[i], h[i], 0, 0, if (ring[i] != 0) 1)
    x <- 0
    for (cc in comps) x <- fp_mix(x, cc)
    x
  }, 0)
}
layer1_ref <- function(A, inv) {
  vapply(seq_along(inv), function(i) {
    nb <- which(A[i, ] != 0)
    o <- A[i, nb]
    v <- inv[nb]
    ord <- order(o, v)
    x <- fp_mix(fp_mix(0, 0), inv[i])
    for (k in ord) x <- fp_mix(fp_mix(x, o[k]), v[k])
    x
  }, 0)
}

test_that("ECFP invariants and first Morgan layer follow the hash definition", {
  inv <- inv_ref(fp_A, fp_Z, fp_H)
  r0 <- Ecfp4(fp_A, fp_Z, numhs = fp_H, radius = 0, nbits = 64)
  expect_equal(r0$identifiers, sort(unique(inv)))
  expect_equal(r0$bits, as.integer(tabulate(inv %% 64 + 1, 64) > 0))
  expect_equal(r0$count, tabulate(inv %% 64 + 1, 64))
  r1 <- Ecfp4(fp_A, fp_Z, numhs = fp_H, radius = 1, nbits = 64)
  expect_equal(r1$identifiers, sort(unique(c(inv, layer1_ref(fp_A, inv)))))
  n1 <- morie_ecfp4(fp_A, fp_Z, numhs = fp_H, radius = 1, nbits = 64)
  expect_equal(n1$identifiers, r1$identifiers)
  ring <- c(0, 1, 1, 0, 0)
  rr <- Ecfp4(fp_A, fp_Z, numhs = fp_H, inring = ring, radius = 0)
  expect_equal(rr$identifiers, sort(unique(inv_ref(fp_A, fp_Z, fp_H, ring))))
})

test_that("ECFP4/ECFP6 twins agree and radius 3 extends radius 2", {
  e4 <- Ecfp4(fp_A, fp_Z, numhs = fp_H, charge = c(0, 0, 0, 0, -1), nbits = 128)
  n4 <- morie_ecfp4(fp_A, fp_Z, numhs = fp_H, charge = c(0, 0, 0, 0, -1), nbits = 128)
  expect_equal(e4$identifiers, n4$identifiers)
  expect_equal(e4$bits, n4$bits)
  e6 <- Ecfp6(fp_A, fp_Z, numhs = fp_H, nbits = 128)
  n6 <- morie_ecfp6(fp_A, fp_Z, numhs = fp_H, nbits = 128)
  expect_equal(e6$identifiers, n6$identifiers)
  e4b <- Ecfp4(fp_A, fp_Z, numhs = fp_H, nbits = 128)
  expect_true(all(e4b$identifiers %in% e6$identifiers))
  expect_equal(e6$radius, 3L)
  expect_error(Ecfp4(fp_A[, 1:3], fp_Z), "square")
  bad <- fp_A
  bad[1, 2] <- 2
  expect_error(Ecfp4(bad, fp_Z), "symmetric")
  expect_error(Ecfp4(fp_A, fp_Z[-1]), "one entry per atom")
  expect_error(Ecfp4(fp_A, fp_Z, numhs = 1:2), "wrong length")
  expect_error(Ecfp4(fp_A, fp_Z, nbits = 0), "positive")
  expect_error(Ecfp6(fp_A, fp_Z[-1]), "one entry per atom")
  expect_error(morie_ecfp4(fp_A[, 1:3], fp_Z), "square")
  expect_error(morie_ecfp4(bad, fp_Z), "symmetric")
  expect_error(morie_ecfp4(fp_A, fp_Z[-1]), "one entry per atom")
  expect_error(morie_ecfp6(fp_A, fp_Z, charge = 1), "wrong length")
})

test_that("FCFP4 encodes the six feature flags as a bit code", {
  expect_identical(MORIE_FCFP_FEATURE_CLASSES,
                   c("donor", "acceptor", "aromatic", "halogen", "basic", "acidic"))
  expect_type(MORIE_FCFP_FEATURE_CLASSES, "character")
  expect_length(MORIE_FCFP_FEATURE_CLASSES, 6L)
  F <- rbind(c(0, 0, 0, 0, 0, 0), c(0, 0, 1, 0, 0, 0), c(0, 0, 1, 0, 0, 0),
             c(0, 0, 0, 0, 0, 0), c(1, 1, 0, 0, 0, 0))
  code <- as.numeric(F %*% 2^(0:5))
  f <- Fcfp4(fp_A, F, nbits = 64, radius = 1)
  expect_equal(f$featurecode, code)
  expect_equal(f$identifiers, sort(unique(c(code, layer1_ref(fp_A, code)))))
  nf <- morie_fcfp4(fp_A, F, nbits = 64, radius = 1)
  expect_equal(nf$identifiers, f$identifiers)
  expect_equal(Fcfp4(fp_A, code, nbits = 64, radius = 1)$identifiers, f$identifiers)
  expect_equal(morie_fcfp4(fp_A, code, nbits = 64, radius = 1)$identifiers, f$identifiers)
  expect_error(Fcfp4(fp_A, F[-1, ]), "one entry per atom")
  expect_error(Fcfp4(fp_A, F[, -1]), "6 flags")
  expect_error(Fcfp4(fp_A, code[-1]), "one entry per atom")
  expect_error(morie_fcfp4(fp_A, F[-1, ]), "one entry per atom")
  expect_error(morie_fcfp4(fp_A, F[, -1]), "6 flags")
  expect_error(morie_fcfp4(fp_A, code[-1]), "one entry per atom")
})

test_that("Atompairfp hashes (type, type, distance) for every connected pair", {
  r <- Atompairfp(fp_A, fp_Z, nbits = 97)
  D <- matrix(Inf, 5, 5)
  D[fp_A != 0] <- 1
  diag(D) <- 0
  for (k in 1:5) D <- pmin(D, outer(D[, k], D[k, ], "+"))
  pr <- which(upper.tri(D), arr.ind = TRUE)
  h <- apply(pr, 1, function(ij) {
    ta <- min(fp_Z[ij])
    tb <- max(fp_Z[ij])
    ((ta * 1000003 + tb) * 1000033 + D[ij[1], ij[2]]) %% 97
  })
  expect_equal(r$count, tabulate(h + 1, 97))
  expect_equal(r$npairs, 10L)
  expect_equal(sort(r$distance), sort(D[upper.tri(D)]))
  expect_equal(Atompairfp(fp_A, fp_Z, maxdist = 1)$npairs, 4L)
  split <- fp_A
  split[4, 5] <- split[5, 4] <- 0
  expect_equal(Atompairfp(split, fp_Z)$npairs, 6L)
  expect_error(Atompairfp(fp_A[, 1:3], fp_Z), "square")
  expect_error(Atompairfp(fp_A, fp_Z[-1]), "one entry per atom")
  expect_error(Atompairfp(fp_A, fp_Z, nbits = 0), "positive")
})
