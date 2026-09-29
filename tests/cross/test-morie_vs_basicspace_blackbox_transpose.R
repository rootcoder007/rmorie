# Cross test: BlackboxTranspose (a port of basicspace's BLACKBOXT Fortran) against basicspace itself.
# basicspace rounds coordinates to three decimals and its LAPACK singular vectors carry arbitrary signs,
# so coordinates are compared column by column up to sign within 6e-4; the fit statistics are unrounded.
.bbt_sim <- function(seed, n, q, nmiss) {
  set.seed(seed)
  theta <- cbind(rnorm(n), rnorm(n))
  z <- cbind(seq(-1.5, 1.5, length.out = q), sin(seq_len(q)))
  X <- round(4 + theta %*% t(z) + matrix(rnorm(n * q, sd = 0.6), n, q))
  X[sample(n * q, nmiss)] <- NA
  colnames(X) <- paste0("s", seq_len(q))
  X
}

.bbt_close <- function(got, ref) {
  for (k in seq_len(ncol(ref))) {
    ok <- !is.na(ref[, k])
    sgn <- if (sum(got[ok, k] * ref[ok, k]) < 0) -1 else 1
    expect_lt(max(abs(sgn * got[ok, k] - ref[ok, k])), 6e-4)
  }
}

test_that("BlackboxTranspose matches basicspace::blackbox_transpose", {
  skip_if_not_installed("basicspace")
  cases <- list(list(X = .bbt_sim(7, 60, 8, 25), dims = 3, miss = NULL),
                list(X = .bbt_sim(11, 150, 12, 90), dims = 2, miss = NULL))
  X3 <- .bbt_sim(3, 80, 9, 0)
  X3[sample(length(X3), 40)] <- 9
  cases[[3]] <- list(X = X3, dims = 2, miss = 9)
  for (cs in cases) {
    ref <- basicspace::blackbox_transpose(cs$X, missing = cs$miss, dims = cs$dims, minscale = 5)
    got <- BlackboxTranspose(cs$X, missing = cs$miss, dims = cs$dims)
    for (d in seq_len(cs$dims)) {
      gs <- do.call(rbind, got$stimuli[[d]])
      rs <- as.matrix(ref$stimuli[[d]])
      expect_equal(gs[, 1], unname(rs[, 1]))
      .bbt_close(gs[, -1, drop = FALSE], unname(rs[, -1, drop = FALSE]))
      gi <- t(vapply(got$individuals[[d]], function(v) if (is.null(v)) rep(NA_real_, d + 2) else v,
                     numeric(d + 2)))
      .bbt_close(gi, unname(as.matrix(ref$individuals[[d]])))
    }
    gf <- t(vapply(got$fits, function(f) c(f$SSE, f$SSE_explained, f$percent, f$SE, f$singular), numeric(5)))
    expect_equal(gf, unname(as.matrix(ref$fits)), tolerance = 1e-9)
    expect_equal(c(got$n_row, got$n_col, got$n_data, got$n_miss, got$ss_mean),
                 unname(c(ref$Nrow, ref$Ncol, ref$Ndata, ref$Nmiss, ref$SS_mean)), tolerance = 1e-12)
  }
})
