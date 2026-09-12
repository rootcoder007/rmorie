# Taphonomy: schemas, the absorbing decay chain, the forensic likelihood
# ratio, and the compositional transforms.
#
# Anchors outside the module: a competing-risks argument for the absorbing
# chain (each transient stage leaves for "mummified" with probability
# m / (m + p) independently of the others, so P(mummified) from the first
# stage is 1 - (p / (p + m))^k and the expected number of steps is
# k / (p + m)) -- computed without the fundamental matrix the module uses;
# stats::dnorm for the Gaussian evidence log-likelihood; VanderWeele &
# Ding's RR ~ exp(0.91 d) bridge; the ENFSI (2015) verbal bands; and
# Aitchison's compositional identities, including the fact that an
# orthonormal ILR basis is an isometry of the CLR plane.

test_that("both schemas are zero-row templates with typed, labelled columns", {
  df <- morie_taphonomy_schema()
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 0L)
  roles <- attr(df, "role")
  # every column carries exactly one role, and the roles are the documented set
  expect_equal(names(roles), names(df))
  expect_true(all(roles %in% c("treatment", "covariate", "measurement",
                               "outcome")))
  # the treatment and the outcome the estimator defaults to are both present
  expect_equal(names(roles)[roles == "treatment"], "lime_treatment")
  expect_equal(names(roles)[roles == "outcome"], "preservation_score")
  # the declared type is the column's actual type
  expect_type(df$lime_treatment, "integer")
  expect_type(df$preservation_score, "double")
  expect_type(df$temp_c, "double")
  expect_type(df$casket_sealed, "integer")

  p <- morie_taphonomy_pmi_schema()
  expect_equal(nrow(p), 0L)
  pr <- attr(p, "role")
  expect_equal(names(pr), names(p))
  expect_equal(names(pr)[pr == "outcome"], "pmi_days")
  # two scored observations, eight environmental drivers, one outcome
  expect_equal(as.vector(table(pr)[c("observation", "environment",
                                     "outcome")]),
               c(2L, 8L, 1L))
  expect_type(p$decomp_stage, "integer")
  expect_type(p$accumulated_deg_days, "double")
})

test_that("the SMD-to-RR bridge is exp(0.91 d), folded to the >1 side", {
  for (d in c(0.1, 0.5, 1.3)) {
    expect_equal(rmorie:::.taphonomy_smd_to_rr(d), exp(0.91 * d))
  }
  # a protective effect is reported as its reciprocal, because the E-value
  # machinery is defined on the ">1" side of the ratio scale
  for (d in c(-0.1, -0.5, -1.3)) {
    expect_equal(rmorie:::.taphonomy_smd_to_rr(d), exp(-0.91 * d))
  }
  # so the map is symmetric in the sign of d, and never below one
  expect_equal(rmorie:::.taphonomy_smd_to_rr(0.7),
               rmorie:::.taphonomy_smd_to_rr(-0.7))
  expect_equal(rmorie:::.taphonomy_smd_to_rr(0), 1)
  expect_true(all(vapply(c(-2, -0.3, 0, 0.3, 2),
                         rmorie:::.taphonomy_smd_to_rr, 0) >= 1))
})

test_that("the verbal scale reports the ENFSI band and the side it favours", {
  f <- rmorie:::.taphonomy_lr_verbal
  expect_equal(f(1), "no support either way")
  expect_equal(f(5), "weak support for H1")
  expect_equal(f(50), "moderate support for H1")
  expect_equal(f(500), "moderately strong support for H1")
  expect_equal(f(5000), "strong support for H1")
  expect_equal(f(5e5), "very strong support for H1")
  expect_equal(f(5e7), "extremely strong support for H1")
  # the band is read off the reciprocal when the evidence favours H2
  expect_equal(f(1 / 5), "weak support for H2")
  expect_equal(f(1 / 500), "moderately strong support for H2")
  expect_equal(f(1 / 5e7), "extremely strong support for H2")
  # the band edges belong to the lower band
  expect_equal(f(10), "weak support for H1")
  expect_equal(f(100), "moderate support for H1")
  expect_equal(f(1000), "moderately strong support for H1")
  expect_equal(f(1e4), "strong support for H1")
  expect_equal(f(1e6), "very strong support for H1")
  # an overflowed ratio is still reported rather than dropped
  expect_equal(f(Inf), "extremely strong support (LR effectively infinite)")
  expect_equal(f(NaN), "extremely strong support (LR effectively infinite)")
})

test_that("the evidence log-likelihood is the summed normal log-density", {
  e <- c(1200, 1310, 1180)
  expect_equal(morie_taphonomy_evidence_loglik(e, mean = 1250, sd = 90),
               sum(stats::dnorm(e, 1250, 90, log = TRUE)))
  # the model parameters recycle over the evidence vector
  expect_equal(morie_taphonomy_evidence_loglik(e, mean = c(1200, 1300, 1200),
                                               sd = c(50, 60, 70)),
               sum(stats::dnorm(e, c(1200, 1300, 1200), c(50, 60, 70),
                                log = TRUE)))
  # a single observation at the model mean is -log(sd sqrt(2 pi))
  expect_equal(morie_taphonomy_evidence_loglik(7, mean = 7, sd = 2),
               -log(2 * sqrt(2 * pi)))
  expect_error(morie_taphonomy_evidence_loglik(numeric(0), 0, 1), "empty")
  expect_error(morie_taphonomy_evidence_loglik(c(1, NA), 0, 1), "non-finite")
  expect_error(morie_taphonomy_evidence_loglik(1, 0, 0), "must be > 0")
})

test_that("the likelihood ratio exponentiates the log-likelihood difference", {
  r <- morie_taphonomy_likelihood_ratio(-3.1, -12.7)
  expect_equal(r$log_lr, -3.1 - (-12.7))
  expect_equal(r$lr, exp(9.6))
  expect_equal(r$log10_lr, 9.6 / log(10))
  expect_equal(r$verbal, "very strong support for H1")
  # equal support under both hypotheses is a ratio of one
  eq <- morie_taphonomy_likelihood_ratio(-5, -5)
  expect_equal(eq$lr, 1)
  expect_equal(eq$log10_lr, 0)
  expect_equal(eq$verbal, "no support either way")
  # reversing the hypotheses inverts the ratio and flips the reported side
  rev <- morie_taphonomy_likelihood_ratio(-12.7, -3.1)
  expect_equal(rev$lr, 1 / r$lr)
  expect_equal(rev$log10_lr, -r$log10_lr)
  expect_true(grepl("for H2", rev$verbal))
  expect_true(grepl("times less", rev$interpretation))
  # the interpretation states the ratio it was given
  expect_true(grepl("not proof", r$interpretation, fixed = TRUE))
  # a difference large enough to overflow is reported, not dropped
  big <- morie_taphonomy_likelihood_ratio(0, -1e4)
  expect_true(is.infinite(big$lr))
  expect_equal(big$log10_lr, 1e4 / log(10))
  expect_true(grepl("infinitely", big$interpretation))
})

test_that("the preservation LR routes both models through the same density", {
  e <- c(1200, 1310, 1180)
  nat <- list(mean = 1250, sd = 90)
  alt <- list(mean = 300, sd = 120)
  r <- morie_taphonomy_preservation_lr(e, nat, alt)
  expect_equal(r$loglik_h1,
               morie_taphonomy_evidence_loglik(e, nat$mean, nat$sd))
  expect_equal(r$loglik_h2,
               morie_taphonomy_evidence_loglik(e, alt$mean, alt$sd))
  expect_equal(r$log_lr, r$loglik_h1 - r$loglik_h2)
  # a lime-processed calcium signature is overwhelming evidence against the
  # untreated model
  expect_true(r$log10_lr > 20)
  expect_equal(r$verbal, "extremely strong support for H1")
  # two identical models can never favour either side
  same <- morie_taphonomy_preservation_lr(e, nat, nat)
  expect_equal(same$lr, 1)
  expect_error(morie_taphonomy_preservation_lr(e, list(mean = 1), alt),
               "list\\(mean=, sd=\\)")
  expect_error(morie_taphonomy_preservation_lr(e, nat, list(sd = 1)),
               "list\\(mean=, sd=\\)")
})

test_that("the decay chain is a stochastic matrix with two absorbing fates", {
  ch <- morie_taphonomy_decay_chain(0.7, decay_rate = 0.5, mummify_rate = 0.5)
  expect_equal(ch$states, c("fresh", "bloat", "active", "advanced",
                            "skeletal", "mummified"))
  expect_equal(unname(rowSums(ch$P)), rep(1, 6))
  expect_true(all(ch$P >= 0))
  # the two terminal fates never leave themselves
  expect_equal(ch$P["skeletal", "skeletal"], 1)
  expect_equal(ch$P["mummified", "mummified"], 1)
  # each transient stage either stays, advances one stage, or is diverted
  p <- 0.5 * (1 - 0.7)
  m <- 0.5 * 0.7
  expect_equal(ch$P["fresh", "bloat"], p)
  expect_equal(ch$P["fresh", "mummified"], m)
  expect_equal(ch$P["fresh", "fresh"], 1 - p - m)
  # the last transient stage advances to skeletal rather than to a stage
  expect_equal(ch$P["advanced", "skeletal"], p)
  expect_equal(ch$P["advanced", "mummified"], m)
  # with no preservation nothing is ever diverted
  nat <- morie_taphonomy_decay_chain(0)
  expect_equal(unname(nat$P[nat$transient, "mummified"]), rep(0, 4))
  expect_equal(nat$P["fresh", "bloat"], 0.5)
  # leaving probabilities above one are renormalised rather than clipped
  hot <- morie_taphonomy_decay_chain(0.5, decay_rate = 1, mummify_rate = 1)
  expect_equal(hot$P["fresh", "fresh"], 0)
  expect_equal(hot$P["fresh", "bloat"], 0.5)
  expect_equal(hot$P["fresh", "mummified"], 0.5)
  # a single transient stage is allowed, and goes straight to a fate
  one <- morie_taphonomy_decay_chain(0.4, states = "fresh")
  expect_equal(one$transient, "fresh")
  expect_equal(one$P["fresh", "skeletal"], 0.5 * 0.6)

  expect_error(morie_taphonomy_decay_chain(1.5), "must be in \\[0, 1\\]")
  expect_error(morie_taphonomy_decay_chain(0, decay_rate = 0), "required")
  expect_error(morie_taphonomy_decay_chain(0, mummify_rate = 2), "required")
  expect_error(morie_taphonomy_decay_chain(0, states = c("a", "a")), "unique")
  expect_error(morie_taphonomy_decay_chain(0, states = character(0)), "unique")
})

test_that("absorption matches the competing-risks closed form", {
  # Each transient stage leaves with probability p + m, and conditional on
  # leaving it is diverted to "mummified" with probability m / (p + m); a
  # body reaches "skeletal" only by surviving that draw at all k stages.
  # Each visit dwells a geometric number of steps with mean 1 / (p + m).
  # This is an argument about the embedded jump chain, independent of the
  # fundamental matrix the module inverts.
  for (pres in c(0.2, 0.5, 0.7, 1)) {
    for (dr in c(0.3, 0.8)) {
      for (mr in c(0.4, 1)) {
        k <- 4L
        ch <- morie_taphonomy_decay_chain(pres, decay_rate = dr,
                                          mummify_rate = mr)
        p <- dr * (1 - pres)
        m <- mr * pres
        tot <- p + m
        if (tot > 1) {
          p <- p / tot
          m <- m / tot
          tot <- 1
        }
        a <- morie_taphonomy_decay_absorption(ch)
        expect_equal(sum(a$absorption), 1)
        expect_equal(a$absorption[["mummified"]], 1 - (p / tot)^k)
        expect_equal(a$absorption[["skeletal"]], (p / tot)^k)
        # a body diverted to "mummified" stops early, so the expected
        # number of steps is the expected number of stages visited,
        # sum_{j<k} (p/tot)^j, times the mean geometric dwell 1/tot
        r <- p / tot
        stages <- if (r < 1) (1 - r^k) / (1 - r) else k
        expect_equal(a$expected_steps, stages / tot)
      }
    }
  }
  # starting later in the sequence leaves fewer stages to be diverted at
  ch <- morie_taphonomy_decay_chain(0.6)
  p <- 0.5 * 0.4
  m <- 0.5 * 0.6
  for (i in seq_len(4L)) {
    a <- morie_taphonomy_decay_absorption(ch, start = ch$transient[i])
    k <- 4L - i + 1L
    r <- p / (p + m)
    expect_equal(a$absorption[["skeletal"]], r^k)
    expect_equal(a$expected_steps, (1 - r^k) / (1 - r) / (p + m))
  }
  # so the chance of skeletonising rises the further along the body starts
  fates <- vapply(ch$transient, function(s) {
    morie_taphonomy_decay_absorption(ch, s)$absorption[["skeletal"]]
  }, 0)
  expect_true(all(diff(fates) > 0))
  # with no preservation every body skeletonises
  nat <- morie_taphonomy_decay_absorption(morie_taphonomy_decay_chain(0))
  expect_equal(nat$absorption[["skeletal"]], 1)
  expect_equal(nat$absorption[["mummified"]], 0)
  # the fundamental matrix is upper triangular: decay never runs backwards
  a <- morie_taphonomy_decay_absorption(ch)
  expect_equal(a$fundamental[lower.tri(a$fundamental)],
               rep(0, sum(lower.tri(a$fundamental))))
  expect_equal(dim(a$B), c(4L, 2L))
  expect_error(morie_taphonomy_decay_absorption(ch, start = "skeletal"),
               "must be a transient state")
})

test_that("the fate delta is the treated-minus-natural mummification gap", {
  d <- morie_taphonomy_decay_delta(0.7)
  expect_equal(d$p_mummified_natural, 0)
  p <- 0.5 * 0.3
  m <- 0.5 * 0.7
  expect_equal(d$p_mummified_treated, 1 - (p / (p + m))^4)
  expect_equal(d$delta, d$p_mummified_treated - d$p_mummified_natural)
  # a stronger preservation factor diverts more bodies
  deltas <- vapply(c(0.2, 0.4, 0.6, 0.8, 1), function(z) {
    morie_taphonomy_decay_delta(z)$delta
  }, 0)
  expect_true(all(diff(deltas) > 0))
  # starting later leaves fewer stages to divert at, so the delta shrinks
  expect_true(morie_taphonomy_decay_delta(0.7, start = "advanced")$delta <
                d$delta)
  # the chain arguments are passed through
  two <- morie_taphonomy_decay_delta(0.7, states = c("fresh", "bloat"))
  expect_equal(two$p_mummified_treated, 1 - (p / (p + m))^2)
  expect_true(grepl("burial practice", d$interpretation))
  expect_error(morie_taphonomy_decay_delta(0), "in \\(0, 1\\]")
  expect_error(morie_taphonomy_decay_delta(1.2), "in \\(0, 1\\]")
})

test_that("a simulated path ends at a fate and is reproducible", {
  ch <- morie_taphonomy_decay_chain(0.7)
  a <- morie_taphonomy_decay_simulate(ch, seed = 1)
  expect_true(is.character(a))
  expect_equal(a[1], "fresh")
  expect_true(tail(a, 1) %in% ch$absorbing)
  # no absorbing state appears before the end, and the path is contiguous
  expect_false(any(head(a, -1) %in% ch$absorbing))
  expect_equal(morie_taphonomy_decay_simulate(ch, seed = 1), a)
  expect_false(identical(morie_taphonomy_decay_simulate(ch, seed = 2), a))
  # a truncated run reports what it reached rather than padding with blanks
  short <- morie_taphonomy_decay_simulate(ch, n_steps = 1L, seed = 3)
  expect_length(short, 2L)
  expect_true(all(nzchar(short)))
  # every visited state is a state of the chain
  expect_true(all(a %in% ch$states))
  # a chain that cannot be diverted can only end skeletal
  nat <- morie_taphonomy_decay_chain(0)
  expect_equal(tail(morie_taphonomy_decay_simulate(nat, seed = 5), 1),
               "skeletal")
  expect_error(morie_taphonomy_decay_simulate(ch, start = "mummified"),
               "must be a transient state")
})

test_that("closure puts every row on the simplex", {
  X <- rbind(c(2, 3, 5), c(1, 1, 8))
  cl <- rmorie:::.taphonomy_close(X, 0)
  expect_equal(unname(rowSums(cl)), c(1, 1))
  expect_equal(cl[1, ], c(0.2, 0.3, 0.5))
  # the pseudocount is added before closing, so it shifts the parts
  ps <- rmorie:::.taphonomy_close(rbind(c(0, 1)), 1)
  expect_equal(unname(ps[1, ]), c(1, 2) / 3)
  # closure is scale invariant: a composition is its ratios
  expect_equal(rmorie:::.taphonomy_close(X * 7, 0), cl)
  expect_error(rmorie:::.taphonomy_close(rbind(c(-1, 2)), 0), "non-negative")
  expect_error(rmorie:::.taphonomy_close(rbind(c("a", "b")), 0),
               "numeric composition")
})

test_that("CLR and ILR satisfy Aitchison's identities", {
  set.seed(11)
  X <- matrix(runif(8 * 5, 1, 10), nrow = 8)
  colnames(X) <- paste0("e", 1:5)
  cl <- morie_taphonomy_clr(X, pseudocount = 0)
  # CLR is the log parts centred, so every row sums to zero -- the rank
  # deficiency the docs warn about
  expect_equal(unname(rowSums(cl)), rep(0, 8))
  L <- log(X / rowSums(X))
  expect_equal(unname(cl), unname(L - rowMeans(L)))
  expect_equal(colnames(cl), colnames(X))
  # CLR is invariant to rescaling a whole sample
  expect_equal(morie_taphonomy_clr(X * 3, pseudocount = 0), cl)

  il <- morie_taphonomy_ilr(X, pseudocount = 0)
  expect_equal(dim(il), c(8L, 4L))
  expect_equal(colnames(il), paste0("ilr", 1:4))
  # the pivot coordinates are the Egozcue closed form
  for (i in 1:4) {
    expect_equal(unname(il[, i]),
                 sqrt(i / (i + 1)) *
                   (rowMeans(L[, seq_len(i), drop = FALSE]) - L[, i + 1L]))
  }
  # the basis is orthonormal, so ILR is an isometry of the CLR plane: the
  # Aitchison norm is preserved and so is every pairwise distance
  expect_equal(unname(rowSums(il^2)), unname(rowSums(cl^2)))
  expect_equal(as.numeric(dist(il)), as.numeric(dist(cl)))
  # and it is scale invariant too
  expect_equal(morie_taphonomy_ilr(X * 3, pseudocount = 0), il)
  # a two-part composition gives a single balance
  two <- morie_taphonomy_ilr(rbind(c(1, 4)), pseudocount = 0)
  expect_equal(dim(two), c(1L, 1L))
  expect_equal(unname(two[1, 1]), sqrt(1 / 2) * log(1 / 4))
  # a perfectly even composition sits at the origin of both charts
  ev <- matrix(rep(1, 4), nrow = 1)
  expect_equal(unname(morie_taphonomy_clr(ev, 0)[1, ]), rep(0, 4))
  expect_equal(unname(morie_taphonomy_ilr(ev, 0)[1, ]), rep(0, 3))
  expect_error(morie_taphonomy_ilr(matrix(1, nrow = 2, ncol = 1)),
               ">= 2 parts")
})

test_that("the pXRF simulator returns closed compositions per condition", {
  d <- morie_taphonomy_simulate_pxrf(50, seed = 1)
  expect_equal(nrow(d), 50L)
  expect_equal(names(d), c("Ca", "P", "Fe", "Sr", "Pb", "Zn", "condition",
                           "lime_treatment"))
  # a Dirichlet draw is closed to one
  expect_equal(rowSums(d[, 1:6]), rep(1, 50))
  expect_equal(unique(d$condition), "control")
  expect_equal(unique(d$lime_treatment), 0L)
  expect_true(attr(d, "synthetic"))
  expect_equal(attr(d, "elements"), c("Ca", "P", "Fe", "Sr", "Pb", "Zn"))
  expect_equal(morie_taphonomy_simulate_pxrf(50, seed = 1)[, 1:6],
               d[, 1:6])
  expect_false(isTRUE(all.equal(morie_taphonomy_simulate_pxrf(50, seed = 2)$Ca,
                                d$Ca)))

  # the treatment condition is the quicklime calcium spike, and each part's
  # mean is its Dirichlet share alpha_j / sum(alpha)
  t50 <- morie_taphonomy_simulate_pxrf(4000, "treatment", seed = 2)
  expect_equal(unique(t50$lime_treatment), 1L)
  al <- attr(t50, "alpha")
  expect_equal(colMeans(t50[, 1:6]), al / sum(al),
               tolerance = 0.02, ignore_attr = TRUE)
  expect_true(mean(t50$Ca) > mean(d$Ca))

  # ppm output is the same composition on a total-signal scale
  pp <- morie_taphonomy_simulate_pxrf(50, seed = 1, as_ppm = TRUE)
  expect_equal(rowSums(pp[, 1:6]), rep(1e6, 50))
  expect_equal(pp[, 1:6] / 1e6, d[, 1:6])
  expect_equal(rowSums(morie_taphonomy_simulate_pxrf(
    3, seed = 1, as_ppm = TRUE, total_ppm = 500)[, 1:6]), rep(500, 3))

  # a caller-supplied concentration vector drives the parts
  cu <- morie_taphonomy_simulate_pxrf(2000, elements = c("Ca", "Sr"),
                                      alpha = c(9, 1), seed = 3)
  expect_equal(names(cu)[1:2], c("Ca", "Sr"))
  expect_equal(mean(cu$Ca), 0.9, tolerance = 0.02)
  expect_error(morie_taphonomy_simulate_pxrf(2, elements = c("Ca", "Sr")),
               "supply `alpha`")
  expect_error(morie_taphonomy_simulate_pxrf(2, elements = c("Ca", "Sr"),
                                             alpha = c(1, 2, 3)),
               "must equal length")
  expect_error(morie_taphonomy_simulate_pxrf(2, elements = "Ca", alpha = 0),
               "must be > 0")
})
