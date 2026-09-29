# Coverage for siu_panel .. Snr2 exports. Every expectation is recomputed in
# the test body.

test_that("morie_siu_panel reads, audits and extracts fields through a supplied chat function", {
  html <- "<html><body><p>The SIU investigated. Two subject officers were involved.</p></body></html>"
  sch <- morie_siu_schema()
  f1 <- sch$name[1]
  calls <- character(0)
  chat <- function(model, prompt) {
    calls <<- c(calls, model)
    if (grepl("You are the AUDITOR", prompt, fixed = TRUE)) {
      sprintf("{\"%s\": {\"value\": \"audited\", \"quote\": \"q\"}}", f1)
    } else {
      sprintf("noise {\"%s\": {\"value\": \"read-%s\", \"quote\": \"q\"}} tail", f1, model)
    }
  }
  r1 <- morie_siu_panel(html, mode = 1L, readers = "m1", chat = chat)
  expect_equal(unname(r1$fields[f1]), "read-m1")
  expect_true(all(is.na(r1$fields[setdiff(sch$name, f1)])))
  expect_length(r1$audit_chain, 0)
  expect_equal(names(r1$readers), "reader_1:m1")
  calls <- character(0)
  r2 <- morie_siu_panel(html, mode = 3L, readers = c("a", "b"), auditors = "z", chat = chat,
                        reader_concurrency = 1L)
  expect_equal(unname(r2$fields[f1]), "audited")
  expect_equal(calls, c("a", "b", "z"))
  expect_equal(r2$models$backend, "custom")
  expect_error(morie_siu_panel(html, mode = 5L, chat = chat))
})

test_that("stick-breaking weights, mixture density and the slice-sampled DP mixture", {
  v <- c(0.3, 0.5, 0.2)
  w <- morie_slbpdg_weights(v)
  expect_equal(w$w, v * c(1, cumprod(1 - v)[1:2]), tolerance = 1e-12)
  expect_equal(w$rest, prod(1 - v), tolerance = 1e-12)
  expect_equal(morie_slbpdg_density(0.4, c(0.6, 0.4), c(0, 1), c(1, 0.25)),
               0.6 * stats::dnorm(0.4, 0, 1) + 0.4 * stats::dnorm(0.4, 1, 0.5), tolerance = 1e-12)
  y <- c(-2.1, -1.9, -2.3, -1.8, 2.0, 2.2, 1.9, 2.4, -2.0, 2.1)
  r <- morie_slbpdg(y, n_iter = 40, burn = 20, seed = 3, keep_draws = TRUE)
  dens <- rowMeans(vapply(r$draws, function(d) {
    vapply(r$grid, function(g) sum(d$w * stats::dnorm(g, d$mu, sqrt(d$s2))), 0)
  }, numeric(length(r$grid))))
  expect_equal(r$density, dens, tolerance = 1e-10)
  expect_equal(r$mean_clusters, mean(r$n_clusters), tolerance = 1e-12)
  expect_equal(r$kept, 20L)
  expect_identical(morie_slbpdg(y, n_iter = 40, burn = 20, seed = 3)$n_clusters, r$n_clusters)
  rk <- morie_slbpdg(y, n_iter = 10, burn = 5, seed = 1, route = "kalli_griffin_walker")
  expect_equal(rk$route, "kalli_griffin_walker")
  expect_match(morie_slbpdg_cheatsheet(), "walker")
  expect_error(morie_slbpdg(y, route = "neal"), "route must be one of")
  expect_error(morie_slbpdg(y, kappa = 1), "kappa must lie")
  expect_error(morie_slbpdg(1), "at least two")
  expect_error(morie_slbpdg(y, n_iter = 5, burn = 5), "burn-in consumed")
})

test_that("morie_slice replays Neal's stepping-out slice sampler", {
  lt <- function(x) -0.5 * x^2
  r <- morie_slice(lt, init = 0.3, width = 1.5, n_iter = 25, max_steps = 10, seed = 7)
  e <- .ghc_rng(7)
  x <- 0.3
  out <- numeric(25)
  for (i in 1:25) {
    ly <- lt(x) + log(.ghc_unif(e, 1L))
    L <- x - 1.5 * .ghc_unif(e, 1L)
    R <- L + 1.5
    j <- floor(10 * .ghc_unif(e, 1L))
    k <- 10 - 1 - j
    while (j > 0 && lt(L) > ly) {
      L <- L - 1.5
      j <- j - 1
    }
    while (k > 0 && lt(R) > ly) {
      R <- R + 1.5
      k <- k - 1
    }
    repeat {
      xp <- L + .ghc_unif(e, 1L) * (R - L)
      if (lt(xp) >= ly) {
        x <- xp
        break
      }
      if (xp < x) L <- xp else R <- xp
    }
    out[i] <- x
  }
  expect_equal(r$samples, out, tolerance = 1e-12)
  expect_error(morie_slice(lt, n_iter = 0), ">= 1")
  expect_error(morie_slice(lt, width = 0), "width must be > 0")
})

test_that("Slope One and the slope product identity", {
  M <- rbind(c(5, 3, NA, 1), c(4, NA, 4, 1), c(1, 1, NA, 5), c(NA, 1, 5, 4), c(2, 4, 3, NA))
  r <- Slope1(M, 0, 2)
  num <- 0
  den <- 0
  plain <- numeric(0)
  for (b in c(1, 2, 4)) {
    both <- !is.na(M[, 3]) & !is.na(M[, b])
    if (!any(both)) next
    dev <- mean(M[both, 3] - M[both, b])
    plain <- c(plain, dev + M[1, b])
    num <- num + (dev + M[1, b]) * sum(both)
    den <- den + sum(both)
  }
  expect_equal(r$weighted_slope_one, num / den, tolerance = 1e-12)
  expect_equal(r$slope_one, mean(plain), tolerance = 1e-12)
  expect_equal(r$user_mean, mean(c(5, 3, 1)), tolerance = 1e-12)
  expect_error(Slope1(M, 9, 0), "u is out of range")
  x <- c(1.2, 2.3, 2.9, 4.1, 5.2)
  y <- c(1.0, 2.6, 3.1, 3.8, 5.5)
  sp <- SlopeProd(x, y)
  a <- unname(stats::coef(stats::lm(y ~ x))[2])
  cc <- unname(stats::coef(stats::lm(x ~ y))[2])
  expect_equal(sp$slope_product_AC, a * cc, tolerance = 1e-12)
  expect_equal(sp$r, stats::cor(x, y), tolerance = 1e-12)
})

test_that("SMC samplers and annealed SMC optimisation", {
  expect_equal(annealing_ladder(4, 8, 1), c(1, 2, 4, 8), tolerance = 1e-12)
  expect_equal(annealing_ladder(3, 5, 1, kind = "linear"), c(1, 3, 5))
  expect_error(annealing_ladder(1), "two steps")
  expect_error(annealing_ladder(3, 1, 2), "phi_min < phi_max")
  expect_equal(temperature_ladder(5), (0:4) / 4)
  expect_equal(temperature_ladder(3, "power", 2), ((0:2) / 2)^2)
  expect_equal(temperature_ladder(3, "prior"), c(0, 1, 1))
  expect_error(temperature_ladder(3, "cosine"), "geometric, power or prior")
  # a kernel that never moves leaves only the reweighting: log Z_n / Z_1 is exact
  stay <- function(x, lt, rng) list(x = x, accept = 0)
  lg <- function(x, phi) phi * (-0.5 * x^2)
  init <- function(rng) .ghc_norm(rng, 1L)
  s <- smcsam(lg, init, n_particles = 50, ladder = c(0, 0.5, 1), kernel = stay, ess_threshold = 1e-9, seed = 2)
  e <- .ghc_rng(2)
  X <- vapply(1:50, function(i) .ghc_norm(e, 1L), 0)
  w1 <- exp(-0.25 * X^2)
  lz <- log(mean(w1)) + log(sum(w1 * exp(-0.25 * X^2)) / sum(w1))
  expect_equal(s$log_norm_const, lz, tolerance = 1e-10)
  W <- exp(-0.5 * X^2) / sum(exp(-0.5 * X^2))
  expect_equal(s$mean, sum(W * X), tolerance = 1e-10)
  # the random-walk kernel draws only from the seeded stream
  a1 <- smcsam(lg, init, n_particles = 20, n_steps = 4, kernel = random_walk_kernel(0.5, 2), seed = 5)
  set.seed(99)
  a2 <- smcsam(lg, init, n_particles = 20, n_steps = 4, kernel = random_walk_kernel(0.5, 2), seed = 5)
  expect_identical(a1$particles, a2$particles)
  expect_error(smcsam(lg, init, n_particles = 1), "two particles")
  expect_error(smcsam(lg, init, weight_rule = "general"), "needs log_forward")
  obj <- function(x) -(x - 1.5)^2
  o <- smcopt(obj, function(rng) 4 * .ghc_unif(rng, 1L) - 2, n_particles = 60, n_steps = 10, seed = 1)
  expect_equal(o$best_value, obj(o$best_x), tolerance = 1e-12)
  expect_lt(abs(o$best_x - 1.5), 0.2)
  expect_same_function(smc_optimise, smcopt)
})

test_that("survey totals, ratios, sample sizes and SMR intervals", {
  y <- c(12, 15, 9, 20, 14, 11)
  r <- Srstotal(y, N = 120)
  expect_equal(r$estimate, 120 * mean(y), tolerance = 1e-12)
  expect_equal(r$se, sqrt(120^2 * (114 / 120) * stats::var(y) / 6), tolerance = 1e-12)
  expect_error(Srstotal(y, N = 3), "at least n")
  x <- c(10, 14, 8, 18, 12, 10)
  q <- Ratioest(y, x, X = 1500, N = 120)
  R <- mean(y) / mean(x)
  expect_equal(q$ratio, R, tolerance = 1e-12)
  expect_equal(q$se_ratio, sqrt((114 / 120) * sum((y - R * x)^2) / 5 / (6 * mean(x)^2)), tolerance = 1e-12)
  expect_equal(q$total, R * 1500, tolerance = 1e-12)
  ns <- Nsamp(e = 2, S = 10, N = 500)
  n0 <- stats::qnorm(0.975)^2 * 100 / 4
  expect_equal(ns$n0, n0, tolerance = 1e-12)
  expect_equal(ns$n, ceiling(n0 / (1 + n0 / 500)))
  expect_error(Nsamp(0, 1), "must be positive")
  sm <- Smrind(12, 8)
  expect_equal(sm$smr, 1.5)
  expect_equal(c(sm$ci_lower, sm$ci_upper), c(stats::qchisq(0.025, 24) / 2, stats::qchisq(0.975, 26) / 2) / 8,
               tolerance = 1e-12)
  expect_equal(Smrind(0, 2)$ci_lower, 0)
  expect_equal(Snr2(0.6, 0.2, 0.9, 0.3)$estimate, 1 - 0.8 / 1.2, tolerance = 1e-12)
  expect_error(Snr2(-1, 0, 1, 1), "non-negative")
})

test_that("Smplqc gives call rates and PLINK --het F", {
  G <- rbind(c(0, 1, 2, 1, NA, 2), c(1, 1, 1, 0, 2, 2), c(2, 0, 1, 1, 1, 0), c(0, 2, 2, 1, 0, 1))
  r <- Smplqc(G)
  p <- colMeans(G, na.rm = TRUE) / 2
  expect_equal(r$freq, p, tolerance = 1e-12)
  expect_equal(r$callrate, rowMeans(!is.na(G)))
  for (i in 1:4) {
    ok <- !is.na(G[i, ])
    oh <- sum(G[i, ok] != 1)
    eh <- sum(1 - 2 * p[ok] * (1 - p[ok]))
    expect_equal(r$F[i], (oh - eh) / (sum(ok) - eh), tolerance = 1e-12)
  }
  expect_equal(r$flag_callrate, rowMeans(!is.na(G)) < 0.98)
})

test_that("small-world coefficient", {
  A <- matrix(0, 6, 6)
  ed <- rbind(c(1, 2), c(1, 3), c(2, 3), c(3, 4), c(4, 5), c(4, 6), c(5, 6))
  A[ed] <- 1
  A[ed[, 2:1]] <- 1
  r <- smwgrp(A)
  skip_if_not_installed("igraph")
  g <- igraph::graph_from_adjacency_matrix(A, mode = "undirected")
  cl <- igraph::transitivity(g, type = "local", isolates = "zero")
  L <- igraph::mean_distance(g)
  kb <- mean(rowSums(A))
  expect_equal(r$clustering, mean(cl), tolerance = 1e-12)
  expect_equal(r$path_length, L, tolerance = 1e-12)
  expect_equal(r$estimate, (mean(cl) / (kb / 6)) / (L / (log(6) / log(kb))), tolerance = 1e-12)
  expect_same_function(morie_smwgrp, smwgrp)
})

test_that("honest DiD identified sets, breakdown and fixed-length CIs", {
  beta <- c(0.1, -0.05, 0.0, 0.4, 0.55, 0.6)
  sd <- identified_set(beta, 3, 3, M = 0.02)
  bf <- identified_set(beta, 3, 3, M = 0.02, grid = 3)
  # the set is linear in the second differences, so the corners of the box
  # (on the brute-force grid) attain the closed-form bounds
  expect_equal(c(sd$lower, sd$upper), c(bf$lower, bf$upper), tolerance = 1e-12)
  lin <- 0 + (0 - (-0.05)) * (1:3)
  expect_equal(sd$estimate, 0.4 - lin[1], tolerance = 1e-12)
  expect_equal(identified_set(beta, 3, 3, M = 0)$width, 0)
  # |e_t| <= M t (t + 1) / 2 with e_t = sum_j (t - j + 1) r_j
  expect_equal(sd$max_deviation, 0.02 * c(1, 3, 6), tolerance = 1e-12)
  expect_equal(sd$coefficients, c(1, 0, 0))
  rm <- identified_set(beta, 3, 3, M = 2, family = "RM", l_vec = c(1 / 3, 1 / 3, 1 / 3))
  expect_equal(rm$bound, 2 * 0.15, tolerance = 1e-12)
  expect_equal(c(rm$lower, rm$upper), mean(beta[4:6]) + c(-1, 1) * 0.3, tolerance = 1e-12)
  bd <- breakdown_value(beta, 3, 3)
  expect_equal(bd$breakdown, sd$estimate / sum(abs(sd$coefficients)), tolerance = 1e-8)
  expect_equal(breakdown_value(-beta, 3, 3)$breakdown, 0)
  sc <- sensitivity_curve(beta, 3, 3, Ms = c(0, 0.01, 0.02))
  expect_equal(sc$width, 2 * c(0, 0.01, 0.02) * sum(abs(sd$coefficients)), tolerance = 1e-12)
  fl <- fixed_length_ci(beta, 0.1, 3, 3, M = 0.02, level = 0.9)
  expect_equal(c(fl$lower, fl$upper), c(sd$lower, sd$upper) + c(-1, 1) * stats::qnorm(0.95) * 0.1, tolerance = 1e-12)
  expect_error(identified_set(beta, 3, 2), "coefficients but")
  expect_error(identified_set(beta, 3, 3, family = "LT"), "family must be SD or RM")
  expect_error(identified_set(beta, 3, 3, M = -1), "non-negative")
  expect_error(fixed_length_ci(beta, -1, 3, 3), "sigma must be")
  expect_same_function(sensitivity_did, breakdown_value)
})

test_that("SNP-BLUP with VanRaden centring", {
  M <- rbind(c(0, 1, 2, 1), c(1, 1, 0, 2), c(2, 0, 1, 1), c(1, 2, 1, 0), c(0, 0, 2, 2), c(2, 1, 1, 0))
  y <- c(3.1, 4.2, 2.8, 4.9, 3.3, 3.6)
  r <- Snpblr(y, M, h2 = 0.4)
  p <- colMeans(M) / 2
  lam <- 0.6 / 0.4 * 2 * sum(p * (1 - p))
  Z <- sweep(M, 2, 2 * p)
  X1 <- cbind(1, Z)
  sol <- solve(crossprod(X1) + diag(c(0, rep(lam, 4))), crossprod(X1, y))
  expect_equal(r$lam, lam, tolerance = 1e-12)
  expect_equal(c(r$mu, r$u), as.numeric(sol), tolerance = 1e-10)
  expect_equal(r$estimate, as.numeric(Z %*% sol[-1]), tolerance = 1e-10)
  expect_error(Snpblr(y, M), "exactly one of lam or h2")
  expect_error(Snpblr(y, M, h2 = 1), "h2 must be")
})

test_that("SnpEff codon table, translation and variant annotation", {
  ct <- codon_table()
  expect_length(ct, 64)
  expect_equal(ct[["ATG"]], "M")
  expect_equal(c(ct[["TAA"]], ct[["TAG"]], ct[["TGA"]]), c("*", "*", "*"))
  expect_equal(unique(unlist(ct[c("GCT", "GCC", "GCA", "GCG")])), "A")
  expect_equal(unique(unlist(ct[c("CGT", "CGC", "CGA", "CGG", "AGA", "AGG")])), "R")
  expect_equal(ct[["TGG"]], "W")
  expect_equal(sum(unlist(ct) == "*"), 3)
  cds <- "ATGGCTTGGAAATAA"
  s <- snpeff(cds, list(list(4, "C", "T"), list(5, "T", "C"), list(7, "G", "A"), list(0, "A", "G"),
                        list(3, "G", "GA")))
  eff <- vapply(s$annotations, function(a) a$effect, "")
  # GCT->GTT A->V; GCT->GCC synonymous; TGG->TAG stop; ATG->GTG start lost; +1 frameshift
  expect_equal(eff, c("missense_variant", "synonymous_variant", "stop_gained", "start_lost", "frameshift_variant"))
  expect_equal(s$annotations[[1]]$hgvs_p, "p.A2V")
  expect_equal(s$annotations[[1]]$hgvs_c, "c.5C>T")
  expect_equal(s$impact_counts$HIGH, 3L)
  expect_equal(s$protein, "MAWK*")
  up <- annotate_variant("CCCCATGAAATAA", 1, "C", "G", cds_start = 4)
  expect_equal(up$effect, "upstream_gene_variant")
  expect_equal(annotate_variant(cds, 3, "GCTT", "T")$effect, "inframe_deletion")
  expect_error(annotate_variant(cds, 4, "G", "A"), "does not match")
  expect_error(annotate_variant(cds, 99, "A", "G"), "outside the sequence")
})
