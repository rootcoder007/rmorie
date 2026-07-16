# SPDX-License-Identifier: AGPL-3.0-or-later
# Benchmark: native ginv / mvrnorm vs MASS.
suppressMessages(pkgload::load_all(quiet = TRUE))
stopifnot(requireNamespace("MASS", quietly = TRUE))
bench_one <- function(n) {
  set.seed(1); A <- matrix(rnorm(n * n), n)
  Sig <- crossprod(matrix(rnorm(n * n), n))
  t_gm <- system.time(rmorie:::.morie_ginv(A))[["elapsed"]]
  t_gM <- system.time(MASS::ginv(A))[["elapsed"]]
  t_vm <- system.time({ set.seed(2); morie_mvrnorm(1000, rep(0, n), Sig) })[["elapsed"]]
  t_vM <- system.time({ set.seed(2); MASS::mvrnorm(1000, rep(0, n), Sig) })[["elapsed"]]
  data.frame(n = n, ginv_rmorie = t_gm, ginv_MASS = t_gM,
             mvrnorm_rmorie = t_vm, mvrnorm_MASS = t_vM)
}
res <- do.call(rbind, lapply(c(50, 200, 500), bench_one))
print(res, row.names = FALSE)
dir.create("inst/benchmarks/results", showWarnings = FALSE, recursive = TRUE)
utils::write.csv(res, sprintf("inst/benchmarks/results/%s-mass.csv",
                              format(Sys.Date())), row.names = FALSE)

# --- Module 31: glm.nb / kde2d / rlm / polr vs MASS ------------------
tm <- function(expr, reps) {
  t <- system.time(for (i in seq_len(reps)) expr)[["elapsed"]]
  1000 * t / reps
}
bench_models <- function(n) {
  set.seed(1); x1 <- rnorm(n); x2 <- runif(n)
  y <- rnbinom(n, mu = exp(0.5 + 0.8 * x1 - 0.4 * x2), size = 2)
  d <- data.frame(y, x1, x2); r <- max(4, round(4000 / n))
  nb_n <- tm(suppressWarnings(morie_glm_nb(y ~ x1 + x2, data = d)), r)
  nb_M <- tm(suppressWarnings(MASS::glm.nb(y ~ x1 + x2, data = d)), r)
  kd_n <- tm(morie_kde2d(x1, x2, n = 64), r * 5)
  kd_M <- tm(MASS::kde2d(x1, x2, n = 64), r * 5)
  yr <- 1 + 2 * x1 - 0.5 * x2 + rt(n, 3); yr[1:5] <- yr[1:5] + 30
  dr <- data.frame(y = yr, x1, x2)
  rl_n <- tm(morie_rlm(y ~ x1 + x2, data = dr), r)
  rl_M <- tm(MASS::rlm(y ~ x1 + x2, data = dr), r)
  u <- runif(n)
  yc <- 1 + (u > plogis(-1 - x1)) + (u > plogis(0.3 - x1)) + (u > plogis(1.5 - x1))
  df <- data.frame(yf = factor(yc, levels = 1:4, ordered = TRUE), x1, x2)
  po_n <- tm(morie_polr(yf ~ x1 + x2, data = df), r)
  po_M <- tm(MASS::polr(yf ~ x1 + x2, data = df, method = "logistic", Hess = FALSE), r)
  data.frame(n = n,
             glm_nb_rmorie_ms = nb_n, glm_nb_MASS_ms = nb_M,
             kde2d_rmorie_ms = kd_n, kde2d_MASS_ms = kd_M,
             rlm_rmorie_ms = rl_n, rlm_MASS_ms = rl_M,
             polr_rmorie_ms = po_n, polr_MASS_ms = po_M)
}
res2 <- do.call(rbind, lapply(c(500, 5000), bench_models))
print(res2, row.names = FALSE)
utils::write.csv(res2, sprintf("inst/benchmarks/results/%s-mass-models.csv",
                               format(Sys.Date())), row.names = FALSE)
