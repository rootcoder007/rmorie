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
