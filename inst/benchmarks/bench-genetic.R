# SPDX-License-Identifier: AGPL-3.0-or-later
# Benchmark: native genetic matching vs Matching::GenMatch.
# Same GA budget both sides (pop 20, 8 generations, M = 1).
suppressMessages(pkgload::load_all(quiet = TRUE))
stopifnot(requireNamespace("Matching", quietly = TRUE),
          requireNamespace("rgenoud", quietly = TRUE))

bench_one <- function(n) {
  set.seed(1)
  x1 <- rnorm(n); x2 <- rnorm(n); x3 <- rbinom(n, 1, 0.4)
  d <- rbinom(n, 1, plogis(-1.4 + 0.5 * x1 - 0.3 * x2 + 0.3 * x3))
  df <- data.frame(x1, x2, x3, d)
  t_o <- system.time(morie_matching_genetic(
    df, "d", c("x1", "x2", "x3"),
    pop_size = 20L, n_generations = 8L))[["elapsed"]]
  Tr <- df$d; X <- as.matrix(df[, 1:3])
  t_m <- tryCatch({
    st <- system.time(invisible(utils::capture.output({
      gen <- Matching::GenMatch(Tr = Tr, X = X, pop.size = 20,
                                max.generations = 8, M = 1,
                                print.level = 0, replace = FALSE,
                                ties = FALSE)
      Matching::Match(Tr = Tr, X = X, Weight.matrix = gen, M = 1,
                      replace = FALSE, ties = FALSE)
    })))
    st[["elapsed"]]
  }, error = function(e) NA)
  data.frame(n = n, genetic_rmorie = t_o, genetic_matching = t_m)
}

res <- do.call(rbind, lapply(c(500, 2000, 10000), bench_one))
print(res, row.names = FALSE)
dir.create("inst/benchmarks/results", showWarnings = FALSE, recursive = TRUE)
utils::write.csv(res, sprintf("inst/benchmarks/results/%s-genetic.csv",
                              format(Sys.Date())), row.names = FALSE)
