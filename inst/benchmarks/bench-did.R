# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Module 14 benchmark: native DiD family vs reference packages
# (fixest, did, DRDID, bacondecomp). Run on the L14 test machine:
#   Rscript inst/benchmarks/bench-did.R
# Writes inst/benchmarks/results/bench-did.csv when run from the
# package root. The acceptance bar is <= 2x the reference runtime.

suppressMessages(library(rmorie))

mk_panel <- function(n_units, n_t = 8L, seed = 42L) {
  set.seed(seed)
  g_onset <- sample(c(0, 4, 5, 6), n_units, replace = TRUE,
                    prob = c(0.4, 0.2, 0.2, 0.2))
  x1 <- rnorm(n_units); x2 <- runif(n_units); u <- rnorm(n_units)
  id <- rep(seq_len(n_units), each = n_t)
  tt <- rep(seq_len(n_t), n_units)
  g <- g_onset[id]
  d <- as.integer(g > 0 & tt >= g)
  data.frame(id = id, tt = tt, g = g, d = d,
             x1 = x1[id], x2 = x2[id],
             y = u[id] + 0.5 * tt + 1.5 * d + rnorm(n_units * n_t, 0, 0.5))
}

tm <- function(expr) {
  gc(FALSE)
  as.numeric(system.time(expr)["elapsed"])
}

rows <- list()
add <- function(task, n, native_s, ref_s, ref_pkg) {
  rows[[length(rows) + 1L]] <<- data.frame(
    task = task, n_units = n, native_s = native_s, ref_s = ref_s,
    ref_pkg = ref_pkg, ratio = native_s / ref_s)
  cat(sprintf("%-12s n=%-7d native %8.3fs  %s %8.3fs  ratio %.2fx\n",
              task, n, native_s, ref_pkg, ref_s, native_s / ref_s))
}

for (n in c(1e3, 1e4, 1e5)) {
  pan <- mk_panel(as.integer(n))

  t_nat <- tm(morie_did_panel_fe(pan, "y", "d", "id", "tt"))
  t_ref <- if (requireNamespace("fixest", quietly = TRUE)) {
    tm(fixest::feols(y ~ d | id + tt, data = pan, cluster = ~id))
  } else NA_real_
  add("twfe", n, t_nat, t_ref, "fixest")

  if (n <= 1e4) {
    t_nat <- tm(morie_did_group_time_att(pan, "y", "id", "tt", "g",
                                         covariates = c("x1", "x2"),
                                         n_bootstrap = 0L))
    t_ref <- if (requireNamespace("did", quietly = TRUE)) {
      tm(did::att_gt(yname = "y", tname = "tt", idname = "id",
                     gname = "g", xformla = ~x1 + x2, data = pan,
                     est_method = "dr", bstrap = FALSE, cband = FALSE,
                     panel = TRUE))
    } else NA_real_
    add("attgt_dr", n, t_nat, t_ref, "did")
  }

  # drdid_rc on a 2-period repeated cross-section of comparable size
  set.seed(7)
  m <- as.integer(n) * 2L
  xx <- cbind(rnorm(m), runif(m))
  D <- rbinom(m, 1, plogis(0.4 * xx[, 1]))
  post <- rbinom(m, 1, 0.5)
  y <- 1 + 0.6 * xx[, 1] + 0.5 * post + 2 * D * post + rnorm(m)
  df2 <- data.frame(y = y, d = D, post = post,
                    x1 = xx[, 1], x2 = xx[, 2])
  X <- cbind(1, xx)
  t_nat <- tm(morie_did_doubly_robust(df2, "y", "d", "post",
                                      covariates = c("x1", "x2"),
                                      n_bootstrap = 0L))
  t_ref <- if (requireNamespace("DRDID", quietly = TRUE)) {
    tm(DRDID::drdid_rc(y = y, post = post, D = D, covariates = X))
  } else NA_real_
  add("drdid_rc", n, t_nat, t_ref, "DRDID")

  t_nat <- tm(morie_did_bacon_decomposition(pan, "y", "d", "id", "tt"))
  t_ref <- if (requireNamespace("bacondecomp", quietly = TRUE)) {
    tm(bacondecomp::bacon(y ~ d, data = pan, id_var = "id",
                          time_var = "tt", quietly = TRUE))
  } else NA_real_
  add("bacon", n, t_nat, t_ref, "bacondecomp")
}

out <- do.call(rbind, rows)
dir.create("inst/benchmarks/results", showWarnings = FALSE,
           recursive = TRUE)
utils::write.csv(out, "inst/benchmarks/results/bench-did.csv",
                 row.names = FALSE)
cat("written inst/benchmarks/results/bench-did.csv\n")
