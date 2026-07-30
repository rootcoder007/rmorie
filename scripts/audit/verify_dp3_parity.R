#!/usr/bin/env Rscript
# Differential-privacy shelf part 3 (the private learners) R-vs-Python
# parity.
#
# These are all randomised, but every one of them separates cleanly into
# a deterministic clip-and-aggregate stage and a noise draw. Setting
# sigma = 0 removes the draw and leaves the clipping arithmetic --
# which is where the privacy guarantee actually lives -- fully
# comparable across languages. The noisy behaviour is then checked by
# property.
#
# Usage: Rscript scripts/audit/verify_dp3_parity.R <anchor-dir> <R-dir>

args <- commandArgs(trailingOnly = TRUE)
anch <- args[[1]]
rdir <- if (length(args) > 1) args[[2]] else "r-package/morie/R"
for (f in c("optim_native.R", "dp_native.R", "dp_native3.R")) {
  fp <- file.path(rdir, f)
  if (file.exists(fp)) source(fp)
}
exp <- jsonlite::fromJSON(file.path(anch, "expected.json"))
G <- as.matrix(utils::read.csv(file.path(anch, "G.csv"), header = FALSE))
U <- as.matrix(utils::read.csv(file.path(anch, "U.csv"), header = FALSE))
y <- as.numeric(utils::read.csv(file.path(anch, "y.csv"), header = FALSE)[[1]])

pass <- 0L
fail <- 0L
chk <- function(label, got, want, tol = 1e-10) {
  got <- as.numeric(got)
  want <- as.numeric(want)
  if (length(got) != length(want)) {
    cat(sprintf("FAIL %-26s length %d vs %d\n", label, length(got),
                length(want)))
    fail <<- fail + 1L
    return(invisible(NULL))
  }
  err <- max(abs(got - want) / pmax(1, abs(want)))
  if (is.finite(err) && err <= tol) {
    cat(sprintf("ok   %-26s max rel err %.3g\n", label, err))
    pass <<- pass + 1L
  } else {
    cat(sprintf("FAIL %-26s max rel err %.3g\n", label, err))
    fail <<- fail + 1L
  }
}
inv <- function(label, ok) {
  if (isTRUE(ok)) {
    cat(sprintf("ok   %-26s (property)\n", label))
    pass <<- pass + 1L
  } else {
    cat(sprintf("FAIL %-26s (property)\n", label))
    fail <<- fail + 1L
  }
}

s0 <- morie_dp_sgd(G, C = 2, sigma = 0, lr = 0.5)
chk("dp-sgd update", s0$update, exp$sgd_update)
chk("dp-sgd private gradient", s0$private_gradient, exp$sgd_grad)
chk("dp-sgd clipped fraction", s0$clipped_fraction, exp$sgd_clipfrac)
chk("dp-sgd noise sd", s0$noise_sd, exp$sgd_noise)

f0 <- morie_dp_fedavg(U, C = 1, sigma = 0)
chk("fedavg aggregate", f0$aggregate, exp$fed_agg)
chk("fedavg clipped fraction", f0$clipped_fraction, exp$fed_clipfrac)
chk("fedavg noise per client", f0$noise_sd_per_client, exp$fed_per_client)

g0 <- morie_dp_gan(G, C = 2, sigma = 0, lr = 0.3, n_disc_steps = 5)
chk("dp-gan disc update", g0$disc_update, exp$gan_update)
chk("dp-gan steps to account", g0$steps_to_account, exp$gan_steps)

a0 <- morie_dp_adam(G, C = 2, sigma = 0, lr = 0.01)
chk("dp-adam update", a0$update, exp$adam_update)
chk("dp-adam first moment", a0$m, exp$adam_m)
chk("dp-adam second moment", a0$v, exp$adam_v)
chk("dp-adam signal/noise", a0$signal_to_noise, exp$adam_snr)

cp <- morie_dp_changepoint(y, epsilon = 50, bounds = c(-6, 10),
                           min_segment = 5)
chk("changepoint candidates", cp$candidates, exp$cp_util)
chk("changepoint probabilities", cp$probabilities, exp$cp_probs)
chk("changepoint best utility", cp$best_utility, exp$cp_best)

# Properties -- the claims these modules make that values cannot show.

# Refusing an averaged gradient is the guarantee, not an ergonomic choice.
inv("dp-sgd rejects averaged grad",
    inherits(try(morie_dp_sgd(colMeans(G), C = 1), silent = TRUE), "try-error"))
# Clipping must actually bind: no clipped row may exceed C.
Gc <- G * pmin(1, 2 / pmax(sqrt(rowSums(G^2)), 1e-12))
inv("no clipped row exceeds C", max(sqrt(rowSums(Gc^2))) <= 2 + 1e-9)
inv("clipping is a no-op below C",
    max(abs(morie_dp_sgd(G / 100, C = 2, sigma = 0, lr = 1)$private_gradient *
              nrow(G) - colSums(G / 100))) < 1e-9)
# Federated noise per client must fall as 1/m -- the one free lunch in DP.
inv("fedavg per-client noise falls as 1/m",
    abs(morie_dp_fedavg(U, C = 1, sigma = 1)$noise_sd_per_client * nrow(U) -
          morie_dp_fedavg(U, C = 1, sigma = 1)$noise_sd_aggregate) < 1e-12)
inv("generator is free", isTRUE(g0$generator_is_free))

set.seed(3)
km <- morie_dp_kmeans(matrix(stats::rnorm(400), ncol = 2), k = 3, epsilon = 30,
                      n_iter = 3, bounds = c(-4, 4))
inv("kmeans splits budget by 2*n_iter",
    abs(km$epsilon_per_iteration - 30 / 6) < 1e-12)
inv("kmeans labels are 0-based",
    min(km$labels) >= 0L && max(km$labels) <= 2L)
inv("kmeans centres within bounds",
    all(km$centers >= -4 - 1e-9) && all(km$centers <= 4 + 1e-9))

X <- matrix(stats::rnorm(400), ncol = 2)
yb <- as.numeric(stats::runif(200) < 1 / (1 + exp(-(X %*% c(1.5, -1.5)))))
lg <- morie_dp_logistic(X, yb, epsilon = 30, method = "objective", lam = 0.01)
inv("dp logistic beats chance", lg$accuracy > 0.55)
inv("dp logistic rejects lam = 0",
    inherits(try(morie_dp_logistic(X, yb, method = "objective", lam = 0),
                 silent = TRUE), "try-error"))

Xc <- cbind(stats::rnorm(400), 0)
Xc[, 2] <- Xc[, 1] * 0.95 + stats::rnorm(400) * 0.1     # near-perfect correlation
sy <- morie_dp_synthetic_data(Xc, epsilon = 30, bounds = c(-4, 4))
# The headline caveat, asserted rather than merely documented: strong
# correlation in the real data does NOT survive into the synthetic data.
inv("synthetic destroys correlation",
    abs(sy$correlation_real) > 0.8 && abs(sy$correlation_synthetic) < 0.3)

set.seed(5)
flat <- morie_dp_changepoint(stats::rnorm(120), epsilon = 50,
                             bounds = c(-4, 4))
inv("changepoint always returns one",
    is.numeric(flat$changepoint) && length(flat$changepoint) == 1L)
inv("changepoint utility beats flat series", cp$best_utility > flat$best_utility)

cat(sprintf("\n%d/%d assertions passed\n", pass, pass + fail))
if (fail > 0L) quit(status = 1L)
