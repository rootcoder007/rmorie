# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Consolidated benchmark for modules 12-24 of the native-
# specializations branch (the earlier modules have dedicated
# bench-*.R scripts). Native engine vs reference package where one
# exists; run sequentially on the L14 test machine from the package
# root:  Rscript inst/benchmarks/bench-batch-modules.R
# Writes inst/benchmarks/results/bench-batch-modules.csv.

suppressMessages(library(rmorie))

tm <- function(expr) {
  gc(FALSE)
  as.numeric(system.time(expr)["elapsed"])
}
rows <- list()
add <- function(module, task, native_s, ref_s = NA_real_,
                ref_pkg = "") {
  rows[[length(rows) + 1L]] <<- data.frame(
    module = module, task = task, native_s = native_s, ref_s = ref_s,
    ref_pkg = ref_pkg,
    ratio = if (is.finite(ref_s)) native_s / ref_s else NA_real_)
  cat(sprintf("m%-3s %-28s native %8.3fs%s\n", module, task, native_s,
              if (is.finite(ref_s))
                sprintf("  %s %8.3fs  ratio %.2fx", ref_pkg, ref_s,
                        native_s / ref_s) else ""))
}
has <- function(p) requireNamespace(p, quietly = TRUE)

## ---- module 12: meta-learners --------------------------------------------
set.seed(1)
n <- 5000
X <- matrix(rnorm(n * 5), n)
W <- rbinom(n, 1, plogis(X[, 1]))
Y <- 1 + (1 + X[, 2]) * W + X[, 1] + rnorm(n)
df12 <- data.frame(y = Y, w = W, X)
covs <- paste0("X", 1:5)
add(12, "x_learner 5k",
    tm(morie_estimate_cate(df12, "w", "y", covs,
                           meta_learner = "x_learner")))

## ---- module 13: DAG toolkit ----------------------------------------------
set.seed(2)
edges <- c(paste0("z", 1:20, " -> x"), paste0("z", 1:20, " -> y"),
           "x -> y")
t13 <- tm({
  g <- morie_dag(edges, "x", "y")
  id <- morie_dag_identify(g)
})
add(13, "dag identify 20 confounders", t13,
    if (has("dagitty")) tm({
      dg <- dagitty::dagitty(paste("dag {",
        paste(gsub("->", "->", edges), collapse = "; "), "}"))
      dagitty::adjustmentSets(dg, "x", "y")
    }) else NA_real_, "dagitty")

## ---- module 15: synthetic control ----------------------------------------
set.seed(3)
n_d <- 30; n_t <- 40
don <- matrix(rnorm(n_d * n_t), n_d)
w0 <- rep(1 / 5, 5)
tr <- as.numeric(t(don[1:5, ]) %*% w0) + c(rep(0, 30), rep(2, 10))
pan15 <- rbind(
  data.frame(unit = "tr", time = seq_len(n_t), y = tr),
  do.call(rbind, lapply(seq_len(n_d), function(i)
    data.frame(unit = paste0("d", i), time = seq_len(n_t),
               y = don[i, ]))))
add(15, "synth 30 donors + placebos",
    tm(morie_synth_control(pan15, "y", "unit", "time",
                           treated_unit = "tr", treatment_time = 31,
                           optimize_v = FALSE)))

## ---- module 16: RDD --------------------------------------------------------
set.seed(4)
n <- 20000
x16 <- runif(n, -1, 1)
y16 <- 0.5 + 0.8 * x16 + 1.2 * (x16 >= 0) + rnorm(n, 0, 0.4)
df16 <- data.frame(x = x16, y = y16)
add(16, "sharp RDD 20k (incl IK bw)",
    tm(morie_rdd_sharp(df16, "y", "x")),
    if (has("rdrobust")) tm(rdrobust::rdrobust(y = y16, x = x16, c = 0))
    else NA_real_, "rdrobust")
add(16, "mccrary 20k",
    tm(morie_rdd_mccrary(x16)),
    if (has("rddensity")) tm(rddensity::rddensity(x16, c = 0))
    else NA_real_, "rddensity")

## ---- module 17: IV ---------------------------------------------------------
set.seed(5)
n <- 50000
x17 <- rnorm(n); z1 <- rnorm(n); z2 <- rnorm(n); u <- rnorm(n)
d17 <- 0.5 * z1 + 0.4 * z2 + u + rnorm(n)
y17 <- 0.8 * d17 + 0.5 * x17 + 0.8 * u + rnorm(n)
df17 <- data.frame(y = y17, d = d17, x = x17, z1 = z1, z2 = z2)
add(17, "2sls 50k",
    tm(morie_iv_tsls(df17, "y", "d", c("z1", "z2"), exogenous = "x")),
    if (has("ivreg")) tm(ivreg::ivreg(y ~ d + x | z1 + z2 + x,
                                      data = df17))
    else NA_real_, "ivreg")
add(17, "its 5k", tm({
  t <- seq_len(5000)
  morie_its(data.frame(tt = t,
                       y = 10 + 0.01 * t + 4 * (t >= 4000) + rnorm(5000)),
            "y", "tt", interruption_time = 4000)
}))

## ---- module 18: IRT --------------------------------------------------------
set.seed(6)
n <- 2000; k <- 10
th <- rnorm(n)
a_t <- runif(k, 0.8, 1.8); b_t <- runif(k, -1.5, 1.5)
R18 <- vapply(seq_len(k), function(j)
  rbinom(n, 1, plogis(a_t[j] * (th - b_t[j]))), numeric(n))
add(18, "2pl 2000x10",
    tm(morie_irt_2pl(R18)),
    if (has("mirt")) tm(mirt::mirt(as.data.frame(R18), 1,
                                   itemtype = "2PL", verbose = FALSE))
    else NA_real_, "mirt")

## ---- module 19: geostatistics ---------------------------------------------
set.seed(7)
n <- 400
xy <- cbind(runif(n), runif(n))
D <- as.matrix(dist(xy))
v19 <- as.numeric(t(chol(exp(-D / 0.3) + diag(0.05, n))) %*% rnorm(n))
new_xy <- cbind(runif(200), runif(200))
vg <- list(model = "exponential", nugget = 0.05, psill = 1,
           range = 0.3)
add(19, "kriging 400 -> 200",
    tm(morie_spatial_krige(xy, v19, new_xy, vgm = vg)),
    if (has("gstat") && has("sp")) tm({
      dfa <- data.frame(x = xy[, 1], y = xy[, 2], z = v19)
      sp::coordinates(dfa) <- ~ x + y
      nd <- data.frame(x = new_xy[, 1], y = new_xy[, 2])
      sp::coordinates(nd) <- ~ x + y
      gm <- gstat::vgm(psill = 1, model = "Exp", range = 0.3,
                       nugget = 0.05)
      suppressWarnings(gstat::krige(z ~ 1, locations = dfa,
                                    newdata = nd, model = gm,
                                    debug.level = 0))
    }) else NA_real_, "gstat")

## ---- module 20: DSP ---------------------------------------------------------
set.seed(8)
x20 <- as.numeric(arima.sim(list(ar = 0.5), 100000))
add(20, "butter+filtfilt 100k", tm({
  bf <- rmorie:::.morie_dsp_butter(4, 0.2, "low")
  rmorie:::.morie_dsp_filtfilt(bf$b, bf$a, x20)
}), if (has("signal")) tm({
  bf <- signal::butter(4, 0.2, type = "low")
  signal::filtfilt(bf, x20)
}) else NA_real_, "signal")
x20b <- rnorm(2^14)
add(20, "dwt+idwt la8 16k", tm({
  d <- rmorie:::.morie_dsp_dwt(x20b, "la8", 6)
  rmorie:::.morie_dsp_idwt(d)
}), if (has("wavelets")) tm({
  d <- wavelets::dwt(x20b, filter = "la8", n.levels = 6)
  wavelets::idwt(d)
}) else NA_real_, "wavelets")
add(20, "welch psd 100k",
    tm(morie_dsp_psd_welch(x20, nperseg = 1024)))

## ---- module 21: Hawkes ------------------------------------------------------
set.seed(9)
tt <- sort(runif(3000, 0, 1000))
add(21, "hawkes negloglik 3000 events",
    tm(rmorie:::.tps_hwka_neg_loglik_general(
      c(log(0.5), 0.4, 1.2), tt, max(tt),
      kernel_kind = "exponential", baseline_kind = "constant")),
    if (has("hawkes")) tm(hawkes::likelihoodHawkes(
      lambda0 = 0.5, alpha = 0.48, beta = 1.2, history = tt))
    else NA_real_, "hawkes")

## ---- module 22: crypto ------------------------------------------------------
big <- paste(rep("a", 5e6), collapse = "")
add(22, "sha256 5MB",
    tm(rmorie:::.rmorie_sha256_hex_impl(big)),
    if (has("digest")) tm(digest::digest(big, algo = "sha256",
                                         serialize = FALSE))
    else NA_real_, "digest")
add(22, "pbkdf2 50k iters",
    tm(rmorie:::.rmorie_pbkdf2_sha256_impl("pw", "salt", 50000L, 32L)))

## ---- module 23: parsers -----------------------------------------------------
recs <- paste0('{"id": ', 1:2000, ', "name": "row', 1:2000,
               '", "v": ', round(runif(2000), 4), '}')
json23 <- paste0("[", paste(recs, collapse = ","), "]")
add(23, "json parse 2k records",
    tm(morie_fetch_json(json23)),
    if (has("jsonlite")) tm(jsonlite::fromJSON(json23))
    else NA_real_, "jsonlite")
xml23 <- paste0("<root>", paste0(
  "<rec id=\"", 1:2000, "\"><name>n", 1:2000, "</name></rec>",
  collapse = ""), "</root>")
add(23, "xml parse 2k records",
    tm(morie_fetch_xml(xml23)),
    if (has("xml2")) tm(xml2::as_list(xml2::read_xml(xml23)))
    else NA_real_, "xml2")

## ---- module 24: MRM ---------------------------------------------------------
set.seed(10)
n <- 3000
x1 <- rnorm(n); x2 <- runif(n)
t24 <- rbinom(n, 1, plogis(0.6 * x1))
y24 <- 1 + 0.8 * t24 + 0.5 * x1 + 0.3 * x2 + rnorm(n)
df24 <- data.frame(y = y24, t = t24, x1 = x1, x2 = x2)
add(24, "mrm 4-estimator pipeline 3k",
    tm(morie_mrm_estimate_causal_effect(df24, "t", "y",
                                        c("x1", "x2"))))
a24 <- data.frame(id = 1:20000, v = rnorm(20000))
b24 <- data.frame(id = sample(1:25000, 20000), v = rnorm(20000))
add(24, "mrm reconcile 20k x 20k",
    tm(morie_mrm_reconcile(a24, b24, keys = "id", compare = "v")))

out <- do.call(rbind, rows)
dir.create("inst/benchmarks/results", showWarnings = FALSE,
           recursive = TRUE)
utils::write.csv(out, "inst/benchmarks/results/bench-batch-modules.csv",
                 row.names = FALSE)
cat("written inst/benchmarks/results/bench-batch-modules.csv\n")
