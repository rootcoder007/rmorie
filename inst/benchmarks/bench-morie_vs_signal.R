# SPDX-License-Identifier: AGPL-3.0-or-later
# Benchmark: native DSP vs signal (skip-if-not-installed).
suppressMessages(pkgload::load_all(quiet = TRUE))
set.seed(3)
x <- sin(2 * pi * 5 * seq(0, 10, length.out = 20000)) + rnorm(20000, sd = 0.3)
bench <- function(thunk) { t0 <- proc.time()[[3]]; thunk(); proc.time()[[3]] - t0 }
rows <- c(
  welch_native = bench(function() morie_dsp_psd_welch(x, fs = 2000)),
  fir_native = bench(function() rgfir(x, cutoff = 0.2))
)
if (requireNamespace("signal", quietly = TRUE)) {
  rows["butter_signal"] <- bench(function() {
    bf <- signal::butter(4, 0.2); signal::filtfilt(bf, x)
  })
}
print(round(rows, 4))
dir.create("inst/benchmarks/results", showWarnings = FALSE)
write.csv(data.frame(t = rows), sprintf("inst/benchmarks/results/%s-signal.csv", Sys.Date()))
