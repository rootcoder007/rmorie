# Same inputs and anchors as the three-way harness ledger/wave3/test_cluseq.R:
# the Python arm, the morie R arm and rmorie agree on every recorded value.
test_that("cluseq runs on the three-way inputs and its anchors hold", {
  record_keys <- function(name, r, keys) {
    for (k in keys) expect_true(k %in% names(r), info = paste("missing key", k))
  }
  emit <- function(k, v) invisible(NULL)
  anchor <- function(name, cond, why) expect_true(isTRUE(cond), info = why)
  base <- strsplit("ACGTTGCAACGTTGCAACGT", "")[[1]]; alt <- c(A = "C", C = "G", G = "T", T = "A")
  seqs <- character(0)
  for (j in 0:11) { s <- base; kk <- j %% 4 + (if (j >= 8) 1L else 0L); if (kk > 0) for (k in 0:(kk - 1)) { p <- (j * 3 + k * 5) %% 20 + 1L; s[p] <- alt[[s[p]]] }; seqs <- c(seqs, paste(s, collapse = "")) }
  KEYS <- c("counts", "distances", "estimate", "max_distance", "method", "n", "n_clusters", "z")
  r <- Cluseq(seqs, 5); record_keys("t5", r, KEYS)
  r2 <- Cluseq(seqs, 2); record_keys("t2", r2, KEYS)
  anchor("n", as.integer(r$n) == 12L, "n")
  anchor("clusters", as.integer(r2$n_clusters) >= as.integer(r$n_clusters), "tighter threshold cannot merge more")
})
