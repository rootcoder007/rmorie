test_that("OlcAssembly reconstructs error-free reads", {
  g <- "ATGCGTACGTTAGCCGATCGATTGCAAGCTTGACCTAGGCATCGAT"
  st <- seq(1, nchar(g) - 14, by = 5)
  reads <- substring(g, st, st + 14)
  if (!endsWith(g, reads[length(reads)])) reads <- c(reads, substring(g, nchar(g) - 14))
  r <- OlcAssembly(c(rev(reads), substring(g, 4, 9)), min_overlap = 5)
  expect_equal(r$contigs, g)
  expect_equal(OlcAssembly(c("ATGGCGT", "GCGTGCA", "TGCAATG", "CAATGGA"))$contigs, "ATGGCGTGCAATGGA")
})

test_that("ProteinDisorder follows the FoldIndex formula", {
  s <- "KKEEGALLVIDRSS"
  a <- strsplit(s, "")[[1]]
  kd <- c(A = 1.8, R = -4.5, D = -3.5, E = -3.5, G = -0.4, K = -3.9, L = 3.8, S = -0.8, V = 4.2, I = 4.5)
  H <- mean((kd[a] + 4.5) / 9)
  R <- (sum(a %in% c("K", "R")) - sum(a %in% c("D", "E"))) / length(a)
  r <- ProteinDisorder(s, window = 5)
  expect_equal(r$fold_index, 2.785 * H - abs(R) - 1.151, tolerance = 1e-12)
  expect_error(ProteinDisorder("MKX"))
})
