.globins <- c("MVLSPADKTNVKAAWGKVGAHAGEYGAEALERMFLSFPTTKTYFPHF", "MVHLTPEEKSAVTALWGKVNVDEVGGEALGRLLVVYPWTQRFFESF",
              "MGLSDGEWQLVLNVWGKVEADIPGHGQEVLIRLFKGHPETLEKFDKF", "MVLSEGEWQLVLHVWAKVEADVAGHGQDILIRLFKSHPETLEKF")

.nuc <- function(a, b) if (a == b) 5 else -4

.pair <- function(s, t, go = 10, ge = 1) {
  a <- strsplit(s, "")[[1]]
  b <- strsplit(t, "")[[1]]
  total <- 0
  prev <- 0
  for (c in seq_along(a)) {
    if (a[c] == "-" && b[c] == "-") next
    if (a[c] != "-" && b[c] != "-") {
      total <- total + .nuc(a[c], b[c])
      prev <- 0
    } else if (a[c] == "-") {
      total <- total - (if (prev == 1) ge else go)
      prev <- 1
    } else {
      total <- total - (if (prev == 2) ge else go)
      prev <- 2
    }
  }
  total
}

.gotoh <- function(x, y, go = 10, ge = 1) {
  x <- strsplit(x, "")[[1]]
  y <- strsplit(y, "")[[1]]
  m <- length(x)
  n <- length(y)
  H <- X <- Y <- matrix(-Inf, m + 1, n + 1)
  H[1, 1] <- 0
  for (i in seq_len(m)) X[i + 1, 1] <- -go - (i - 1) * ge
  for (j in seq_len(n)) Y[1, j + 1] <- -go - (j - 1) * ge
  for (i in seq_len(m)) {
    for (j in seq_len(n)) {
      H[i + 1, j + 1] <- .nuc(x[i], y[j]) + max(H[i, j], X[i, j], Y[i, j])
      X[i + 1, j + 1] <- max(H[i, j + 1] - go, X[i, j + 1] - ge, Y[i, j + 1] - go)
      Y[i + 1, j + 1] <- max(H[i + 1, j] - go, Y[i + 1, j] - ge, X[i + 1, j] - go)
    }
  }
  max(H[m + 1, n + 1], X[m + 1, n + 1], Y[m + 1, n + 1])
}

test_that("the alignment is valid and its score recomputes", {
  r <- MuscleAlign(.globins)
  expect_equal(gsub("-", "", r$alignment), .globins)
  expect_length(unique(nchar(r$alignment)), 1)
  expect_equal(r$sp_score, SumOfPairsScore(r$alignment))
  expect_gte(r$sp_score, r$sp_improved)
})

test_that("two sequences reach the optimal affine score", {
  x <- "ACGTTGCAACGTAGC"
  y <- "ACGTGCAACGTTAGGC"
  r <- MuscleAlign(c(x, y))
  expect_equal(.pair(r$alignment[1], r$alignment[2]), .gotoh(x, y), tolerance = 1e-12)
  expect_equal(r$sp_score, .gotoh(x, y), tolerance = 1e-12)
})

test_that("nucleotide sum of pairs and the k-mer guide tree", {
  seqs <- c("ACGTTGCAACGT", "ACGTGCAACGTT", "ACTTGCAAGT", "AGGTTGCAACGA", "ACGTTGCACGT")
  r <- MuscleAlign(seqs)
  sp <- 0
  for (i in 1:4) for (j in (i + 1):5) sp <- sp + .pair(r$alignment[i], r$alignment[j])
  expect_equal(r$sp_score, sp, tolerance = 1e-9)
  kd <- function(a, b, k = 3) {
    ka <- substring(a, 1:(nchar(a) - k + 1), k:nchar(a))
    kb <- substring(b, 1:(nchar(b) - k + 1), k:nchar(b))
    common <- sum(vapply(unique(ka), function(t) min(sum(ka == t), sum(kb == t)), numeric(1)))
    1 - common / (min(nchar(a), nchar(b)) - k + 1)
  }
  best <- c(Inf, 0, 0)
  for (i in 1:4) for (j in (i + 1):5) if (kd(seqs[i], seqs[j]) < best[1]) best <- c(kd(seqs[i], seqs[j]), i, j)
  expect_equal(r$tree1[[1]][1:2], best[2:3])
  expect_equal(r$tree1[[1]][3], best[1] / 2, tolerance = 1e-15)
})

test_that("identical sequences and errors", {
  r <- MuscleAlign(rep("MKTAYIAK", 3))
  expect_equal(r$alignment, rep("MKTAYIAK", 3))
  expect_equal(r$sp_score, 3 * (5 + 5 + 5 + 4 + 7 + 4 + 4 + 5))
  expect_error(MuscleAlign("ACGT"), "two sequences")
  expect_error(MuscleAlign(c("MKJ", "MKT")), "BLOSUM62")
})
