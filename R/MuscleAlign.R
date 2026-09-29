# NCBI BLOSUM62 (Henikoff and Henikoff 1992), ftp.ncbi.nlm.nih.gov/blast/matrices/BLOSUM62
.msa_b62_alpha <- "ARNDCQEGHILKMFPSTWYVBZX*"
.msa_b62 <- c(
  4, -1, -2, -2, 0, -1, -1, 0, -2, -1, -1, -1, -1, -2, -1, 1, 0, -3, -2, 0, -2, -1, 0, -4,
  -1, 5, 0, -2, -3, 1, 0, -2, 0, -3, -2, 2, -1, -3, -2, -1, -1, -3, -2, -3, -1, 0, -1, -4,
  -2, 0, 6, 1, -3, 0, 0, 0, 1, -3, -3, 0, -2, -3, -2, 1, 0, -4, -2, -3, 3, 0, -1, -4,
  -2, -2, 1, 6, -3, 0, 2, -1, -1, -3, -4, -1, -3, -3, -1, 0, -1, -4, -3, -3, 4, 1, -1, -4,
  0, -3, -3, -3, 9, -3, -4, -3, -3, -1, -1, -3, -1, -2, -3, -1, -1, -2, -2, -1, -3, -3, -2, -4,
  -1, 1, 0, 0, -3, 5, 2, -2, 0, -3, -2, 1, 0, -3, -1, 0, -1, -2, -1, -2, 0, 3, -1, -4,
  -1, 0, 0, 2, -4, 2, 5, -2, 0, -3, -3, 1, -2, -3, -1, 0, -1, -3, -2, -2, 1, 4, -1, -4,
  0, -2, 0, -1, -3, -2, -2, 6, -2, -4, -4, -2, -3, -3, -2, 0, -2, -2, -3, -3, -1, -2, -1, -4,
  -2, 0, 1, -1, -3, 0, 0, -2, 8, -3, -3, -1, -2, -1, -2, -1, -2, -2, 2, -3, 0, 0, -1, -4,
  -1, -3, -3, -3, -1, -3, -3, -4, -3, 4, 2, -3, 1, 0, -3, -2, -1, -3, -1, 3, -3, -3, -1, -4,
  -1, -2, -3, -4, -1, -2, -3, -4, -3, 2, 4, -2, 2, 0, -3, -2, -1, -2, -1, 1, -4, -3, -1, -4,
  -1, 2, 0, -1, -3, 1, 1, -2, -1, -3, -2, 5, -1, -3, -1, 0, -1, -3, -2, -2, 0, 1, -1, -4,
  -1, -1, -2, -3, -1, 0, -2, -3, -2, 1, 2, -1, 5, 0, -2, -1, -1, -1, -1, 1, -3, -1, -1, -4,
  -2, -3, -3, -3, -2, -3, -3, -3, -1, 0, 0, -3, 0, 6, -4, -2, -2, 1, 3, -1, -3, -3, -1, -4,
  -1, -2, -2, -1, -3, -1, -1, -2, -2, -3, -3, -1, -2, -4, 7, -1, -1, -4, -3, -2, -2, -1, -2, -4,
  1, -1, 1, 0, -1, 0, 0, 0, -1, -2, -2, 0, -1, -2, -1, 4, 1, -3, -2, -2, 0, 0, 0, -4,
  0, -1, 0, -1, -1, -1, -1, -2, -2, -1, -1, -1, -1, -2, -1, 1, 5, -2, -2, 0, -1, -1, 0, -4,
  -3, -3, -4, -4, -2, -2, -3, -2, -2, -3, -2, -3, -1, 1, -4, -3, -2, 11, 2, -3, -4, -3, -2, -4,
  -2, -2, -2, -3, -2, -1, -2, -3, 2, -1, -1, -2, -1, 3, -3, -2, -2, 2, 7, -1, -3, -2, -1, -4,
  0, -3, -3, -3, -1, -2, -2, -3, -3, 3, 1, -2, 1, -1, -2, -2, 0, -3, -1, 4, -3, -2, -1, -4,
  -2, -1, 3, 4, -3, 0, 1, -1, 0, -3, -4, 0, -3, -3, -2, 0, -1, -4, -3, -3, 4, 1, -1, -4,
  -1, 0, 0, 1, -3, 3, 4, -2, 0, -3, -3, 1, -1, -3, -1, 0, -1, -3, -2, -2, 1, 4, -1, -4,
  0, -1, -1, -1, -2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -2, 0, 0, -2, -1, -1, -1, -1, -1, -4,
  -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, 1
)

.msa_matrix <- function(seqs, matrix) {
  letters <- sort(unique(unlist(strsplit(seqs, ""))), method = "radix")
  L <- length(letters)
  S <- matrix(0, L, L, dimnames = list(letters, letters))
  if (is.matrix(matrix)) {
    for (a in letters) for (b in letters) S[a, b] <- matrix[a, b]
    return(list(letters = letters, S = S))
  }
  if (is.null(matrix)) matrix <- if (all(letters %in% strsplit("ACGTUN", "")[[1]])) "nucleotide" else "blosum62"
  if (identical(matrix, "blosum62")) {
    alpha <- strsplit(.msa_b62_alpha, "")[[1]]
    B <- matrix(.msa_b62, 24, 24, byrow = TRUE, dimnames = list(alpha, alpha))
    bad <- setdiff(letters, alpha)
    if (length(bad)) stop("letters not in BLOSUM62: ", paste(bad, collapse = ""))
    S[, ] <- B[letters, letters]
    return(list(letters = letters, S = S))
  }
  if (identical(matrix, "nucleotide")) {
    for (a in letters) {
      for (b in letters) {
        S[a, b] <- if ("N" %in% c(a, b)) -2 else if (a == b || setequal(c(a, b), c("T", "U"))) 5 else -4
      }
    }
    return(list(letters = letters, S = S))
  }
  stop("matrix must be 'blosum62', 'nucleotide' or a matrix")
}

.msa_kmer <- function(x, y, k) {
  km <- function(s) if (nchar(s) >= k) table(substring(s, seq_len(nchar(s) - k + 1), seq_len(nchar(s) - k + 1) + k - 1))
  den <- min(nchar(x), nchar(y)) - k + 1
  if (den <= 0) return(1)
  cx <- km(x)
  cy <- km(y)
  common <- intersect(names(cx), names(cy))
  1 - sum(pmin(as.numeric(cx[common]), as.numeric(cy[common]))) / den
}

.msa_upgma <- function(D) {
  n <- nrow(D)
  dist <- new.env()
  key <- function(i, j) paste(min(i, j), max(i, j))
  for (i in seq_len(n)) for (j in seq_len(n)) if (i < j) dist[[key(i, j)]] <- D[i, j]
  size <- c(rep(1, n), numeric(n))
  active <- seq_len(n)
  merges <- list()
  nxt <- n + 1
  while (length(active) > 1) {
    best <- NULL
    for (a in seq_along(active)) {
      for (b in seq_along(active)) {
        if (b <= a) next
        d <- dist[[key(active[a], active[b])]]
        if (is.null(best) || d < best[1]) best <- c(d, active[a], active[b])
      }
    }
    i <- best[2]
    j <- best[3]
    merges[[length(merges) + 1]] <- c(i, j, best[1] / 2)
    active <- active[active != i & active != j]
    for (m in active) {
      dist[[key(m, nxt)]] <- (size[i] * dist[[key(i, m)]] + size[j] * dist[[key(j, m)]]) / (size[i] + size[j])
    }
    size[nxt] <- size[i] + size[j]
    active <- c(active, nxt)
    nxt <- nxt + 1
  }
  merges
}

.msa_profile <- function(rows, letters) {
  ch <- do.call(rbind, strsplit(rows, ""))
  if (is.null(dim(ch))) ch <- matrix(ch, nrow = 1)
  ns <- nrow(ch)
  lapply(seq_len(ncol(ch)), function(c) {
    f <- numeric(length(letters))
    for (r in seq_len(ns)) if (ch[r, c] != "-") f[match(ch[r, c], letters)] <- f[match(ch[r, c], letters)] + 1
    f / ns
  })
}

.msa_align <- function(A, B, letters, S, go, ge) {
  pa <- .msa_profile(A, letters)
  pb <- .msa_profile(B, letters)
  L <- length(letters)
  m <- length(pa)
  n <- length(pb)
  pam <- lapply(pa, function(col) {
    v <- numeric(L)
    for (a in seq_len(L)) if (col[a] != 0) for (b in seq_len(L)) v[b] <- v[b] + col[a] * S[a, b]
    v
  })
  H <- matrix(-Inf, m + 1, n + 1)
  X <- matrix(-Inf, m + 1, n + 1)
  Y <- matrix(-Inf, m + 1, n + 1)
  H[1, 1] <- 0
  for (i in seq_len(m)) X[i + 1, 1] <- -go - (i - 1) * ge
  for (j in seq_len(n)) Y[1, j + 1] <- -go - (j - 1) * ge
  for (i in seq_len(m)) {
    va <- pam[[i]]
    for (j in seq_len(n)) {
      s <- 0
      cb <- pb[[j]]
      for (b in seq_len(L)) s <- s + va[b] * cb[b]
      H[i + 1, j + 1] <- s + max(H[i, j], X[i, j], Y[i, j])
      X[i + 1, j + 1] <- max(H[i, j + 1] - go, X[i, j + 1] - ge, Y[i, j + 1] - go)
      Y[i + 1, j + 1] <- max(H[i + 1, j] - go, Y[i + 1, j] - ge, X[i + 1, j] - go)
    }
  }
  i <- m
  j <- n
  state <- 1
  best <- H[m + 1, n + 1]
  if (X[m + 1, n + 1] > best) {
    state <- 2
    best <- X[m + 1, n + 1]
  }
  if (Y[m + 1, n + 1] > best) state <- 3
  ops <- integer(0)
  while (i > 0 || j > 0) {
    if (state == 1) {
      ops <- c(ops, 1L)
      prev <- c(H[i, j], X[i, j], Y[i, j])
      i <- i - 1
      j <- j - 1
    } else if (state == 2) {
      ops <- c(ops, 2L)
      prev <- c(H[i, j + 1] - go, X[i, j + 1] - ge, Y[i, j + 1] - go)
      i <- i - 1
    } else {
      ops <- c(ops, 3L)
      prev <- c(H[i + 1, j] - go, X[i + 1, j] - ge, Y[i + 1, j] - go)
      j <- j - 1
    }
    if (i == 0 && j == 0) break
    state <- which.max(prev)
  }
  ops <- rev(ops)
  ca <- do.call(rbind, strsplit(A, ""))
  cb <- do.call(rbind, strsplit(B, ""))
  if (is.null(dim(ca))) ca <- matrix(ca, nrow = 1)
  if (is.null(dim(cb))) cb <- matrix(cb, nrow = 1)
  ia <- cumsum(ops != 3)
  ib <- cumsum(ops != 2)
  outA <- vapply(seq_len(nrow(ca)), function(k) paste(ifelse(ops == 3, "-", ca[k, pmax(ia, 1)]), collapse = ""), "")
  outB <- vapply(seq_len(nrow(cb)), function(k) paste(ifelse(ops == 2, "-", cb[k, pmax(ib, 1)]), collapse = ""), "")
  c(outA, outB)
}

.msa_pair <- function(s, t, S, go, ge) {
  a <- strsplit(s, "")[[1]]
  b <- strsplit(t, "")[[1]]
  score <- 0
  prev <- 0
  for (c in seq_along(a)) {
    if (a[c] == "-" && b[c] == "-") next
    if (a[c] != "-" && b[c] != "-") {
      score <- score + S[a[c], b[c]]
      prev <- 0
    } else if (a[c] == "-") {
      score <- score - (if (prev == 1) ge else go)
      prev <- 1
    } else {
      score <- score - (if (prev == 2) ge else go)
      prev <- 2
    }
  }
  score
}

.msa_sp <- function(rows, S, go, ge) {
  total <- 0
  for (i in seq_along(rows)) for (j in seq_along(rows)) if (i < j) total <- total + .msa_pair(rows[i], rows[j], S, go, ge)
  total
}

.msa_progressive <- function(seqs, merges, letters, S, go, ge) {
  n <- length(seqs)
  groups <- list()
  for (i in seq_len(n)) groups[[i]] <- list(ids = i, rows = seqs[i])
  nxt <- n + 1
  for (mg in merges) {
    ga <- groups[[mg[1]]]
    gb <- groups[[mg[2]]]
    groups[[nxt]] <- list(ids = c(ga$ids, gb$ids), rows = .msa_align(ga$rows, gb$rows, letters, S, go, ge))
    nxt <- nxt + 1
  }
  g <- groups[[nxt - 1]]
  out <- character(n)
  out[g$ids] <- g$rows
  out
}

.msa_strip <- function(rows) {
  ch <- do.call(rbind, strsplit(rows, ""))
  if (is.null(dim(ch))) ch <- matrix(ch, nrow = 1)
  keep <- colSums(ch != "-") > 0
  apply(ch[, keep, drop = FALSE], 1, paste, collapse = "")
}

#' MUSCLE-style multiple sequence alignment
#'
#' \code{MuscleAlign} follows the three stages of MUSCLE (Edgar 2004): a draft
#' from k-mer distances (\code{1 - F}, \code{F} the shared k-mer count over
#' \code{min(|x|, |y|) - k + 1}), a UPGMA guide tree and progressive
#' profile-profile alignment (Gotoh affine gaps, frequency-weighted mean
#' substitution score per column pair); an improved alignment from Kimura
#' distances \code{-ln(1 - p - p^2/5)} of the draft (10 when undefined) and a
#' second UPGMA tree; and tree-dependent restricted partitioning, re-aligning
#' the two sub-alignments on either side of each edge (deepest first) and
#' keeping the result when the sum-of-pairs score rises, until a pass brings
#' no gain or \code{max_iters}. Simplifications against MUSCLE 3: no sequence
#' weights, constant gap penalties (ends included) and BLOSUM62 or +5/-4
#' nucleotide scores instead of the log-expectation profile function.
#' \code{SumOfPairsScore} is the objective: over all row pairs, columns gapped
#' in both are dropped, residue pairs score \code{S(a, b)} and a gap run of
#' length \code{L} costs \code{gap_open + (L - 1) gap_extend}. Identical to
#' the Python arm \code{morie.fn.musclemsa}.
#'
#' @param sequences Character vector of unaligned sequences.
#' @param alignment Character vector of aligned rows (gap \code{-}).
#' @param matrix \code{"blosum62"}, \code{"nucleotide"} or a square score
#'   matrix with letter dimnames; chosen from the alphabet when \code{NULL}.
#' @param gap_open,gap_extend Affine gap costs.
#' @param k k-mer length for the draft distances.
#' @param max_iters Maximum refinement passes.
#' @return \code{MuscleAlign}: list with \code{alignment} (input order),
#'   \code{sp_score}, \code{sp_draft}, \code{sp_improved}, \code{accepted},
#'   \code{tree1} and \code{tree2} (UPGMA merges, 1-based node ids).
#'   \code{SumOfPairsScore}: the score.
#' @references Edgar, R. C. (2004). MUSCLE: multiple sequence alignment with
#'   high accuracy and high throughput. Nucleic Acids Research 32, 1792-1797.
#'
#'   Edgar, R. C. (2004). MUSCLE: a multiple sequence alignment method with
#'   reduced time and space complexity. BMC Bioinformatics 5, 113.
#' @examples
#' r <- MuscleAlign(c("ACGTACGT", "ACGACGT", "ACGTTACGT"))
#' r$alignment
#' SumOfPairsScore(c("AC-GT", "ACGGT"), matrix = "nucleotide")
#' @export
MuscleAlign <- function(sequences, matrix = NULL, gap_open = 10, gap_extend = 1, k = 3, max_iters = 16) {
  seqs <- toupper(sequences)
  if (length(seqs) < 2) stop("need at least two sequences")
  mt <- .msa_matrix(seqs, matrix)
  letters <- mt$letters
  S <- mt$S
  n <- length(seqs)
  D <- matrix(0, n, n)
  for (i in seq_len(n)) for (j in seq_len(n)) if (i < j) D[i, j] <- D[j, i] <- .msa_kmer(seqs[i], seqs[j], k)
  tree1 <- .msa_upgma(D)
  draft <- .msa_progressive(seqs, tree1, letters, S, gap_open, gap_extend)
  sp_draft <- .msa_sp(draft, S, gap_open, gap_extend)
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (i >= j) next
      a <- strsplit(draft[i], "")[[1]]
      b <- strsplit(draft[j], "")[[1]]
      both <- a != "-" & b != "-"
      tot <- sum(both)
      p <- if (tot) 1 - sum(a[both] == b[both]) / tot else 1
      arg <- 1 - p - p * p / 5
      D[i, j] <- D[j, i] <- if (arg > 0) -log(arg) else 10
    }
  }
  tree2 <- .msa_upgma(D)
  aln <- .msa_progressive(seqs, tree2, letters, S, gap_open, gap_extend)
  sp <- .msa_sp(aln, S, gap_open, gap_extend)
  sp_improved <- sp
  root <- 2 * n - 1
  kids <- list()
  for (t in seq_along(tree2)) kids[[n + t]] <- tree2[[t]][1:2]
  depth <- numeric(root)
  for (node in root:(n + 1)) for (cc in kids[[node]]) depth[cc] <- depth[node] + 1
  leaves <- function(node) if (node <= n) node else c(leaves(kids[[node]][1]), leaves(kids[[node]][2]))
  edges <- order(-depth[seq_len(root - 1)], seq_len(root - 1))
  accepted <- 0
  for (pass in seq_len(max_iters)) {
    improved <- FALSE
    for (v in edges) {
      inside <- sort(leaves(v))
      outside <- setdiff(seq_len(n), inside)
      if (!length(outside)) next
      merged <- .msa_align(.msa_strip(aln[inside]), .msa_strip(aln[outside]), letters, S, gap_open, gap_extend)
      cand <- character(n)
      cand[c(inside, outside)] <- merged
      sc <- .msa_sp(cand, S, gap_open, gap_extend)
      if (sc > sp) {
        aln <- cand
        sp <- sc
        accepted <- accepted + 1
        improved <- TRUE
      }
    }
    if (!improved) break
  }
  list(alignment = aln, sp_score = sp, sp_draft = sp_draft, sp_improved = sp_improved, accepted = accepted,
       tree1 = tree1, tree2 = tree2)
}

#' @rdname MuscleAlign
#' @export
SumOfPairsScore <- function(alignment, matrix = NULL, gap_open = 10, gap_extend = 1) {
  mt <- .msa_matrix(gsub("-", "", alignment, fixed = TRUE), matrix)
  .msa_sp(alignment, mt$S, gap_open, gap_extend)
}
