# BLAST maximal segment pair + Karlin-Altschul statistics.
# Source: Altschul, Gish, Miller, Myers & Lipman (1990), J. Mol.
# Biol. 215(3), 403-410, Secs. 2-3 (fetched-wave3/Basic Local
# Alignment Search Tool.pdf).  Mirrors Python morie.fn.blastp
# exactly (same ungapped diagonal scan, same tie-breaking).

#' .blast_best
#'
#' A step of the blastp_native implementation. Called by \code{morie_blastp}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a A vector; its length is taken and its elements indexed.
#' @param b A vector; its length is taken and its elements indexed.
#' @param score Accepted by the signature and not used anywhere in the body.
#' @return A list with \code{score}, \code{qi}, \code{sj}, \code{ln}.
#' @export
.blast_best <- function(a, b, score) {
  n <- length(a)
  m <- length(b)
  best <- 0
  bi <- 0
  bj <- 0
  ln <- 0
  for (d in (-(m - 1)):(n - 1)) {
    run <- 0
    start <- 0
    i0 <- max(0, d)
    j0 <- i0 - d
    pos <- 0
    i <- i0
    j <- j0
    while (i < n && j < m) {
      run <- run + score(a[i + 1], b[j + 1])
      if (run <= 0) {
        run <- 0
        start <- pos + 1
      } else if (run > best) {
        best <- run
        ln <- pos - start + 1
        bi <- max(0, d) + start
        bj <- bi - d
      }
      i <- i + 1
      j <- j + 1
      pos <- pos + 1
    }
  }
  list(score = best, qi = bi, sj = bj, ln = ln)
}

#' BLAST maximal segment pair with Karlin-Altschul E-value
#'
#' MSP = highest-scoring ungapped local segment pair (Altschul et
#' al. 1990 Sec. 2a); E = K m n exp(-lambda S) (Karlin-Altschul
#' extreme-value statistics); p = 1 - exp(-E).
#'
#' @param query,subject Sequences (character strings).
#' @param match,mismatch Substitution scores.
#' @param K,lam Karlin-Altschul parameters.
#' @param score_matrix Optional named list keyed "a|b".
#' @return A list with elements \code{score}, \code{e_value},
#'   \code{p_value}, \code{q_start}, \code{s_start}, \code{length},
#'   \code{method}.
#' @references Altschul, S. F., Gish, W., Miller, W., Myers, E. W.
#'   and Lipman, D. J. (1990). Basic local alignment search tool.
#'   Journal of Molecular Biology, 215(3), 403-410.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_blastp(V, V)
morie_blastp <- function(query, subject, match = 1, mismatch = -1,
                         K = 0.1, lam = 1, score_matrix = NULL) {
  q <- strsplit(as.character(query), "")[[1]]
  s <- strsplit(as.character(subject), "")[[1]]
  if (!length(q) || !length(s)) stop("sequences must be non-empty")
  score <- if (!is.null(score_matrix)) {
    function(x, y) {
      v <- score_matrix[[paste0(x, "|", y)]]
      if (is.null(v)) v <- score_matrix[[paste0(y, "|", x)]]
      if (is.null(v)) mismatch else v
    }
  } else {
    function(x, y) if (x == y) match else mismatch
  }
  res <- .blast_best(q, s, score)
  m <- length(q)
  n <- length(s)
  E <- K * m * n * exp(-lam * res$score)
  list(score = res$score, e_value = E, p_value = 1 - exp(-E),
       q_start = res$qi, s_start = res$sj, length = res$ln,
       method = "BLAST MSP + Karlin-Altschul E-value (Altschul 1990)")
}
