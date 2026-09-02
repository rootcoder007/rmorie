# SPDX-License-Identifier: AGPL-3.0-or-later
#' Check that a moment sequence is realisable by a measure on \[0, 1\]
#'
#' Hausdorff's condition -- alternating finite differences all
#' non-negative -- is necessary AND sufficient. \code{min_difference} is
#' returned because a sequence can be feasible by a tiny margin.
#'
#' Formula: m realisable on \[0, 1\] iff (-1)^k (Delta^k m)_j >= 0 for all
#'   j, k >= 0, where (Delta m)_j = m_\{j+1\} - m_j
#'
#' @param moments m_0, m_1, ..., m_n with m_0 = 1.
#' @return List with \code{feasible}, \code{min_difference},
#'   \code{n_violations}, \code{order}, \code{differences}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, Section 3.4.4. That section gives
#'   NO formula; the realisability condition is Hausdorff (1921),
#'   Mathematische Zeitschrift 9, 74-109, and is cited to its own source.
#'   Ghosal read from the copy of the book held in the corpus.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Momprior(V)
Momprior <- function(moments) {
  m <- .t1_vec(moments)
  N <- length(m)
  if (N < 1L) stop("at least the zeroth moment is required")
  if (abs(m[1] - 1) > 1e-12)
    stop("m_0 must equal 1 for a probability measure")
  tri <- list(m)
  cur <- m
  for (k in seq_len(N - 1L)) {
    cur <- cur[-length(cur)] - cur[-1]
    tri[[k + 1L]] <- cur
  }
  allv <- unlist(tri)
  .t1_result(feasible = as.numeric(all(allv >= -1e-12)),
             min_difference = min(allv),
             n_violations = sum(allv < -1e-12), order = as.numeric(N - 1L),
             differences = tri,
             method = "Hausdorff moment feasibility, Ghosal Section 3.4.4")
}
