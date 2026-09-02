# Mutual information from k-nearest-neighbour statistics.
# Source: Kraskov, Stogbauer & Grassberger (2004), Phys. Rev. E 69,
# 066138, Eqs. 6, 8 and 9 (fetched-wave3/Kraskov_2004_MI_kNN.pdf).
# Mirrors Python morie.fn.miest1 exactly (same max-norm, same
# tie-breaking on the joint-neighbour ordering).

#' .mi_maxnorm
#'
#' A step of the miest1_native implementation. Called by \code{morie_miest1}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a Numeric; combined arithmetically in the body.
#' @param b Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.mi_maxnorm <- function(a, b) max(abs(a - b))

#' Mutual information by k-nearest-neighbour statistics (KSG)
#'
#' Joint space uses the maximum norm (Eq. 6).  Algorithm 1 (Eq. 8):
#' I = psi(k) - <psi(nx+1) + psi(ny+1)> + psi(N), counting points
#' with projected distance strictly less than the k-th joint
#' neighbour distance.  Algorithm 2 (Eq. 9):
#' I = psi(k) - 1/k - <psi(nx) + psi(ny)> + psi(N), counting points
#' inside the smallest rectangle holding the k joint neighbours.
#' Both are provided; algorithm 1 is the default (the paper reports
#' smaller statistical error for it at equal k).  Units are nats.
#'
#' @param X,Y Paired samples: vectors, or matrices with one row per
#'   observation.
#' @param k Number of neighbours (1 <= k < n).
#' @param algorithm 1 (Eq. 8, default) or 2 (Eq. 9).
#' @return A list with elements \code{mi}, \code{mi_bits}, \code{k},
#'   \code{algorithm}, \code{n}, \code{method}.
#' @references Kraskov, A., Stogbauer, H. and Grassberger, P. (2004).
#'   Estimating mutual information. Physical Review E, 69, 066138.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_miest1(V, V)
morie_miest1 <- function(X, Y, k = 3, algorithm = 1) {
  xs <- as.matrix(X); ys <- as.matrix(Y)
  if (ncol(xs) > 1 && nrow(xs) == 1) xs <- t(xs)
  if (ncol(ys) > 1 && nrow(ys) == 1) ys <- t(ys)
  n <- nrow(xs)
  if (nrow(ys) != n || n < 2) stop("X and Y must be paired, length >= 2")
  k <- as.integer(k)
  if (k < 1 || k >= n) stop("need 1 <= k < n")
  if (!algorithm %in% c(1, 2)) stop("algorithm must be 1 or 2")
  mi_sum <- 0
  for (i in seq_len(n)) {
    dx <- vapply(seq_len(n), function(j) .mi_maxnorm(xs[i, ], xs[j, ]),
                 numeric(1))
    dy <- vapply(seq_len(n), function(j) .mi_maxnorm(ys[i, ], ys[j, ]),
                 numeric(1))
    dz <- pmax(dx, dy)
    others <- setdiff(seq_len(n), i)
    ord <- others[order(dz[others], others)]
    if (algorithm == 1) {
      eps <- dz[ord[k]]
      nx <- sum(dx[others] < eps)
      ny <- sum(dy[others] < eps)
      mi_sum <- mi_sum + digamma(nx + 1) + digamma(ny + 1)
    } else {
      knn <- ord[seq_len(k)]
      ex <- max(dx[knn]); ey <- max(dy[knn])
      nx <- sum(dx[others] <= ex)
      ny <- sum(dy[others] <= ey)
      mi_sum <- mi_sum + digamma(max(nx, 1)) + digamma(max(ny, 1))
    }
  }
  mean_term <- mi_sum / n
  mi <- if (algorithm == 1) {
    digamma(k) - mean_term + digamma(n)
  } else {
    digamma(k) - 1 / k - mean_term + digamma(n)
  }
  list(mi = mi, mi_bits = mi / log(2), k = k,
       algorithm = as.integer(algorithm), n = n,
       method = sprintf("KSG mutual information, Eq. %d (Kraskov 2004)",
                        if (algorithm == 1) 8 else 9))
}
