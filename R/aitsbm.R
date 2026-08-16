# SPDX-License-Identifier: AGPL-3.0-or-later

# Internal: closure operator -- rescale each row to unit sum.
#' Internal: closure operator -- rescale each row to unit sum
#'
#' A step of the aitsbm implementation. Called by \code{morie_subcompositional_incoherence}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param X Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.aitsbm_close <- function(X) X / rowSums(X)

#' Spurious correlation of compositional parts, and its cure
#'
#' Pearson (1897) showed that correlations between ratios sharing a
#' denominator are constrained by the denominator, not by any relation
#' among the numerators. Compositional data carry exactly that defect:
#' the parts are closed to a constant sum, so their correlations are
#' driven by the closure. The visible symptom is subcompositional
#' incoherence -- the correlation between two parts changes when other,
#' unrelated parts are dropped from the composition.
#'
#' This diagnostic measures that change directly. It takes the same two
#' parts, computes their Pearson correlation in the full composition and
#' again after re-closing to the subcomposition named by \code{idx}, and
#' reports the difference. A large \code{delta} means a correlation read
#' off the raw parts says more about which parts happened to be measured
#' than about the parts themselves.
#'
#' Alongside it the function returns the variation-array entry
#' \eqn{\tau_{ij} = var(log(x_i / x_j))}, which is invariant under
#' closure because the log-ratio cancels any common factor. It therefore
#' takes the same value in the full composition and in every
#' subcomposition containing both parts, so \code{tau_delta} is zero up
#' to floating-point error. That is the point: it is the coherent
#' quantity to report where the raw correlation is not.
#'
#' Mirrors \code{morie.fn.aitsbm} on the Python side. See also
#' \code{\link{morie_taphonomy_clr}} for the centred log-ratio transform.
#'
#' @param x Numeric matrix of compositional data (n x D), D >= 3, all
#'   entries strictly positive. Rows need not already sum to a constant.
#' @param idx Integer vector of column indices forming the
#'   subcomposition, length >= 2 and < D. The first two entries are the
#'   pair whose correlation is compared.
#' @return Named list with \code{rho_full}, \code{rho_sub}, \code{delta}
#'   (\code{rho_sub - rho_full}), \code{tau_full}, \code{tau_sub},
#'   \code{tau_delta}, \code{pair}, \code{idx}, \code{n}, \code{D},
#'   \code{method}.
#' @references Aitchison J (1986). \emph{The Statistical Analysis of
#'   Compositional Data}. Monographs on Statistics and Applied
#'   Probability. Chapman & Hall, London, 416 pp.
#'
#'   Pearson K (1897). Mathematical contributions to the theory of
#'   evolution -- on a form of spurious correlation which may arise when
#'   indices are used in the measurement of organs. \emph{Proceedings of
#'   the Royal Society of London}, 60, 489-498.
#' @examples
#' set.seed(42)
#' x <- exp(matrix(rnorm(200 * 5), 200, 5))
#' morie_subcompositional_incoherence(x, idx = c(1, 2, 3))$delta
#' @export
morie_subcompositional_incoherence <- function(x, idx) {
  X <- as.matrix(x)
  if (length(dim(X)) != 2L) {
    stop("x must be a matrix (n observations x D parts).", call. = FALSE)
  }
  n <- nrow(X)
  D <- ncol(X)
  if (D < 3L) {
    stop("Need at least 3 parts to form a proper subcomposition, got D=",
      D, ".",
      call. = FALSE
    )
  }
  if (n < 3L) stop("Need at least 3 observations, got ", n, ".", call. = FALSE)
  if (!all(X > 0)) {
    stop("Compositional parts must be strictly positive; log-ratios are ",
      "undefined at zero.",
      call. = FALSE
    )
  }

  sub <- as.integer(idx)
  if (length(sub) < 2L) {
    stop("idx must name at least 2 parts, got ", length(sub), ".", call. = FALSE)
  }
  if (length(sub) >= D) {
    stop("idx must name fewer than D=", D,
      " parts, else the subcomposition is the composition.",
      call. = FALSE
    )
  }
  if (any(sub < 1L) || any(sub > D)) {
    stop("idx entries must lie in [1, ", D, "].", call. = FALSE)
  }
  if (anyDuplicated(sub)) stop("idx must not repeat a part.", call. = FALSE)

  i <- sub[1L]
  j <- sub[2L]
  full <- .aitsbm_close(X)
  subc <- .aitsbm_close(X[, sub, drop = FALSE])

  # Population variance (denominator n), matching numpy's np.var default,
  # so the two languages report the same tau.
  pvar <- function(v) mean((v - mean(v))^2)

  list(
    rho_full = stats::cor(full[, i], full[, j]),
    rho_sub = stats::cor(subc[, 1L], subc[, 2L]),
    delta = stats::cor(subc[, 1L], subc[, 2L]) - stats::cor(full[, i], full[, j]),
    tau_full = pvar(log(full[, i] / full[, j])),
    tau_sub = pvar(log(subc[, 1L] / subc[, 2L])),
    tau_delta = pvar(log(subc[, 1L] / subc[, 2L])) -
      pvar(log(full[, i] / full[, j])),
    pair = c(i, j),
    idx = sub,
    n = n,
    D = D,
    method = "Pearson correlation under closure vs. Aitchison variation array"
  )
}
