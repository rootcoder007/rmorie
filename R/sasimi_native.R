# morie.fn -- function file (rootcoder007/morie)
# r"""Binary fingerprint similarity, and why the coefficient matters.
#
# For two fingerprints with :math:`a` and :math:`b` bits set and
# :math:`c` bits set in both,
#
# .. math:: T = \frac{c}{a + b - c}, \qquad
#           D = \frac{2c}{a + b}, \qquad
#           C = \frac{c}{\sqrt{ab}}, \qquad
#           Tv_{\alpha\beta} = \frac{c}{\alpha(a-c) + \beta(b-c) + c}.
#
# Tanimoto (the Jaccard coefficient on sets) is the default in
# chemical searching. The others are not cosmetic variants:
#
# *Tversky is the general case.* :math:`\alpha = \beta = 1` **is**
# Tanimoto and :math:`\alpha = \beta = \tfrac{1}{2}` **is** Dice --
# exact identities, and the anchor checks them rather than trusting the
# algebra. Unequal weights make the coefficient asymmetric on purpose:
# :math:`\alpha = 0.9, \beta = 0.1` asks "is A largely contained in B",
# which is the right question for substructure-style searching and the
# wrong one for symmetric clustering.
#
# *Dice and cosine always exceed Tanimoto.* They are monotone
# transformations of it for a fixed pair, so they rank identically --
# but they are not interchangeable as thresholds. A 0.85 Dice cut is a
# 0.74 Tanimoto cut.
#
# *Size bias.* Tanimoto systematically favours molecules with few bits
# set: the denominator grows with the union, so a large molecule needs
# proportionally more shared bits to reach the same score. Reporting
# the raw bit counts alongside the coefficient makes that visible
# rather than leaving it to be rediscovered.
#
# **The distance is a metric.** :math:`1 - T` satisfies the triangle
# inequality on binary fingerprints, which is what lets
# neighbour-search and clustering be reasoned about at all; the anchor
# checks it exhaustively on small fingerprints instead of citing it.
#
# References
# ----------
# Jaccard, P. (1912) "The distribution of the flora in the alpine
# zone", *New Phytologist* 11(2), 37-50,
# doi:10.1111/j.1469-8137.1912.tb05611.x, for the coefficient
# :math:`c/(a+b-c)` itself.
#
# Willett, P., Barnard, J. M. & Downs, G. M. (1998) "Chemical
# similarity searching", *Journal of Chemical Information and Computer
# Sciences* 38(6), 983-996, doi:10.1021/ci9800211. The survey these
# formulae are taken from: the Tanimoto, Dice, cosine and Tversky
# coefficients on binary fingerprints, the relations between them, and
# the size bias of Tanimoto.
# """

COEFFICIENTS <- c("tanimoto", "dice", "cosine")


#' sasimi_fingerprint
#'
#' Part of the sasimi_native implementation; see the file header for the
#' source it follows.
#'
#' @param bits See Usage.
#' @param n_bits Defaults to \code{NULL}.
#' @return A vector, from \code{sort}.
#' @export
sasimi_fingerprint <- function(bits, n_bits = NULL) {
  bits <- as.vector(bits)

  is_seq <- FALSE
  if (length(bits) > 0L) {
    if (is.logical(bits)) {
      is_seq <- TRUE
    } else if (is.numeric(bits)) {
      nums <- as.numeric(bits)
      if (all(!is.na(nums)) && all(nums %in% c(0, 1))) {
        is_seq <- TRUE
      }
    }
  }

  if (is_seq && !is.null(n_bits) && length(bits) != as.integer(n_bits)) {
    is_seq <- FALSE
  }

  if (is_seq) {
    nums <- as.numeric(bits)
    idx <- which(nums == 1) - 1L
  } else {
    idx <- as.integer(bits)
  }

  if (any(idx < 0L)) {
    stop("sasimi: a bit index cannot be negative")
  }
  if (!is.null(n_bits) && length(idx) > 0L &&
      max(idx) >= as.integer(n_bits)) {
    stop(sprintf("sasimi: bit %d is outside a %d-bit fingerprint",
                 max(idx), as.integer(n_bits)))
  }

  return(sort(unique(idx)))
}


#' sasimi_counts
#'
#' Part of the sasimi_native implementation; see the file header for the
#' source it follows.
#'
#' @param fp_a See Usage.
#' @param fp_b See Usage.
#' @return A list with \code{a}, \code{b}, \code{c}, \code{union}.
#' @export
sasimi_counts <- function(fp_a, fp_b) {
  A <- sasimi_fingerprint(fp_a)
  B <- sasimi_fingerprint(fp_b)
  list(
    a = length(A),
    b = length(B),
    c = length(intersect(A, B)),
    union = length(union(A, B))
  )
}


#' .sasimi_guard
#'
#' Part of the sasimi_native implementation; see the file header for the
#' source it follows.
#'
#' @param n See Usage.
#' @return One of two values, depending on the branch taken.
#' @export
.sasimi_guard <- function(n) {
  if (n$a == 0L && n$b == 0L) {
    stop("sasimi: both fingerprints are empty, so no similarity is defined")
  }
}


#' sasimi_tanimoto
#'
#' Part of the sasimi_native implementation; see the file header for the
#' source it follows.
#'
#' @param fp_a See Usage.
#' @param fp_b See Usage.
#' @return A numeric value.
#' @export
sasimi_tanimoto <- function(fp_a, fp_b) {
  n <- sasimi_counts(fp_a, fp_b)
  .sasimi_guard(n)
  n$c / as.numeric(n$a + n$b - n$c)
}


#' sasimi_dice
#'
#' Part of the sasimi_native implementation; see the file header for the
#' source it follows.
#'
#' @param fp_a See Usage.
#' @param fp_b See Usage.
#' @return A numeric value.
#' @export
sasimi_dice <- function(fp_a, fp_b) {
  n <- sasimi_counts(fp_a, fp_b)
  .sasimi_guard(n)
  2.0 * n$c / as.numeric(n$a + n$b)
}


#' sasimi_cosine
#'
#' Part of the sasimi_native implementation; see the file header for the
#' source it follows.
#'
#' @param fp_a See Usage.
#' @param fp_b See Usage.
#' @return A numeric value.
#' @export
sasimi_cosine <- function(fp_a, fp_b) {
  n <- sasimi_counts(fp_a, fp_b)
  .sasimi_guard(n)
  if (n$a == 0L || n$b == 0L) {
    return(0.0)
  }
  n$c / sqrt(as.numeric(n$a) * as.numeric(n$b))
}


#' sasimi_tversky
#'
#' Part of the sasimi_native implementation; see the file header for the
#' source it follows.
#'
#' @param fp_a See Usage.
#' @param fp_b See Usage.
#' @param alpha Defaults to \code{1}.
#' @param beta Defaults to \code{1}.
#' @return A numeric value.
#' @export
sasimi_tversky <- function(fp_a, fp_b, alpha = 1.0, beta = 1.0) {
  al <- as.numeric(alpha)
  be <- as.numeric(beta)
  if (al < 0.0 || be < 0.0) {
    stop("sasimi: the Tversky weights cannot be negative")
  }
  n <- sasimi_counts(fp_a, fp_b)
  .sasimi_guard(n)
  den <- al * (n$a - n$c) + be * (n$b - n$c) + n$c
  if (den == 0) {
    stop(sprintf("sasimi: the Tversky denominator vanishes for alpha=%g, beta=%g on these fingerprints",
                 al, be))
  }
  n$c / as.numeric(den)
}


#' .sasimi_coef
#'
#' Part of the sasimi_native implementation; see the file header for the
#' source it follows.
#'
#' @param name See Usage.
#' @return Nothing; this branch always raises.
#' @export
.sasimi_coef <- function(name) {
  if (name == "tanimoto") return(sasimi_tanimoto)
  if (name == "dice") return(sasimi_dice)
  if (name == "cosine") return(sasimi_cosine)
  stop(sprintf("sasimi: coefficient must be one of %s, got %s",
               paste(COEFFICIENTS, collapse = ", "), name))
}


#' sasimi_distance
#'
#' Part of the sasimi_native implementation; see the file header for the
#' source it follows.
#'
#' @param fp_a See Usage.
#' @param fp_b See Usage.
#' @param coefficient Defaults to \code{"tanimoto"}.
#' @return A numeric value.
#' @export
sasimi_distance <- function(fp_a, fp_b, coefficient = "tanimoto") {
  1.0 - .sasimi_coef(coefficient)(fp_a, fp_b)
}


#' sasimi_similarity_matrix
#'
#' Part of the sasimi_native implementation; see the file header for the
#' source it follows.
#'
#' @param fps See Usage.
#' @param coefficient Defaults to \code{"tanimoto"}.
#' @return The value of \code{M}, as built in the body.
#' @export
sasimi_similarity_matrix <- function(fps, coefficient = "tanimoto") {
  f <- .sasimi_coef(coefficient)
  F <- lapply(fps, sasimi_fingerprint)
  if (length(F) < 2L) {
    stop("sasimi: need at least two fingerprints")
  }
  n <- length(F)
  M <- matrix(1.0, nrow = n, ncol = n)
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (j > i) {
        M[i, j] <- M[j, i] <- f(F[[i]], F[[j]])
      }
    }
  }
  M
}


#' sasimi_nearest_neighbours
#'
#' Part of the sasimi_native implementation; see the file header for the
#' source it follows.
#'
#' @param query See Usage.
#' @param fps See Usage.
#' @param k Defaults to \code{5L}.
#' @param coefficient Defaults to \code{"tanimoto"}.
#' @return The value of \code{result}, as built in the body.
#' @export
sasimi_nearest_neighbours <- function(query, fps, k = 5L, coefficient = "tanimoto") {
  if (as.integer(k) < 1L) {
    stop("sasimi: k must be at least 1")
  }
  f <- .sasimi_coef(coefficient)
  q <- sasimi_fingerprint(query)
  n <- length(fps)
  if (n == 0L) {
    return(list())
  }
  scores <- numeric(n)
  for (i in seq_len(n)) {
    scores[i] <- f(q, sasimi_fingerprint(fps[[i]]))
  }
  ord <- order(-scores, seq_len(n) - 1L)
  kk <- min(as.integer(k), n)
  result <- vector("list", kk)
  for (idx in seq_len(kk)) {
    i <- ord[idx]
    result[[idx]] <- list(index = i - 1L, similarity = scores[i])
  }
  result
}


#' morie_sasimi
#'
#' Part of the sasimi_native implementation; see the file header for the
#' source it follows.
#'
#' @param fp_a See Usage.
#' @param fp_b See Usage.
#' @param coefficient Defaults to \code{"tanimoto"}.
#' @param alpha Defaults to \code{NULL}.
#' @param beta Defaults to \code{NULL}.
#' @return A list with \code{estimate}, \code{similarity}, \code{distance}, \code{bits_a}, \code{bits_b}, \code{bits_shared}, \code{coefficient}, \code{method}.
#' @export
morie_sasimi <- function(fp_a, fp_b, coefficient = "tanimoto",
                         alpha = NULL, beta = NULL) {
  n <- sasimi_counts(fp_a, fp_b)
  if (!is.null(alpha) || !is.null(beta)) {
    al <- if (is.null(alpha)) 1.0 else alpha
    be <- if (is.null(beta)) 1.0 else beta
    s <- sasimi_tversky(fp_a, fp_b, al, be)
    how <- sprintf("Tversky(alpha=%g, beta=%g)", al, be)
  } else {
    s <- .sasimi_coef(coefficient)(fp_a, fp_b)
    how <- coefficient
  }
  list(
    estimate = s,
    similarity = s,
    distance = 1.0 - s,
    bits_a = n$a,
    bits_b = n$b,
    bits_shared = n$c,
    coefficient = how,
    method = "Willett, Barnard & Downs (1998) binary fingerprint coefficients"
  )
}
