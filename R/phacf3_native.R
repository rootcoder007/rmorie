# SPDX-License-Identifier: AGPL-3.0-or-later
# R arm of phacf3 -- three-point 3D pharmacophore fingerprints. Mirrors
# src/morie/fn/phacf3.py operation for operation, on the shared numerics
# in R/aaa_helpers_w3num.R.
#
# A pharmacophore is the abstraction that survives when you throw away
# the chemistry and keep only what the protein can feel: a hydrogen-bond
# donor here, an acceptor four angstroms away, an aromatic ring seven
# from both. Two molecules from unrelated series that present the same
# triangle of features to the same pocket bind the same way, and a
# fingerprint over those triangles is what lets you find that.
#
# The construction reduces the molecule to typed FEATURE POINTS, takes
# every triple of them, bins the three inter-feature distances (a
# pharmacophore that only matched at exact distances would never match
# anything), canonicalises the triangle so the same geometry lands on
# the same bit whatever order the atoms came in, and sets that bit.
#
# The canonicalisation is the whole difficulty. A triangle has six
# vertex orderings and each gives a different tuple of (types, bins);
# the canonical form is the smallest of the six, and getting it wrong
# means the same pharmacophore in two molecules hits two different bits
# and the comparison silently fails. It is done here by generating all
# six and taking the minimum, which is slow and obviously correct,
# rather than by a sorting rule that is fast and subtly wrong on ties.
#
# The bit space is enumerated the same way -- every possible canonical
# (type triple, bin triple) -- so a bit index means the same thing in
# every molecule and the space is a fixed size that both arms agree on.
#
# What a three-point fingerprint CANNOT do is tell a molecule from its
# mirror image: three points and three distances are the same in both
# hands, and chirality only becomes visible with a fourth point and a
# signed volume. That is not a defect in this implementation, it is why
# four-point pharmacophores exist, and there is a check that
# demonstrates it rather than a comment claiming it.
#
# Feature perception is not done here. Which atoms are donors is a
# question for a chemistry toolkit, and guessing would put fabricated
# chemistry underneath everything above. Feature points come in already
# typed and placed.
#
# References
#   Gund, P. (1977) "Three-dimensional pharmacophoric pattern
#     searching." Progress in Molecular and Subcellular Biology 5,
#     117-143.
#   Mason, J.S., Good, A.C. and Martin, E.J. (2001) "3-D pharmacophores
#     in drug discovery." Current Pharmaceutical Design 7(7), 567-597.
#   Mason, J.S., Morize, I., Menard, P.R., Cheney, D.L., Hulme, C. and
#     Labaudiniere, R.F. (1999) "New 4-point pharmacophore method for
#     molecular similarity and diversity applications." Journal of
#     Medicinal Chemistry 42(17), 3251-3264.
#   Tanimoto, T.T. (1958) "An elementary mathematical theory of
#     classification and prediction." IBM internal report.

# The six feature classes of the classical pharmacophore alphabet.
.PHACF3_FEATURES <- c("donor", "acceptor", "positive", "negative",
                      "hydrophobic", "aromatic")

# Bin EDGES in angstroms, so there are length(edges) - 1 bins and
# anything outside the outer edges is not a pharmacophore distance at
# all. These are a default, not a standard: bin boundaries are a
# modelling choice and different published schemes use different ones.
.PHACF3_DEFAULT_EDGES <- c(2.0, 4.5, 7.0, 10.0, 14.0, 20.0, 24.0)

.PHACF3_MODES <- c("binary", "count")

#' Which distance bin, or -1 for a distance outside the range
#'
#' Half-open on the left, so a distance sitting exactly on a boundary
#' goes to the upper bin and no distance can land in two bins because of
#' a rounding accident.
#'
#' @param d The distance.
#' @param edges The bin edges.
#' @return The zero-based bin index, or -1.
#' @export
morie_phacf3_bin <- function(d, edges) {
  d <- as.numeric(d)
  if (d < edges[1] || d >= edges[length(edges)]) return(-1L)
  for (k in seq_len(length(edges) - 1L))
    if (d < edges[k + 1L]) return(k - 1L)
  as.integer(length(edges) - 2L)
}

# Lexicographic comparison of two length-six integer vectors: TRUE when
# a is strictly smaller. Written out rather than leaning on a string
# encoding, so the ordering is the ordering of the numbers and not of
# their spellings.
#' Lexicographic comparison of two length-six integer vectors: TRUE when
#'
#' a is strictly smaller. Written out rather than leaning on a string
#' encoding, so the ordering is the ordering of the numbers and not of
#' their spellings.
#'
#' @param a A vector; indexed elementwise.
#' @param b A vector; indexed elementwise.
#' @return A logical value.
#' @export
#' @examples
#' A <- matrix(c(4, 1, 0.5, 1, 3, 0.8, 0.5, 0.8, 2), nrow = 3)
#' b <- c(1.5, 2.5, 3.5)
#' res <- .phacf3_less(a = A, b = b)
#' res
.phacf3_less <- function(a, b) {
  for (i in seq_len(6L)) {
    if (a[i] < b[i]) return(TRUE)
    if (a[i] > b[i]) return(FALSE)
  }
  FALSE
}

#' The canonical (types, bins) form of one feature triangle
#'
#' Each vertex carries a type and each EDGE a bin. Under a permutation
#' of the vertices the edges permute with them, so all six orderings are
#' generated and the smallest tuple wins. Generating all six is the
#' point: a comparison rule that sorted the types first and hoped the
#' edges followed is wrong whenever two types are equal, which on a
#' six-letter alphabet is most of the time.
#'
#' @param t1 First vertex type, zero-based.
#' @param t2 Second vertex type.
#' @param t3 Third vertex type.
#' @param d12 Bin of the edge between vertices one and two.
#' @param d13 Bin of the edge between vertices one and three.
#' @param d23 Bin of the edge between vertices two and three.
#' @return A length-six integer vector, the canonical key.
#' @export
morie_phacf3_canonical <- function(t1, t2, t3, d12, d13, d23) {
  # (i, j, k) vertex order, one-based here; edges are (ij, ik, jk).
  perms <- list(c(1, 2, 3), c(1, 3, 2), c(2, 1, 3), c(2, 3, 1),
                c(3, 1, 2), c(3, 2, 1))
  tt <- c(t1, t2, t3)
  dd <- matrix(0L, 3L, 3L)
  dd[1, 2] <- d12
  dd[2, 1] <- d12
  dd[1, 3] <- d13
  dd[3, 1] <- d13
  dd[2, 3] <- d23
  dd[3, 2] <- d23
  best <- NULL
  for (p in perms) {
    i <- p[1]
    j <- p[2]
    k <- p[3]
    cand <- c(tt[i], tt[j], tt[k], dd[i, j], dd[i, k], dd[j, k])
    if (is.null(best) || .phacf3_less(cand, best)) best <- cand
  }
  as.integer(best)
}

# A key's string form, zero padded so that byte order is numeric order.
# Only used to index the lookup table; the ORDERING of the bit space is
# done on the numbers themselves.
#' A key\'s string form, zero padded so that byte order is numeric order
#'
#' Only used to index the lookup table; the ORDERING of the bit space is
#' done on the numbers themselves.
#'
#' @param k A vector; indexed elementwise.
#' @return A character value.
#' @export
#' @examples
#' res <- .phacf3_str(k = 3L)
#' res
.phacf3_str <- function(k)
  sprintf("%02d,%02d,%02d,%02d,%02d,%02d", k[1], k[2], k[3], k[4], k[5],
          k[6])

#' Every canonical triangle the alphabet allows, in a fixed order
#'
#' Built by enumerating all orderings and canonicalising, so it is
#' exactly the set of keys the canonical function can produce and
#' nothing else -- a space derived by a formula could be off by the
#' number of degenerate triangles and nobody would notice until two
#' molecules disagreed.
#'
#' @param features The feature alphabet.
#' @param n_bins The number of distance bins, or NULL to take it from
#'   the edges.
#' @param edges The bin edges.
#' @return A list with the key matrix, their string forms and a lookup
#'   environment from string to zero-based bit index.
#' @export
morie_phacf3_space <- function(features = .PHACF3_FEATURES,
                               n_bins = NULL,
                               edges = .PHACF3_DEFAULT_EDGES) {
  nb <- if (is.null(n_bins)) length(edges) - 1L else as.integer(n_bins)
  if (nb < 1L) stop("need at least one distance bin")
  nf <- length(features)
  seen <- character(0)
  keep <- list()
  for (a in 0:(nf - 1L)) for (b in 0:(nf - 1L)) for (cc in 0:(nf - 1L))
    for (p in 0:(nb - 1L)) for (q in 0:(nb - 1L)) for (r in 0:(nb - 1L)) {
      k <- morie_phacf3_canonical(a, b, cc, p, q, r)
      s <- .phacf3_str(k)
      if (!(s %in% seen)) {
        seen <- c(seen, s)
        keep[[length(keep) + 1L]] <- k
      }
    }
  ord <- order(seen, method = "radix")
  seen <- seen[ord]
  keep <- keep[ord]
  idx <- new.env(hash = TRUE, parent = emptyenv())
  for (i in seq_along(seen)) assign(seen[i], i - 1L, envir = idx)
  list(keys = keep, strings = seen, index = idx)
}

#' .phacf3_dist
#'
#' A step of the phacf3_native implementation. Called by \code{morie_phacf3}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a Numeric; combined arithmetically in the body.
#' @param b Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
#' @examples
#' A <- matrix(c(4, 1, 0.5, 1, 3, 0.8, 0.5, 0.8, 2), nrow = 3)
#' b <- c(1.5, 2.5, 3.5)
#' res <- .phacf3_dist(a = A, b = b)
#' res
.phacf3_dist <- function(a, b) sqrt(.w3_csum((a - b) * (a - b)))

#' Tanimoto coefficient of two equal-length fingerprints
#'
#' On counts rather than bits it is the weighted form: shared over
#' union, with the minimum as the intersection. Two empty fingerprints
#' have no features in common and none apart, which is a zero over zero;
#' it is reported as NaN rather than as the 1 that would call two blank
#' molecules identical.
#'
#' @param a The first fingerprint.
#' @param b The second fingerprint.
#' @return The Tanimoto coefficient.
#' @export
morie_phacf3_tanimoto <- function(a, b) {
  if (length(a) != length(b))
    stop("fingerprints must be the same length")
  inter <- numeric(length(a))
  union <- numeric(length(a))
  for (i in seq_along(a)) {
    x <- as.numeric(a[i])
    y <- as.numeric(b[i])
    inter[i] <- if (x < y) x else y
    union[i] <- if (x > y) x else y
  }
  num <- .w3_csum(inter)
  den <- .w3_csum(union)
  if (den > 0) num / den else NaN
}

#' Fingerprint a set of typed 3D feature points
#'
#' @param mol_3d A list of rows: x, y, z, type. The type must be a
#'   member of the alphabet.
#' @param feature_set The alphabet. Its ORDER fixes the bit space, so
#'   two fingerprints are only comparable if they were built on the same
#'   alphabet in the same order.
#' @param edges Bin edges in angstroms.
#' @param mode "binary" sets a bit once however many triangles hit it;
#'   "count" counts them, which keeps the information that a molecule
#'   presented the same pharmacophore in six different places.
#' @param space A prebuilt space, since building it is the expensive
#'   part and it does not depend on the molecule.
#' @return A list with the fingerprint, the bits set, the triangles that
#'   produced them, and the counts of triangles rejected for falling
#'   outside the distance range or for failing the triangle inequality.
#' @export
morie_phacf3 <- function(mol_3d, feature_set = .PHACF3_FEATURES,
                         edges = .PHACF3_DEFAULT_EDGES, mode = "binary",
                         space = NULL) {
  if (!(mode %in% .PHACF3_MODES))
    stop("mode must be one of ", paste(.PHACF3_MODES, collapse = ", "))
  feats <- as.character(feature_set)
  pts <- lapply(mol_3d, function(row) {
    t <- as.character(row[[4]])
    if (!(t %in% feats))
      stop("feature type ", t, " is not in the alphabet")
    list(xyz = c(as.numeric(row[[1]]), as.numeric(row[[2]]),
                 as.numeric(row[[3]])),
         type = which(feats == t)[1] - 1L)
  })
  n <- length(pts)
  if (is.null(space))
    space <- morie_phacf3_space(feats, length(edges) - 1L, edges)
  fp <- integer(length(space$strings))
  hi <- integer(0)
  hj <- integer(0)
  hk <- integer(0)
  hb <- integer(0)
  out_of_range <- 0L
  degenerate <- 0L
  if (n >= 3L) for (i in 1:(n - 2L)) for (j in (i + 1L):(n - 1L))
    for (k in (j + 1L):n) {
      d12 <- .phacf3_dist(pts[[i]]$xyz, pts[[j]]$xyz)
      d13 <- .phacf3_dist(pts[[i]]$xyz, pts[[k]]$xyz)
      d23 <- .phacf3_dist(pts[[j]]$xyz, pts[[k]]$xyz)
      # Three points in space always satisfy the triangle inequality, so
      # a violation means the distances did not come from one geometry
      # -- which happens as soon as somebody feeds in a distance matrix
      # instead of coordinates. Counting it is cheaper than debugging
      # the fingerprint later.
      if (d12 + d13 < d23 || d12 + d23 < d13 || d13 + d23 < d12) {
        degenerate <- degenerate + 1L
        next
      }
      b12 <- morie_phacf3_bin(d12, edges)
      b13 <- morie_phacf3_bin(d13, edges)
      b23 <- morie_phacf3_bin(d23, edges)
      if (b12 < 0L || b13 < 0L || b23 < 0L) {
        out_of_range <- out_of_range + 1L
        next
      }
      key <- morie_phacf3_canonical(pts[[i]]$type, pts[[j]]$type,
                                    pts[[k]]$type, b12, b13, b23)
      bit <- get(.phacf3_str(key), envir = space$index)
      if (mode == "binary") fp[bit + 1L] <- 1L
      else fp[bit + 1L] <- fp[bit + 1L] + 1L
      hi <- c(hi, i - 1L)
      hj <- c(hj, j - 1L)
      hk <- c(hk, k - 1L)
      hb <- c(hb, bit)
    }
  on <- which(fp > 0L) - 1L
  list(fingerprint = fp, bits_on = on, n_bits_on = length(on),
       n_bits = length(fp),
       density = if (length(fp)) length(on) / length(fp) else NaN,
       total = sum(fp), hit_i = hi, hit_j = hj, hit_k = hk, hit_bit = hb,
       n_triangles = length(hb), n_out_of_range = out_of_range,
       n_degenerate = degenerate, n_features = n,
       estimate = length(on), se = NaN, mode = mode,
       method = "three-point 3D pharmacophore fingerprint")
}

#' One-line summary of the phacf3 module
#'
#' @return A character scalar.
#' @export
morie_phacf3_cheatsheet <- function()
  paste0("phacf3: three-point 3D pharmacophore fingerprint. modes ",
         paste(.PHACF3_MODES, collapse = ", "), "; features ",
         paste(.PHACF3_FEATURES, collapse = ", "))
