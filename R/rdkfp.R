# SPDX-License-Identifier: AGPL-3.0-or-later
#
# RDKit path/subgraph-based topological fingerprint (Rdkfp).
# Bit-identical mirror of src/morie/fn/rdkfp.py; uses .ecfp_mix and
# .ecfp_bonds from ecfp4.R.  Specification: RDKit
# Code/GraphMol/Fingerprints/RDKitFPGenerator.cpp lines 44-54 (atom
# invariant) and 196-249 (enumeration, feature assembly), and
# Code/GraphMol/Fingerprints/FingerprintUtil.cpp lines 357-444
# (generateBondHashes); master revision fetched 2026-08-09, stored at
# library/pdf/fetched-wave3/rdkit-reference-source/.
# Anchored against RDKit: subgraph counts and distinct-feature counts
# reproduce RDKit exactly -- benzene 31 / 6, ethanol 3 / 3, isobutane
# 7 / 3, cyclopropane 7 / 3, aspirin 301 / 201.

#' .rdkfp_subgraphs
#'
#' Part of the rdkfp implementation; see the file header for the source
#' it follows.
#'
#' @param B See Usage.
#' @param minpath See Usage.
#' @param maxpath See Usage.
#' @param branched See Usage.
#' @return The value of \code{out}, as built in the body.
#' @export
.rdkfp_subgraphs <- function(B, minpath, maxpath, branched) {
  a <- B$a; nb <- length(B$i)
  touch <- vector("list", a)
  for (i in seq_len(a)) touch[[i]] <- integer(0)
  if (nb) for (k in seq_len(nb)) {
    touch[[B$i[k]]] <- c(touch[[B$i[k]]], k)
    touch[[B$j[k]]] <- c(touch[[B$j[k]]], k)
  }
  key <- function(v) paste0(sprintf("%04d", sort(as.integer(v), method = "radix")), collapse = "")
  out <- list()
  if (branched) {
    cur <- as.list(seq_len(nb))
    size <- 1L
    while (size <= maxpath && length(cur)) {
      if (size >= minpath) {
        ks <- vapply(cur, key, character(1))
        ord <- order(ks, method = "radix")
        for (t in ord) out[[length(out) + 1L]] <- sort(as.integer(cur[[t]]))
      }
      if (size == maxpath) break
      seen <- character(0); nxt <- list()
      for (s in cur) {
        at <- unique(c(B$i[s], B$j[s]))
        for (v in at) for (bi in touch[[v]]) if (!(bi %in% s)) {
          cand <- sort(as.integer(c(s, bi)))
          kk <- key(cand)
          if (!(kk %in% seen)) { seen <- c(seen, kk); nxt[[length(nxt) + 1L]] <- cand }
        }
      }
      cur <- nxt; size <- size + 1L
    }
  } else {
    acc <- list()
    walk <- function(path, ends) {
      if (length(path) >= minpath) acc[[length(acc) + 1L]] <<- sort(as.integer(path))
      if (length(path) == maxpath) return(invisible(NULL))
      for (k in 1:2) {
        at <- ends[k]
        for (bi in touch[[at]]) {
          if (bi %in% path) next
          other <- if (B$i[bi] == at) B$j[bi] else B$i[bi]
          used <- unique(c(B$i[path], B$j[path]))
          if (other %in% used) next
          ne <- ends; ne[k] <- other
          walk(c(path, bi), ne)
        }
      }
      invisible(NULL)
    }
    if (nb) for (bi in seq_len(nb)) walk(bi, c(B$i[bi], B$j[bi]))
    if (length(acc)) {
      ks <- vapply(acc, key, character(1))
      keep <- !duplicated(ks)
      acc <- acc[keep]; ks <- ks[keep]
      ord <- order(vapply(acc, length, integer(1)), ks, method = "radix")
      out <- acc[ord]
    }
  }
  out
}

#' RDKit path-based (subgraph) topological fingerprint
#'
#' Every connected subgraph of between \code{minpath} and \code{maxpath}
#' bonds is enumerated and reduced to one integer feature, which is folded
#' into the bit vector.  The reduction follows RDKit step for step: the
#' atom invariant is twice the atomic number modulo 128 plus the aromatic
#' flag; each bond of the subgraph is hashed together with the number of
#' other subgraph bonds it touches, the bond order, and the two end-atom
#' invariants and in-subgraph degrees ordered so that the larger invariant
#' comes first; the bond hashes are sorted, the number of distinct atoms
#' covered is appended -- this is what separates cyclopropane from
#' isobutane -- and the sequence is hashed into the feature.  A one-bond
#' subgraph uses its single bond hash directly.
#'
#' Two departures are deliberate and stated.  The hash is the closed form
#' \eqn{h \leftarrow (1000003 h + v) \bmod (2^{31}-1)} rather than the
#' Boost hash, so that the Python and both R arms agree exactly; bit
#' indices are therefore this implementation's own while the feature
#' partition is RDKit's.  And one bit is set per feature, where RDKit's
#' default of two draws the second from a Boost random generator that
#' cannot be reproduced outside Boost.
#'
#' The molecule arrives as a pre-parsed graph; SMILES parsing and
#' aromaticity perception are out of scope.
#'
#' @param adjacency Symmetric bond-order matrix; 0 no bond, 1 single,
#'   2 double, 3 triple, 4 aromatic.
#' @param atomnum Atomic number per atom.
#' @param aromatic Per-atom aromaticity flag; default 0.
#' @param nbits Width of the folded fingerprint.
#' @param minpath Smallest subgraph size in bonds.
#' @param maxpath Largest subgraph size in bonds.
#' @param branched Enumerate all connected subgraphs rather than linear
#'   paths only.
#' @param use_bond_order Include bond order in the bond hash.
#' @return List with \code{bits}, \code{count}, \code{nset},
#'   \code{features}, \code{nfeature}, \code{nsubgraph}, \code{a},
#'   \code{nbits}, \code{minpath}, \code{maxpath}, \code{method}.
#' @references RDKit: Open-Source Cheminformatics, https://www.rdkit.org.
#'   The fingerprint has no journal paper; the reference implementation is
#'   the specification.  Files followed:
#'   Code/GraphMol/Fingerprints/RDKitFPGenerator.cpp lines 44-54 and
#'   196-249, and Code/GraphMol/Fingerprints/FingerprintUtil.cpp lines
#'   357-444, master revision fetched 2026-08-09.
#' @export
Rdkfp <- function(adjacency, atomnum, aromatic = NULL, nbits = 2048,
                  minpath = 1, maxpath = 7, branched = TRUE,
                  use_bond_order = TRUE) {
  B <- .ecfp_bonds(adjacency)
  a <- B$a
  at <- as.numeric(.t1_vec(atomnum))
  if (length(at) != a) stop("atomnum must have one entry per atom", call. = FALSE)
  ar <- if (is.null(aromatic)) rep(0, a) else {
    v <- as.numeric(.t1_vec(aromatic))
    if (length(v) != a) stop("aromatic must have one entry per atom", call. = FALSE)
    as.numeric(v != 0)
  }
  ainv <- (at %% 128) * 2 + ar
  minpath <- as.integer(minpath); maxpath <- as.integer(maxpath)
  if (minpath < 1L) stop("minpath must be at least 1", call. = FALSE)
  if (maxpath < minpath) stop("maxpath must be at least minpath", call. = FALSE)
  nbits <- as.integer(nbits)
  if (is.na(nbits) || nbits < 1L) stop("nbits must be positive", call. = FALSE)

  subs <- .rdkfp_subgraphs(B, minpath, maxpath, isTRUE(branched))
  bits <- integer(nbits); cnt <- integer(nbits); feats <- numeric(0)
  for (sub in subs) {
    ats <- unique(c(B$i[sub], B$j[sub]))
    deg <- integer(a)
    for (bi in sub) { deg[B$i[bi]] <- deg[B$i[bi]] + 1L; deg[B$j[bi]] <- deg[B$j[bi]] + 1L }
    bh <- numeric(length(sub))
    for (k in seq_along(sub)) {
      bi <- sub[k]; ii <- B$i[bi]; jj <- B$j[bi]
      nbr <- 0L
      for (m in seq_along(sub)) {
        if (m == k) next
        bj <- sub[m]
        if (B$i[bj] == ii || B$i[bj] == jj || B$j[bj] == ii || B$j[bj] == jj) nbr <- nbr + 1L
      }
      a1 <- ainv[ii]; a2 <- ainv[jj]; d1 <- deg[ii]; d2 <- deg[jj]
      if (a1 < a2) { tmp <- a1; a1 <- a2; a2 <- tmp; tmp <- d1; d1 <- d2; d2 <- tmp }
      else if (a1 == a2 && d1 < d2) { tmp <- d1; d1 <- d2; d2 <- tmp }
      bo <- if (isTRUE(use_bond_order)) B$o[bi] else 1
      h <- .ecfp_mix(0, nbr)
      for (v in c(bo, a1, d1, a2, d2)) h <- .ecfp_mix(h, v)
      bh[k] <- h
    }
    if (length(sub) > 1L) {
      bh <- c(sort(bh, method = "radix"), length(ats))
      seed <- 0
      for (v in bh) seed <- .ecfp_mix(seed, v)
    } else seed <- bh[1]
    feats <- c(feats, seed)
    b <- seed %% nbits
    bits[b + 1L] <- 1L; cnt[b + 1L] <- cnt[b + 1L] + 1L
  }
  uniq <- sort(unique(feats))
  .t1_result(bits = bits, count = cnt, nset = sum(bits), features = uniq,
             nfeature = length(uniq), nsubgraph = length(subs), a = a,
             nbits = nbits, minpath = minpath, maxpath = maxpath,
             method = "RDKit path-based topological fingerprint")
}
