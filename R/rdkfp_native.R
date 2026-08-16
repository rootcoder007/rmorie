# RDKit path/subgraph topological fingerprint.
# Source: the RDKit reference implementation of RDKFingerprint
# (Landrum, G., RDKit: Open-Source Cheminformatics, and the "RDKit
# Book", section on topological fingerprints), which is a Daylight-
# style path fingerprint: enumerate every subgraph of the molecule
# between minPath and maxPath bonds, hash each into an integer from
# its bonds and their end-atom invariants, and fold the hashes into a
# bit vector.  Daylight, Inc. (2019), Daylight Theory Manual, Ch. 6
# (fingerprints as hashed path sets) is the original of the scheme.
#
# Native implementation mirroring Python morie.fn.rdkfp exactly: the
# same two enumeration routes (branched subgraphs and linear paths),
# the same canonical end-atom ordering inside each bond hash, and the
# same identifier mixer as the ECFP family.

# canonical fixed-width key of a sorted bond-index tuple: usable for
# ordering because every tuple compared shares a length
#' Canonical fixed-width key of a sorted bond-index tuple: usable for
#'
#' ordering because every tuple compared shares a length
#'
#' @param v See Usage.
#' @return A character value.
#' @export
.mor_rdk_key <- function(v) paste(sprintf("%06d", v), collapse = "")

#' .mor_rdk_subgraphs
#'
#' A step of the rdkfp_native implementation. Called by \code{morie_rdkfp}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a A count; the body uses it as \code{seq_len(...)}.
#' @param bd A list; the body reads \code{$i}, \code{$j} from it.
#' @param minpath See Usage.
#' @param maxpath A count; the body uses it as \code{seq_len(...)}.
#' @param branched A flag; the body branches on it.
#' @return The value of \code{out}, as built in the body.
#' @export
.mor_rdk_subgraphs <- function(a, bd, minpath, maxpath, branched) {
  nb <- length(bd$i)
  touch <- vector("list", a)
  for (k in seq_len(a)) touch[[k]] <- integer(0)
  for (k in seq_len(nb)) {
    ii <- bd$i[k] + 1L; jj <- bd$j[k] + 1L
    touch[[ii]] <- c(touch[[ii]], k)
    touch[[jj]] <- c(touch[[jj]], k)
  }
  out <- list()
  if (branched) {
    cur <- lapply(seq_len(nb), function(k) k)
    for (size in seq_len(maxpath)) {
      if (size >= minpath && length(cur) > 0L) {
        ord <- order(vapply(cur, .mor_rdk_key, character(1)),
                     method = "radix")
        out <- c(out, cur[ord])
      }
      if (size == maxpath) break
      nxt <- list(); seen <- new.env(hash = TRUE, parent = emptyenv())
      for (s in cur) {
        atoms <- unique(c(bd$i[s], bd$j[s])) + 1L
        for (at in atoms) for (bi in touch[[at]]) {
          if (bi %in% s) next
          cand <- sort(c(s, bi))
          kk <- paste0("k", .mor_rdk_key(cand))
          if (is.null(seen[[kk]])) {
            assign(kk, TRUE, envir = seen)
            nxt[[length(nxt) + 1L]] <- cand
          }
        }
      }
      cur <- nxt
      if (length(cur) == 0L) break
    }
  } else {
    # linear bond paths grown at either end, no atom revisited
    acc <- list()
    walk <- function(path, ends) {
      if (length(path) >= minpath) acc[[length(acc) + 1L]] <<- sort(path)
      if (length(path) == maxpath) return(invisible(NULL))
      used <- unique(c(bd$i[path], bd$j[path])) + 1L
      for (k in 1:2) {
        at <- ends[k]
        for (bi in touch[[at]]) {
          if (bi %in% path) next
          ii <- bd$i[bi] + 1L; jj <- bd$j[bi] + 1L
          other <- if (ii == at) jj else ii
          if (other %in% used) next
          ne <- ends; ne[k] <- other
          walk(c(path, bi), ne)
        }
      }
      invisible(NULL)
    }
    for (bi in seq_len(nb)) walk(bi, c(bd$i[bi] + 1L, bd$j[bi] + 1L))
    if (length(acc) > 0L) {
      keys <- vapply(acc, .mor_rdk_key, character(1))
      keep <- !duplicated(keys)
      acc <- acc[keep]; keys <- keys[keep]
      # final order is (length, tuple), so the walk order does not
      # survive and need not be reproduced
      out <- acc[order(vapply(acc, length, integer(1)), keys,
                       method = "radix")]
    }
  }
  out
}

#' RDKit path-based topological fingerprint
#'
#' Enumerates molecular subgraphs of between \code{minpath} and
#' \code{maxpath} bonds, hashes each subgraph from its bonds and the
#' invariants and degrees of their end atoms, and folds the hashes
#' into a bit vector -- the Daylight-style path fingerprint that
#' RDKit's \code{RDKFingerprint} computes.
#'
#' @param adjacency Square symmetric matrix of bond orders.
#' @param atomnum Atomic number per atom.
#' @param aromatic Optional 0/1 aromaticity flag per atom.
#' @param nbits Folded fingerprint length, default 2048.
#' @param minpath Smallest subgraph size in bonds, default 1.
#' @param maxpath Largest subgraph size in bonds, default 7.
#' @param branched \code{TRUE} (default) enumerates all connected
#'   subgraphs; \code{FALSE} enumerates only linear paths.  Both
#'   routes the reference implementation offers are kept, branched
#'   being RDKit's default.
#' @param use_bond_order Hash bond orders (default) or treat every
#'   bond as single.
#' @return A list with \code{bits}, \code{count}, \code{nset},
#'   \code{features} (sorted unique subgraph hashes),
#'   \code{nfeature}, \code{nsubgraph}, \code{a}, \code{nbits},
#'   \code{minpath}, \code{maxpath}, \code{method}.
#' @references Daylight Chemical Information Systems (2019). Daylight
#'   Theory Manual, Chapter 6: Fingerprints.
#' @export
morie_rdkfp <- function(adjacency, atomnum, aromatic = NULL,
                        nbits = 2048L, minpath = 1L, maxpath = 7L,
                        branched = TRUE, use_bond_order = TRUE) {
  bd <- .mor_fp_bonds(adjacency)
  a <- bd$a
  at <- trunc(as.numeric(atomnum))
  if (length(at) != a) stop("atomnum must have one entry per atom")
  if (is.null(aromatic)) {
    ar <- rep(0, a)
  } else {
    ar <- as.numeric(as.numeric(aromatic) != 0)
    if (length(ar) != a) stop("aromatic must have one entry per atom")
  }
  ainv <- (at %% 128) * 2 + ar
  minpath <- as.integer(minpath); maxpath <- as.integer(maxpath)
  if (minpath < 1L) stop("minpath must be at least 1")
  if (maxpath < minpath) stop("maxpath must be at least minpath")
  nbits <- as.integer(nbits)
  if (nbits < 1L) stop("nbits must be positive")

  subs <- .mor_rdk_subgraphs(a, bd, minpath, maxpath, isTRUE(branched))
  bits <- integer(nbits); cnt <- integer(nbits)
  feats <- numeric(length(subs))
  for (si in seq_along(subs)) {
    sub <- subs[[si]]
    ei <- bd$i[sub] + 1L; ej <- bd$j[sub] + 1L
    atoms <- unique(c(ei, ej))
    deg <- integer(a)
    for (k in seq_along(sub)) {
      deg[ei[k]] <- deg[ei[k]] + 1L
      deg[ej[k]] <- deg[ej[k]] + 1L
    }
    bh <- numeric(length(sub))
    for (k in seq_along(sub)) {
      i <- ei[k]; j <- ej[k]
      nbr <- 0
      for (m in seq_along(sub)) {
        if (m == k) next
        p <- ei[m]; q <- ej[m]
        if (p == i || p == j || q == i || q == j) nbr <- nbr + 1
      }
      a1 <- ainv[i]; a2 <- ainv[j]
      d1 <- deg[i]; d2 <- deg[j]
      if (a1 < a2) {
        tmp <- a1; a1 <- a2; a2 <- tmp
        tmp <- d1; d1 <- d2; d2 <- tmp
      } else if (a1 == a2 && d1 < d2) {
        tmp <- d1; d1 <- d2; d2 <- tmp
      }
      bo <- if (isTRUE(use_bond_order)) bd$o[sub[k]] else 1
      h <- .mor_fp_mix(0, nbr)
      for (v in c(bo, a1, d1, a2, d2)) h <- .mor_fp_mix(h, v)
      bh[k] <- h
    }
    if (length(sub) > 1L) {
      bh <- c(sort(bh), length(atoms))
      seed <- 0
      for (v in bh) seed <- .mor_fp_mix(seed, v)
    } else {
      seed <- bh[1]
    }
    feats[si] <- seed
    b <- seed %% nbits
    bits[b + 1L] <- 1L
    cnt[b + 1L] <- cnt[b + 1L] + 1L
  }
  uniq <- sort(unique(feats))
  list(bits = bits, count = cnt, nset = sum(bits),
       features = uniq, nfeature = length(uniq), nsubgraph = length(subs),
       a = a, nbits = nbits, minpath = minpath, maxpath = maxpath,
       method = "RDKit path-based topological fingerprint")
}
