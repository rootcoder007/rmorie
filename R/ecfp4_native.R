# Extended-connectivity fingerprints (ECFP / Morgan fingerprints).
# Source: Rogers, D. and Hahn, M. (2010), Extended-connectivity
# fingerprints, Journal of Chemical Information and Modeling 50(5),
# 742-754: the initial atom identifiers of their Table 1 / Sec.
# "Initial assignment", the iterative update of their Sec. "Iteration"
# (one identifier per atom per layer, neighbours entered as
# (bond order, neighbour identifier) pairs sorted before hashing), and
# the duplicate-structure removal of their Sec. "Duplicate identifier
# removal", where two features covering the SAME bond set collapse to
# one.  ECFP4 is radius 2 (two iterations), ECFP6 radius 3.
#
# Native implementation mirroring Python morie.fn.ecfp4 exactly.  The
# identifier mixer is deliberately arithmetic modulo 2^31 - 1 rather
# than a 32-bit unsigned hash: every intermediate product stays below
# 2^53 and so is exact in IEEE doubles, which lets this R arm
# reproduce the Python integers bit for bit.

.MOR_FP_MOD <- 2147483647
.MOR_FP_MUL <- 1000003

#' .mor_fp_mix
#'
#' Part of the ecfp4_native implementation; see the file header for the
#' source it follows.
#'
#' @param h See Usage.
#' @param v See Usage.
#' @return A numeric value.
#' @export
.mor_fp_mix <- function(h, v) (h * .MOR_FP_MUL + (v %% .MOR_FP_MOD)) %% .MOR_FP_MOD

# bond list: 0-based (i, j, order) triples in the Python enumeration
# order (i ascending, then j > i ascending)
#' Bond list: 0-based (i, j, order) triples in the Python enumeration
#'
#' order (i ascending, then j > i ascending)
#'
#' @param adjacency See Usage.
#' @return A list with \code{a}, \code{i}, \code{j}, \code{o}.
#' @export
.mor_fp_bonds <- function(adjacency) {
  A <- as.matrix(adjacency)
  a <- nrow(A)
  if (ncol(A) != a) stop("adjacency must be square")
  bi <- integer(0); bj <- integer(0); bo <- numeric(0)
  if (a > 1L) for (i in seq_len(a - 1L)) for (j in seq.int(i + 1L, a)) {
    if (A[i, j] != A[j, i]) stop("adjacency must be symmetric")
    if (A[i, j] != 0) {
      bi <- c(bi, i - 1L); bj <- c(bj, j - 1L); bo <- c(bo, trunc(A[i, j]))
    }
  }
  list(a = a, i = bi, j = bj, o = bo)
}

#' .mor_fp_invariants
#'
#' Part of the ecfp4_native implementation; see the file header for the
#' source it follows.
#'
#' @param a See Usage.
#' @param bd See Usage.
#' @param atomnum See Usage.
#' @param numhs See Usage.
#' @param charge See Usage.
#' @param inring See Usage.
#' @param isotope_delta See Usage.
#' @return The value of \code{inv}, as built in the body.
#' @export
.mor_fp_invariants <- function(a, bd, atomnum, numhs, charge, inring,
                               isotope_delta) {
  deg <- integer(a)
  for (k in seq_along(bd$i)) {
    deg[bd$i[k] + 1L] <- deg[bd$i[k] + 1L] + 1L
    deg[bd$j[k] + 1L] <- deg[bd$j[k] + 1L] + 1L
  }
  inv <- numeric(a)
  for (i in seq_len(a)) {
    comps <- c(atomnum[i], deg[i] + numhs[i], numhs[i], charge[i],
               isotope_delta[i])
    if (inring[i] != 0) comps <- c(comps, 1)
    h <- 0
    for (cc in comps) h <- .mor_fp_mix(h, cc)
    inv[i] <- h
  }
  inv
}

# canonical key of a bond set: sorted 0-based indices, four digits each
#' Canonical key of a bond set: sorted 0-based indices, four digits each
#'
#' Part of the ecfp4_native implementation; see the file header for the
#' source it follows.
#'
#' @param bs See Usage.
#' @return A character value.
#' @export
.mor_fp_envkey <- function(bs) {
  if (length(bs) == 0L) return("")
  paste(sprintf("%04d", sort(bs)), collapse = "")
}

#' .mor_fp_morgan
#'
#' Part of the ecfp4_native implementation; see the file header for the
#' source it follows.
#'
#' @param a See Usage.
#' @param bd See Usage.
#' @param invariants See Usage.
#' @param radius See Usage.
#' @param nbits See Usage.
#' @param use_bond_order Defaults to \code{TRUE}.
#' @return A list with \code{bits}, \code{count}, \code{ident}.
#' @export
.mor_fp_morgan <- function(a, bd, invariants, radius, nbits,
                           use_bond_order = TRUE) {
  inc_b <- vector("list", a); inc_o <- vector("list", a)
  inc_n <- vector("list", a)
  for (k in seq_len(a)) {
    inc_b[[k]] <- integer(0); inc_o[[k]] <- numeric(0)
    inc_n[[k]] <- integer(0)
  }
  for (k in seq_along(bd$i)) {
    ii <- bd$i[k] + 1L; jj <- bd$j[k] + 1L
    oo <- if (use_bond_order) bd$o[k] else 1
    inc_b[[ii]] <- c(inc_b[[ii]], k - 1L)
    inc_n[[ii]] <- c(inc_n[[ii]], jj)
    inc_o[[ii]] <- c(inc_o[[ii]], oo)
    inc_b[[jj]] <- c(inc_b[[jj]], k - 1L)
    inc_n[[jj]] <- c(inc_n[[jj]], ii)
    inc_o[[jj]] <- c(inc_o[[jj]], oo)
  }
  cur <- as.numeric(invariants)
  ident <- as.numeric(invariants)
  seen <- new.env(hash = TRUE, parent = emptyenv())
  atom_env <- vector("list", a)
  for (k in seq_len(a)) atom_env[[k]] <- integer(0)
  dead <- rep(FALSE, a)

  for (layer in seq_len(as.integer(radius)) - 1L) {
    nxt <- numeric(a)
    round_env <- atom_env
    keys <- character(0); vals <- numeric(0); idxs <- integer(0)
    for (i in seq_len(a)) {
      if (dead[i]) next
      no <- numeric(0); nv <- numeric(0)
      for (m in seq_along(inc_b[[i]])) {
        round_env[[i]] <- union(round_env[[i]], inc_b[[i]][m])
        round_env[[i]] <- union(round_env[[i]], atom_env[[inc_n[[i]][m]]])
        no <- c(no, inc_o[[i]][m])
        nv <- c(nv, cur[inc_n[[i]][m]])
      }
      if (length(no) > 0L) {
        oidx <- order(no, nv)
        no <- no[oidx]; nv <- nv[oidx]
      }
      invar <- .mor_fp_mix(0, layer)
      invar <- .mor_fp_mix(invar, cur[i])
      for (m in seq_along(no))
        invar <- .mor_fp_mix(.mor_fp_mix(invar, no[m]), nv[m])
      nxt[i] <- invar
      keys <- c(keys, .mor_fp_envkey(round_env[[i]]))
      vals <- c(vals, invar)
      idxs <- c(idxs, i)
    }
    if (length(keys) > 0L) {
      ord <- order(keys, vals, idxs, method = "radix")
      for (p in ord) {
        # "k" prefix only so that an isolated atom's EMPTY bond set is
        # still a legal environment name; a constant prefix cannot
        # change the ordering computed above
        kk <- paste0("k", keys[p])
        if (is.null(seen[[kk]])) {
          assign(kk, TRUE, envir = seen)
          ident <- c(ident, vals[p])
        } else {
          dead[idxs[p]] <- TRUE
        }
      }
    }
    for (i in seq_len(a)) if (!dead[i]) cur[i] <- nxt[i]
    atom_env <- round_env
  }

  nbits <- as.integer(nbits)
  if (nbits < 1L) stop("nbits must be positive")
  bits <- integer(nbits); cnt <- integer(nbits)
  for (v in ident) {
    b <- v %% nbits
    bits[b + 1L] <- 1L
    cnt[b + 1L] <- cnt[b + 1L] + 1L
  }
  list(bits = bits, count = cnt, ident = ident)
}

#' .mor_fp_defaults
#'
#' Part of the ecfp4_native implementation; see the file header for the
#' source it follows.
#'
#' @param a See Usage.
#' @param numhs See Usage.
#' @param charge See Usage.
#' @param inring See Usage.
#' @param isotope_delta See Usage.
#' @return A list with \code{numhs}, \code{charge}, \code{inring}, \code{isotope_delta}.
#' @export
.mor_fp_defaults <- function(a, numhs, charge, inring, isotope_delta) {
  col <- function(x, default) {
    if (is.null(x)) return(rep(default, a))
    v <- trunc(as.numeric(x))
    if (length(v) != a) stop("per-atom vector has the wrong length")
    v
  }
  list(numhs = col(numhs, 0), charge = col(charge, 0),
       inring = col(inring, 0), isotope_delta = col(isotope_delta, 0))
}

#' ECFP4 extended-connectivity fingerprint
#'
#' Morgan fingerprint of radius 2 (Rogers and Hahn 2010).  Initial
#' atom identifiers hash the Daylight-style invariants (atomic number,
#' heavy-atom degree plus attached hydrogens, hydrogen count, formal
#' charge, isotope offset, ring membership); each iteration rehashes
#' an atom together with its neighbours' identifiers, entered as
#' (bond order, identifier) pairs in sorted order so the result is
#' independent of atom numbering; features whose bond sets coincide
#' are removed, keeping the first in canonical order.
#'
#' @param adjacency Square symmetric matrix of bond orders; zero means
#'   no bond.
#' @param atomnum Atomic number per atom.
#' @param numhs Attached hydrogens per atom (default 0).
#' @param charge Formal charge per atom (default 0).
#' @param inring Ring-membership flag per atom (default 0).
#' @param isotope_delta Isotope offset per atom (default 0).
#' @param nbits Folded fingerprint length, default 2048.
#' @param radius Number of iterations, default 2 (i.e. ECFP4).
#' @return A list with \code{bits} (0/1 folded fingerprint),
#'   \code{count} (per-bit collision counts), \code{nset},
#'   \code{identifiers} (sorted unique feature identifiers),
#'   \code{nenv}, \code{a}, \code{nbits}, \code{radius},
#'   \code{method}.
#' @references Rogers, D. and Hahn, M. (2010). Extended-connectivity
#'   fingerprints. Journal of Chemical Information and Modeling,
#'   50(5), 742-754.
#' @export
morie_ecfp4 <- function(adjacency, atomnum, numhs = NULL, charge = NULL,
                        inring = NULL, isotope_delta = NULL,
                        nbits = 2048L, radius = 2L) {
  bd <- .mor_fp_bonds(adjacency)
  a <- bd$a
  at <- trunc(as.numeric(atomnum))
  if (length(at) != a) stop("atomnum must have one entry per atom")
  df <- .mor_fp_defaults(a, numhs, charge, inring, isotope_delta)
  inv <- .mor_fp_invariants(a, bd, at, df$numhs, df$charge, df$inring,
                            df$isotope_delta)
  m <- .mor_fp_morgan(a, bd, inv, as.integer(radius), as.integer(nbits))
  list(bits = m$bits, count = m$count, nset = sum(m$bits),
       identifiers = sort(unique(m$ident)), nenv = length(m$ident),
       a = a, nbits = as.integer(nbits), radius = as.integer(radius),
       method = "ECFP4 (Morgan radius 2), Rogers-Hahn / RDKit")
}
