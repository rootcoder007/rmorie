# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Extended-connectivity fingerprint, Morgan radius 2 (Ecfp4).
# Bit-identical mirror of src/morie/fn/ecfp4.py.  Specification followed:
# the RDKit reference implementation, MorganGenerator.cpp lines 395-495
# (rounds, duplicate-environment retirement) and FingerprintUtil.cpp
# lines 242-265 (getConnectivityInvariants); master revision fetched
# 2026-08-09, stored at library/pdf/fetched-wave3/rdkit-reference-source/.
# Anchored against RDKit itself: environment counts and distinct-identifier
# counts reproduce RDKit exactly on benzene, ethanol, isobutane,
# cyclopropane, aspirin and caffeine at radius 2 and 3.

.ecfp_mod <- 2147483647
.ecfp_mul <- 1000003

#' .ecfp_mix
#'
#' A step of the ecfp4 implementation. Called by \code{.ecfp_conninv}, \code{.ecfp_morgan}, \code{Rdkfp}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param h Numeric; combined arithmetically in the body.
#' @param v Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.ecfp_mix <- function(h, v) (h * .ecfp_mul + (v %% .ecfp_mod)) %% .ecfp_mod

#' .ecfp_bonds
#'
#' A step of the ecfp4 implementation. Called by \code{Ecfp4}, \code{Ecfp6}, \code{Fcfp4} and 1 others in the module.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param adjacency Passed to \code{.t1_mat}.
#' @return A list with \code{a}, \code{i}, \code{j}, \code{o}.
#' @export
.ecfp_bonds <- function(adjacency) {
  A <- .t1_mat(adjacency)
  a <- nrow(A)
  if (ncol(A) != a) stop("adjacency must be square", call. = FALSE)
  bi <- integer(0); bj <- integer(0); bo <- numeric(0)
  if (a > 1L) for (i in seq_len(a - 1L)) for (j in (i + 1L):a) {
    if (A[i, j] != A[j, i]) stop("adjacency must be symmetric", call. = FALSE)
    if (A[i, j] != 0) { bi <- c(bi, i); bj <- c(bj, j); bo <- c(bo, A[i, j]) }
  }
  list(a = a, i = bi, j = bj, o = as.numeric(as.integer(bo)))
}

#' .ecfp_conninv
#'
#' A step of the ecfp4 implementation. Called by \code{Ecfp4}, \code{Ecfp6}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param B A list; the body reads \code{$a}, \code{$i}, \code{$j} from it.
#' @param atomnum A vector; indexed elementwise.
#' @param numhs A vector; indexed elementwise.
#' @param charge A vector; indexed elementwise.
#' @param inring A vector; indexed elementwise.
#' @param isodelta A vector; indexed elementwise.
#' @return The value of \code{inv}, as built in the body.
#' @export
.ecfp_conninv <- function(B, atomnum, numhs, charge, inring, isodelta) {
  a <- B$a
  deg <- integer(a)
  if (length(B$i)) for (k in seq_along(B$i)) {
    deg[B$i[k]] <- deg[B$i[k]] + 1L; deg[B$j[k]] <- deg[B$j[k]] + 1L
  }
  inv <- numeric(a)
  for (i in seq_len(a)) {
    comps <- c(atomnum[i], deg[i] + numhs[i], numhs[i], charge[i], isodelta[i])
    if (inring[i] != 0) comps <- c(comps, 1)
    h <- 0
    for (cc in comps) h <- .ecfp_mix(h, cc)
    inv[i] <- h
  }
  inv
}

#' .ecfp_envkey
#'
#' A step of the ecfp4 implementation. Called by \code{.ecfp_morgan}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param v A vector; its length is taken.
#' @return A character value.
#' @export
.ecfp_envkey <- function(v) {
  if (!length(v)) return("")
  paste0(sprintf("%04d", sort(as.integer(v), method = "radix")), collapse = "")
}

#' .ecfp_morgan
#'
#' A step of the ecfp4 implementation. Called by \code{Ecfp4}, \code{Ecfp6}, \code{Fcfp4}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param B A list; the body reads \code{$a}, \code{$i}, \code{$j}, \code{$o} from it.
#' @param invariants Coerced to numeric by the body, with \code{as.numeric}.
#' @param radius A count; the body uses it as \code{seq_len(...)}.
#' @param nbits A count; the body uses it as \code{integer(...)}.
#' @param use_bond_order A flag; the body branches on it. Defaults to \code{TRUE}.
#' @return A list with \code{bits}, \code{count}, \code{ident}.
#' @export
.ecfp_morgan <- function(B, invariants, radius, nbits, use_bond_order = TRUE) {
  a <- B$a
  nb <- length(B$i)
  inc_b <- vector("list", a); inc_n <- vector("list", a); inc_o <- vector("list", a)
  for (i in seq_len(a)) { inc_b[[i]] <- integer(0); inc_n[[i]] <- integer(0); inc_o[[i]] <- numeric(0) }
  if (nb) for (k in seq_len(nb)) {
    ii <- B$i[k]; jj <- B$j[k]; oo <- if (use_bond_order) B$o[k] else 1
    inc_b[[ii]] <- c(inc_b[[ii]], k); inc_n[[ii]] <- c(inc_n[[ii]], jj); inc_o[[ii]] <- c(inc_o[[ii]], oo)
    inc_b[[jj]] <- c(inc_b[[jj]], k); inc_n[[jj]] <- c(inc_n[[jj]], ii); inc_o[[jj]] <- c(inc_o[[jj]], oo)
  }
  cur <- as.numeric(invariants)
  ident <- as.numeric(invariants)
  seen <- character(0)
  atom_env <- vector("list", a)
  for (i in seq_len(a)) atom_env[[i]] <- integer(0)
  dead <- rep(FALSE, a)

  radius <- as.integer(radius)
  if (radius > 0L) for (layer in seq_len(radius) - 1L) {
    nxt <- numeric(a)
    round_env <- atom_env
    keys <- character(0); invars <- numeric(0); idxs <- integer(0)
    for (i in seq_len(a)) {
      if (dead[i]) next
      e <- round_env[[i]]
      bts <- numeric(0); nis <- numeric(0)
      if (length(inc_b[[i]])) for (m in seq_along(inc_b[[i]])) {
        e <- c(e, inc_b[[i]][m], atom_env[[inc_n[[i]][m]]])
        bts <- c(bts, inc_o[[i]][m]); nis <- c(nis, cur[inc_n[[i]][m]])
      }
      round_env[[i]] <- unique(as.integer(e))
      ord <- order(bts, nis, method = "radix")
      bts <- bts[ord]; nis <- nis[ord]
      invar <- .ecfp_mix(0, layer)
      invar <- .ecfp_mix(invar, cur[i])
      if (length(bts)) for (m in seq_along(bts)) invar <- .ecfp_mix(.ecfp_mix(invar, bts[m]), nis[m])
      nxt[i] <- invar
      keys <- c(keys, .ecfp_envkey(round_env[[i]]))
      invars <- c(invars, invar); idxs <- c(idxs, i)
    }
    if (length(keys)) {
      ord <- order(keys, invars, idxs, method = "radix")
      for (t in ord) {
        if (!(keys[t] %in% seen)) {
          seen <- c(seen, keys[t]); ident <- c(ident, invars[t])
        } else dead[idxs[t]] <- TRUE
      }
    }
    for (i in seq_len(a)) if (!dead[i]) cur[i] <- nxt[i]
    atom_env <- round_env
  }

  nbits <- as.integer(nbits)
  if (is.na(nbits) || nbits < 1L) stop("nbits must be positive", call. = FALSE)
  bits <- integer(nbits); cnt <- integer(nbits)
  for (v in ident) { b <- v %% nbits; bits[b + 1L] <- 1L; cnt[b + 1L] <- cnt[b + 1L] + 1L }
  list(bits = bits, count = cnt, ident = ident)
}

#' .ecfp_percol
#'
#' A step of the ecfp4 implementation. Called by \code{Ecfp4}, \code{Ecfp6}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param x Optional; may be \code{NULL}. Passed to \code{.t1_vec}.
#' @param a A count; the body uses it as \code{rep(...)}.
#' @param default A count; the body uses it as \code{rep(...)}.
#' @return The value of \code{v}, as built in the body.
#' @export
.ecfp_percol <- function(x, a, default) {
  if (is.null(x)) return(rep(default, a))
  v <- as.numeric(.t1_vec(x))
  if (length(v) != a) stop("per-atom vector has the wrong length", call. = FALSE)
  v
}

#' Extended-connectivity fingerprint, radius 2 (ECFP4)
#'
#' ECFP diameter 4 is Morgan radius 2.  The molecule arrives as a
#' pre-parsed graph, not as SMILES: \code{adjacency} is a symmetric
#' bond-order matrix with 0 no bond, 1 single, 2 double, 3 triple and
#' 4 aromatic (that encoding is this implementation's own and is stated
#' rather than attributed).  SMILES parsing and aromaticity perception are
#' out of scope; this function starts from the graph a parser would hand
#' it.
#'
#' Round-0 atom invariants are the component vector of RDKit's
#' \code{getConnectivityInvariants}: atomic number, total degree, total
#' hydrogen count, formal charge, isotope mass delta, and a trailing 1 for
#' ring atoms.  Each further round hashes the layer number, the atom's own
#' identifier and the sorted list of neighbour bond-order and identifier
#' pairs; an environment already emitted is not emitted again and its atom
#' is retired.
#'
#' The hash itself departs from RDKit deliberately: \code{boost::hash_combine}
#' is replaced by the stated closed form \eqn{h \leftarrow (1000003 h + v)
#' \bmod (2^{31}-1)}, so that the Python and both R arms agree exactly
#' without 32-bit unsigned arithmetic.  Bit indices are therefore this
#' implementation's own; the identifier partition is the published ECFP
#' one.
#'
#' @param adjacency Symmetric bond-order matrix; 0 means no bond.
#' @param atomnum Atomic number per atom.
#' @param numhs Attached hydrogen count per atom; default 0.
#' @param charge Formal charge per atom; default 0.
#' @param inring Ring-membership flag per atom; default 0.
#' @param isotope_delta Isotope mass delta per atom; default 0.
#' @param nbits Width of the folded fingerprint.
#' @param radius Morgan radius; 2 for ECFP4.
#' @return List with \code{bits}, \code{count}, \code{nset},
#'   \code{identifiers}, \code{nenv}, \code{a}, \code{nbits},
#'   \code{radius}, \code{method}.
#' @references Rogers, D. and Hahn, M. (2010), "Extended-connectivity
#'   fingerprints", Journal of Chemical Information and Modeling 50(5),
#'   742-754, doi:10.1021/ci100050t -- paywalled at ACS, not read for this
#'   implementation.  The specification followed is the open-source RDKit
#'   reference implementation, Code/GraphMol/Fingerprints/
#'   MorganGenerator.cpp lines 395-495 and Code/GraphMol/Fingerprints/
#'   FingerprintUtil.cpp lines 242-265, master revision fetched
#'   2026-08-09.  RDKit: Open-Source Cheminformatics,
#'   https://www.rdkit.org.
#' @export
Ecfp4 <- function(adjacency, atomnum, numhs = NULL, charge = NULL,
                  inring = NULL, isotope_delta = NULL, nbits = 2048,
                  radius = 2) {
  B <- .ecfp_bonds(adjacency)
  at <- as.numeric(.t1_vec(atomnum))
  if (length(at) != B$a) stop("atomnum must have one entry per atom", call. = FALSE)
  nh <- .ecfp_percol(numhs, B$a, 0)
  ch <- .ecfp_percol(charge, B$a, 0)
  ir <- .ecfp_percol(inring, B$a, 0)
  isd <- .ecfp_percol(isotope_delta, B$a, 0)
  inv <- .ecfp_conninv(B, at, nh, ch, ir, isd)
  M <- .ecfp_morgan(B, inv, radius, nbits)
  .t1_result(bits = M$bits, count = M$count, nset = sum(M$bits),
             identifiers = sort(unique(M$ident)), nenv = length(M$ident),
             a = B$a, nbits = as.integer(nbits), radius = as.integer(radius),
             method = "ECFP4 (Morgan radius 2), Rogers-Hahn / RDKit")
}
