# SPDX-License-Identifier: AGPL-3.0-or-later
# R arm of avalon -- the Avalon-style feature fingerprint. Mirrors
# src/morie/fn/avalon.py operation for operation.
#
# A fingerprint is a lossy summary with one job: two molecules that
# share substructure must share bits, so that a similarity computed on
# the bits stands in for a similarity computed on the graphs. Avalon's
# approach, as described by Gedeck, Rohde and Bartels, is to enumerate a
# fixed catalogue of query features rather than to grow environments
# around every atom the way a Morgan fingerprint does, and to fold the
# catalogue into a bit vector of the caller's chosen width.
#
# WHAT IS FAITHFUL AND WHAT IS THIS MODULE'S OWN. The paper describes
# the fingerprint's design and evaluates it; the exact feature
# enumeration and the hash seeds that assign a feature to a bit live in
# the Avalon toolkit's C source, not in the paper. So the bit POSITIONS
# here will not agree with the Avalon toolkit's, and this module does
# not claim they do. What is implemented is the fingerprint's structure
# -- five named feature classes -- and its contract:
#
#   atom  element, aromaticity, formal charge, degree, ring membership.
#   bond  the two atom types and the bond order, ends in a fixed order
#         so a bond and its reverse are one feature and not two.
#   path  every simple path up to a length limit, canonicalised against
#         its own reverse.
#   ring  ring size, whether aromatic, and the sorted elements round it.
#   pair  Carhart's atom pairs: two atom types and the bond distance.
#
# The classes do not all behave the same way. The path and bond classes
# are SUBGRAPH MONOTONE -- every path or bond of a fragment is a path or
# bond of anything containing it -- which is what lets a fingerprint be
# used as a substructure screen. The atom class is not, because an atom
# feature carries the atom's degree and hydrogen count, and those are
# properties of the whole molecule: propane has a carbon of degree two,
# and isobutane, which contains propane, has none. Neither are the ring
# and pair classes. That distinction is real and it is anchored in both
# directions rather than glossed.
#
# THE PARSER. SMILES is parsed here rather than assumed: the organic
# subset, bracket atoms, bond symbols, branches, ring-closure digits and
# percent labels, and the dot disconnection. Anything outside that is
# REFUSED with a message naming what was found, because a parser that
# silently drops what it does not understand produces a fingerprint of a
# different molecule.
#
# Implicit hydrogens follow the standard valences of the organic subset,
# with an aromatic atom counted as carrying one extra bond -- which is
# what makes each carbon of benzene come out with exactly one hydrogen.
#
# Rings are found from the ring-closure bonds: the smallest ring through
# a closure bond is that bond plus the shortest path between its ends
# that avoids it. For a fused system this is a valid ring set of the
# right size but not necessarily the canonical smallest set of smallest
# rings, and it is described as what it is.
#
# References
#   Gedeck, P., Rohde, B. and Bartels, C. (2006) "QSAR -- how good is it
#     in practice? Comparison of descriptor sets on an unbiased cross
#     section of database candidates." Journal of Chemical Information
#     and Modeling 46(5), 1924-1936. doi:10.1021/ci050413p.
#   Carhart, R.E., Smith, D.H. and Venkataraghavan, R. (1985) "Atom
#     pairs as molecular features in structure-activity studies."
#     Journal of Chemical Information and Computer Sciences 25(2),
#     64-73.
#   Weininger, D. (1988) "SMILES, a chemical language and information
#     system. 1." Journal of Chemical Information and Computer Sciences
#     28(1), 31-36.
#   Fowler, G., Noll, L.C. and Vo, K.-P. FNV-1a, the 32-bit
#     multiply-and-xor hash used to fold a feature into a bit.

# The organic subset: elements that may be written without brackets, and
# the valence each is filled up to when counting implicit hydrogens.
.avalon_organic <- c(B = 3, C = 4, N = 3, O = 2, P = 3, S = 2,
                     F = 1, Cl = 1, Br = 1, I = 1)
.avalon_aromatic <- c(b = "B", c = "C", n = "N", o = "O", p = "P",
                      s = "S")
.avalon_bondsym <- c("-" = 1, "=" = 2, "#" = 3, ":" = 4, "/" = 1,
                     "\\" = 1)
.avalon_classes <- c("atom", "bond", "path", "ring", "pair")

# Byte order, not the locale's collation. R would otherwise compare
# strings by whatever collation happens to be in force, and the
# canonical spelling of a path would depend on the machine.
#' Byte order, not the locale\'s collation. R would otherwise compare
#'
#' strings by whatever collation happens to be in force, and the
#' canonical spelling of a path would depend on the machine.
#'
#' @param a Passed to \code{utf8ToInt}.
#' @param b Passed to \code{utf8ToInt}.
#' @return A logical value.
#' @export
.avalon_lte <- function(a, b) {
  x <- utf8ToInt(a)
  y <- utf8ToInt(b)
  m <- min(length(x), length(y))
  if (m > 0L) for (i in seq_len(m)) {
    if (x[i] < y[i]) return(TRUE)
    if (x[i] > y[i]) return(FALSE)
  }
  length(x) <= length(y)
}

#' .avalon_sortkeys
#'
#' A step of the avalon_native implementation. Called by \code{morie_avalon_features}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v Passed to \code{unique}.
#' @return A vector, from \code{sort}.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' res <- .avalon_sortkeys(v = x)
#' res
.avalon_sortkeys <- function(v) sort(unique(v), method = "radix")

#' FNV-1a over the bytes of a feature key
#'
#' Written as explicit modular arithmetic rather than with a language's
#' integer type, so both arms of the package compute the same number
#' instead of a number that rounds the same way.
#'
#' @param s A character scalar.
#' @param seed The FNV offset basis.
#' @return A number below two to the thirty-second.
#' @export
morie_avalon_fnv <- function(s, seed = 2166136261) {
  h <- seed
  b <- utf8ToInt(s)
  if (length(b) && any(b > 255L))
    stop("a feature key must be plain ASCII")
  for (v in b) {
    lo <- h %% 256
    h <- h - lo + bitwXor(as.integer(lo), as.integer(v))
    hi <- h %/% 65536
    low <- h %% 65536
    h <- ((hi * 16777619) %% 65536) * 65536 + low * 16777619
    h <- h %% 4294967296
  }
  h
}

#' A SMILES string as an atom-bond graph
#'
#' Returns the element of each atom, whether it was written aromatic,
#' its formal charge, its explicit hydrogen count if one was given, and
#' the bonds as index triples with the order. Ring-closure bonds are
#' reported separately because the ring perception starts from them.
#'
#' @param smiles The molecule, in the subset the parser accepts.
#' @return A list with el, arom, chg, hexp, bonds and closures.
#' @export
morie_avalon_parse <- function(smiles) {
  s <- as.character(smiles)
  el <- character(0)
  arom <- integer(0)
  chg <- integer(0)
  hexp <- integer(0)
  bonds <- list()
  closures <- integer(0)
  open_ring <- list()
  stack <- integer(0)
  prev <- -1L
  order <- 0L
  i <- 1L
  n <- nchar(s)
  isdig <- function(x) nchar(x) == 1L && x >= "0" && x <= "9"
  while (i <= n) {
    ch <- substr(s, i, i)
    if (ch == "(") {
      if (prev < 0L) stop("a branch cannot open before an atom")
      stack <- c(stack, prev)
      i <- i + 1L
      next
    }
    if (ch == ")") {
      if (!length(stack)) stop("a branch closed that was never opened")
      prev <- stack[length(stack)]
      stack <- stack[-length(stack)]
      i <- i + 1L
      next
    }
    if (ch %in% names(.avalon_bondsym)) {
      order <- as.integer(.avalon_bondsym[[ch]])
      i <- i + 1L
      next
    }
    if (ch == ".") { prev <- -1L
    order <- 0L
    i <- i + 1L
    next }
    if (ch == "%" || isdig(ch)) {
      if (ch == "%") {
        if (i + 2L > n || !isdig(substr(s, i + 1L, i + 1L)) ||
            !isdig(substr(s, i + 2L, i + 2L)))
          stop("a percent ring label needs two digits")
        lab <- substr(s, i + 1L, i + 2L)
        i <- i + 3L
      } else { lab <- ch
      i <- i + 1L }
      if (prev < 0L) stop("a ring closure cannot precede an atom")
      if (lab %in% names(open_ring)) {
        a <- open_ring[[lab]][1]
        o <- open_ring[[lab]][2]
        open_ring[[lab]] <- NULL
        oo <- if (order != 0L) order else if (o != 0L) o else 1L
        bonds[[length(bonds) + 1L]] <- c(a, prev, oo)
        closures <- c(closures, length(bonds))
      } else {
        open_ring[[lab]] <- c(prev, order)
      }
      order <- 0L
      next
    }
    if (ch == "[") {
      j <- i
      while (j <= n && substr(s, j, j) != "]") j <- j + 1L
      if (j > n) stop("a bracket atom was never closed")
      body <- substr(s, i + 1L, j - 1L)
      i <- j + 1L
      k <- 1L
      nb <- nchar(body)
      # A leading isotope number is read and discarded: mass is not one
      # of the feature classes, so keeping it would put two bits where
      # the fingerprint defines one.
      while (k <= nb && isdig(substr(body, k, k))) k <- k + 1L
      sym <- ""
      if (k <= nb && grepl("^[A-Za-z]$", substr(body, k, k))) {
        sym <- substr(body, k, k)
        k <- k + 1L
        if (k <= nb && substr(body, k, k) %in% letters &&
            paste0(sym, substr(body, k, k)) %in%
              names(.avalon_organic)) {
          sym <- paste0(sym, substr(body, k, k))
          k <- k + 1L
        }
      }
      if (!nzchar(sym)) stop("a bracket atom must name an element")
      ar <- sym %in% names(.avalon_aromatic)
      e <- if (ar) .avalon_aromatic[[sym]] else sym
      hh <- -1L
      cg <- 0L
      while (k <= nb) {
        cc <- substr(body, k, k)
        if (cc == "H") {
          k <- k + 1L
          d <- ""
          while (k <= nb && isdig(substr(body, k, k))) {
            d <- paste0(d, substr(body, k, k))
            k <- k + 1L
          }
          hh <- if (nzchar(d)) as.integer(d) else 1L
        } else if (cc == "+" || cc == "-") {
          sgn <- if (cc == "+") 1L else -1L
          k <- k + 1L
          d <- ""
          while (k <= nb && isdig(substr(body, k, k))) {
            d <- paste0(d, substr(body, k, k))
            k <- k + 1L
          }
          if (nzchar(d)) {
            cg <- sgn * as.integer(d)
          } else {
            cg <- sgn
            while (k <= nb && substr(body, k, k) == cc) {
              cg <- cg + sgn
              k <- k + 1L
            }
          }
        } else if (cc == "@") {
          # Stereochemistry is not a feature of this fingerprint, and
          # dropping it is a decision, not an oversight: two enantiomers
          # get the same bits.
          k <- k + 1L
          while (k <= nb && substr(body, k, k) == "@") k <- k + 1L
        } else {
          stop("unsupported bracket atom field: ", cc)
        }
      }
    } else {
      two <- if (i + 1L <= n) substr(s, i, i + 1L) else ""
      if (nzchar(two) && two %in% names(.avalon_organic)) {
        sym <- two
        i <- i + 2L
      } else if (ch %in% names(.avalon_organic) ||
                 ch %in% names(.avalon_aromatic)) {
        sym <- ch
        i <- i + 1L
      } else {
        stop("unsupported SMILES character: ", ch)
      }
      ar <- sym %in% names(.avalon_aromatic)
      e <- if (ar) .avalon_aromatic[[sym]] else sym
      hh <- -1L
      cg <- 0L
    }
    el <- c(el, e)
    arom <- c(arom, if (ar) 1L else 0L)
    chg <- c(chg, cg)
    hexp <- c(hexp, hh)
    cur <- length(el) - 1L
    if (prev >= 0L) {
      oo <- if (order != 0L) order else
        if (arom[prev + 1L] == 1L && ar) 4L else 1L
      bonds[[length(bonds) + 1L]] <- c(prev, cur, oo)
    }
    prev <- cur
    order <- 0L
  }
  if (length(open_ring))
    stop("a ring closure was opened and never matched")
  if (length(stack)) stop("a branch was opened and never closed")
  if (!length(el)) stop("an empty SMILES string is not a molecule")
  list(el = el, arom = arom, chg = chg, hexp = hexp, bonds = bonds,
       closures = closures)
}

#' .avalon_adj
#'
#' A step of the avalon_native implementation. Called by \code{morie_avalon_features},
#' \code{morie_avalon_rings}, \code{morie_cypin_descriptors} and 3 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n A count; the body uses it as \code{seq_len(...)}.
#' @param bonds A vector; its length is taken and its elements indexed.
#' @return The value of \code{adj}, as built in the body.
#' @export
.avalon_adj <- function(n, bonds) {
  adj <- vector("list", n)
  for (i in seq_len(n)) adj[[i]] <- list()
  for (k in seq_along(bonds)) {
    b <- bonds[[k]]
    a1 <- b[1] + 1L
    a2 <- b[2] + 1L
    adj[[a1]][[length(adj[[a1]]) + 1L]] <- c(b[2], b[3], k)
    adj[[a2]][[length(adj[[a2]]) + 1L]] <- c(b[1], b[3], k)
  }
  adj
}

#' Hydrogens filled in to the standard valence of the organic subset
#'
#' An aromatic atom is counted as carrying one extra bond, which is what
#' makes each carbon of benzene come out with exactly one hydrogen. An
#' explicit count in brackets is taken as given and nothing is filled
#' in.
#'
#' @param el Element symbols.
#' @param arom Aromatic flags.
#' @param chg Formal charges.
#' @param hexp Explicit hydrogen counts, or minus one.
#' @param bonds The bond list.
#' @return One count per atom.
#' @export
morie_avalon_h <- function(el, arom, chg, hexp, bonds) {
  n <- length(el)
  used <- rep(0, n)
  for (b in bonds) {
    w <- if (b[3] == 4) 1 else b[3]
    used[b[1] + 1L] <- used[b[1] + 1L] + w
    used[b[2] + 1L] <- used[b[2] + 1L] + w
  }
  out <- integer(n)
  for (i in seq_len(n)) {
    if (hexp[i] >= 0L) { out[i] <- hexp[i]
    next }
    v <- .avalon_organic[el[i]]
    if (is.na(v)) { out[i] <- 0L
    next }
    need <- v + chg[i] - used[i] - (if (arom[i] == 1L) 1 else 0)
    out[i] <- as.integer(if (need > 0) need else 0)
  }
  out
}

#' .avalon_shortest
#'
#' A step of the avalon_native implementation. Called by \code{morie_avalon_rings}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param adj A vector; its length is taken and its elements indexed.
#' @param src Numeric; combined arithmetically in the body.
#' @param dst Numeric; combined arithmetically in the body.
#' @param banned Passed to \code{==}.
#' @return A vector, from \code{rev}.
#' @export
.avalon_shortest <- function(adj, src, dst, banned) {
  n <- length(adj)
  dist <- rep(-1L, n)
  prv <- rep(-1L, n)
  dist[src + 1L] <- 0L
  q <- c(src)
  head <- 1L
  while (head <= length(q)) {
    u <- q[head]
    head <- head + 1L
    for (e in adj[[u + 1L]]) {
      if (e[3] == banned || dist[e[1] + 1L] >= 0L) next
      dist[e[1] + 1L] <- dist[u + 1L] + 1L
      prv[e[1] + 1L] <- u
      q <- c(q, e[1])
    }
  }
  if (dist[dst + 1L] < 0L) return(NULL)
  path <- c(dst)
  while (path[length(path)] != src)
    path <- c(path, prv[path[length(path)] + 1L])
  rev(path)
}

#' One ring per ring-closure bond: that bond plus the shortest path
#'
#' For a single ring this is the ring. For a fused system it is a valid
#' ring set of the right size -- the cyclomatic number -- but not
#' necessarily the canonical smallest set of smallest rings, and it is
#' reported as what it is rather than labelled SSSR.
#'
#' @param n The atom count.
#' @param bonds The bond list.
#' @param closures Which bonds were ring closures.
#' @return A list with the rings and a per-atom ring-membership flag.
#' @export
morie_avalon_rings <- function(n, bonds, closures) {
  adj <- .avalon_adj(n, bonds)
  rings <- list()
  inring <- integer(n)
  for (k in closures) {
    b <- bonds[[k]]
    p <- .avalon_shortest(adj, b[1], b[2], k)
    if (is.null(p)) next
    rings[[length(rings) + 1L]] <- p
    for (v in p) inring[v + 1L] <- 1L
  }
  list(rings = rings, inring = inring)
}

#' .avalon_ty
#'
#' A step of the avalon_native implementation. Called by \code{morie_avalon_features}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param el A vector; indexed elementwise.
#' @param arom A vector; indexed elementwise.
#' @param i See Usage.
#' @return One of two values, depending on the branch taken.
#' @export
.avalon_ty <- function(el, arom, i)
  if (arom[i] == 1L) tolower(el[i]) else el[i]

#' A path and its reverse are the same feature, so only the smaller of
#'
#' the two spellings is kept -- otherwise a symmetric molecule would
#' light twice as many bits as an asymmetric one for no chemical reason.
#' The key is built here rather than by the caller so the two arms of
#' this package cannot canonicalise it differently.
#'
#' @param adj A vector; indexed elementwise.
#' @param n A count; the body uses it as \code{rep(...)}.
#' @param maxpath Passed to \code{>=}.
#' @param ty A vector; indexed elementwise.
#' @return The value of \code{unique}.
#' @export
.avalon_paths <- function(adj, n, maxpath, ty) {
  # A path and its reverse are the same feature, so only the smaller of
  # the two spellings is kept -- otherwise a symmetric molecule would
  # light twice as many bits as an asymmetric one for no chemical
  # reason. The key is built here rather than by the caller so the two
  # arms of this package cannot canonicalise it differently.
  out <- character(0)
  key <- function(seq) {
    m <- length(seq)
    fwd <- character(m)
    rev_ <- character(m)
    for (q in seq_len(m)) {
      fwd[q] <- if (q %% 2 == 1L) ty[seq[q] + 1L] else
        as.character(seq[q])
      r <- m - q + 1L
      rev_[q] <- if (r %% 2 == 1L) ty[seq[r] + 1L] else
        as.character(seq[r])
    }
    a <- paste(fwd, collapse = "|")
    b <- paste(rev_, collapse = "|")
    paste0("P|", if (.avalon_lte(a, b)) a else b)
  }
  walk <- function(seq, used) {
    if (length(seq) >= 2L) out[[length(out) + 1L]] <<- key(seq)
    if ((length(seq) - 1L) %/% 2L >= maxpath) return(invisible(NULL))
    u <- seq[length(seq)]
    for (e in adj[[u + 1L]]) {
      v <- e[1]
      if (used[v + 1L]) next
      used[v + 1L] <- TRUE
      walk(c(seq, e[2], v), used)
      used[v + 1L] <- FALSE
    }
    invisible(NULL)
  }
  for (s in 0:(n - 1L)) {
    used <- rep(FALSE, n)
    used[s + 1L] <- TRUE
    walk(c(s), used)
  }
  unique(out)
}

#' .avalon_dist
#'
#' A step of the avalon_native implementation. Called by \code{morie_avalon_features},
#' \code{morie_scfhop_cats}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param adj A vector; indexed elementwise.
#' @param n A count; the body uses it as \code{matrix(...)}.
#' @return The value of \code{D}, as built in the body.
#' @export
.avalon_dist <- function(adj, n) {
  D <- matrix(-1L, n, n)
  for (s in 0:(n - 1L)) {
    D[s + 1L, s + 1L] <- 0L
    q <- c(s)
    head <- 1L
    while (head <= length(q)) {
      u <- q[head]
      head <- head + 1L
      for (e in adj[[u + 1L]]) {
        if (D[s + 1L, e[1] + 1L] < 0L) {
          D[s + 1L, e[1] + 1L] <- D[s + 1L, u + 1L] + 1L
          q <- c(q, e[1])
        }
      }
    }
  }
  D
}

#' The feature keys of a molecule, as sorted strings
#'
#' Exposed because a fingerprint that cannot be asked what it saw is a
#' fingerprint that cannot be debugged: the bits are these keys hashed,
#' and a surprising bit can always be traced back to a feature here.
#'
#' @param smiles The molecule.
#' @param maxpath The longest path feature, in bonds.
#' @param classes Which feature classes to use, or NULL for all five.
#' @return A sorted character vector.
#' @export
morie_avalon_features <- function(smiles, maxpath = 5L, classes = NULL) {
  if (is.null(classes)) classes <- .avalon_classes
  for (cl in classes) if (!(cl %in% .avalon_classes))
    stop("unknown feature class: ", cl)
  g <- morie_avalon_parse(smiles)
  el <- g$el
  arom <- g$arom
  chg <- g$chg
  bonds <- g$bonds
  n <- length(el)
  adj <- .avalon_adj(n, bonds)
  rr <- morie_avalon_rings(n, bonds, g$closures)
  inring <- rr$inring
  nh <- morie_avalon_h(el, arom, chg, g$hexp, bonds)
  ty <- vapply(seq_len(n), function(i) .avalon_ty(el, arom, i),
               character(1))
  out <- character(0)
  if ("atom" %in% classes) for (i in seq_len(n))
    out <- c(out, sprintf("A|%s|%d|%d|%d|%d|%d", ty[i], arom[i], chg[i],
                          length(adj[[i]]), inring[i], nh[i]))
  if ("bond" %in% classes) for (b in bonds) {
    x <- ty[b[1] + 1L]
    y <- ty[b[2] + 1L]
    if (!.avalon_lte(x, y)) { tmp <- x
    x <- y
    y <- tmp }
    out <- c(out, sprintf("B|%s|%d|%s", x, b[3], y))
  }
  if ("path" %in% classes)
    out <- c(out, .avalon_paths(adj, n, maxpath, ty))
  if ("ring" %in% classes) for (r in rr$rings) {
    allarom <- 1L
    for (v in r) if (arom[v + 1L] != 1L) allarom <- 0L
    elems <- sort(el[r + 1L], method = "radix")
    out <- c(out, sprintf("R|%d|%d|%s", length(r), allarom,
                          paste(elems, collapse = ",")))
  }
  if ("pair" %in% classes) {
    D <- .avalon_dist(adj, n)
    for (i in seq_len(n)) if (i < n) for (j in (i + 1L):n) {
      if (D[i, j] < 0L) next
      x <- ty[i]
      y <- ty[j]
      if (!.avalon_lte(x, y)) { tmp <- x
      x <- y
      y <- tmp }
      out <- c(out, sprintf("D|%s|%s|%d", x, y, D[i, j]))
    }
  }
  .avalon_sortkeys(out)
}

#' The Tanimoto coefficient of two bit vectors of the same width
#'
#' Two empty fingerprints have nothing in common and nothing to disagree
#' about; the ratio is zero over zero and is reported as zero rather
#' than as a division that happened to not raise.
#'
#' @param a,b Bit vectors.
#' @return A number between zero and one.
#' @export
morie_avalon_tanimoto <- function(a, b) {
  if (length(a) != length(b))
    stop("two fingerprints of different widths cannot be compared")
  both <- sum(a != 0 & b != 0)
  either <- sum(a != 0 | b != 0)
  if (either == 0L) 0 else both / either
}

#' Hash a molecule's structural features into a bit vector
#'
#' @param smiles The molecule, in the SMILES subset the parser accepts.
#' @param n_bits The width of the vector. Folding is by remainder, so a
#'   narrower vector is the wider one folded and collisions rise with it
#'   -- which is why the collision count is reported and not hidden.
#' @param maxpath The longest path feature, in bonds.
#' @param classes Which feature classes to use; NULL is all five.
#'   Restricted to bond and path the fingerprint is subgraph monotone.
#' @return A list with the bits, the features that set them, and the
#'   collision count.
#' @export
morie_avalon <- function(smiles, n_bits = 512L, maxpath = 5L,
                         classes = NULL) {
  n_bits <- as.integer(n_bits)
  if (n_bits < 1L) stop("a fingerprint needs at least one bit")
  feats <- morie_avalon_features(smiles, maxpath, classes)
  bits <- integer(n_bits)
  seen <- rep(FALSE, n_bits)
  coll <- 0L
  for (f in feats) {
    b <- as.integer(morie_avalon_fnv(f) %% n_bits)
    if (seen[b + 1L]) coll <- coll + 1L else seen[b + 1L] <- TRUE
    bits[b + 1L] <- 1L
  }
  g <- morie_avalon_parse(smiles)
  rr <- morie_avalon_rings(length(g$el), g$bonds, g$closures)
  list(bits = bits, on = which(bits == 1L) - 1L, features = feats,
       n_features = length(feats), n_on = sum(bits),
       n_collisions = coll, density = sum(bits) / n_bits,
       n_atoms = length(g$el), n_bonds = length(g$bonds),
       n_rings = length(rr$rings),
       n_hydrogens = sum(morie_avalon_h(g$el, g$arom, g$chg, g$hexp,
                                        g$bonds)),
       n_bits = n_bits, maxpath = as.integer(maxpath),
       classes = if (is.null(classes)) .avalon_classes else classes,
       method = "Avalon-style hashed feature fingerprint")
}

#' One-line summary of the avalon module
#'
#' @return A character scalar.
#' @export
morie_avalon_cheatsheet <- function()
  paste0("avalon: Avalon-style feature fingerprint. Atom, bond, path, ",
         "ring and atom-pair features hashed with FNV-1a into a folded ",
         "bit vector; SMILES parsed, not assumed")
