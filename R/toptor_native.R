# Topological torsion descriptors.
# Sources: Nilakantan, R., Bauman, N., Dixon, J. S. & Venkataraghavan,
# R. (1987) Topological Torsion: A New Molecular Descriptor for SAR
# Applications. Comparison with Other Descriptors, Journal of
# Chemical Information and Computer Sciences 27(2), 82-85.
#
# Native implementation mirroring Python morie.fn.toptor exactly: the
# same (NPI, TYPE, NBR) coding with NBR = total branches minus 1 at
# the ends and minus 2 in the middle, the same canonical direction by
# min(code, rev), the same a > d guard against counting the same
# undirected path twice, the same S = 2 D / (d_i + d_j) similarity,
# the same trend vector with a Fisher-Yates randomisation test, the
# same thirteen-element default, the same validation messages.

.COMMON_TYPES <- c("C", "N", "O", "S", "P", "F", "Cl", "Br", "I", "Si",
                   "B", "Se", "As")

#' .neighbours
#'
#' A step of the toptor_native implementation. Called by \code{topological_torsions}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n_atoms A count; the body uses it as \code{seq_len(...)}.
#' @param bonds See Usage.
#' @return The value of \code{adj}, as built in the body.
#' @export
.neighbours <- function(n_atoms, bonds) {
  adj <- vector("list", n_atoms)
  for (i in seq_len(n_atoms)) adj[[i]] <- integer(0)
  for (b in bonds) {
    i <- as.integer(b[[1]])
    j <- as.integer(b[[2]])
    if (i == j) stop("toptor: a bond from an atom to itself")
    if (!(i >= 0 && i < n_atoms && j >= 0 && j < n_atoms))
      stop("toptor: bond refers to an atom outside the molecule")
    adj[[i + 1L]] <- c(adj[[i + 1L]], j)
    adj[[j + 1L]] <- c(adj[[j + 1L]], i)
  }
  adj
}

#' .pi_electrons
#'
#' A step of the toptor_native implementation. Called by \code{topological_torsions}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n_atoms A count; the body uses it as \code{rep(...)}.
#' @param bonds See Usage.
#' @return The value of \code{as.integer}.
#' @export
.pi_electrons <- function(n_atoms, bonds) {
  npi <- rep(0, n_atoms)
  for (b in bonds) {
    order <- if (length(b) >= 3L) as.numeric(b[[3]]) else 1
    if (order < 1) stop("toptor: bond order below 1")
    npi[as.integer(b[[1]]) + 1L] <- npi[as.integer(b[[1]]) + 1L] +
      order - 1
    npi[as.integer(b[[2]]) + 1L] <- npi[as.integer(b[[2]]) + 1L] +
      order - 1
  }
  as.integer(round(npi))
}

#' Every topological torsion in a molecule, with multiplicities
#'
#' @param elements Heavy-atom element symbols.
#' @param bonds Bonds as \code{(i, j)} or \code{(i, j, order)}.
#' @param common_types Atom types kept as themselves; the rest become
#'   \code{"Y"}.
#' @return Canonical torsion code -> count.
#' @references Nilakantan, R. et al. (1987).
#' @export
#' @examples
#' elements <- c("C", "C", "C", "C")
#' bonds <- list(c(0, 1), c(1, 2), c(2, 3))
#' topological_torsions(elements, bonds)
#' @keywords internal
topological_torsions <- function(elements, bonds, common_types = NULL) {
  els <- as.character(elements)
  n <- length(els)
  if (n == 0L) stop("toptor: the molecule has no heavy atoms")
  keep <- if (is.null(common_types)) .COMMON_TYPES else as.character(common_types)
  types <- ifelse(els %in% keep, els, "Y")
  adj <- .neighbours(n, bonds)
  npi <- .pi_electrons(n, bonds)
  degree <- vapply(seq_len(n), function(i) length(adj[[i]]), integer(1))
  out <- list()
  for (a in seq_len(n) - 1L) {
    for (b in adj[[a + 1L]] + 1L) {
      for (c in adj[[b]]) {
        if (c == a + 1L) next
        for (d in adj[[c + 1L]] + 1L) {
          if (d == b || d == a + 1L) next
          path <- c(a + 1L, b, c, d)
          code <- lapply(seq_along(path), function(k) {
            p <- path[k]
            c(npi[p],
              types[p],
              degree[p] - if (k %in% c(1L, 4L)) 1L else 2L)
          })
          rev_code <- code[4:1]
          code_s <- do.call(paste, c(lapply(code, paste, collapse = ":"),
                                     sep = "|"))
          rev_s <- do.call(paste, c(lapply(rev_code, paste, collapse = ":"),
                                    sep = "|"))
          canon <- if (code_s <= rev_s) code else rev_code
          if (a + 1L > d) next
          key <- paste0(sapply(canon, function(tr) paste(tr, collapse = ":")),
                        collapse = "|")
          out[[key]] <- if (is.null(out[[key]])) 1L
                        else out[[key]] + 1L
        }
      }
    }
  }
  out
}

#' The paper's similarity score S = 2 D / (d_i + d_j)
#'
#' @param t1 First torsion dictionary (or iterable of codes).
#' @param t2 Second torsion dictionary (or iterable of codes).
#' @return Scalar similarity in \[0, 1\].
#' @references Nilakantan, R. et al. (1987).
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' torsion_similarity(V, V)
#' @keywords internal
torsion_similarity <- function(t1, t2) {
  s1 <- if (is.list(t1)) names(t1) else unique(as.character(t1))
  s2 <- if (is.list(t2)) names(t2) else unique(as.character(t2))
  if (length(s1) == 0L && length(s2) == 0L)
    stop("toptor: both molecules have no torsions, so the similarity is undefined")
  2 * length(intersect(s1, s2)) / (length(s1) + length(s2))
}

#' .trend_vector
#'
#' A step of the toptor_native implementation. Called by \code{morie_toptor}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param torsion_sets Iterated over elementwise, with \code{lapply}.
#' @param activities Coerced to numeric by the body, with \code{as.numeric}.
#' @param permutations A count; the body uses it as \code{seq_len(...)}.
#' @param seed Passed to \code{.ghc_rng}.
#' @return A list with \code{vector}, \code{descriptors}, \code{length},
#' \code{null_mean}, \code{null_sd}, \code{z}.
#' @export
.trend_vector <- function(torsion_sets, activities, permutations, seed) {
  sets <- lapply(torsion_sets, function(t) {
    if (is.list(t)) names(t) else unique(as.character(t))
  })
  a <- as.numeric(activities)
  n <- length(sets)
  if (n != length(a)) stop("toptor: one activity per structure is required")
  if (n < 2L) stop("toptor: the trend vector needs at least two structures")
  permutations <- as.integer(permutations)
  if (permutations < 1L) stop("toptor: permutations must be >= 1")
  keys <- sort(unique(unlist(sets)))
  if (length(keys) == 0L) stop("toptor: no descriptors in any structure")
  S <- t(vapply(sets, function(s) as.numeric(keys %in% s),
                 numeric(length(keys))))
  build <- function(order) {
    mean_a <- mean(a)
    vec <- numeric(length(keys))
    for (i in seq_len(n)) {
      w <- a[order[i]] - mean_a
      for (j in seq_along(keys)) vec[j] <- vec[j] + w * S[i, j]
    }
    vec / n
  }
  real <- build(seq_len(n))
  length_real <- sqrt(sum(real^2))
  e <- .ghc_rng(seed)
  lens <- numeric(permutations)
  for (k in seq_len(permutations)) {
    order <- seq_len(n)
    for (t in n:2) {
      u <- as.integer(.ghc_unif(e, 1L) * t) + 1L
      tmp <- order[t]
      order[t] <- order[u]
      order[u] <- tmp
    }
    v <- build(order)
    lens[k] <- sqrt(sum(v^2))
  }
  m <- mean(lens)
  var <- sum((lens - m)^2) / max(1L, length(lens) - 1L)
  sd <- sqrt(var)
  z <- if (sd > 0) (length_real - m) / sd
       else if (abs(length_real - m) < 1e-12) 0 else Inf
  list(vector = real, descriptors = keys, length = length_real,
       null_mean = m, null_sd = sd, z = z)
}

#' Topological torsions for a molecule, or for a set of molecules
#'
#' @param elements Element symbols (or list thereof for many molecules).
#' @param bonds Bond list(s).
#' @param reference Optional \code{(elements, bonds)} probe.
#' @param common_types Atom types kept as themselves.
#' @param activities Optional activities for the trend vector.
#' @param permutations Number of randomisations.
#' @param seed Seed for the shared generator.
#' @return A list with \code{estimate}, \code{torsions},
#'   \code{n_distinct}, \code{n_total}, \code{method}, and optionally
#'   \code{reference_torsions}, \code{similarity}, \code{ranking},
#'   \code{trend}.
#' @references Nilakantan, R. et al. (1987).
#' @export
#' @examples
#' elements <- c("C", "C", "C", "C")
#' bonds <- list(c(0, 1), c(1, 2), c(2, 3))
#' morie_toptor(elements, bonds)
#' @keywords internal
morie_toptor <- function(elements, bonds, reference = NULL,
                          common_types = NULL, activities = NULL,
                          permutations = 40, seed = 0) {
  many <- length(elements) > 0L &&
    (is.list(elements[[1]]) || is.list(elements[[1]]) ||
     inherits(elements[[1]], "list"))
  if (many) {
    mols <- mapply(function(e, b) list(list(as.character(e)),
                                       as.list(b)),
                   elements, bonds, SIMPLIFY = FALSE)
    mols <- lapply(seq_along(elements), function(k)
      list(as.character(elements[[k]]), as.list(bonds[[k]])))
  } else {
    mols <- list(list(as.character(elements), as.list(bonds)))
  }
  tors <- lapply(mols, function(m)
    topological_torsions(m[[1]], m[[2]], common_types))
  out <- list(estimate = if (many) tors else tors[[1]],
              torsions = if (many) tors else tors[[1]],
              n_distinct = if (many) vapply(tors, length, integer(1))
                            else length(tors[[1]]),
              n_total = if (many) vapply(tors, function(t) sum(unlist(t)),
                                          integer(1))
                         else sum(unlist(tors[[1]])),
              method = "topological torsion descriptors (Nilakantan et al. 1987)")
  if (!is.null(reference)) {
    ref <- topological_torsions(reference[[1]], reference[[2]],
                                 common_types)
    sims <- vapply(tors, function(t) torsion_similarity(ref, t),
                    numeric(1))
    out$reference_torsions <- ref
    out$similarity <- if (many) sims else sims[1]
    out$ranking <- order(-sims) - 1L
  }
  if (!is.null(activities))
    out$trend <- .trend_vector(tors, activities, permutations, seed)
  out
}

#' Compact alias per ledger/NAMING.md
#' @rdname morie_toptor
#' @export
morie_topological_torsion <- morie_toptor

# -- restored: morie-only definition kept through the rmorie sync --
#' Lexicographic comparison of two 4-tuples of (NPI, TYPE, NBR) codes
#'
#' A step of the toptor_native implementation. Called by \code{morie_topological_torsions}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a A vector; its length is taken and its elements indexed.
#' @param b A vector; indexed elementwise.
#' @return A logical value.
#' @export
#' @examples
#' A <- matrix(c(4, 1, 0.5, 1, 3, 0.8, 0.5, 0.8, 2), nrow = 3)
#' b <- c(1.5, 2.5, 3.5)
#' res <- .lex_le(a = A, b = b)
#' res
.lex_le <- function(a, b) {
  for (k in seq_along(a)) {
    if (a[[k]][[1]] != b[[k]][[1]]) return(a[[k]][[1]] < b[[k]][[1]])
    ta <- a[[k]][[2]]
    tb <- b[[k]][[2]]
    if (ta != tb) return(ta < tb)
    if (a[[k]][[3]] != b[[k]][[3]]) return(a[[k]][[3]] < b[[k]][[3]])
  }
  TRUE
}

# -- restored: morie-only definition kept through the rmorie sync --
#' Every topological torsion in a molecule, with multiplicities
#'
#' Returns a list keyed by the canonical 4-tuple of \code{(NPI, TYPE,
#' NBR)} codes; values are the multiplicity of each.
#'
#' @param elements Character vector of heavy-atom element symbols.
#' @param bonds List of bonds; each entry is a length-2 or length-3
#'   vector \code{c(i, j)} or \code{c(i, j, order)}.
#' @param common_types Optional character vector of element symbols
#'   kept as themselves; everything else becomes \code{"Y"}.
#' @return A named list mapping canonical codes to integer counts.
#' @references Nilakantan, R. et al. (1987).
#' @export
morie_topological_torsions <- function(elements, bonds, common_types = NULL) {
  els <- as.character(elements)
  n <- length(els)
  if (n == 0L) stop("toptor: the molecule has no heavy atoms")
  keep <- if (is.null(common_types)) .COMMON_TYPES else common_types
  types <- vapply(els, function(e) if (e %in% keep) e else "Y", character(1))
  adj <- .neighbours(n, bonds)
  npi <- .pi_electrons(n, bonds)
  degree <- vapply(adj, length, integer(1))
  out <- list()
  for (a in seq_len(n) - 1L) {
    for (b in adj[[a + 1L]]) {
      for (c in adj[[b + 1L]]) {
        if (c == a) next
        for (d in adj[[c + 1L]]) {
          if (d == b || d == a) next
          if (a > d) next
          path <- c(a, b, c, d)
          code <- lapply(seq_along(path), function(k) {
            p <- path[k] + 1L
            nbr <- degree[p] - if (k == 1L || k == 4L) 1L else 2L
            list(npi[p], types[p], nbr)
          })
          rev_ <- rev(code)
          canon <- if (.lex_le(code, rev_)) code else rev_
          key <- paste0(vapply(canon, function(t)
            paste0(t[[1]], ":", t[[2]], ":", t[[3]]), character(1)),
            collapse = "|")
          out[[key]] <- if (is.null(out[[key]])) 1L else out[[key]] + 1L
        }
      }
    }
  }
  out
}

# -- restored: morie-only definition kept through the rmorie sync --
#' Compact one-line summary of the toptor recipe
#'
#' @return A character string.
#' @export
morie_toptor_cheatsheet <- function() {
  paste("toptor: topological torsion (Nilakantan 1987). Four",
        "consecutively bonded HEAVY atoms, each coded (NPI, TYPE,",
        "NBR); NBR excludes the torsion itself -- total branches minus",
        "1 at the ends, minus 2 in the middle. Pi electrons stand in",
        "for bond types on purpose: it makes every torsion in benzene",
        "the same descriptor, where explicit bond types would give",
        "two. Each undirected path counted once, canonical direction.",
        "Similarity S = 2D/(d_i + d_j); trend vector",
        "T = (1/N) sum (a_i - A) S_i with a 40-fold randomisation test.")
}

# -- restored: morie-only definition kept through the rmorie sync --
#' The paper's similarity score S = 2 D / (d_i + d_j)
#'
#' @param t1,t2 Either torsion dictionaries or iterables of codes.
#' @return A numeric similarity in \code{[0, 1]}.
#' @references Nilakantan, R. et al. (1987).
#' @export
morie_torsion_similarity <- function(t1, t2) {
  s1 <- if (is.list(t1) && !is.null(names(t1))) names(t1) else as.character(t1)
  s2 <- if (is.list(t2) && !is.null(names(t2))) names(t2) else as.character(t2)
  if (length(s1) == 0L && length(s2) == 0L)
    stop("toptor: both molecules have no torsions, so the similarity is undefined")
  2 * length(intersect(s1, s2)) / (length(s1) + length(s2))
}

# -- restored: morie-only definition kept through the rmorie sync --
#' The trend vector T = (1/N) sum (a_i - A) S_i and its randomisation test
#'
#' @param torsion_sets List of torsion dictionaries.
#' @param activities Numeric activity vector.
#' @param permutations Number of permutations.
#' @param seed Seed for the shared generator.
#' @return A list with the vector, descriptors, length, null mean and
#'   sd, and the z-score.
#' @references Nilakantan, R. et al. (1987).
#' @export
morie_trend_vector <- function(torsion_sets, activities,
                                permutations = 40, seed = 0) {
  sets <- lapply(torsion_sets, function(t)
    if (is.list(t) && !is.null(names(t))) names(t) else as.character(t))
  a <- as.numeric(activities)
  n <- length(sets)
  if (n != length(a))
    stop("toptor: one activity per structure is required")
  if (n < 2L) stop("toptor: the trend vector needs at least two structures")
  permutations <- as.integer(permutations)
  if (permutations < 1L) stop("toptor: permutations must be >= 1")
  keys <- sort(unlist(sets))
  if (length(keys) == 0L) stop("toptor: no descriptors in any structure")
  # Collapse duplicates while keeping order
  keys <- keys[!duplicated(keys)]
  S <- vapply(sets, function(s) as.numeric(keys %in% s), numeric(length(keys)))
  build <- function(order) {
    mn <- sum(a) / n
    w <- a[order + 1L] - mn
    as.numeric((w %*% S) / n)
  }
  real <- build(seq_len(n) - 1L)
  length_ <- sqrt(sum(real * real))
  e <- .ghc_rng(as.integer(seed))
  lens <- numeric(permutations)
  for (kk in seq_len(permutations)) {
    order <- seq_len(n) - 1L
    for (t in (n - 1L):1L) {
      u <- as.integer(.ghc_unif(e, 1L) * (t + 1L))
      if (u > t) u <- t
      tmp <- order[t + 1L]
      order[t + 1L] <- order[u + 1L]
      order[u + 1L] <- tmp
    }
    v <- build(order)
    lens[kk] <- sqrt(sum(v * v))
  }
  m <- mean(lens)
  var <- sum((lens - m)^2) / max(1L, permutations - 1L)
  sd <- sqrt(var)
  if (sd > 0) {
    z <- (length_ - m) / sd
  } else if (abs(length_ - m) < 1e-12) {
    z <- 0
  } else {
    z <- Inf
  }
  list(vector = real, descriptors = keys, length = length_,
       null_mean = m, null_sd = sd, z = z)
}
