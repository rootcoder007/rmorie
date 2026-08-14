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

.neighbours <- function(n_atoms, bonds) {
  adj <- vector("list", n_atoms)
  for (i in seq_len(n_atoms)) adj[[i]] <- integer(0)
  for (b in bonds) {
    i <- as.integer(b[[1]]); j <- as.integer(b[[2]])
    if (i == j) stop("toptor: a bond from an atom to itself")
    if (!(i >= 0 && i < n_atoms && j >= 0 && j < n_atoms))
      stop("toptor: bond refers to an atom outside the molecule")
    adj[[i + 1L]] <- c(adj[[i + 1L]], j)
    adj[[j + 1L]] <- c(adj[[j + 1L]], i)
  }
  adj
}

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
topological_torsions <- function(elements, bonds, common_types = NULL) {
  els <- as.character(elements); n <- length(els)
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
          canon <- if (do.call(min, lapply(list(code, rev_code),
                                           function(x)
            do.call(paste, c(lapply(x, paste, collapse = ":"),
                              sep = "|"))))) code else rev_code
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
#' @return Scalar similarity in [0, 1].
#' @references Nilakantan, R. et al. (1987).
#' @export
torsion_similarity <- function(t1, t2) {
  s1 <- if (is.list(t1)) names(t1) else unique(as.character(t1))
  s2 <- if (is.list(t2)) names(t2) else unique(as.character(t2))
  if (length(s1) == 0L && length(s2) == 0L)
    stop("toptor: both molecules have no torsions, so the similarity is undefined")
  2 * length(intersect(s1, s2)) / (length(s1) + length(s2))
}

.trend_vector <- function(torsion_sets, activities, permutations, seed) {
  sets <- lapply(torsion_sets, function(t) {
    if (is.list(t)) names(t) else unique(as.character(t))
  })
  a <- as.numeric(activities); n <- length(sets)
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
      tmp <- order[t]; order[t] <- order[u]; order[u] <- tmp
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
#' @export
morie_topological_torsion <- morie_toptor
