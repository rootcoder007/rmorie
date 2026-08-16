# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Topological combinatorics. Mirrors morie.fn.topcmb.
#
# Homology is over F_2, which trades torsion information for exact
# arithmetic: every rank is an integer computed by mod-2 Gaussian
# elimination, so a Betti number is never off by rounding. The checks
# are structural -- boundary-of-boundary must vanish identically before
# any rank is trusted, and the Euler-Poincare identity is computed from
# both sides independently.
#
# Faces are keyed by comma-joined vertex strings, matching the sorted
# tuples of the Python mirror.
#
# Sperner E (1928) Abh Math Sem Univ Hamburg 6:265-272; Euler L (1758);
# Munkres JR (1984) Elements of Algebraic Topology; Matousek J (2003)
# Using the Borsuk-Ulam Theorem.

#' .morie_face_key
#'
#' Part of the topological_combinatorics_native implementation; see the
#' file header for the source it follows.
#'
#' @param v Passed to \code{paste}.
#' @return A character value.
#' @export
.morie_face_key <- function(v) paste(v, collapse = ",")

#' Close maximal simplices under taking faces
#'
#' @param maximal_simplices A list of integer vectors.
#' @return A list keyed by dimension as character ("0", "1", ...), each
#'   a list of sorted integer vectors in lexicographic order.
#' @export
morie_simplicial_complex_faces <- function(maximal_simplices) {
  if (length(maximal_simplices) == 0L) {
    stop("no simplices supplied.", call. = FALSE)
  }
  maximal <- lapply(maximal_simplices, function(s) sort(unique(as.integer(s))))
  if (any(vapply(maximal, length, integer(1)) == 0L)) {
    stop("empty simplices are not allowed.", call. = FALSE)
  }
  seen <- new.env(parent = emptyenv())
  for (s in maximal) {
    for (k in seq_along(s)) {
      cmb <- utils::combn(s, k, simplify = FALSE)
      for (f in cmb) assign(.morie_face_key(f), f, envir = seen)
    }
  }
  faces <- mget(ls(seen), envir = seen)
  by_dim <- list()
  for (f in faces) {
    d <- as.character(length(f) - 1L)
    by_dim[[d]] <- c(by_dim[[d]], list(f))
  }
  for (d in names(by_dim)) {
    keys <- vapply(by_dim[[d]], function(f) {
      paste(formatC(f, width = 12, flag = "0"), collapse = ",")
    }, character(1))
    by_dim[[d]] <- by_dim[[d]][order(keys)]
  }
  by_dim[order(as.integer(names(by_dim)))]
}

#' The Euler characteristic
#'
#' \eqn{\chi = \sum_k (-1)^k f_k} from the closed face set.
#'
#' @param maximal_simplices A list of integer vectors.
#' @return A list with `chi`, `f_vector`, `dimension`.
#' @export
morie_euler_characteristic <- function(maximal_simplices) {
  by_dim <- morie_simplicial_complex_faces(maximal_simplices)
  dims <- as.integer(names(by_dim))
  f_vec <- vapply(by_dim, length, integer(1))
  chi <- sum((-1)^dims * f_vec)
  list(chi = chi, f_vector = as.integer(f_vec), dimension = max(dims),
       estimate = as.numeric(chi), n = sum(f_vec),
       method = "Euler characteristic (Euler 1758)")
}

# rank over F_2 by elimination on 0/1 matrices; exact at any size
#' Rank over F_2 by elimination on 0/1 matrices; exact at any size
#'
#' Part of the topological_combinatorics_native implementation; see the
#' file header for the source it follows.
#'
#' @param m Optional; may be \code{NULL}. A matrix; indexed by row and column.
#' @return The value of \code{rank}, as built in the body.
#' @export
.morie_gf2_rank <- function(m) {
  if (is.null(m) || nrow(m) == 0L || ncol(m) == 0L) return(0L)
  m <- m %% 2L
  rank <- 0L
  row <- 1L
  for (col in seq_len(ncol(m))) {
    piv <- which(m[row:nrow(m), col] == 1L)
    if (length(piv) == 0L) next
    piv <- piv[1] + row - 1L
    if (piv != row) m[c(row, piv), ] <- m[c(piv, row), ]
    for (r in seq_len(nrow(m))) {
      if (r != row && m[r, col] == 1L) {
        m[r, ] <- (m[r, ] + m[row, ]) %% 2L
      }
    }
    rank <- rank + 1L
    row <- row + 1L
    if (row > nrow(m)) break
  }
  rank
}

#' Simplicial Betti numbers over F_2
#'
#' \eqn{b_k = \dim\ker\partial_k - \mathrm{rank}\,\partial_{k+1}}, every
#' rank exact by mod-2 elimination. Before any rank is trusted,
#' \eqn{\partial_k \partial_{k+1} = 0} is verified entry by entry, and
#' the Euler-Poincare identity is computed from both sides
#' independently and compared.
#'
#' @param maximal_simplices A list of integer vectors.
#' @param check_boundary_squared Verify d^2 = 0 before reporting.
#' @return A list with `betti`, `f_vector`, `boundary_ranks`,
#'   `chi_from_faces`, `chi_from_betti`, `euler_poincare_holds`,
#'   `boundary_squared_zero`, `warnings`.
#' @references Munkres JR (1984) \emph{Elements of Algebraic Topology}.
#' @export
morie_betti_numbers_gf2 <- function(maximal_simplices,
                                    check_boundary_squared = TRUE) {
  by_dim <- morie_simplicial_complex_faces(maximal_simplices)
  dims <- as.integer(names(by_dim))
  top <- max(dims)
  index <- lapply(by_dim, function(fl) {
    stats::setNames(seq_along(fl),
                    vapply(fl, .morie_face_key, character(1)))
  })

  boundary_matrix <- function(d) {
    lower <- by_dim[[as.character(d - 1L)]]
    upper <- by_dim[[as.character(d)]]
    m <- matrix(0L, nrow = length(upper), ncol = length(lower))
    idx <- index[[as.character(d - 1L)]]
    for (r in seq_along(upper)) {
      s <- upper[[r]]
      for (omit in seq_along(s)) {
        m[r, idx[[.morie_face_key(s[-omit])]]] <- 1L
      }
    }
    m
  }

  ranks <- integer(top + 2L)   # ranks[d + 1] = rank of boundary_d
  mats <- vector("list", top + 1L)
  for (d in seq_len(top)) {
    mats[[d + 1L]] <- boundary_matrix(d)
    ranks[d + 1L] <- .morie_gf2_rank(mats[[d + 1L]])
  }
  squared_zero <- TRUE
  if (isTRUE(check_boundary_squared) && top >= 2L) {
    for (d in 2:top) {
      prod2 <- (mats[[d + 1L]] %*% mats[[d]]) %% 2L
      if (any(prod2 != 0L)) squared_zero <- FALSE
    }
  }
  betti <- integer(top + 1L)
  f_vec <- integer(top + 1L)
  for (d in 0:top) {
    n_d <- length(by_dim[[as.character(d)]])
    f_vec[d + 1L] <- n_d
    rank_d <- ranks[d + 1L]
    rank_next <- if (d + 2L <= length(ranks)) ranks[d + 2L] else 0L
    betti[d + 1L] <- (n_d - rank_d) - rank_next
  }
  chi_f <- sum((-1)^(0:top) * f_vec)
  chi_b <- sum((-1)^(0:top) * betti)
  warns <- character(0)
  if (!squared_zero) {
    warns <- c(warns, paste(
      "The boundary map does not square to zero, so the matrices are",
      "indexed wrongly and the Betti numbers above are noise."))
  }
  if (chi_f != chi_b) {
    warns <- c(warns, paste(
      "The Euler-Poincare identity fails, which is impossible over a",
      "field; the rank computation is defective."))
  }
  list(betti = betti, f_vector = f_vec, boundary_ranks = ranks,
       chi_from_faces = chi_f, chi_from_betti = chi_b,
       euler_poincare_holds = chi_f == chi_b,
       boundary_squared_zero = squared_zero,
       estimate = as.numeric(betti[1]), n = sum(f_vec), warnings = warns,
       method = "Simplicial homology over F_2 (Munkres 1984)")
}

#' Sperner's lemma on the standard subdivided triangle
#'
#' The triangle with corners 0, 1, 2 is subdivided into \eqn{k^2}
#' cells; grid point \eqn{(i, j)} has barycentric coordinates
#' \eqn{(k - i - j, i, j)/k}. A Sperner labelling picks, at every
#' point, a label whose coordinate is positive; the lemma says the
#' number of RAINBOW cells is odd, hence nonzero. Oddness is the full
#' strength of the result and is what is asserted.
#'
#' The default labelling takes the smallest admissible label. A custom
#' labelling is a named list keyed `"i,j"` and is validated against
#' admissibility rather than trusted -- an improper labelling is
#' exactly the case where the lemma is false, so it is refused loudly.
#'
#' @param subdivisions Subdivision count k.
#' @param labels Optional named list, key `"i,j"`, value 0, 1 or 2.
#' @return A list with `rainbow_count`, `is_odd`, `n_cells`,
#'   `n_points`.
#' @references Sperner E (1928) \emph{Abh Math Sem Univ Hamburg}
#'   6:265-272.
#' @export
morie_sperner_lemma_triangle <- function(subdivisions, labels = NULL) {
  k <- as.integer(subdivisions)
  if (is.na(k) || k < 1L) {
    stop(sprintf("subdivisions must be positive; got %s", subdivisions),
         call. = FALSE)
  }
  admissible <- function(i, j) {
    allowed <- integer(0)
    if (k - i - j > 0L) allowed <- c(allowed, 0L)
    if (i > 0L) allowed <- c(allowed, 1L)
    if (j > 0L) allowed <- c(allowed, 2L)
    allowed
  }
  lab <- new.env(parent = emptyenv())
  n_points <- 0L
  for (i in 0:k) {
    for (j in 0:(k - i)) {
      n_points <- n_points + 1L
      allowed <- admissible(i, j)
      key <- paste0(i, ",", j)
      if (!is.null(labels)) {
        if (is.null(labels[[key]])) {
          stop(sprintf("no label supplied for grid point (%d, %d).", i, j),
               call. = FALSE)
        }
        v <- as.integer(labels[[key]])
        if (!(v %in% allowed)) {
          stop(sprintf(paste(
            "label %d at (%d, %d) is not admissible; a Sperner labelling",
            "must pick from {%s}, and the lemma is FALSE for improper",
            "labellings."), v, i, j, paste(allowed, collapse = ", ")),
            call. = FALSE)
        }
        assign(key, v, envir = lab)
      } else {
        assign(key, allowed[1], envir = lab)
      }
    }
  }
  g <- function(i, j) get(paste0(i, ",", j), envir = lab)
  rainbow <- 0L
  cells <- 0L
  for (i in 0:(k - 1L)) {
    for (j in 0:(k - 1L - i)) {
      cells <- cells + 1L
      if (setequal(c(g(i, j), g(i + 1L, j), g(i, j + 1L)), 0:2)) {
        rainbow <- rainbow + 1L
      }
      if (i + j <= k - 2L) {
        cells <- cells + 1L
        if (setequal(c(g(i + 1L, j), g(i, j + 1L), g(i + 1L, j + 1L)),
                     0:2)) {
          rainbow <- rainbow + 1L
        }
      }
    }
  }
  list(rainbow_count = rainbow, is_odd = rainbow %% 2L == 1L,
       n_cells = cells, n_points = n_points,
       estimate = as.numeric(rainbow), n = k,
       method = "Sperner's lemma (Sperner 1928)")
}
