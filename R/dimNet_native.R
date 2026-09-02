# DimeNet: directional message passing.
# Sources: Klicpera, J., Gross, J. & Gunnemann, S. (2020) "Directional
# Message Passing for Molecular Graphs", ICLR 2020, arXiv:2003.03123
# (messages embedded rather than atoms, each associated with a
# direction in coordinate space and hence rotationally equivariant;
# the belief-propagation-style update through the angle between
# messages; spherical Bessel and spherical harmonics bases for
# orthogonal representations; the triplet interaction count); Gilmer,
# J. et al. (2017) "Neural Message Passing for Quantum Chemistry",
# ICML 2017, PMLR 70, 1263-1272, arXiv:1704.01212 (the general
# message-passing framework this specialises); Schutt, K. T. et al.
# (2017) "SchNet: A continuous-filter convolutional neural network
# for modeling quantum interactions", NeurIPS 2017, arXiv:1706.08566
# (the distance-only predecessor).
#
# Native implementation mirroring Python morie.fn.dimNet exactly: the
# same angle formula, the same triplet count, the same Bessel radial
# basis and spherical-harmonic angular basis, and the same
# belief-propagation update.

#' Angle at j between the bonds j-k and j-i
#'
#' Two configurations can share every pairwise distance in the
#' message's own pair and differ here -- which is precisely what a
#' distance-only model cannot see.
#'
#' @param r_k Numeric position of atom k.
#' @param r_j Numeric position of atom j.
#' @param r_i Numeric position of atom i.
#' @return Numeric angle in radians.
#' @export
morie_dimNet_angle_between <- function(r_k, r_j, r_i) {
  a <- as.numeric(r_k); b <- as.numeric(r_j); c <- as.numeric(r_i)
  u <- a - b; v <- c - b
  nu <- sqrt(sum(u * u)); nv <- sqrt(sum(v * v))
  if (nu <= 1e-12 || nv <= 1e-12)
    stop("dimNet: an angle needs three distinct positions")
  cs <- sum(u * v) / (nu * nv)
  acos(max(min(cs, 1), -1))
}

#' Triplet count of a directed graph
#'
#' @param adj Named list mapping each node to its neighbours.
#' @return A list with \code{triplets} and \code{pairs}.
#' @export
morie_dimNet_triplet_count <- function(adj) {
  pairs <- 0; trips <- 0
  for (j in names(adj)) {
    d <- length(setdiff(adj[[j]], j))
    pairs <- pairs + d
    trips <- trips + d * (d - 1)
  }
  list(triplets = trips, pairs = pairs,
       note = paste("directional message passing interacts over",
                    "TRIPLETS; the cost follows the angle count"))
}

#' Spherical Bessel radial basis
#'
#' \code{sqrt(2/c) sin(n pi d/c) / d}, orthogonal on \code{\[0, c\]}.
#'
#' @param d Numeric distance.
#' @param cutoff Numeric cutoff.
#' @param n_basis Integer number of basis functions.
#' @return Numeric vector of length \code{n_basis}.
#' @export
morie_dimNet_bessel_basis <- function(d, cutoff = 5.0, n_basis = 8L) {
  c <- as.numeric(cutoff); dv <- as.numeric(d)
  if (c <= 0) stop("dimNet: the cutoff must be positive")
  if (dv <= 0) stop("dimNet: the distance must be positive")
  vapply(seq_len(as.integer(n_basis)),
         function(n) sqrt(2 / c) * sin(n * pi * dv / c) / dv,
         numeric(1))
}

#' Spherical harmonic angular basis (m = 0)
#'
#' Legendre polynomials in \code{cos(alpha)}, evaluated by the
#' standard three-term recurrence. The \code{m = 0} spherical
#' harmonics, orthogonal on the sphere.
#'
#' @param angle Numeric angle in radians.
#' @param n_basis Integer number of basis functions.
#' @return Numeric vector of length \code{n_basis}.
#' @export
morie_dimNet_spherical_harmonic_basis <- function(angle, n_basis = 4L) {
  x <- cos(as.numeric(angle))
  n <- as.integer(n_basis)
  if (n < 1L) stop("dimNet: at least one basis function is needed")
  out <- numeric(n)
  if (n >= 1L) out[1] <- 1
  if (n >= 2L) out[2] <- x
  for (l in 3:n)
    out[l] <- ((2 * l - 3) * x * out[l - 1] - (l - 2) * out[l - 2]) / (l - 1)
  out
}

#' One round of directional message passing
#'
#' @param messages Named list keyed by directed edges \code{(j, i)}.
#' @param adj Named list mapping each node to its neighbours.
#' @param R Numeric matrix of positions.
#' @param interact Function of \code{(message, rbf, sbf)} returning a
#'   numeric vector.
#' @param update Function of \code{(message, aggregated)} returning a
#'   numeric vector.
#' @param cutoff Numeric cutoff for the Bessel basis.
#' @param n_rbf Integer number of radial basis functions.
#' @param n_sbf Integer number of angular basis functions.
#' @return A list with the updated messages, the count and the
#'   triplets.
#' @export
morie_dimNet_directional_message_pass <- function(messages, adj, R,
                                                   interact, update,
                                                   cutoff = 5.0,
                                                   n_rbf = 8L,
                                                   n_sbf = 4L) {
  pos <- as.matrix(R)
  out <- list()
  keys <- names(messages)
  for (k in keys) {
    parts <- strsplit(k, "->", fixed = TRUE)[[1]]
    j <- parts[1]; i <- parts[2]
    nbrs <- setdiff(adj[[j]], c(i, j))
    acc <- NULL
    for (kk in nbrs) {
      d <- sqrt(sum((pos[as.integer(kk), ] - pos[as.integer(j), ]) ^ 2))
      ang <- morie_dimNet_angle_between(pos[as.integer(kk), ],
                                        pos[as.integer(j), ],
                                        pos[as.integer(i), ])
      contrib <- as.numeric(interact(messages[[k]],
                                     morie_dimNet_bessel_basis(d, cutoff,
                                                                n_rbf),
                                     morie_dimNet_spherical_harmonic_basis(ang,
                                                                            n_sbf)))
      if (is.null(acc)) acc <- contrib else acc <- acc + contrib
    }
    if (is.null(acc)) acc <- rep(0, length(messages[[k]]))
    out[[k]] <- as.numeric(update(messages[[k]], acc))
  }
  list(estimate = out, messages = out, n_messages = length(out),
       triplets = morie_dimNet_triplet_count(adj)$triplets,
       method = paste("directional message passing; Klicpera, Gross &",
                      "Gunnemann (2020)"),
       note = paste("messages carry DIRECTION, so they are",
                    "rotationally equivariant, and interact through",
                    "the ANGLE between them"))
}

# house entry point: the package exports one morie_<module>
morie_dimNet <- morie_dimNet_angle_between
