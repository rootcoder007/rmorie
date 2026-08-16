# SchNet: continuous-filter convolutions for atoms.
# Sources: Schutt, K. T., Kindermans, P.-J., Sauceda, H. E.,
# Chmiela, S., Tkatchenko, A. & Muller, K.-R. (2017) "SchNet: A
# continuous-filter convolutional neural network for modeling
# quantum interactions", *Advances in Neural Information Processing
# Systems 30 (NeurIPS 2017)*, 991-1001, arXiv:1706.08566. The key
# contributions as stated: the continuous-filter convolutional
# (cfconv) layer as a means to move beyond grid-bound data such as
# images or audio towards objects with arbitrary positions such as
# atoms in molecules and materials; and SchNet as a network designed
# to respect essential quantum-chemical constraints, using cfconv
# layers in R^3 to model interactions of atoms at arbitrary positions,
# delivering rotationally INVARIANT energy predictions and
# rotationally EQUIVARIANT force predictions. Gilmer, J., Schoenholz,
# S. S., Riley, P. F., Vinyals, O. & Dahl, G. E. (2017) "Neural
# Message Passing for Quantum Chemistry", *ICML 2017*, PMLR 70,
# 1263-1272, arXiv:1704.01212. The framework this instantiates;
# implemented in mpfn.

.SCHN_EPS <- 1e-12

#' .schn_mat
#'
#' A step of the schN_native implementation. Called by \code{cfconv}, \code{forces_from_energy}, \code{invariance_error}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x A matrix; the body checks with \code{is.matrix}.
#' @return Nothing; this branch always raises.
#' @export
.schn_mat <- function(x) {
  if (is.list(x) && !is.matrix(x)) return(do.call(rbind, x))
  if (is.matrix(x)) { storage.mode(x) <- "double"; return(x) }
  stop("schn: expected a matrix or list of rows")
}

#' .schn_vec
#'
#' A step of the schN_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x A list; the body checks with \code{is.list}.
#' @return A vector, from \code{as.numeric}.
#' @export
.schn_vec <- function(x) {
  if (is.list(x)) return(as.numeric(unlist(x)))
  as.numeric(x)
}

#' gaussian_expansion
#'
#' A step of the schN_native implementation. Called by \code{cfconv}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param r Coerced to numeric by the body, with \code{as.numeric}.
#' @param mu_min Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.
#' @param mu_max Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{6}.
#' @param n_gaussians Coerced to integer by the body, with \code{as.integer}. Defaults to \code{25}.
#' @param gamma Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @return A numeric value.
#' @export
gaussian_expansion <- function(r, mu_min = 0.0, mu_max = 6.0, n_gaussians = 25,
                                gamma = NULL) {
  n <- as.integer(n_gaussians)
  if (n < 2L) stop("schn: at least 2 Gaussians are needed")
  lo <- as.numeric(mu_min); hi <- as.numeric(mu_max)
  if (hi <= lo) stop("schn: mu_max must exceed mu_min")
  step <- (hi - lo) / (n - 1L)
  g <- if (is.null(gamma)) 1 / (2 * step^2) else as.numeric(gamma)
  mus <- lo + step * (0:(n - 1L))
  diff <- as.numeric(r) - mus
  exp(-g * diff^2)
}

#' cosine_cutoff
#'
#' A step of the schN_native implementation. Called by \code{cfconv}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param r Coerced to numeric by the body, with \code{as.numeric}.
#' @param cutoff Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{5}.
#' @return One of two values, depending on the branch taken.
#' @export
cosine_cutoff <- function(r, cutoff = 5.0) {
  rc <- as.numeric(cutoff)
  if (rc <= 0) stop("schn: the cutoff must be positive")
  v <- as.numeric(r)
  if (v < rc) 0.5 * (cos(pi * v / rc) + 1) else 0
}

#' cfconv
#'
#' A step of the schN_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X Passed to \code{.schn_mat}.
#' @param R Passed to \code{.schn_mat}.
#' @param filter_net Accepted by the signature and not used anywhere in the body.
#' @param cutoff Defaults to \code{5}.
#' @param ... Passed through.
#' @return The value of \code{out}, as built in the body.
#' @export
cfconv <- function(X, R, filter_net, cutoff = 5.0, ...) {
  feats <- .schn_mat(X)
  pos <- .schn_mat(R)
  n <- nrow(feats); d <- ncol(feats)
  if (nrow(pos) != n) stop(sprintf("schn: %d feature rows but %d positions", n, nrow(pos)))
  out <- matrix(0, n, d)
  for (i in 1:n) {
    acc <- numeric(d)
    for (j in 1:n) {
      if (i == j) next
      diff <- pos[i, ] - pos[j, ]
      r <- sqrt(sum(diff^2))
      w <- as.numeric(filter_net(gaussian_expansion(r, ...)))
      fc <- cosine_cutoff(r, cutoff)
      if (length(w) != d) stop(sprintf("schn: the filter is %d-dimensional but the features are %d", length(w), d))
      for (a in 1:d) acc[a] <- acc[a] + feats[j, a] * w[a] * fc
    }
    out[i, ] <- acc
  }
  out
}

#' forces_from_energy
#'
#' A step of the schN_native implementation. Called by \code{invariance_error}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param energy_fn Accepted by the signature and not used anywhere in the body.
#' @param R Passed to \code{.schn_mat}.
#' @param h Numeric; combined arithmetically in the body. Defaults to \code{1e-05}.
#' @return A list with \code{estimate}, \code{forces}, \code{net_force}, \code{method}, \code{note}.
#' @export
forces_from_energy <- function(energy_fn, R, h = 1e-5) {
  pos <- .schn_mat(R)
  n <- nrow(pos); d <- ncol(pos)
  F <- matrix(0, n, d)
  for (i in 1:n) for (a in 1:d) {
    up <- pos; dn <- pos
    up[i, a] <- up[i, a] + h
    dn[i, a] <- dn[i, a] - h
    F[i, a] <- -(as.numeric(energy_fn(up)) - as.numeric(energy_fn(dn))) / (2 * h)
  }
  net <- numeric(d)
  for (a in 1:d) net[a] <- sum(F[, a])
  list(estimate = F, forces = F, net_force = net,
       method = "forces as the negative gradient of the energy; Schutt et al. (2017)",
       note = "conservative and equivariant by construction; a separate force head would be neither")
}

#' invariance_error
#'
#' A step of the schN_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param energy_fn See Usage.
#' @param R Passed to \code{.schn_mat}.
#' @param Q Passed to \code{.schn_mat}.
#' @param g Defaults to \code{NULL}.
#' @return A list with \code{energy_error}, \code{force_error}, \code{energy_invariant}, \code{forces_equivariant}, \code{note}.
#' @export
invariance_error <- function(energy_fn, R, Q, g = NULL) {
  pos <- .schn_mat(R)
  d <- ncol(pos)
  gv <- if (is.null(g)) rep(0, d) else as.numeric(unlist(g))
  Qm <- .schn_mat(Q)
  rot <- t(Qm %*% t(pos)) + matrix(gv, nrow = nrow(pos), ncol = d, byrow = TRUE)
  e0 <- as.numeric(energy_fn(pos))
  e1 <- as.numeric(energy_fn(rot))
  F0 <- forces_from_energy(energy_fn, pos)$forces
  F1 <- forces_from_energy(energy_fn, rot)$forces
  want <- t(Qm %*% t(F0))
  fe <- max(abs(F1 - want))
  list(energy_error = abs(e1 - e0), force_error = fe,
       energy_invariant = abs(e1 - e0) < 1e-8,
       forces_equivariant = fe < 1e-5,
       note = "energy INVARIANT, forces EQUIVARIANT -- two different properties from one design choice")
}

#' .schN_cheatsheet
#'
#' A step of the schN_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
.schN_cheatsheet <- function() {
  paste("schn: a convolution needs a grid and atoms have none, so ",
        "make the filter a FUNCTION of interatomic distance -- a ",
        "continuous-filter convolution, generated by a small network ",
        "from the distance. Positions enter only as ||r_i - r_j||, ",
        "so the energy is rotationally INVARIANT; forces come from ",
        "-dE/dr, so they are EQUIVARIANT and the field is ",
        "conservative, which a separate force head would not be. ",
        "Expand the distance in GAUSSIANS or the filter varies too ",
        "sharply for molecular dynamics; a cosine cutoff keeps ",
        "neighbourhood changes continuous.", sep = "")
}

schnet <- cfconv

morie_schN <- cfconv
