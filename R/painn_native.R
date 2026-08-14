# PaiNN: equivariant message passing for tensorial properties.
# Sources: Schutt, K. T., Unke, O. T. & Gastegger, M. (2021)
# "Equivariant message passing for the prediction of tensorial
# properties and molecular spectra", ICML 2021, PMLR 139, 9377-9388,
# arXiv:2102.03150. The data-efficiency gap of invariant message
# passing; equivariance of vector features; the polarizable atom
# interaction neural network improving on common molecule benchmarks
# while REDUCING model size and inference time; tensorial properties
# read off directly with 4-5 orders-of-magnitude speed-ups over the
# electronic-structure reference. Schutt, K. T. et al. (2017)
# "SchNet", NeurIPS 2017, arXiv:1706.08566, the invariant predecessor.
# Satorras, V. G., Hoogeboom, E. & Welling, M. (2021) "E(n) Equivariant
# Graph Neural Networks", ICML 2021, arXiv:2102.09844.

# Base R only, faithful translation of painn_python_reference.py.

.PAINN_EPS <- 1e-12

.painn_vec <- function(v) as.numeric(unlist(v))

.painn_mat <- function(M) {
  if (is.matrix(M)) {
    storage.mode(M) <- "double"
    M
  } else {
    do.call(rbind, lapply(M, function(r) as.numeric(r)))
  }
}

vector_norm <- function(v) {
  a <- .painn_mat(v)
  F <- ncol(a)
  out <- numeric(F)
  for (f in seq_len(F)) {
    s <- 0.0
    for (d in seq_len(nrow(a))) s <- s + a[d, f]^2
    out[f] <- sqrt(s)
  }
  out
}

scalar_vector_message <- function(s_j, v_j, r_ij, phi_s, phi_v,
                                  W_rbf) {
  s <- .painn_vec(s_j)
  V <- .painn_mat(v_j)
  r <- .painn_vec(r_ij)
  d <- sqrt(sum(r * r))
  if (d <= .PAINN_EPS)
    stop("painn: two atoms occupy the same position")
  hat <- r / d
  w <- as.numeric(W_rbf(d))
  ds <- as.numeric(phi_s(s, w))
  dv_scale <- as.numeric(phi_v(s, w))
  F <- length(s)
  if (length(ds) != F || length(dv_scale) != 2L * F)
    stop("painn: the message networks are mis-sized (need F scalars and 2F vector gates)")
  D <- length(hat)
  dv <- matrix(0.0, D, F)
  for (a in seq_len(D)) {
    for (f in seq_len(F)) {
      dv[a, f] <- dv_scale[f] * V[a, f] + dv_scale[F + f] * hat[a]
    }
  }
  list(
    ds = ds,
    dv = dv,
    note = "scalar*vector and s*r_hat give VECTORS; nothing mixes the types"
  )
}

gated_update <- function(s, v, U, V, phi) {
  sv <- .painn_vec(s)
  Vv <- .painn_mat(v)
  D <- nrow(Vv)
  F <- ncol(Vv)
  Uv <- Vv %*% t(U)
  Vw <- Vv %*% t(V)
  dot <- numeric(F)
  for (f in seq_len(F)) {
    acc <- 0.0
    for (a in seq_len(D)) acc <- acc + Uv[a, f] * Vw[a, f]
    dot[f] <- acc
  }
  nrm <- vector_norm(Vw)
  out <- phi(sv, dot, nrm)
  ds <- as.numeric(out$ds)
  gate <- as.numeric(out$gate)
  if (length(ds) != F || length(gate) != F)
    stop("painn: the update network is mis-sized")
  dv <- matrix(0.0, D, F)
  for (a in seq_len(D)) {
    for (f in seq_len(F)) dv[a, f] <- gate[f] * Uv[a, f]
  }
  list(
    ds = ds,
    dv = dv,
    scalar_from_vectors = as.numeric(dot),
    note = "the vector-vector inner product is the ONLY path back to the scalar channel, and it is invariant"
  )
}

dipole_moment <- function(charges, R, centre = NULL) {
  q <- .painn_vec(charges)
  pos <- .painn_mat(R)
  if (length(q) != nrow(pos))
    stop("painn: ", length(q), " charges but ", nrow(pos),
         " positions")
  d <- ncol(pos)
  if (is.null(centre)) {
    c <- numeric(d)
    for (a in seq_len(d)) {
      acc <- 0.0
      for (i in seq_len(nrow(pos))) acc <- acc + pos[i, a]
      c[a] <- acc / nrow(pos)
    }
  } else {
    c <- .painn_vec(centre)
  }
  mu <- numeric(d)
  for (a in seq_len(d)) {
    acc <- 0.0
    for (i in seq_len(length(q))) acc <- acc + q[i] * (pos[i, a] - c[a])
    mu[a] <- acc
  }
  list(
    dipole = mu,
    magnitude = sqrt(sum(mu * mu)),
    note = "a VECTOR property; an invariant network cannot produce one without a separate head"
  )
}

equivariance_error <- function(model, s, v, R, Q, tol = 1e-9) {
  pos <- .painn_mat(R)
  d <- ncol(pos)
  Qm <- .painn_mat(Q)
  rot_R <- pos %*% t(Qm)
  V <- .painn_mat(v)
  F <- ncol(V)
  rot_v <- matrix(0.0, d, F)
  for (a in seq_len(d)) {
    for (f in seq_len(F)) {
      acc <- 0.0
      for (b in seq_len(d)) acc <- acc + Qm[a, b] * V[b, f]
      rot_v[a, f] <- acc
    }
  }
  base <- model(s, V, pos)
  other <- model(s, rot_v, rot_R)
  base_s <- as.numeric(base$s)
  other_s <- as.numeric(other$s)
  se <- max(abs(base_s - other_s))
  base_v <- as.numeric(base$v)
  other_v <- as.numeric(other$v)
  want <- matrix(0.0, d, F)
  for (a in seq_len(d)) {
    for (f in seq_len(F)) {
      acc <- 0.0
      for (b in seq_len(d)) acc <- acc + Qm[a, b] * base_v[(b - 1L) * F + f]
      want[a, f] <- acc
    }
  }
  ve <- max(abs(other_v - as.numeric(want)))
  list(
    scalar_error = se,
    vector_error = ve,
    scalars_invariant = se < as.numeric(tol),
    vectors_equivariant = ve < as.numeric(tol),
    note = "both must hold; checking only the scalars passes a model that has lost its vectors"
  )
}

cheatsheet <- function() {
  paste("painn: message passing was LESS DATA EFFICIENT than kernel ",
        "methods, and the diagnosis is INVARIANT representations -- a ",
        "network of scalars can only combine distances and cannot ",
        "emit a tensor at all. Carry BOTH a scalar and a VECTOR ",
        "feature per atom and preserve type: s*s and ||v|| give ",
        "scalars, s*v and s*r_hat give vectors, and v1.v2 is the ",
        "ONLY route back from vectors to scalars -- invariant, so ",
        "the energy stays invariant while direction is used. ",
        "Tensorial properties are read off directly, and the model ",
        "is SMALLER, not larger.", sep = "")
}

# compact alias per ledger/NAMING.md
painnnet <- gated_update

# public names resolved by fn/_lazy_map.json
painn <- gated_update

morie_painn <- gated_update
