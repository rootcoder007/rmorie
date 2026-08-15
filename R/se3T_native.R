# morie.fn -- function file (rootcoder007/morie)
# SE(3)-Transformers: attention that commutes with rotation.
#
# For a point cloud there is no canonical orientation, so a model
# without a symmetry constraint must **learn** that a rotated molecule is
# the same molecule -- from data, imperfectly, and with no guarantee at
# test time. Equivariance builds it in:
#
# .. math:: f(R x + t) = R\,f(x) \quad\text{(type-1 output)},\qquad
#           f(R x + t) = f(x) \quad\text{(type-0 output)},
#
# so a transformation of the input appears as the equivalent
# transformation of the output, exactly. That is a generalisation of the
# translational weight-tying convolutions already have, and it restricts
# the learnable function space to one that respects the task's symmetry.
#
# **Attention makes it work on graphs of varying size**; equivariance
# makes it robust. The construction is: attention *weights* are built
# from **invariant** quantities -- pairwise distances and the relative
# geometry -- so they are unchanged by a global rotation, while the
# *values* being aggregated are equivariant. An invariant convex
# combination of equivariant vectors is equivariant, which is the whole
# proof and the reason ``se3_attention`` returns both.
#
# **The anchor is the definition, not a proxy.** Rotate the input, run
# the layer, and check the output equals the rotated original output to
# machine precision. A model that merely *learned* rotation invariance
# fails that check by a visible margin; this one cannot fail it without
# a bug.
#
# References
# ----------
# Fuchs, F. B., Worrall, D. E., Fischer, V. & Welling, M. (2020)
# "SE(3)-Transformers: 3D Roto-Translation Equivariant Attention
# Networks", *Advances in Neural Information Processing Systems 33
# (NeurIPS 2020)*, 1970-1981, arXiv:2006.10503. The abstract and Sec.
# 1-3: a variant of the self-attention module for 3D point clouds and
# graphs which is equivariant under continuous 3D roto-translations;
# that equivariance is important to ensure stable and predictable
# performance in the presence of nuisance transformations of the input,
# with increased weight-tying as a positive corollary; that
# SE(3)-equivariance generalises the translational weight-tying of
# conventional convolutions to roto-translations in 3D and restricts the
# space of learnable functions to a subspace adhering to the symmetries
# of the task; the use of self-attention to operate on point clouds and
# graphs with varying numbers of points; and that the model outperforms
# both a strong non-equivariant attention baseline and an equivariant
# model without attention.
#
# Thomas, N., Smidt, T., Kearnes, S., Yang, L., Li, L., Kohlhoff, K. &
# Riley, P. (2018) "Tensor field networks: Rotation- and
# translation-equivariant neural networks for 3D point clouds",
# arXiv:1802.08219. The equivariant kernel basis.
#
# Vaswani, A. et al. (2017) "Attention Is All You Need", *NIPS 2017*,
# 5998-6008, arXiv:1706.03762. The attention module being made
# equivariant.

# Tolerance constant
.se3T_EPS <- 1e-12

# Helper: coerce input to a numeric vector
.se3T_vec <- function(x) {
  if (is.null(x)) return(numeric(0))
  as.numeric(unlist(x))
}

# Helper: coerce input to a numeric matrix
.se3T_mat <- function(x) {
  if (is.matrix(x)) {
    return(matrix(as.numeric(x), nrow=nrow(x), ncol=ncol(x)))
  }
  if (is.list(x)) {
    n <- length(x)
    if (n == 0) return(matrix(numeric(0), nrow=0, ncol=3))
    m <- length(x[[1]])
    result <- matrix(0, nrow=n, ncol=m)
    for (i in seq_len(n)) {
      result[i, ] <- as.numeric(x[[i]])
    }
    return(result)
  }
  mat <- as.matrix(x)
  matrix(as.numeric(mat), nrow=nrow(mat), ncol=ncol(mat))
}

# Apply 3x3 rotation matrix R to a 3-vector v
.se3T_apply <- function(R, v) {
  v <- as.numeric(v)
  c(R[1,1]*v[1] + R[1,2]*v[2] + R[1,3]*v[3],
    R[2,1]*v[1] + R[2,2]*v[2] + R[2,3]*v[3],
    R[3,1]*v[1] + R[3,2]*v[2] + R[3,3]*v[3])
}

# Rodrigues' formula -- a genuine element of SO(3)
morie_se3T_rotation_matrix <- function(axis, angle) {
  a <- .se3T_vec(axis)
  n <- sqrt(sum(a * a))
  if (n <= .se3T_EPS) stop("se3T: the rotation axis is zero")
  x <- a[1] / n
  y <- a[2] / n
  z <- a[3] / n
  cc <- cos(as.numeric(angle))
  ss <- sin(as.numeric(angle))
  C <- 1.0 - cc
  matrix(c(
    cc + x*x*C,       x*y*C - z*ss,   x*z*C + y*ss,
    y*x*C + z*ss,     cc + y*y*C,     y*z*C - x*ss,
    z*x*C - y*ss,     z*y*C + x*ss,   cc + z*z*C
  ), nrow=3, ncol=3, byrow=TRUE)
}

# Invariant features: the distance is invariant, the direction is equivariant
morie_se3T_invariant_features <- function(positions, i, j) {
  P <- .se3T_mat(positions)
  d <- P[j, ] - P[i, ]
  r <- sqrt(sum(d * d))
  list(
    distance = r,
    direction = if (r > .se3T_EPS) as.numeric(d / r) else c(0.0, 0.0, 0.0),
    note = "the DISTANCE is invariant; the direction is equivariant and must not enter the weights"
  )
}

# Radial kernel: a learnable function of the distance ALONE
morie_se3T_radial_kernel <- function(distance, weights=NULL, sigma=1.0) {
  r <- as.numeric(distance)
  if (r < 0.0) stop("se3T: a distance cannot be negative")
  if (is.null(weights)) {
    w <- c(1.0)
  } else {
    w <- .se3T_vec(weights)
  }
  s <- as.numeric(sigma)
  if (s <= 0.0) stop("se3T: sigma must be positive")
  m_vec <- 0:(length(w) - 1)
  sum(w * exp(-((r - m_vec)^2) / (2.0 * s * s)))
}

# SE(3) attention: invariant weights over equivariant values
morie_se3T_se3_attention <- function(positions, type0, type1, weights=NULL,
                                    sigma=1.0, temperature=1.0) {
  P <- .se3T_mat(positions)
  S <- .se3T_vec(type0)
  V <- .se3T_mat(type1)
  n <- nrow(P)
  if (length(S) != n || nrow(V) != n) {
    stop(sprintf("se3T: %d positions, %d scalars, %d vectors",
                 n, length(S), nrow(V)))
  }
  if (ncol(V) != 3) stop("se3T: type-1 features must be 3-vectors")
  t <- as.numeric(temperature)
  if (t <= 0.0) stop("se3T: the temperature must be positive")

  out_v <- matrix(0, nrow=n, ncol=3)
  out_s <- numeric(n)
  W <- matrix(0, nrow=n, ncol=n)

  for (i in seq_len(n)) {
    logits <- numeric(n)
    for (j in seq_len(n)) {
      g <- morie_se3T_invariant_features(P, i, j)
      logits[j] <- (S[i] * S[j] +
                    morie_se3T_radial_kernel(g$distance, weights, sigma)) / t
    }
    m <- max(logits)
    e <- exp(logits - m)
    z <- sum(e)
    w <- e / z
    W[i, ] <- w
    for (a in seq_len(3)) {
      out_v[i, a] <- sum(w * V[, a])
    }
    out_s[i] <- sum(w * S)
  }

  list(
    type1 = out_v,
    type0 = out_s,
    weights = W,
    note = "invariant weights, equivariant values -- an invariant convex combination of equivariant vectors is equivariant"
  )
}

# Check equivariance: rotate and translate the input; the output must follow
morie_se3T_check_equivariance <- function(positions, type0, type1, layer=NULL,
                                          axis=c(0.3, -0.7, 0.4), angle=1.1,
                                          translation=c(2.0, -1.0, 0.5),
                                          tol=1e-9) {
  f <- if (is.null(layer)) morie_se3T_se3_attention else layer
  P <- .se3T_mat(positions)
  R <- morie_se3T_rotation_matrix(axis, angle)
  tv <- .se3T_vec(translation)

  base <- f(P, type0, type1)

  # Transform positions: R*p + t
  P_moved <- matrix(0, nrow=nrow(P), ncol=3)
  for (i in seq_len(nrow(P))) {
    P_moved[i, ] <- .se3T_apply(R, P[i, ]) + tv
  }

  # Transform type1: R*v
  V <- .se3T_mat(type1)
  V_moved <- matrix(0, nrow=nrow(V), ncol=3)
  for (i in seq_len(nrow(V))) {
    V_moved[i, ] <- .se3T_apply(R, V[i, ])
  }

  moved <- f(P_moved, type0, V_moved)

  dev_v <- 0.0
  for (i in seq_len(nrow(P))) {
    want <- .se3T_apply(R, base$type1[i, ])
    got <- moved$type1[i, ]
    dev_v <- max(dev_v, max(abs(want - got)))
  }

  dev_s <- max(abs(base$type0 - moved$type0))
  dev_w <- max(abs(base$weights - moved$weights))

  list(
    estimate = dev_v,
    type1_deviation = dev_v,
    type0_deviation = dev_s,
    weight_deviation = dev_w,
    equivariant = (dev_v < as.numeric(tol)) && (dev_s < as.numeric(tol)),
    weights_invariant = dev_w < as.numeric(tol),
    method = "SE(3)-equivariance check; Fuchs et al. (2020)",
    note = "type-1 outputs ROTATE with the input, type-0 outputs and the attention weights do not move at all"
  )
}

# Cheatsheet
morie_se3T_cheatsheet <- function() {
  paste0("se3T: a point cloud has no canonical orientation, so ",
         "without a symmetry constraint the model must LEARN that a ",
         "rotated molecule is the same molecule -- imperfectly, with ",
         "no test-time guarantee. Equivariance builds it in: ",
         "f(Rx+t) = R f(x) for type-1 outputs, f(x) for type-0. ",
         "Mechanism: attention WEIGHTS are built only from ",
         "INVARIANT quantities (distances, scalars), while the ",
         "VALUES aggregated are equivariant -- an invariant convex ",
         "combination of equivariant vectors is equivariant. The ",
         "radial kernel sees the DISTANCE alone. Check it by ",
         "rotating the input and comparing to machine precision.")
}

# compact alias per ledger/NAMING.md
morie_se3T_se3transformer <- morie_se3T_se3_attention

# public names resolved by fn/_lazy_map.json
morie_se3T_se3_transformer <- morie_se3T_se3_attention
