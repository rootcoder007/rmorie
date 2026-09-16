# E(n)-equivariant graph neural networks.
# Sources: Satorras, V. G., Hoogeboom, E. & Welling, M. (2021) "E(n)
# Equivariant Graph Neural Networks", *Proceedings of the 38th
# International Conference on Machine Learning (ICML 2021)*, PMLR
# 139, 9323-9332, arXiv:2102.09844. Sec. 3 (the EGCL of eqs. (3)-(6),
# with C = 1/(M-1); the statement that eq. (4) is the main difference
# from standard GNNs and the reason equivariances 1 and 2 are
# preserved). Sec. 3.1 (the equivariance condition Qx + g; that m_ij
# is E(n) invariant because it depends on positions only through
# squared distances; that the weighted sum of differences transforms
# as a type-1 vector; and the inductive argument for composed
# layers). Sec. 3.2 (the momentum variant replacing eq. (4)). Thomas,
# N., Smidt, T., Kearnes, S., Yang, L., Li, L., Kohlhoff, K. &
# Riley, P. (2018) "Tensor Field Networks: Rotation- and
# Translation-Equivariant Neural Networks for 3D Point Clouds",
# arXiv:1802.08219. The higher-order-representation approach this
# avoids.
#
# Native implementation mirroring Python morie.fn.egnnL exactly: the
# same four-equation layer (m_ij, x-update, m_i, h-update), the same
# E(n)-invariance argument, the same C = 1/(n-1) averaging, the same
# momentum variant (eq. 4 replaced by a velocity update integrated
# with the same X <- X + dt * Vn step), and the same equivariance
# error measurement (max abs gap between transforming the input vs
# transforming the output) with the same 1e-9 tolerance.

.EGNNL_MODES <- c("position", "momentum")

#' .sqdist
#'
#' A step of the egnnL_native implementation. Called by \code{edge_message}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a Numeric; combined arithmetically in the body.
#' @param b Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
#' @examples
#' A <- matrix(c(4, 1, 0.5, 1, 3, 0.8, 0.5, 0.8, 2), nrow = 3)
#' b <- c(1.5, 2.5, 3.5)
#' res <- .sqdist(a = A, b = b)
#' res
.sqdist <- function(a, b) {
  a <- as.numeric(a)
  b <- as.numeric(b)
  sum((a - b)^2)
}

#' Eq. (3). Positions enter ONLY as ||x_i - x_j||^2, which is what
#'
#' makes the message invariant.
#'
#' @param h_i Coerced to numeric by the body, with \code{as.numeric}.
#' @param h_j Coerced to numeric by the body, with \code{as.numeric}.
#' @param x_i Passed to \code{.sqdist}.
#' @param x_j Passed to \code{.sqdist}.
#' @param phi_e Accepted by the signature and not used anywhere in the body.
#' @param a_ij Passed to \code{phi_e}.
#' @return The value of \code{phi_e}.
#' @export
#' @examples
#' phi_e <- function(hi, hj, d2, a) c(hi + hj, d2)
#' edge_message(c(0.2, -0.1), c(0.4, 0.3), c(1, 0, 0), c(0, 1, 0), phi_e)
edge_message <- function(h_i, h_j, x_i, x_j, phi_e, a_ij = NULL) {
  # Eq. (3). Positions enter ONLY as ||x_i - x_j||^2, which is what
  # makes the message invariant.
  phi_e(as.numeric(h_i), as.numeric(h_j), .sqdist(x_i, x_j), a_ij)
}

#' Eq. (4): x_i + C sum_j (x_i - x_j) phi_x(m_\{ij\})
#'
#' A step of the egnnL_native implementation. Called by \code{egcl}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A vector; its length is taken and its elements indexed.
#' @param M A vector; indexed elementwise.
#' @param phi_x Accepted by the signature and not used anywhere in the body.
#' @param C Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @return The value of \code{out}, as built in the body.
#' @export
#' @examples
#' set.seed(1)
#' X <- lapply(1:3, function(i) rnorm(3))
#' phi_e <- function(hi, hj, d2, a) c(hi + hj, d2)
#' phi_x <- function(m) sum(m) * 0.01
#' M <- lapply(seq_len(3), function(i) vector("list", 3))
#' for (i in 1:3) for (j in 1:3) if (i != j)
#'   M[[i]][[j]] <- edge_message(rnorm(2), rnorm(2), X[[i]], X[[j]], phi_e)
#' Xn <- coord_update(X, M, phi_x)
#' length(Xn)
coord_update <- function(X, M, phi_x, C = NULL) {
  # Eq. (4): x_i + C sum_j (x_i - x_j) phi_x(m_{ij}).
  n <- length(X)
  if (n < 2L)
    stop("egnnL: need at least 2 particles")
  c <- if (is.null(C)) 1 / (n - 1) else as.numeric(C)
  out <- vector("list", n)
  for (i in seq_len(n)) {
    acc <- as.numeric(X[[i]])
    for (j in seq_len(n)) {
      if (j == i) next
      w <- as.numeric(phi_x(M[[i]][[j]]))
      xi <- as.numeric(X[[i]])
      xj <- as.numeric(X[[j]])
      acc <- acc + c * (xi - xj) * w
    }
    out[[i]] <- acc
  }
  out
}

#' egcl
#'
#' A step of the egnnL_native implementation. Called by \code{run_egnn}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param H A vector; its length is taken and its elements indexed.
#' @param X A vector; indexed elementwise.
#' @param phi_e Passed to \code{edge_message}.
#' @param phi_x Passed to \code{coord_update}.
#' @param phi_h Accepted by the signature and not used anywhere in the body.
#' @param A Optional; may be \code{NULL}. A vector; indexed elementwise.
#' @param C Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @param V Optional; may be \code{NULL}. A vector; indexed elementwise.
#' @param mode Compared against \code{"position"}. Defaults to \code{"position"}.
#' @param phi_v The body requires: egnnL: the momentum variant needs V and phi_v.
#' @param dt Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.
#' @return A list with \code{H}, \code{X}, \code{V}, \code{messages}.
#' @export
#' @examples
#' set.seed(1)
#' H <- lapply(1:4, function(i) rnorm(2))
#' X <- lapply(1:4, function(i) rnorm(3))
#' phi_e <- function(hi, hj, d2, a) c(hi + hj, d2)
#' phi_x <- function(m) sum(m) * 0.01
#' phi_h <- function(hi, agg) hi + 0.1 * agg[seq_along(hi)]
#' r <- egcl(H, X, phi_e, phi_x, phi_h)
#' names(r)
egcl <- function(H, X, phi_e, phi_x, phi_h, A = NULL, C = NULL,
                 V = NULL, mode = "position", phi_v = NULL,
                 dt = 1) {
  # One equivariant graph convolutional layer, eqs. (3)-(6).
  if (!(mode %in% .EGNNL_MODES))
    stop("egnnL: mode must be one of ",
         paste(.EGNNL_MODES, collapse = ", "), ", got ",
         deparse(mode))
  n <- length(H)
  M <- vector("list", n)
  for (i in seq_len(n)) {
    M[[i]] <- vector("list", n)
    for (j in seq_len(n)) {
      if (i != j) {
        a <- if (is.null(A)) NULL else A[[paste(i, j, sep = ",")]]
        M[[i]][[j]] <- edge_message(H[[i]], H[[j]], X[[i]], X[[j]],
                                    phi_e, a)
      }
    }
  }
  if (mode == "position") {
    Xn <- coord_update(X, M, phi_x, C)
    Vn <- V
  } else {
    if (is.null(V) || is.null(phi_v))
      stop("egnnL: the momentum variant needs V and phi_v")
    c <- if (is.null(C)) 1 / (n - 1) else as.numeric(C)
    Vn <- vector("list", n)
    for (i in seq_len(n)) {
      acc <- as.numeric(phi_v(H[[i]])) * as.numeric(V[[i]])
      for (j in seq_len(n)) {
        if (j == i) next
        w <- as.numeric(phi_x(M[[i]][[j]]))
        xi <- as.numeric(X[[i]])
        xj <- as.numeric(X[[j]])
        acc <- acc + c * (xi - xj) * w
      }
      Vn[[i]] <- acc
    }
    Xn <- lapply(seq_len(n), function(i)
      as.numeric(X[[i]]) + as.numeric(dt) * Vn[[i]])
  }
  Hn <- vector("list", n)
  for (i in seq_len(n)) {
    mi <- NULL
    for (j in seq_len(n)) {
      if (j == i) next
      mij <- as.numeric(M[[i]][[j]])
      if (is.null(mi)) mi <- mij
      else mi <- mi + mij
    }
    Hn[[i]] <- phi_h(as.numeric(H[[i]]), mi)
  }
  list(H = Hn, X = Xn, V = Vn, messages = M)
}

#' run_egnn
#'
#' A step of the egnnL_native implementation. Called by \code{e_gcn}, \code{egnn_layer},
#' \code{egnnlayer} and 5 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param H Iterated over elementwise, with \code{lapply}.
#' @param X Iterated over elementwise, with \code{lapply}.
#' @param layers Coerced to integer by the body, with \code{as.integer}.
#' @param phi_e Passed to \code{egcl}.
#' @param phi_x Passed to \code{egcl}.
#' @param phi_h Passed to \code{egcl}.
#' @param A Passed to \code{egcl}.
#' @param C Passed to \code{egcl}.
#' @return A list with \code{estimate}, \code{H}, \code{X}, \code{layers}, \code{method},
#' \code{note}.
#' @export
#' @examples
#' set.seed(1)
#' H <- lapply(1:4, function(i) rnorm(2))
#' X <- lapply(1:4, function(i) rnorm(3))
#' phi_e <- function(hi, hj, d2, a) c(hi + hj, d2)
#' phi_x <- function(m) sum(m) * 0.01
#' phi_h <- function(hi, agg) hi + 0.1 * agg[seq_along(hi)]
#' r <- run_egnn(H, X, 2L, phi_e, phi_x, phi_h)
#' str(r, max.level = 1)
run_egnn <- function(H, X, layers, phi_e, phi_x, phi_h, A = NULL,
                     C = NULL) {
  # Compose layers; equivariance is preserved inductively.
  h <- lapply(H, function(r) as.numeric(r))
  x <- lapply(X, function(r) as.numeric(r))
  for (k in seq_len(as.integer(layers))) {
    r <- egcl(h, x, phi_e, phi_x, phi_h, A, C)
    h <- r$H
    x <- r$X
  }
  list(estimate = list(h, x), H = h, X = x,
       layers = as.integer(layers),
       method = "EGNN; Satorras, Hoogeboom & Welling (2021) eqs. (3)-(6)",
       note = "h is E(n) INVARIANT, x is E(n) EQUIVARIANT")
}

#' morie_egnnL_equivariance_error
#'
#' A step of the egnnL_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param H Passed to \code{run_egnn}.
#' @param X A vector; its length is taken and its elements indexed.
#' @param phi_e Passed to \code{run_egnn}.
#' @param phi_x Passed to \code{run_egnn}.
#' @param phi_h Passed to \code{run_egnn}.
#' @param Q A vector; indexed elementwise.
#' @param g A vector; indexed elementwise.
#' @param layers Passed to \code{run_egnn}. Defaults to \code{2}.
#' @param C Passed to \code{run_egnn}.
#' @return A list with \code{coordinate_error}, \code{feature_error}, \code{equivariant},
#' \code{invariant}, \code{note}.
#' @export
#' @examples
#' set.seed(1)
#' H <- lapply(1:4, function(i) rnorm(2))
#' X <- lapply(1:4, function(i) rnorm(3))
#' phi_e <- function(hi, hj, d2, a) c(hi + hj, d2)
#' phi_x <- function(m) sum(m) * 0.01
#' phi_h <- function(hi, agg) hi + 0.1 * agg[seq_along(hi)]
#' th <- pi / 5
#' Qm <- matrix(c(cos(th), -sin(th), 0, sin(th), cos(th), 0, 0, 0, 1), 3, 3)
#' Q <- lapply(seq_len(3), function(i) Qm[i, ])
#' g <- c(0.5, -0.2, 0.1)
#' r <- morie_egnnL_equivariance_error(H, X, phi_e, phi_x, phi_h, Q, g,
#'                                     layers = 2L)
#' c(r$equivariant, r$invariant)
morie_egnnL_equivariance_error <- function(H, X, phi_e, phi_x, phi_h, Q, g,
                               layers = 2, C = NULL) {
  # Transform the input, run, and compare against transforming the
  # output. The property is stated as an equality; this measures the
  # gap.
  n <- length(X)
  d <- length(X[[1]])
  base <- run_egnn(H, X, layers, phi_e, phi_x, phi_h, C = C)
  Xt <- lapply(seq_len(n), function(i) {
    xi <- as.numeric(X[[i]])
    out <- rep(0, d)
    for (a in seq_len(d)) {
      s <- 0
      for (b in seq_len(d)) s <- s + Q[[a]][[b]] * xi[b]
      out[a] <- s + g[a]
    }
    out
  })
  other <- run_egnn(H, Xt, layers, phi_e, phi_x, phi_h, C = C)
  want <- lapply(seq_len(n), function(i) {
    xi <- as.numeric(base$X[[i]])
    out <- rep(0, d)
    for (a in seq_len(d)) {
      s <- 0
      for (b in seq_len(d)) s <- s + Q[[a]][[b]] * xi[b]
      out[a] <- s + g[a]
    }
    out
  })
  ex <- max(sapply(seq_len(n), function(i)
    max(abs(as.numeric(other$X[[i]]) - as.numeric(want[[i]])))))
  eh <- max(sapply(seq_len(n), function(i)
    max(abs(as.numeric(other$H[[i]]) - as.numeric(base$H[[i]])))))
  list(coordinate_error = ex, feature_error = eh,
       equivariant = ex < 1e-9, invariant = eh < 1e-9,
       note = "x must transform WITH Q and g; h must not move at all")
}

#' .egnnL_cheatsheet
#'
#' A step of the egnnL_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
#' @examples
#' res <- .egnnL_cheatsheet()
#' res
.egnnL_cheatsheet <- function() {
  paste0("egnnL: equivariance to translation, rotation and reflection ",
         "WITHOUT spherical harmonics. m_ij depends on position only ",
         "through ||x_i - x_j||^2, so it is invariant; x_i <- x_i + C ",
         "sum_j (x_i - x_j) phi_x(m_ij) adds a weighted sum of ",
         "RELATIVE DIFFERENCES, which transforms as a vector. That ",
         "one equation is the entire difference from a standard GNN. ",
         "C = 1/(M-1). Composition preserves both properties by ",
         "induction. A momentum variant replaces eq. (4) when velocity ",
         "matters.")
}

# compact alias per ledger/NAMING.md
#' Compact alias per ledger/NAMING.md
#'
#' A step of the egnnL_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param H Passed to \code{run_egnn}.
#' @param X Passed to \code{run_egnn}.
#' @param layers Passed to \code{run_egnn}.
#' @param phi_e Passed to \code{run_egnn}.
#' @param phi_x Passed to \code{run_egnn}.
#' @param phi_h Passed to \code{run_egnn}.
#' @param A Passed to \code{run_egnn}.
#' @param C Passed to \code{run_egnn}.
#' @return The value of \code{run_egnn}.
#' @export
#' @examples
#' set.seed(1)
#' H <- lapply(1:4, function(i) rnorm(2))
#' X <- lapply(1:4, function(i) rnorm(3))
#' phi_e <- function(hi, hj, d2, a) c(hi + hj, d2)
#' phi_x <- function(m) sum(m) * 0.01
#' phi_h <- function(hi, agg) hi + 0.1 * agg[seq_along(hi)]
#' r <- equivariantgnn(H, X, layers = 2L, phi_e, phi_x, phi_h)
#' length(r$X)
equivariantgnn <- function(H, X, layers, phi_e, phi_x, phi_h, A = NULL,
                           C = NULL) {
  run_egnn(H, X, layers, phi_e, phi_x, phi_h, A, C)
}

# public names resolved by fn/_lazy_map.json
#' Public names resolved by fn/_lazy_map.json
#'
#' A step of the egnnL_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param H Passed to \code{run_egnn}.
#' @param X Passed to \code{run_egnn}.
#' @param layers Passed to \code{run_egnn}.
#' @param phi_e Passed to \code{run_egnn}.
#' @param phi_x Passed to \code{run_egnn}.
#' @param phi_h Passed to \code{run_egnn}.
#' @param A Passed to \code{run_egnn}.
#' @param C Passed to \code{run_egnn}.
#' @return The value of \code{run_egnn}.
#' @export
#' @examples
#' set.seed(1)
#' H <- lapply(1:4, function(i) rnorm(2))
#' X <- lapply(1:4, function(i) rnorm(3))
#' phi_e <- function(hi, hj, d2, a) c(hi + hj, d2)
#' phi_x <- function(m) sum(m) * 0.01
#' phi_h <- function(hi, agg) hi + 0.1 * agg[seq_along(hi)]
#' r <- egnn_layer(H, X, layers = 1L, phi_e, phi_x, phi_h)
#' length(r$H)
egnn_layer <- function(H, X, layers, phi_e, phi_x, phi_h, A = NULL,
                       C = NULL) {
  run_egnn(H, X, layers, phi_e, phi_x, phi_h, A, C)
}
#' egnnlayer
#'
#' A step of the egnnL_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param H Passed to \code{run_egnn}.
#' @param X Passed to \code{run_egnn}.
#' @param layers Passed to \code{run_egnn}.
#' @param phi_e Passed to \code{run_egnn}.
#' @param phi_x Passed to \code{run_egnn}.
#' @param phi_h Passed to \code{run_egnn}.
#' @param A Passed to \code{run_egnn}.
#' @param C Passed to \code{run_egnn}.
#' @return The value of \code{run_egnn}.
#' @export
#' @examples
#' set.seed(1)
#' H <- lapply(1:4, function(i) rnorm(2))
#' X <- lapply(1:4, function(i) rnorm(3))
#' phi_e <- function(hi, hj, d2, a) c(hi + hj, d2)
#' phi_x <- function(m) sum(m) * 0.01
#' phi_h <- function(hi, agg) hi + 0.1 * agg[seq_along(hi)]
#' r <- egnnlayer(H, X, layers = 1L, phi_e, phi_x, phi_h)
#' length(r$X)
egnnlayer <- function(H, X, layers, phi_e, phi_x, phi_h, A = NULL,
                      C = NULL) {
  run_egnn(H, X, layers, phi_e, phi_x, phi_h, A, C)
}

# morie entry point: matches the Python payload keys
#' Morie entry point: matches the Python payload keys
#'
#' A step of the egnnL_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param H Passed to \code{run_egnn}.
#' @param X Passed to \code{run_egnn}.
#' @param layers Passed to \code{run_egnn}.
#' @param phi_e Passed to \code{run_egnn}.
#' @param phi_x Passed to \code{run_egnn}.
#' @param phi_h Passed to \code{run_egnn}.
#' @param A Passed to \code{run_egnn}.
#' @param C Passed to \code{run_egnn}.
#' @return The value of \code{run_egnn}.
#' @export
#' @examples
#' set.seed(1)
#' H <- lapply(1:4, function(i) rnorm(2))
#' X <- lapply(1:4, function(i) rnorm(3))
#' phi_e <- function(hi, hj, d2, a) c(hi + hj, d2)
#' phi_x <- function(m) sum(m) * 0.01
#' phi_h <- function(hi, agg) hi + 0.1 * agg[seq_along(hi)]
#' r <- morie_egnnL(H, X, layers = 2L, phi_e, phi_x, phi_h)
#' length(r$H)
morie_egnnL <- function(H, X, layers, phi_e, phi_x, phi_h, A = NULL,
                        C = NULL) {
  run_egnn(H, X, layers, phi_e, phi_x, phi_h, A, C)
}
