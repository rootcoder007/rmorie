# morie.fn -- function file (rootcoder007/morie)
# R translation of thrtmt module
# Luedtke & van der Laan (2016) "Optimal individualized treatments in
# resource-limited settings", Int. J. Biostat. 12(1), 283-303,
# doi:10.1515/ijb-2015-0007. Sec. 2, eq. (1)-(3) and Theorem 1.
# van der Laan, M. J. & Rose, S. (2018) "Targeted Learning in Data
# Science", Springer, doi:10.1007/978-3-319-65304-4, Ch. 12.

.thrtmt_vec <- function(x) {
  if (is.null(x)) return(numeric(0))
  as.numeric(x)
}

.thrtmt_mat <- function(W) {
  if (is.null(W)) return(matrix(0, nrow=0, ncol=0))
  m <- as.matrix(W)
  if (ncol(m) == 0 && nrow(m) == 0) return(matrix(0, nrow=0, ncol=0))
  m
}

.thrtmt_design <- function(rows, n) {
  if (length(rows) == 0 || n == 0) {
    return(matrix(0, nrow=n, ncol=0))
  }
  # rows is a list of numeric vectors
  nr <- length(rows)
  nc <- max(sapply(rows, length))
  Z <- matrix(0, nrow=nr, ncol=nc)
  for (i in seq_len(nr)) {
    r <- rows[[i]]
    if (length(r) > 0) Z[i, seq_along(r)] <- r
  }
  Z
}

.thrtmt_lstsq <- function(Z, y, ridge) {
  # Z: n x p, y: length n
  # Returns b solving (Z'Z + ridge I) b = Z' y
  if (ncol(Z) == 0) {
    return(numeric(0))
  }
  p <- ncol(Z)
  G <- crossprod(Z) + diag(ridge, p, p)
  rhs <- crossprod(Z, y)
  # Solve
  b <- tryCatch(solve(G, rhs), error = function(e) {
    # Fallback for singular
    sv <- svd(G)
    d <- sv$d
    d_inv <- ifelse(d > 1e-12, 1/d, 0)
    sv$v %*% (d_inv * crossprod(sv$u, rhs))
  })
  as.numeric(b)
}

.thrtmt_matvec <- function(Z, b) {
  if (length(b) == 0 || ncol(Z) == 0) {
    return(rep(0, nrow(Z)))
  }
  as.numeric(Z %*% b)
}

thrtmt_blip_function <- function(y, A, W, V = NULL, ridge = 1e-8) {
  yv <- .thrtmt_vec(y)
  av <- .thrtmt_vec(A)
  n <- length(yv)
  if (length(av) != n) {
    stop(sprintf("blip_function: %d outcomes but %d treatments", n, length(av)))
  }
  if (any(!(av %in% c(0.0, 1.0)))) {
    stop("blip_function: treatment must be binary 0/1")
  }
  
  if (!is.null(W)) {
    Wm <- .thrtmt_mat(W)
    if (nrow(Wm) != n) {
      # W might be a list, try as matrix
      Wm <- .thrtmt_mat(W)
    }
    p <- ncol(Wm)
  } else {
    Wm <- matrix(0, nrow=n, ncol=0)
    p <- 0
  }
  
  # Build design matrix: [1, a, W[1], ..., W[p], a*W[1], ..., a*W[p]]
  rows <- vector("list", n)
  for (i in seq_len(n)) {
    row <- c(1.0, av[i])
    if (p > 0) {
      w_row <- if (is.null(dim(Wm))) numeric(0) else Wm[i, ]
      row <- c(row, w_row, av[i] * w_row)
    }
    rows[[i]] <- row
  }
  Z <- .thrtmt_design(rows, n)
  b <- .thrtmt_lstsq(Z, yv, ridge)
  
  q1 <- numeric(n)
  q0 <- numeric(n)
  blip_w <- numeric(n)
  for (i in seq_len(n)) {
    r1 <- c(1.0, 1.0)
    r0 <- c(1.0, 0.0)
    if (p > 0) {
      w_row <- Wm[i, ]
      r1 <- c(r1, w_row, 1.0 * w_row)
      r0 <- c(r0, w_row, 0.0 * w_row)
    }
    q1[i] <- sum(b * r1)
    q0[i] <- sum(b * r0)
    blip_w[i] <- q1[i] - q0[i]
  }
  
  if (is.null(V)) {
    return(list(blip = blip_w, 
                info = list(coef = b, q1 = q1, q0 = q0)))
  }
  
  # E[blip | V]: regress the blip on the summary V
  Vm <- .thrtmt_mat(V)
  if (nrow(Vm) != n) {
    Vm <- .thrtmt_mat(V)
  }
  pv <- ncol(Vm)
  v_rows <- vector("list", n)
  for (i in seq_len(n)) {
    v_row <- c(1.0)
    if (pv > 0) {
      v_row <- c(v_row, Vm[i, ])
    }
    v_rows[[i]] <- v_row
  }
  Zv <- .thrtmt_design(v_rows, n)
  bv <- .thrtmt_lstsq(Zv, blip_w, ridge)
  proj <- .thrtmt_matvec(Zv, bv)
  
  return(list(blip = as.numeric(proj),
              info = list(coef = b, v_coef = bv, blip_w = blip_w,
                          q1 = q1, q0 = q0)))
}

#' @rdname thrtmt_blip_function
#' @export
morie_thrtmt <- thrtmt_blip_function

#' @rdname thrtmt_blip_function
#' @export
morie_thrtmt <- thrtmt_blip_function

#' @rdname thrtmt_blip_function
#' @export
morie_thrtmt <- thrtmt_blip_function

#' @rdname thrtmt_blip_function
#' @export
morie_thrtmt <- thrtmt_blip_function

#' @rdname thrtmt_blip_function
#' @export
morie_thrtmt <- thrtmt_blip_function

#' @rdname thrtmt_blip_function
#' @export
morie_thrtmt <- thrtmt_blip_function

#' @rdname thrtmt_blip_function
#' @export
morie_thrtmt <- thrtmt_blip_function

#' @rdname thrtmt_blip_function
#' @export
morie_thrtmt <- thrtmt_blip_function

#' @rdname thrtmt_blip_function
#' @export
morie_thrtmt <- thrtmt_blip_function
