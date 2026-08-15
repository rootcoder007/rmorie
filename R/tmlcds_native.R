# tmlcds.R -- function file (rootcoder007/morie)
# Collaborative targeted minimum loss-based estimation (C-TMLE).
# ... (header comments) ...

# RichResult equivalent - just use named list
.RichResult <- function(payload) {
  payload  # R's named list does what RichResult does
}

# Sigmoid function
.tmlcds_sigmoid <- function(x) {
  1.0 / (1.0 + exp(-x))
}

# Logit
.tmlcds_logit <- function(p) {
  log(p / (1.0 - p))
}

# Convert to vector
.tmlcds_vec <- function(x) {
  if (is.null(x)) return(numeric(0))
  as.numeric(x)
}

# Convert to matrix
.tmlcds_mat <- function(X) {
  if (is.null(X)) return(matrix(numeric(0), nrow=0, ncol=0))
  if (is.data.frame(X)) X <- as.matrix(X)
  if (is.vector(X)) matrix(X, ncol=1)
  else as.matrix(X)
}

# Design matrix - if cols is NULL/empty, returns intercept only
.tmlcds_design <- function(cols, n) {
  if (is.null(cols) || length(cols) == 0) {
    return(matrix(1, nrow=n, ncol=1))
  }
  M <- do.call(cbind, cols)
  cbind(1, M)  # add intercept
}

# Matrix-vector multiplication
.tmlcds_matvec <- function(M, v) {
  as.numeric(M %*% v)
}

# Least squares
.tmlcds_lstsq <- function(Z, y) {
  # Solve Z %*% b = y in least squares sense
  # Add small ridge for stability
  if (ncol(Z) > nrow(Z)) {
    # Underdetermined - use minimum norm
    return(as.numeric(solve(crossprod(Z) + 1e-10*diag(ncol(Z)), crossprod(Z, y))))
  }
  as.numeric(solve(crossprod(Z), crossprod(Z, y)))
}
