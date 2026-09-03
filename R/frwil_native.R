# frwil.R -- function file (rootcoder007/morie)
#
# Free-Wilson analysis: additive substituent contributions.
#
# The model. For a common scaffold with k substitution positions,
# biological activity is taken to be the sum of the parent activity
# and a contribution from whichever group occupies each position:
#
#   y_i = mu + sum_{p=1}^k sum_{g in G_p} a_{pg} I(i has g at p) + eps_i.
#
# Fit by least squares on the indicator design. Every coefficient is
# then a directly readable "this group at this position is worth
# a_{pg} log units", and activity of an unmade compound is predicted
# by adding up its parts.
#
# The design is singular, always. Exactly one group occupies each
# position, so the indicator columns for a position sum to the
# intercept column -- k exact linear dependencies before any data is
# collected. The fit is therefore not unique and the raw normal
# equations have no inverse. Two constraints are in use and they
# give different numbers for the same fit:
#
# reference drops one group per position, so each remaining
# coefficient reads as "relative to the reference group", and mu is
# the activity of the all-reference compound. This is what Free and
# Wilson's original tables show.
#
# sum_zero requires the contributions at each position to sum to
# zero -- weighted by occurrence counts, as in Fujita and Ban's
# reformulation -- so mu becomes the mean activity and each
# coefficient a deviation from it.
#
# Fitted values are identical under both. The constraint fixes
# which of the infinitely many solutions is reported; it cannot
# change what the model predicts. That is the anchor, and it is the
# check that catches a constraint applied to the wrong column.
#
# What the model cannot do. Additivity is the assumption, not a
# result: a substituent whose effect depends on what sits at another
# position is invisible to it, and a group observed at only one
# position in one compound has its coefficient determined by that
# single compound. Both are reported -- occurrence counts per
# coefficient, and the residual degrees of freedom -- rather than
# left to be discovered from a suspiciously good fit.
#
# References
# ----------
# Free, S. M. & Wilson, J. W. (1964) "A mathematical contribution to
# structure-activity studies", Journal of Medicinal Chemistry 7(4),
# 395-399, doi:10.1021/jm00334a001. The additive model above, the
# indicator design over substituent positions, least-squares fitting,
# and the singularity that forces a constraint.

CONSTRAINTS <- c("reference", "sum_zero")

#' .frwil_prep
#'
#' A step of the frwil_native implementation. Called by \code{.frwil_free_wilson}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param compounds Iterated over elementwise, with \code{lapply}.
#' @param activity Passed to \code{unlist}.
#' @return A list with \code{C}, \code{y}, \code{k}.
#' @export
.frwil_prep <- function(compounds, activity) {
  C <- lapply(compounds, function(row) as.character(unlist(row)))
  y <- as.numeric(unlist(activity))
  if (length(C) != length(y)) {
    stop(sprintf("frwil: %d compounds but %d activities",
                 length(C), length(y)))
  }
  if (length(C) == 0L) {
    stop("frwil: no compounds given")
  }
  k <- length(C[[1]])
  if (k == 0L || any(lengths(C) != k)) {
    stop("frwil: every compound must list a group for the same number of positions")
  }
  list(C = C, y = y, k = k)
}

#' .frwil_design_matrix
#'
#' A step of the frwil_native implementation. Called by \code{.frwil_free_wilson}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param compounds Iterated over elementwise, with \code{lapply}.
#' @param constraint Compared against \code{"reference"}. Defaults to \code{"reference"}.
#' @return A list with \code{matrix}, \code{names}, \code{groups}, \code{columns},
#' \code{constraint}, \code{n_positions}, \code{reference}.
#' @export
.frwil_design_matrix <- function(compounds, constraint = "reference") {
  if (!(constraint %in% CONSTRAINTS)) {
    stop(sprintf("frwil: constraint must be one of %s, got %s",
                 paste(CONSTRAINTS, collapse = ", "),
                 sQuote(constraint)))
  }
  C <- lapply(compounds, function(row) as.character(unlist(row)))
  if (length(C) == 0L) {
    stop("frwil: no compounds given")
  }
  k <- length(C[[1]])
  groups <- list()
  for (p in seq_len(k)) {
    seen <- character(0)
    col_vals <- sapply(C, `[`, p)
    for (v in col_vals) {
      if (!(v %in% seen)) {
        seen <- c(seen, v)
      }
    }
    if (length(seen) < 2L) {
      stop(sprintf("frwil: position %d has only the group %s, so its contribution cannot be separated from the intercept",
                   p, sQuote(seen[1])))
    }
    groups[[p]] <- seen
  }
  names_vec <- "intercept"
  cols <- list()
  for (p in seq_len(k)) {
    keep <- if (constraint == "reference") groups[[p]][-1] else groups[[p]]
    for (g in keep) {
      names_vec <- c(names_vec, sprintf("P%d:%s", p, g))
      cols[[length(cols) + 1L]] <- c(p, g)
    }
  }
  n <- length(C)
  p_total <- length(names_vec)
  M <- matrix(0.0, nrow = n, ncol = p_total)
  M[, 1] <- 1.0
  for (i in seq_len(n)) {
    row <- C[[i]]
    for (j in seq_along(cols)) {
      col <- cols[[j]]
      if (row[col[1]] == col[2]) {
        M[i, j + 1L] <- 1.0
      }
    }
  }
  list(
    matrix = M, names = names_vec, groups = groups, columns = cols,
    constraint = constraint, n_positions = k,
    reference = vapply(groups, `[`, character(1), 1L)
  )
}

#' .frwil_lstsq
#'
#' A step of the frwil_native implementation. Called by \code{.frwil_free_wilson}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param M A matrix; indexed by row and column.
#' @param y A vector; indexed elementwise.
#' @param ridge Defaults to \code{0}.
#' @return A vector, from \code{vapply}.
#' @export
#' @examples
#' X <- cbind(1, c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9), c(0.4, 1.1, 0.9, 1.8, 2.2,
#' 2.6, 3.4, 3.9))
#' y <- c(2.9, 5.1, 6.8, 9.4, 11.2, 13.1, 15.0, 17.6)
#' res <- .frwil_lstsq(M = X, y = y)
#' res
.frwil_lstsq <- function(M, y, ridge = 0.0) {
  n <- nrow(M)
  p <- ncol(M)
  A <- matrix(0.0, nrow = p, ncol = p)
  for (a in seq_len(p)) {
    for (b in seq_len(p)) {
      s <- 0.0
      for (i in seq_len(n)) {
        s <- s + M[i, a] * M[i, b]
      }
      A[a, b] <- s + (if (a == b) ridge else 0.0)
    }
  }
  b <- numeric(p)
  for (a in seq_len(p)) {
    s <- 0.0
    for (i in seq_len(n)) {
      s <- s + M[i, a] * y[i]
    }
    b[a] <- s
  }
  Ab <- cbind(A, b)
  for (c in seq_len(p)) {
    piv <- c
    max_val <- abs(Ab[c, c])
    if (c < p) {
      for (r in (c + 1L):p) {
        v <- abs(Ab[r, c])
        if (v > max_val) {
          max_val <- v
          piv <- r
        }
      }
    }
    if (max_val < 1e-10) {
      stop("frwil: the design is rank deficient even after the constraint -- some group appears in no compound that distinguishes it")
    }
    if (piv != c) {
      tmp <- Ab[c, ]
      Ab[c, ] <- Ab[piv, ]
      Ab[piv, ] <- tmp
    }
    for (r in seq_len(p)) {
      if (r == c) next
      f <- Ab[r, c] / Ab[c, c]
      for (kk in c:(p + 1L)) {
        Ab[r, kk] <- Ab[r, kk] - f * Ab[c, kk]
      }
    }
  }
  vapply(seq_len(p), function(i) Ab[i, p + 1L] / Ab[i, i], numeric(1))
}

#' .frwil_free_wilson
#'
#' A step of the frwil_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param compounds Passed to \code{.frwil_prep}.
#' @param activity Passed to \code{.frwil_prep}.
#' @param constraint Compared against \code{"sum_zero"}. Defaults to \code{"reference"}.
#' @return A list with \code{estimate}, \code{coefficients}, \code{names}, \code{beta},
#' \code{fitted}, \code{residuals}, \code{rss}, \code{tss}, \code{r_squared},
#' \code{sigma}, \code{df_residual}, \code{n_parameters}, \code{occurrences},
#' \code{groups}, \code{reference}, \code{constraint}, \code{n_positions}, \code{method}.
#' @export
.frwil_free_wilson <- function(compounds, activity, constraint = "reference") {
  prep <- .frwil_prep(compounds, activity)
  C <- prep$C
  y_full <- prep$y
  k <- prep$k
  n <- length(C)
  D <- .frwil_design_matrix(C, constraint)
  M <- D$matrix
  names_vec <- D$names
  if (constraint == "sum_zero") {
    for (p in seq_len(k)) {
      row <- rep(0.0, length(names_vec))
      for (j in seq_along(D$columns)) {
        col <- D$columns[[j]]
        if (col[1] == p) {
          count <- sum(sapply(C, `[`, col[1]) == col[2])
          row[j + 1L] <- as.numeric(count)
        }
      }
      M <- rbind(M, row)
      y_full <- c(y_full, 0.0)
    }
    beta <- .frwil_lstsq(M, y_full)
    fitted <- vapply(seq_len(n), function(i) {
      sum(D$matrix[i, ] * beta)
    }, numeric(1))
  } else {
    beta <- .frwil_lstsq(M, y_full)
    fitted <- vapply(seq_len(n), function(i) {
      sum(M[i, ] * beta)
    }, numeric(1))
  }
  y_obs <- y_full[seq_len(n)]
  resid <- y_obs - fitted
  mu <- sum(y_obs) / n
  sst <- sum((y_obs - mu)^2)
  sse <- sum(resid^2)
  p_eff <- length(beta) - (if (constraint == "sum_zero") k else 0L)
  df <- n - p_eff
  counts <- list()
  for (j in seq_along(D$columns)) {
    col <- D$columns[[j]]
    count <- sum(sapply(C, `[`, col[1]) == col[2])
    counts[[names_vec[j + 1L]]] <- as.integer(count)
  }
  coef_list <- as.list(setNames(beta, names_vec))
  list(
    estimate = coef_list,
    coefficients = coef_list,
    names = names_vec, beta = beta,
    fitted = fitted, residuals = resid,
    rss = sse, tss = sst,
    r_squared = if (sst > 0) 1.0 - sse / sst else NaN,
    sigma = if (df > 0) sqrt(sse / df) else NaN,
    df_residual = df, n_parameters = p_eff,
    occurrences = counts,
    groups = D$groups, reference = D$reference,
    constraint = constraint, n_positions = k,
    method = sprintf("Free & Wilson (1964) additive substituent model, %s constraint", constraint)
  )
}

#' .frwil_predict_activity
#'
#' A step of the frwil_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param fit A list; the body reads \code{$coefficients}, \code{$groups},
#' \code{$n_positions} from it.
#' @param compound Passed to \code{unlist}.
#' @return The value of \code{total}, as built in the body.
#' @export
.frwil_predict_activity <- function(fit, compound) {
  row <- as.character(unlist(compound))
  if (length(row) != fit$n_positions) {
    stop(sprintf("frwil: the compound lists %d positions but the model has %d",
                 length(row), fit$n_positions))
  }
  coef <- fit$coefficients
  total <- coef[["intercept"]]
  for (p in seq_along(row)) {
    g <- row[p]
    if (!(g %in% fit$groups[[p]])) {
      stop(sprintf("frwil: group %s was never observed at position %d, so the model says nothing about it",
                   sQuote(g), p))
    }
    key <- sprintf("P%d:%s", p, g)
    if (key %in% names(coef)) {
      total <- total + coef[[key]]
    }
  }
  total
}

morie_frwil <- list(
  design_matrix = .frwil_design_matrix,
  free_wilson = .frwil_free_wilson,
  predict_activity = .frwil_predict_activity,
  CONSTRAINTS = CONSTRAINTS
)
