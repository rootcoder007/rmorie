# SPDX-License-Identifier: AGPL-3.0-or-later
# Deterministic random forest core shared by Rfmlt, Rfmdi and Rfpmi.
#
# The algorithm is the one printed in Montesinos Lopez, Montesinos Lopez and
# Crossa (2022), Multivariate Statistical Machine Learning Methods for Genomic
# Prediction, Springer, volume [Pages 633-681], Chapter 15, Section 15.4,
# pp. 639-640, read as rendered page images: for b = 1, ..., B bootstrap
# samples, draw a bootstrap sample of size N_train; grow a tree by recursively,
# for each terminal node, (a) randomly drawing mtry of the p independent
# variables, (b) picking the best one among them, (c) splitting into two child
# nodes, until the minimum node size is reached, with no pruning; and predict
# by yhat_i = (1/B) sum_b T_b(x_i).  Page 643 gives the defaults: mtry = p/3
# for regression, "values are always rounded up", nodesize 5 for regression,
# and the weighted mean squared error ("the least square criterion") as the
# splitting rule, attributed there to Breiman, Friedman, Olshen and Stone
# (1984), Chapter 8.4.
#
# DETERMINISM.  Steps 1 and 2(a) both say "randomly", and a forest whose two
# arms draw different samples cannot be compared at 1e-9.  The bootstrap uses
# the Numerical Recipes 32-bit LCG x <- (1664525 x + 1013904223) mod 2^32
# seeded from the tree index, NOT a low-discrepancy sequence: a van der Corput
# sweep was tried first and is wrong here, because being low-discrepancy it
# visits almost every index exactly once and the out-of-bag set collapsed to 2
# rows out of 40 instead of the 36.8% p. 640 states.  Permutation importance is
# computed on that set.  The mtry candidates at the s-th split of tree b start
# at offset floor(vdc(b*4096 + s + 1, base 3) * p) and run consecutively modulo
# p.  Ties in the split search go to the lowest variable index and then to the
# lowest threshold.  Every LCG quantity is an exact integer below 2^53, so both
# arms reproduce it bit for bit in double precision.

.rfLCGA <- 1664525
.rfLCGC <- 1013904223
.rfLCGM <- 4294967296

#' .rfboot
#'
#' A step of the mvsml_rf_shared implementation. Called by \code{.rfforest}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param b Numeric; combined arithmetically in the body.
#' @param n A count; the body uses it as \code{seq_len(...)}.
#' @return A numeric value.
#' @export
.rfboot <- function(b, n) {
  x <- ((b + 1) * 2654435761) %% .rfLCGM
  rows <- numeric(n)
  for (k in seq_len(n)) {
    x <- (.rfLCGA * x + .rfLCGC) %% .rfLCGM
    rows[k] <- floor(x / .rfLCGM * n)
  }
  pmin(pmax(rows, 0), n - 1) + 1L
}

#' .rfcand
#'
#' A step of the mvsml_rf_shared implementation. Called by \code{.rfgrow}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param b Numeric; combined arithmetically in the body.
#' @param s Numeric; combined arithmetically in the body.
#' @param p Numeric; combined arithmetically in the body.
#' @param mtry A count; the body uses it as \code{seq_len(...)}.
#' @return A numeric value.
#' @export
.rfcand <- function(b, s, p, mtry) {
  off <- floor(.s03vdc(b * 4096 + s + 1, 3L) * p)
  ((off + seq_len(mtry) - 1L) %% p) + 1L
}

#' .rfmtry
#'
#' A step of the mvsml_rf_shared implementation. Called by \code{Rfmdi}, \code{Rfmlt}, \code{Rfpmi}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param p Numeric; passed to \code{sqrt}.
#' @param kind Passed to \code{identical}. Defaults to \code{"regression"}.
#' @return A numeric value.
#' @export
.rfmtry <- function(p, kind = "regression") {
  m <- if (identical(kind, "regression")) ceiling(p / 3) else ceiling(sqrt(p))
  max(1L, min(as.integer(p), as.integer(m)))
}

# The (15.6) objective, MAXIMISED -- see the erratum recorded in rfmlt.R.
#' The (15.6) objective, MAXIMISED -- see the erratum recorded in
#' rfmlt.R
#'
#' A step of the mvsml_rf_shared implementation. Called by \code{.rfbest}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param Y A matrix; indexed by row and column.
#' @param left A vector; its length is taken.
#' @param right A vector; its length is taken.
#' @param q A count; the body uses it as \code{seq_len(...)}.
#' @return The value of \code{g}, as built in the body.
#' @export
.rfgain <- function(Y, left, right, q) {
  nL <- length(left); nR <- length(right)
  if (nL == 0L || nR == 0L) return(NULL)
  g <- 0
  for (j in seq_len(q)) {
    sL <- sum(Y[left, j]); sR <- sum(Y[right, j])
    g <- g + sL * sL / nL + sR * sR / nR
  }
  g
}

# Within-node sum of squares, the least-square criterion of p. 643.
#' Within-node sum of squares, the least-square criterion of p. 643
#'
#' A step of the mvsml_rf_shared implementation. Called by \code{.rfgrow}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param Y A matrix; indexed by row and column.
#' @param rows A vector; its length is taken.
#' @param q A count; the body uses it as \code{seq_len(...)}.
#' @return The value of \code{tot}, as built in the body.
#' @export
.rfimp <- function(Y, rows, q) {
  n <- length(rows)
  if (n == 0L) return(0)
  tot <- 0
  for (j in seq_len(q)) {
    s <- sum(Y[rows, j])
    tot <- tot + sum(Y[rows, j]^2) - s * s / n
  }
  tot
}

#' .rfbest
#'
#' A step of the mvsml_rf_shared implementation. Called by \code{.rfgrow}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param X A matrix; indexed by row and column.
#' @param Y Passed to \code{.rfgain}.
#' @param rows A vector; indexed elementwise.
#' @param cand See Usage.
#' @param q Passed to \code{.rfgain}.
#' @return The value of \code{best}, as built in the body.
#' @export
.rfbest <- function(X, Y, rows, cand, q) {
  best <- NULL
  for (v in cand) {
    vals <- sort(unique(X[rows, v]))
    if (length(vals) < 2L) next
    for (a in seq_len(length(vals) - 1L)) {
      thr <- 0.5 * (vals[a] + vals[a + 1L])
      left <- rows[X[rows, v] <= thr]
      right <- rows[X[rows, v] > thr]
      g <- .rfgain(Y, left, right, q)
      if (is.null(g)) next
      if (is.null(best) || g > best$g + 1e-15) {
        best <- list(g = g, var = v, thr = thr, left = left, right = right)
      }
    }
  }
  best
}

# A node is list(var, thr, li, ri, value, n, drop).  A leaf has var = -1L.
#' A node is list(var, thr, li, ri, value, n, drop).  A leaf has var =
#' -1L
#'
#' A step of the mvsml_rf_shared implementation. Called by \code{.rfforest}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param X A matrix; passed to \code{ncol}.
#' @param Y A matrix; indexed by row and column.
#' @param rows A vector; its length is taken.
#' @param b Passed to \code{.rfcand}.
#' @param nodesize Numeric; combined arithmetically in the body.
#' @param mtry Passed to \code{.rfcand}.
#' @param q A count; the body uses it as \code{numeric(...)}.
#' @param env A list; the body reads \code{$k}, \code{$nodes}, \code{$s} from it.
#' @return The value of \code{idx}, as built in the body.
#' @export
.rfgrow <- function(X, Y, rows, b, nodesize, mtry, q, env) {
  n <- length(rows)
  idx <- env$k + 1L
  env$k <- idx
  env$nodes[[idx]] <- NA
  mean <- if (n > 0L) colSums(Y[rows, , drop = FALSE]) / n else numeric(q)
  if (n < 2L * nodesize || n < 2L) {
    env$nodes[[idx]] <- list(var = -1L, thr = 0, li = -1L, ri = -1L,
                             value = mean, n = n, drop = 0)
    return(idx)
  }
  p <- ncol(X)
  cand <- .rfcand(b, env$s, p, mtry)
  env$s <- env$s + 1L
  sp <- .rfbest(X, Y, rows, cand, q)
  if (is.null(sp) || length(sp$left) < nodesize || length(sp$right) < nodesize) {
    env$nodes[[idx]] <- list(var = -1L, thr = 0, li = -1L, ri = -1L,
                             value = mean, n = n, drop = 0)
    return(idx)
  }
  drop <- .rfimp(Y, rows, q) - .rfimp(Y, sp$left, q) - .rfimp(Y, sp$right, q)
  li <- .rfgrow(X, Y, sp$left, b, nodesize, mtry, q, env)
  ri <- .rfgrow(X, Y, sp$right, b, nodesize, mtry, q, env)
  env$nodes[[idx]] <- list(var = sp$var, thr = sp$thr, li = li, ri = ri,
                           value = mean, n = n, drop = drop)
  idx
}

#' .rfpredtree
#'
#' A step of the mvsml_rf_shared implementation. Called by \code{.rfperm}, \code{.rfpredict}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param nodes A vector; indexed elementwise.
#' @param root See Usage.
#' @param x A vector; indexed elementwise.
#' @return The value of \code{$}.
#' @export
.rfpredtree <- function(nodes, root, x) {
  i <- root
  while (nodes[[i]]$var != -1L) {
    i <- if (x[nodes[[i]]$var] <= nodes[[i]]$thr) nodes[[i]]$li else nodes[[i]]$ri
  }
  nodes[[i]]$value
}

#' .rfforest
#'
#' A step of the mvsml_rf_shared implementation. Called by \code{Rfmdi}, \code{Rfmlt}, \code{Rfpmi}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param X A matrix; passed to \code{nrow}.
#' @param Y Passed to \code{.rfgrow}.
#' @param n_trees A count; the body uses it as \code{seq_len(...)}.
#' @param nodesize Passed to \code{.rfgrow}.
#' @param mtry Passed to \code{.rfgrow}.
#' @param q Passed to \code{.rfgrow}.
#' @return A list with \code{trees}, \code{oob}.
#' @export
.rfforest <- function(X, Y, n_trees, nodesize, mtry, q) {
  n <- nrow(X)
  trees <- vector("list", n_trees)
  oob <- vector("list", n_trees)
  for (b in seq_len(n_trees) - 1L) {
    rows <- .rfboot(b, n)
    env <- new.env()
    env$nodes <- list(); env$k <- 0L; env$s <- 0L
    root <- .rfgrow(X, Y, rows, b, nodesize, mtry, q, env)
    trees[[b + 1L]] <- list(nodes = env$nodes, root = root)
    oob[[b + 1L]] <- setdiff(seq_len(n), unique(rows))
  }
  list(trees = trees, oob = oob)
}

# yhat_i = (1/B) sum_b T_b(x_i), the p. 640 aggregation.
#' Yhat_i = (1/B) sum_b T_b(x_i), the p. 640 aggregation
#'
#' A step of the mvsml_rf_shared implementation. Called by \code{Rfmlt}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param trees A vector; its length is taken.
#' @param Xnew A matrix; indexed by row and column.
#' @param q A count; the body uses it as \code{numeric(...)}.
#' @return The value of \code{out}, as built in the body.
#' @export
.rfpredict <- function(trees, Xnew, q) {
  B <- length(trees)
  out <- matrix(0, nrow(Xnew), q)
  for (i in seq_len(nrow(Xnew))) {
    acc <- numeric(q)
    for (tb in trees) acc <- acc + .rfpredtree(tb$nodes, tb$root, Xnew[i, ])
    out[i, ] <- acc / B
  }
  out
}

#' .rfcheck
#'
#' A step of the mvsml_rf_shared implementation. Called by \code{Rfmdi}, \code{Rfmlt}, \code{Rfpmi}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param X A matrix; passed to \code{nrow}.
#' @param Y A matrix; passed to \code{nrow}.
#' @return A vector, from \code{c}.
#' @export
.rfcheck <- function(X, Y) {
  n <- nrow(X)
  if (n == 0L) stop("random forest: X is empty")
  p <- ncol(X)
  if (p == 0L) stop("random forest: X has no columns")
  if (nrow(Y) != n) stop("random forest: Y has a different number of rows than X")
  q <- ncol(Y)
  if (q == 0L) stop("random forest: Y has no columns")
  c(n, p, q)
}

# The p. 656 requirement: responses standardized before splitting.
#' The p. 656 requirement: responses standardized before splitting
#'
#' A step of the mvsml_rf_shared implementation. Called by \code{Rfmdi}, \code{Rfmlt}, \code{Rfpmi}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param Y A matrix; indexed by row and column.
#' @param n A count; the body uses it as \code{matrix(...)}.
#' @param q A count; the body uses it as \code{seq_len(...)}.
#' @return The value of \code{out}, as built in the body.
#' @export
.rfstd <- function(Y, n, q) {
  out <- matrix(0, n, q)
  for (j in seq_len(q)) {
    m <- sum(Y[, j]) / n
    sdv <- sqrt(sum((Y[, j] - m)^2) / n)
    out[, j] <- if (sdv > 0) (Y[, j] - m) / sdv else 0
  }
  out
}

# Out-of-bag permutation VIM, Chapter 15 pp. 642-643: "the values of the jth
# variable are randomly permuted in the OOB observations and j new PE is
# computed.  The differences between the two are then averaged over all the
# trees, and normalized by the standard deviation of the differences."  The
# permutation is the deterministic reversal of the OOB row order.
#' Out-of-bag permutation VIM, Chapter 15 pp. 642-643: "the values of
#' the jth
#'
#' variable are randomly permuted in the OOB observations and j new PE
#' is computed.  The differences between the two are then averaged over
#' all the trees, and normalized by the standard deviation of the
#' differences."  The permutation is the deterministic reversal of the
#' OOB row order.
#'
#' @param trees A vector; its length is taken and its elements indexed.
#' @param oob A vector; indexed elementwise.
#' @param X A matrix; indexed by row and column.
#' @param Y A matrix; indexed by row and column.
#' @param q Accepted by the signature and not used anywhere in the body.
#' @param normalise A flag; the body branches on it. Defaults to \code{TRUE}.
#' @return The value of \code{imp}, as built in the body.
#' @export
.rfperm <- function(trees, oob, X, Y, q, normalise = TRUE) {
  p <- ncol(X)
  imp <- numeric(p)
  for (j in seq_len(p)) {
    diffs <- numeric(0)
    for (b in seq_along(trees)) {
      ob <- oob[[b]]
      if (length(ob) < 2L) next
      nodes <- trees[[b]]$nodes; root <- trees[[b]]$root
      base <- 0
      for (i in ob) {
        v <- .rfpredtree(nodes, root, X[i, ])
        base <- base + sum((Y[i, ] - v)^2)
      }
      base <- base / length(ob)
      perm <- 0
      rev <- rev(ob)
      for (a in seq_along(ob)) {
        i <- ob[a]
        x <- X[i, ]
        x[j] <- X[rev[a], j]
        v <- .rfpredtree(nodes, root, x)
        perm <- perm + sum((Y[i, ] - v)^2)
      }
      perm <- perm / length(ob)
      diffs <- c(diffs, perm - base)
    }
    if (length(diffs) == 0L) { imp[j] <- NaN; next }
    m <- sum(diffs) / length(diffs)
    if (normalise && length(diffs) > 1L) {
      sdv <- sqrt(sum((diffs - m)^2) / (length(diffs) - 1L))
      imp[j] <- if (sdv > 0) m / sdv else 0
    } else imp[j] <- m
  }
  imp
}

# Mean decrease in impurity: Imp(X_j) = (1/B) sum_b sum_{t: split on X_j}
# (n_t/n) [ i(t) - (n_L/n_t) i(t_L) - (n_R/n_t) i(t_R) ], i the within-node
# sum of squares.  The stored drop is already in raw sum-of-squares units.
#' Mean decrease in impurity: Imp(X_j) = (1/B) sum_b sum_{t: split on
#' X_j}
#'
#' (n_t/n) \[ i(t) - (n_L/n_t) i(t_L) - (n_R/n_t) i(t_R) \], i the
#' within-node sum of squares.  The stored drop is already in raw
#' sum-of-squares units.
#'
#' @param trees A vector; its length is taken.
#' @param p A count; the body uses it as \code{numeric(...)}.
#' @return A numeric value.
#' @export
.rfmdi_imp <- function(trees, p) {
  B <- length(trees)
  imp <- numeric(p)
  for (tb in trees) {
    ntot <- tb$nodes[[tb$root]]$n
    if (ntot <= 0) next
    for (nd in tb$nodes) {
      if (nd$var == -1L) next
      if (nd$n > 0) imp[nd$var] <- imp[nd$var] + (nd$n / ntot) * nd$drop / nd$n
    }
  }
  imp / B
}
