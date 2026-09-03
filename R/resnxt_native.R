# morie.fn -- function file (rootcoder007/morie)
# ResNeXt: cardinality as a dimension in its own right.
# VGG and ResNet stack blocks of the same shape, which keeps the
# hyper-parameter space small and, the paper argues, reduces the risk of
# over-adapting to one dataset. Inception blocks are more accurate but
# carefully hand-designed per stage -- the split-transform-merge is
# powerful, but the filter numbers and sizes are bespoke.
# ResNeXt keeps the repeat-the-same-block discipline *and* the
# split-transform-merge, by aggregating a set of transformations that all
# have the **same topology**:
#   y = x + sum_{i=1..C} T_i(x).
# C is the **cardinality**, exposed as a dimension of the design space
# alongside depth and width. Increasing it improves accuracy while
# holding complexity fixed, and does so more effectively than going
# deeper or wider.
# Three equivalent forms: (a) C separate paths each ending in a
# full-width projection, (b) C paths concatenated then projected once,
# (c) a single grouped convolution with C groups. ``block_equivalence``
# checks the identity numerically.
# Complexity accounting: for a bottleneck of width d per path, the
# parameter count is C * (2*W*d + 9*d^2); matching a baseline means
# trading C against d under fixed cost.
# References
# Xie, S., Girshick, R., Dollar, P., Tu, Z. & He, K. (2017) "Aggregated
# Residual Transformations for Deep Neural Networks", CVPR 2017, 5987-5995,
# arXiv:1611.05431.
# He, K., Zhang, X., Ren, S. & Sun, J. (2016) "Deep Residual Learning for
# Image Recognition", CVPR 2016, 770-778, arXiv:1512.03385.
# Szegedy, C. et al. (2015) "Going deeper with convolutions", CVPR 2015,
# 1-9.

#' .resnxt_vec
#'
#' A step of the resnxt_native implementation. Called by \code{.resnxt_aggregated_block},
#' \code{.resnxt_grouped_block}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @return A vector, from \code{as.numeric}.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' res <- .resnxt_vec(x = x)
#' res
.resnxt_vec <- function(x) {
  as.numeric(x)
}

#' .resnxt_lin
#'
#' A step of the resnxt_native implementation. Called by \code{.resnxt_aggregated_block},
#' \code{.resnxt_grouped_block}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param W A vector; its length is taken and its elements indexed.
#' @param x Numeric; combined arithmetically in the body.
#' @return A vector, from \code{vapply}.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' res <- .resnxt_lin(W = x, x = x)
#' res
.resnxt_lin <- function(W, x) {
  vapply(seq_along(W), function(o) sum(W[[o]] * x), numeric(1))
}

#' .resnxt_relu
#'
#' A step of the resnxt_native implementation. Called by \code{.resnxt_aggregated_block},
#' \code{.resnxt_grouped_block}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v Passed to \code{pmax}.
#' @return The value of \code{pmax}.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' res <- .resnxt_relu(v = x)
#' res
.resnxt_relu <- function(v) {
  pmax(0, v)
}

#' .resnxt_aggregated_block
#'
#' A step of the resnxt_native implementation. Called by \code{.resnxt_block_equivalence}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Passed to \code{.resnxt_vec}.
#' @param Wins A vector; its length is taken and its elements indexed.
#' @param Wmids A vector; indexed elementwise.
#' @param Wouts A vector; indexed elementwise.
#' @return A numeric value.
#' @export
.resnxt_aggregated_block <- function(x, Wins, Wmids, Wouts) {
  xv <- .resnxt_vec(x)
  acc <- rep(0, length(xv))
  for (i in seq_along(Wins)) {
    h <- .resnxt_relu(.resnxt_lin(Wins[[i]], xv))
    h <- .resnxt_relu(.resnxt_lin(Wmids[[i]], h))
    o <- .resnxt_lin(Wouts[[i]], h)
    acc <- acc + o
  }
  xv + acc
}

#' .resnxt_grouped_block
#'
#' A step of the resnxt_native implementation. Called by \code{.resnxt_block_equivalence}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Passed to \code{.resnxt_vec}.
#' @param Wins A vector; its length is taken and its elements indexed.
#' @param Wmids A vector; indexed elementwise.
#' @param Wout_concat Passed to \code{.resnxt_lin}.
#' @return A numeric value.
#' @export
.resnxt_grouped_block <- function(x, Wins, Wmids, Wout_concat) {
  xv <- .resnxt_vec(x)
  cat <- numeric(0)
  for (i in seq_along(Wins)) {
    h <- .resnxt_relu(.resnxt_lin(Wins[[i]], xv))
    h <- .resnxt_relu(.resnxt_lin(Wmids[[i]], h))
    cat <- c(cat, h)
  }
  o <- .resnxt_lin(Wout_concat, cat)
  xv + o
}

#' .resnxt_block_equivalence
#'
#' A step of the resnxt_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Passed to \code{.resnxt_aggregated_block}.
#' @param Wins Passed to \code{.resnxt_aggregated_block}.
#' @param Wmids Passed to \code{.resnxt_aggregated_block}.
#' @param Wouts A vector; indexed elementwise.
#' @param tol Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1e-09}.
#' @return A list with \code{equivalent}, \code{max_deviation}, \code{aggregated},
#' \code{grouped}, \code{note}.
#' @export
.resnxt_block_equivalence <- function(x, Wins, Wmids, Wouts, tol = 1e-9) {
  a <- .resnxt_aggregated_block(x, Wins, Wmids, Wouts)
  n_outs <- length(Wouts[[1]])
  cat_mat <- vector("list", n_outs)
  for (o in seq_len(n_outs)) {
    cat_mat[[o]] <- unlist(lapply(Wouts, function(W) W[[o]]))
  }
  c <- .resnxt_grouped_block(x, Wins, Wmids, cat_mat)
  dev <- max(abs(a - c))
  list(equivalent = dev < as.numeric(tol),
       max_deviation = dev,
       aggregated = a,
       grouped = c,
       note = "same function; the grouped form is what runs fast")
}

#' .resnxt_block_parameters
#'
#' A step of the resnxt_native implementation. Called by \code{.resnxt_match_complexity}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param width Coerced to integer by the body, with \code{as.integer}.
#' @param cardinality Coerced to integer by the body, with \code{as.integer}.
#' @param bottleneck Coerced to integer by the body, with \code{as.integer}.
#' @return A list with \code{parameters}, \code{cardinality}, \code{bottleneck}, \code{width}.
#' @export
.resnxt_block_parameters <- function(width, cardinality, bottleneck) {
  W <- as.integer(width)
  C <- as.integer(cardinality)
  d <- as.integer(bottleneck)
  if (min(W, C, d) < 1) {
    stop("resnxt: width, cardinality and bottleneck must all be at least 1")
  }
  list(parameters = C * (W * d + 9 * d * d + d * W),
       cardinality = C,
       bottleneck = d,
       width = W)
}

#' .resnxt_match_complexity
#'
#' A step of the resnxt_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param width Coerced to integer by the body, with \code{as.integer}.
#' @param cardinality Coerced to integer by the body, with \code{as.integer}.
#' @param target_parameters Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{bottleneck}, \code{rounded}, \code{parameters},
#' \code{target}, \code{cardinality}.
#' @export
.resnxt_match_complexity <- function(width, cardinality, target_parameters) {
  W <- as.integer(width)
  C <- as.integer(cardinality)
  T <- as.numeric(target_parameters)
  a <- 9.0 * C
  b <- 2.0 * C * W
  disc <- b * b + 4.0 * a * T
  d <- (-b + sqrt(disc)) / (2.0 * a)
  d_round <- max(1, as.integer(round(d)))
  list(bottleneck = d,
       rounded = d_round,
       parameters = .resnxt_block_parameters(W, C, d_round)$parameters,
       target = T,
       cardinality = C)
}

#' .resnxt_cheatsheet
#'
#' A step of the resnxt_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
#' @examples
#' res <- .resnxt_cheatsheet()
#' res
.resnxt_cheatsheet <- function() {
  "resnxt: y = x + sum_{i=1..C} T_i(x), every T_i with the SAME TOPOLOGY -- Inception's split-transform-merge without its per-stage hand design. C is CARDINALITY, a design dimension beside depth and width, and raising it beats going deeper or wider AT FIXED COMPLEXITY. Three equivalent block forms: C separate paths, concatenate-then-project, or one GROUPED CONVOLUTION -- same function, and the third is what runs fast."
}

# Public names
aggregated_block <- .resnxt_aggregated_block
grouped_block <- .resnxt_grouped_block
block_equivalence <- .resnxt_block_equivalence
block_parameters <- .resnxt_block_parameters
match_complexity <- .resnxt_match_complexity
.resnxt_cheatsheet <- .resnxt_cheatsheet

# Compact alias per ledger/NAMING.md
resnext <- aggregated_block

# Public names resolved by fn/_lazy_map.json
resnext_block <- aggregated_block
resnextblock <- aggregated_block

# Entry point
morie_resnxt <- aggregated_block
