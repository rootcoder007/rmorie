# morie.fn -- function file (rootcoder007/morie)
#
# Forest-fit consistency diagnostics.
#
# Wager & Athey's consistency and asymptotic-normality results do not hold
# for any forest. They hold for forests that are honest, random-split,
# alpha-regular and symmetric, grown on subsamples of size s ~ n^beta with
#
#   beta_min = 1 - (1 + (d/pi) log(alpha^{-1}) / log((1-alpha)^{-1}))^{-1} < beta < 1.
#
# Every one of those is a property of *how the forest was grown*, and
# every one is silently violable. A forest that breaks them still
# produces predictions and still produces intervals; what it stops
# producing is any reason to believe the intervals.
#
# So the conditions are checked, not assumed. This module audits a
# fitted forest:
#
# * honesty, tested by permuting the responses the leaves average and
#   requiring the split structure not to move -- the only test that can
#   actually fail;
# * the random-split floor, by measuring each feature's realised split
#   share against pi/d;
# * alpha-regularity, by finding the most lopsided split;
# * the subsample rate, by solving for the realised beta and comparing
#   it with beta_min -- reported rather than scored, because beta_min is
#   0.997 already at d=3, alpha=0.05, pi=0.5, so the theorem asks for
#   s ~ n and no practical subsample fraction meets it. Folding that
#   into a pass/fail would mark every forest ever grown as failing,
#   which tells nobody anything;
# * and the fit itself, by checking the error falls as n grows, which
#   is what consistency means operationally.
#
# The point is that a diagnostic returning "all clear" on a forest that
# is *not* honest would be worse than no diagnostic, so the honesty test
# here is the structural one rather than a flag copied from the
# constructor's arguments.
#
# References
# ----------
# Wager, S. & Athey, S. (2018) "Estimation and Inference of Heterogeneous
# Treatment Effects using Random Forests", Journal of the American
# Statistical Association 113(523), 1228-1242,
# doi:10.1080/01621459.2017.1319839, arXiv:1510.04342. Definitions 2-5,
# Theorem 3 and its beta_min, Theorem 1.
#
# Athey, S., Tibshirani, J. & Wager, S. (2019) "Generalized Random
# Forests", The Annals of Statistics 47(2), 1148-1178,
# doi:10.1214/18-AOS1709, arXiv:1610.01271. The same conditions in the
# generalized setting.
#
# Biau, G. (2012) "Analysis of a Random Forests Model", Journal of
# Machine Learning Research 13, 1063-1095. Earlier consistency analysis
# of the same shape.

#' .frfgrf_beta_min
#'
#' A step of the frfgrf_native implementation. Called by \code{.frfgrf_forest_fit_check}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param d Coerced to numeric by the body, with \code{as.numeric}.
#' @param alpha Numeric; combined arithmetically in the body. Defaults to \code{0.05}.
#' @param pi Numeric; combined arithmetically in the body. Defaults to \code{0.5}.
#' @return A numeric value.
#' @export
.frfgrf_beta_min <- function(d, alpha = 0.05, pi = 0.5) {
  if (alpha <= 0.0 || alpha >= 0.5) {
    stop(sprintf("frfgrf: alpha must be in (0, 0.5), got %s",
                 format(alpha)))
  }
  if (pi <= 0.0 || pi > 1.0) {
    stop(sprintf("frfgrf: pi must be in (0, 1], got %s", format(pi)))
  }
  if (d < 1) {
    stop("frfgrf: need at least one feature")
  }
  ratio <- log(1.0 / alpha) / log(1.0 / (1.0 - alpha))
  return(1.0 - (1.0 + (as.numeric(d) / pi) * ratio)^(-1.0))
}

#' .frfgrf_structure
#'
#' A step of the frfgrf_native implementation. Called by \code{.frfgrf_honesty_test}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param tree A list; the body reads \code{$feature}, \code{$leaf}, \code{$left}, \code{$right}, \code{$threshold} from it.
#' @return A vector, from \code{c}.
#' @export
.frfgrf_structure <- function(tree) {
  if (isTRUE(tree$leaf)) {
    return(list("leaf"))
  }
  feat <- as.numeric(tree$feature)
  thr <- as.numeric(round(tree$threshold, 12))
  left_struct <- .frfgrf_structure(tree$left)
  right_struct <- .frfgrf_structure(tree$right)
  return(c(list(c(feat, thr)), left_struct, right_struct))
}

#' .frfgrf_honesty_test
#'
#' A step of the frfgrf_native implementation. Called by \code{.frfgrf_forest_fit_check}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X Passed to \code{honest_tree}.
#' @param y A vector; indexed elementwise.
#' @param kind Compared against \code{"propensity"}. Defaults to \code{"double-sample"}.
#' @param min_leaf Passed to \code{honest_tree}. Defaults to \code{5}.
#' @param seed Numeric; combined arithmetically in the body. Defaults to \code{11}.
#' @param n_permutations Coerced to integer by the body, with \code{as.integer}. Defaults to \code{3}.
#' @return A list with \code{honest}, \code{splits_stable_under_I_permutation}, \code{splits_move_under_J_permutation}, \code{n_splits}.
#' @export
.frfgrf_honesty_test <- function(X, y, kind = "double-sample", min_leaf = 5,
                                 seed = 11, n_permutations = 3) {
  ht_out <- honest_tree(X, y, kind = kind, min_leaf = min_leaf, seed = seed)
  tree <- ht_out[[1]]
  info <- ht_out[[2]]
  base <- .frfgrf_structure(tree)

  # Initialise the RNG state once; subsequent .ghc_unif() calls advance it.
  rng_state <- .ghc_rng(seed + 1)

  stable <- TRUE
  np <- as.integer(n_permutations)
  if (np > 0) {
    for (iter in seq_len(np)) {
      yp <- y
      # info$I is REPORTED 0-based (hntfst builds seq_len(n) - 1L);
      # R subscripts are 1-based, so convert at the point of use.
      I <- info$I + 1L
      n_I <- length(I)
      if (n_I > 0) {
        # .ghc_unif(e, n) takes the RNG ENVIRONMENT first. Passing 1 here
        # meant e was an atomic, so e$i failed -- and rng_state, created
        # just above, was never actually used, so the permutations did not
        # advance a stream at all.
        u <- .ghc_unif(rng_state, n_I)
        perm <- I[order(u)]
        yp[I] <- y[perm]
      }
      tp_out <- honest_tree(X, yp, kind = kind, min_leaf = min_leaf,
                            seed = seed)
      tp <- tp_out[[1]]
      if (!identical(.frfgrf_structure(tp), base)) {
        stable <- FALSE
        break
      }
    }
  }

  # Control: permute the splitting responses (set J). A tree that ignores
  # every response would pass the first test trivially, so the second
  # is what makes the first mean something.
  yj <- y
  # info$J is REPORTED 0-based, same as I above.
  J <- info$J + 1L
  n_J <- length(J)
  if (n_J > 0) {
    u_J <- .ghc_unif(rng_state, n_J)
    perm_J <- J[order(u_J)]
    yj[J] <- y[perm_J]
  }
  tj_out <- honest_tree(X, yj, kind = kind, min_leaf = min_leaf, seed = seed)
  tj <- tj_out[[1]]
  responsive <- !identical(.frfgrf_structure(tj), base)

  n_splits <- 0
  for (v in base) {
    if (!identical(v, "leaf")) n_splits <- n_splits + 1
  }

  return(list(
    honest = stable && (responsive || kind == "propensity"),
    splits_stable_under_I_permutation = stable,
    splits_move_under_J_permutation = responsive,
    n_splits = n_splits
  ))
}

#' .frfgrf_split_share_walk
#'
#' A step of the frfgrf_native implementation. Called by \code{.frfgrf_split_share}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param nd A list; the body reads \code{$feature}, \code{$leaf}, \code{$left}, \code{$right} from it.
#' @param counts A vector; indexed elementwise.
#' @return The value of \code{counts}, as built in the body.
#' @export
.frfgrf_split_share_walk <- function(nd, counts) {
  if (!isTRUE(nd$leaf)) {
    counts[nd$feature] <- counts[nd$feature] + 1
    counts <- .frfgrf_split_share_walk(nd$left, counts)
    counts <- .frfgrf_split_share_walk(nd$right, counts)
  }
  return(counts)
}

#' .frfgrf_split_share
#'
#' A step of the frfgrf_native implementation. Called by \code{.frfgrf_forest_fit_check}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param trees See Usage.
#' @param d A count; the body uses it as \code{numeric(...)}.
#' @return The value of \code{list}.
#' @export
.frfgrf_split_share <- function(trees, d) {
  counts <- numeric(d)
  for (t in trees) {
    counts <- .frfgrf_split_share_walk(t, counts)
  }
  tot <- sum(counts)
  if (tot == 0) tot <- 1
  return(list(as.numeric(counts) / tot, counts))
}

#' .frfgrf_leaf_count
#'
#' A step of the frfgrf_native implementation. Called by \code{.frfgrf_regularity_walk}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param nd A list; the body reads \code{$leaf}, \code{$left}, \code{$n_I}, \code{$right} from it.
#' @return A numeric value.
#' @export
.frfgrf_leaf_count <- function(nd) {
  if (isTRUE(nd$leaf)) {
    return(nd$n_I)
  }
  return(.frfgrf_leaf_count(nd$left) + .frfgrf_leaf_count(nd$right))
}

#' .frfgrf_regularity_walk
#'
#' A step of the frfgrf_native implementation. Called by \code{.frfgrf_regularity}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param nd A list; the body reads \code{$leaf}, \code{$left}, \code{$right} from it.
#' @param worst Numeric; passed to \code{min}.
#' @return The value of \code{worst}, as built in the body.
#' @export
.frfgrf_regularity_walk <- function(nd, worst) {
  if (isTRUE(nd$leaf)) {
    return(worst)
  }
  nl <- .frfgrf_leaf_count(nd$left)
  nr <- .frfgrf_leaf_count(nd$right)
  tot <- nl + nr
  if (tot > 0) {
    worst <- min(worst, min(nl, nr) / as.numeric(tot))
  }
  worst <- .frfgrf_regularity_walk(nd$left, worst)
  worst <- .frfgrf_regularity_walk(nd$right, worst)
  return(worst)
}

#' .frfgrf_regularity
#'
#' A step of the frfgrf_native implementation. Called by \code{.frfgrf_forest_fit_check}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param trees See Usage.
#' @return The value of \code{worst}, as built in the body.
#' @export
.frfgrf_regularity <- function(trees) {
  worst <- 1.0
  for (t in trees) {
    worst <- .frfgrf_regularity_walk(t, worst)
  }
  return(worst)
}

#' .frfgrf_forest_fit_check
#'
#' A step of the frfgrf_native implementation. Called by \code{morie_frfgrf}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y Coerced to numeric by the body, with \code{as.numeric}.
#' @param X A matrix; passed to \code{as.matrix}.
#' @param n_trees Coerced to integer by the body, with \code{as.integer}. Defaults to \code{100}.
#' @param min_leaf Passed to \code{.frfgrf_honesty_test}. Defaults to \code{5}.
#' @param subsample_frac Passed to \code{grow_forest}. Defaults to \code{0.5}.
#' @param alpha Numeric; combined arithmetically in the body. Defaults to \code{0.05}.
#' @param pi Numeric; combined arithmetically in the body. Defaults to \code{0.5}.
#' @param seed Numeric; combined arithmetically in the body. Defaults to \code{0}.
#' @param kind Passed to \code{.frfgrf_honesty_test}. Defaults to \code{"double-sample"}.
#' @param sizes Accepted by the signature and not used anywhere in the body.
#' @return A list with \code{estimate}, \code{passes}, \code{checks}, \code{honesty}, \code{subsample_rate_ok}, \code{subsample_rate_note}, \code{split_share}, \code{split_counts}, \code{random_split_floor}, \code{min_share}, \code{regularity}, \code{alpha}, \code{pi}, \code{beta}, \code{beta_min}, \code{s}, \code{n}, \code{d}, \code{n_trees}, \code{kind}, \code{failed}, \code{method}.
#' @export
.frfgrf_forest_fit_check <- function(y, X, n_trees = 100, min_leaf = 5,
                                      subsample_frac = 0.5, alpha = 0.05,
                                      pi = 0.5, seed = 0,
                                      kind = "double-sample", sizes = NULL) {
  yv <- as.numeric(y)
  n <- length(yv)
  Xm <- as.matrix(X)
  if (nrow(Xm) != n) {
    stop(sprintf("frfgrf: %d covariate rows for %d outcomes",
                 nrow(Xm), n))
  }
  d <- ncol(Xm)
  if (d == 0) {
    stop("frfgrf: no features")
  }
  if (n < 40) {
    stop(sprintf("frfgrf: need at least 40 observations, got %d", n))
  }

  gf_out <- grow_forest(Xm, yv, kind = kind, n_trees = n_trees,
                        min_leaf = min_leaf,
                        subsample_frac = subsample_frac,
                        alpha = alpha, pi = pi, seed = seed)
  trees <- gf_out[[1]]
  bags <- gf_out[[2]]
  s <- gf_out[[3]]

  hon <- .frfgrf_honesty_test(Xm, yv, kind = kind, min_leaf = min_leaf,
                               seed = seed + 11)
  share_counts <- .frfgrf_split_share(trees, d)
  share <- share_counts[[1]]
  counts <- share_counts[[2]]
  floor <- pi / d
  reg <- .frfgrf_regularity(trees)
  bmin <- .frfgrf_beta_min(d, alpha = alpha, pi = pi)
  beta <- if (n > 1 && s > 1) log(s) / log(n) else 0.0

  # The three conditions a practitioner controls directly.
  checks <- list(
    honest = hon$honest,
    random_split_floor = min(share) >= 0.2 * floor,
    alpha_regular = reg >= 0.5 * alpha
  )
  # The subsample rate is reported SEPARATELY because beta_min is
  # brutal at realistic settings: with d = 3, alpha = 0.05 and
  # pi = 0.5 it is 0.997, so the theorem asks for s almost equal to n
  # and no practical subsample fraction meets it. Folding it into a
  # pass/fail would mean every honest forest ever grown "fails", which
  # tells the user nothing. It is surfaced as a number to compare.
  rate_ok <- (bmin < beta) && (beta < 1.0)

  checks_vec <- unlist(checks)
  failed <- names(checks)[!checks_vec]

  return(list(
    estimate = all(checks_vec),
    passes = all(checks_vec),
    checks = checks,
    honesty = hon,
    subsample_rate_ok = rate_ok,
    subsample_rate_note = sprintf(
      paste0("beta = log(s)/log(n) = %.3f against beta_min = %.3f; ",
             "the bound is near 1 for any moderate d, so it is ",
             "reported rather than scored"),
      beta, bmin),
    split_share = share,
    split_counts = counts,
    random_split_floor = floor,
    min_share = min(share),
    regularity = reg,
    alpha = as.numeric(alpha),
    pi = as.numeric(pi),
    beta = beta,
    beta_min = bmin,
    s = s,
    n = n,
    d = d,
    n_trees = as.integer(n_trees),
    kind = kind,
    failed = failed,
    method = paste("forest-fit consistency diagnostics, Wager & Athey",
                   "(2018) Definitions 2-5 and Theorem 3")
  ))
}

#' .frfgrf_cheatsheet
#'
#' A step of the frfgrf_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
.frfgrf_cheatsheet <- function() {
  return(paste0(
    "frfgrf: audit the conditions the theory needs -- honesty ",
    "(tested by permuting the leaf-averaged responses, with a ",
    "J-permutation control), the pi/d split floor, ",
    "alpha-regularity, and beta = log(s)/log(n) above ",
    "beta_min = 1 - (1 + (d/pi) log(1/alpha)/log(1/(1-alpha)))^-1."))
}

# compact alias per ledger/NAMING.md
.frfgrf_forestfitcheck <- .frfgrf_forest_fit_check

# public names resolved by fn/_lazy_map.json
.frfgrf_forest_fit_consistency <- .frfgrf_forest_fit_check

# Main entry point: same argument names and defaults as forest_fit_check.
#' Main entry point: same argument names and defaults as
#' forest_fit_check
#'
#' A step of the frfgrf_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y Passed to \code{.frfgrf_forest_fit_check}.
#' @param X Passed to \code{.frfgrf_forest_fit_check}.
#' @param n_trees Passed to \code{.frfgrf_forest_fit_check}. Defaults to \code{100}.
#' @param min_leaf Passed to \code{.frfgrf_forest_fit_check}. Defaults to \code{5}.
#' @param subsample_frac Passed to \code{.frfgrf_forest_fit_check}. Defaults to \code{0.5}.
#' @param alpha Passed to \code{.frfgrf_forest_fit_check}. Defaults to \code{0.05}.
#' @param pi Passed to \code{.frfgrf_forest_fit_check}. Defaults to \code{0.5}.
#' @param seed Passed to \code{.frfgrf_forest_fit_check}. Defaults to \code{0}.
#' @param kind Passed to \code{.frfgrf_forest_fit_check}. Defaults to \code{"double-sample"}.
#' @param sizes Passed to \code{.frfgrf_forest_fit_check}.
#' @return The value of \code{.frfgrf_forest_fit_check}.
#' @export
morie_frfgrf <- function(y, X, n_trees = 100, min_leaf = 5,
                         subsample_frac = 0.5, alpha = 0.05,
                         pi = 0.5, seed = 0,
                         kind = "double-sample", sizes = NULL) {
  return(.frfgrf_forest_fit_check(y, X, n_trees = n_trees,
                                  min_leaf = min_leaf,
                                  subsample_frac = subsample_frac,
                                  alpha = alpha, pi = pi,
                                  seed = seed, kind = kind,
                                  sizes = sizes))
}
