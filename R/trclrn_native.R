# morie.fn -- function file (rootcoder007/morie)
# Tree-based individualized treatment rules.
# Two ways to choose who gets what treatment, and they fail differently.
# Fit a regression of outcome on covariates and treatment, then assign
# whatever the model says is best -- but a model simple enough to be
# interpretable is probably misspecified, and one complex enough to be
# right is unreadable. Or search directly over rules for the one with the
# best estimated value -- but then the rule has no interpretable form at
# all, which is why clinicians will not use it.
# Trees get both. Search directly over rules, but restrict the search
# to rules representable as a decision tree. The result is a policy that
# maximises estimated value *and* prints as a handful of if-then splits.
# The value of a rule, and why a regression is not needed to get it.
# With Y*(a) the potential outcome under treatment a and pi a rule,
# the target is E[Y*(pi)]. Under positivity, strong ignorability and
# consistency (Assumptions 1-3), inverse probability weighting
# identifies it from the data alone:
#   V(pi) = (1/n) sum_i Y_i * 1{A_i = pi(X_i)} / p(A_i | X_i)
# Only subjects whose observed treatment agrees with the rule contribute,
# reweighted by how likely that agreement was.
# Positivity is not a technicality. If some treatment has probability
# near zero for some covariate pattern, its weight explodes and the
# value of any rule assigning it there is estimated from almost nothing.
# Assumption 1 demands p(a | X) >= epsilon; min_propensity enforces it
# and refuses rather than returning a number built on a single
# reweighted observation.
# Purity, but for treatment allocation. A classification tree splits
# to make the response pure within nodes. Here the analogue is to split
# so that the best treatment is homogeneous within a node: the split
# score is the gain in estimated value from letting the two children
# choose different treatments rather than sharing one. That is the
# minimum-impurity decision assignment; each leaf then takes the
# treatment maximising its own value estimate.
# The augmented alternative, and why both are here. Pure IPW discards
# every discordant observation, which is wasteful and high-variance.
# Augmenting with an outcome model -- the adaptive contrast of Tao and
# Wang -- uses all the data and stays consistent if either the
# propensity or the outcome model is right. method="augmented" does
# that; method="ipw" is the unaugmented estimator.
# References:
# Laber, E. B. & Zhao, Y. Q. (2015) "Tree-based methods for
# individualized treatment regimes", Biometrika 102(3), 501-514,
# doi:10.1093/biomet/asv028. Sec. 2.1 (the optimal rule and Assumptions
# 1-3: positivity, strong ignorability, consistency), Sec. 2.2 (purity
# measures for treatment allocation and the rectangular-region
# representation), and the treatment of continuous treatments by kernel
# smoothing.
# Tao, Y. & Wang, L. (2017) "Adaptive Contrast Weighted Learning for
# Multi-Stage Multi-Treatment Decision-Making", Biometrics 73,
# 145-155, doi:10.1111/biom.12539. The adaptive contrast that augments
# the weighted-learning objective with an outcome model, implemented
# here as method="augmented".
# Zhang, B., Tsiatis, A. A., Laber, E. B. & Davidian, M. (2012) "A robust
# method for estimating optimal treatment regimes", Biometrics 68(4),
# 1010-1018, doi:10.1111/j.1541-0420.2012.01763.x. The recasting of
# treatment selection as a weighted classification problem that the
# tree search rests on.

.trclrn_EPS <- 1e-12
.trclrn_METHODS <- c("ipw", "augmented")

.trclrn_check <- function(Y, A, X, propensity, min_propensity) {
  y <- as.numeric(unlist(Y))
  a <- as.integer(unlist(A))
  if (is.data.frame(X)) {
    Xm <- as.matrix(X)
  } else if (is.matrix(X)) {
    Xm <- X
  } else if (is.list(X)) {
    Xm <- do.call(rbind, lapply(X, function(r) as.numeric(unlist(r))))
  } else {
    Xm <- matrix(as.numeric(X), ncol = 1)
  }
  storage.mode(Xm) <- "double"
  n <- length(y)
  if (!(length(a) == nrow(Xm) && length(a) == n)) {
    stop(sprintf("trclrn: Y, A and X must agree in length (%d, %d, %d)",
                 n, length(a), nrow(Xm)))
  }
  if (n < 4) {
    stop(sprintf("trclrn: need at least 4 observations, got %d", n))
  }
  arms <- sort(unique(a))
  if (length(arms) < 2) {
    stop(sprintf("trclrn: at least 2 treatment arms are needed, got %d",
                 length(arms)))
  }
  if (is.null(propensity)) {
    p <- rep(1.0 / length(arms), n)
  } else if (is.numeric(propensity) && length(propensity) == 1L) {
    p <- rep(as.numeric(propensity), n)
  } else {
    p <- as.numeric(unlist(propensity))
  }
  if (length(p) != n) {
    stop(sprintf("trclrn: %d propensities for %d observations",
                 length(p), n))
  }
  bad <- p[p < as.numeric(min_propensity)]
  if (length(bad) > 0L) {
    stop(sprintf(paste0("trclrn: %d observation(s) have a propensity ",
                        "below %g (smallest %.4g) -- Assumption 1 ",
                        "(positivity) fails and the value of a rule ",
                        "assigning that arm there is not estimable"),
                 length(bad), min_propensity, min(bad)))
  }
  return(list(y = y, a = a, Xm = Xm, p = p, n = n, arms = arms))
}

#' trclrn_rule_value
#'
#' Part of the trclrn_native implementation; see the file header for the
#' source it follows.
#'
#' @param Y See Usage.
#' @param A See Usage.
#' @param X See Usage.
#' @param rule See Usage.
#' @param propensity Defaults to \code{NULL}.
#' @param method Defaults to \code{"ipw"}.
#' @param outcome_model Defaults to \code{NULL}.
#' @param min_propensity Defaults to \code{0.01}.
#' @return A numeric value.
#' @export
trclrn_rule_value <- function(Y, A, X, rule, propensity = NULL,
                              method = "ipw", outcome_model = NULL,
                              min_propensity = 0.01) {
  if (!(method %in% .trclrn_METHODS)) {
    stop(sprintf("trclrn: method must be ipw or augmented, got %r",
                 method))
  }
  chk <- .trclrn_check(Y, A, X, propensity, min_propensity)
  y <- chk$y; a <- chk$a; Xm <- chk$Xm; p <- chk$p; n <- chk$n
  tot <- 0.0
  for (i in seq_len(n)) {
    xi <- Xm[i, , drop = FALSE]
    pi_val <- rule(xi)
    agree <- if (pi_val == a[i]) 1.0 else 0.0
    if (method == "ipw") {
      tot <- tot + (agree * y[i] / p[i])
    } else {
      if (is.null(outcome_model)) {
        stop(paste0("trclrn: method='augmented' needs an ",
                    "outcome_model(x, a) -> E[Y | x, a]"))
      }
      m <- as.numeric(outcome_model(xi, pi_val))
      tot <- tot + (m + (agree * (y[i] - m) / p[i]))
    }
  }
  return(tot / n)
}

.trclrn_best_treatment <- function(y, a, p, rows, arms, method, Xm,
                                  outcome_model) {
  best <- NULL
  bv <- NULL
  for (arm in arms) {
    tot <- 0.0
    for (i in rows) {
      agree <- if (a[i] == arm) 1.0 else 0.0
      if (method == "ipw") {
        tot <- tot + (agree * y[i] / p[i])
      } else {
        xi <- Xm[i, , drop = FALSE]
        m <- as.numeric(outcome_model(xi, arm))
        tot <- tot + (m + (agree * (y[i] - m) / p[i]))
      }
    }
    if (is.null(bv) || tot > bv) {
      best <- arm
      bv <- tot
    }
  }
  return(list(best = best, bv = bv))
}

#' trclrn_fit_tree
#'
#' Part of the trclrn_native implementation; see the file header for the
#' source it follows.
#'
#' @param Y See Usage.
#' @param A See Usage.
#' @param X See Usage.
#' @param propensity Defaults to \code{NULL}.
#' @param method Defaults to \code{"ipw"}.
#' @param outcome_model Defaults to \code{NULL}.
#' @param max_depth Defaults to \code{3}.
#' @param min_leaf Defaults to \code{10}.
#' @param n_thresholds Defaults to \code{20}.
#' @param min_propensity Defaults to \code{0.01}.
#' @return The value of \code{result}, as built in the body.
#' @export
trclrn_fit_tree <- function(Y, A, X, propensity = NULL, method = "ipw",
                            outcome_model = NULL, max_depth = 3,
                            min_leaf = 10, n_thresholds = 20,
                            min_propensity = 0.01) {
  if (!(method %in% .trclrn_METHODS)) {
    stop(sprintf("trclrn: method must be ipw or augmented, got %r",
                 method))
  }
  if (method == "augmented" && is.null(outcome_model)) {
    stop(paste0("trclrn: method='augmented' needs an ",
                "outcome_model(x, a) -> E[Y | x, a]"))
  }
  chk <- .trclrn_check(Y, A, X, propensity, min_propensity)
  y <- chk$y; a <- chk$a; Xm <- chk$Xm; p <- chk$p; n <- chk$n
  arms <- chk$arms
  if (as.integer(min_leaf) < 1L) {
    stop("trclrn: min_leaf must be at least 1")
  }
  d <- ncol(Xm)

  grow <- function(rows, depth) {
    bt <- .trclrn_best_treatment(y, a, p, rows, arms, method, Xm,
                                 outcome_model)
    arm <- bt$best
    val <- bt$bv
    node <- list(leaf = TRUE, treatment = arm, n = length(rows),
                 value = val / max(length(rows), 1L))
    if (depth >= as.integer(max_depth) ||
        length(rows) < 2L * as.integer(min_leaf)) {
      return(node)
    }
    best <- NULL
    for (j in seq_len(d)) {
      vals <- sort(unique(Xm[rows, j]))
      if (length(vals) < 2L) next
      step <- max(1L, length(vals) %/% as.integer(n_thresholds))
      for (q in seq(from = step + 1L, to = length(vals), by = step)) {
        thr <- vals[q]
        L <- rows[Xm[rows, j] < thr]
        R <- rows[Xm[rows, j] >= thr]
        if (length(L) < as.integer(min_leaf) ||
            length(R) < as.integer(min_leaf)) next
        vl_res <- .trclrn_best_treatment(y, a, p, L, arms, method, Xm,
                                         outcome_model)
        vr_res <- .trclrn_best_treatment(y, a, p, R, arms, method, Xm,
                                         outcome_model)
        vl <- vl_res$bv
        vr <- vr_res$bv
        gain <- (vl + vr) - val
        if (is.null(best) || gain > best$gain) {
          best <- list(gain = gain, j = j, thr = thr, L = L, R = R)
        }
      }
    }
    if (is.null(best) || best$gain <= .trclrn_EPS) {
      return(node)
    }
    return(list(leaf = FALSE, feature = best$j,
                threshold = best$thr, gain = best$gain,
                n = length(rows), left = grow(best$L, depth + 1L),
                right = grow(best$R, depth + 1L)))
  }

  tree <- grow(seq_len(n), 0L)

  rule <- function(x) {
    nd <- tree
    while (!nd$leaf) {
      nd <- if (as.numeric(x[nd$feature]) < nd$threshold) nd$left
            else nd$right
    }
    return(nd$treatment)
  }

  v <- trclrn_rule_value(y, a, Xm, rule, propensity = p, method = method,
                         outcome_model = outcome_model,
                         min_propensity = min_propensity)
  make_const_rule <- function(a_val) {
    function(x) a_val
  }
  fixed <- list()
  for (arm in arms) {
    fixed[[as.character(arm)]] <- trclrn_rule_value(
      y, a, Xm, make_const_rule(arm), propensity = p, method = method,
      outcome_model = outcome_model, min_propensity = min_propensity)
  }
  result <- list(
    estimate = v,
    value = v,
    tree = tree,
    rule = rule,
    fixed_arm_values = fixed,
    best_fixed_arm = as.integer(names(which.max(unlist(fixed)))),
    n = n,
    arms = arms,
    method = method,
    max_depth = as.integer(max_depth),
    min_leaf = as.integer(min_leaf),
    n_leaves = .trclrn_count_leaves(tree),
    method_name = paste0("tree-based individualized treatment rule; ",
                         "Laber & Zhao (2015) Sec. 2.2")
  )
  return(result)
}

.trclrn_count_leaves <- function(nd) {
  if (nd$leaf) return(1L)
  return(.trclrn_count_leaves(nd$left) + .trclrn_count_leaves(nd$right))
}

#' trclrn_predict_rule
#'
#' Part of the trclrn_native implementation; see the file header for the
#' source it follows.
#'
#' @param tree See Usage.
#' @param X See Usage.
#' @return The value of \code{out}, as built in the body.
#' @export
trclrn_predict_rule <- function(tree, X) {
  if (is.data.frame(X)) {
    Xm <- as.matrix(X)
  } else if (is.matrix(X)) {
    Xm <- X
  } else if (is.list(X)) {
    Xm <- do.call(rbind, lapply(X, function(r) as.numeric(unlist(r))))
  } else {
    Xm <- matrix(as.numeric(X), ncol = 1)
  }
  storage.mode(Xm) <- "double"
  if (nrow(Xm) == 0L) return(integer(0))
  out <- integer(nrow(Xm))
  for (i in seq_len(nrow(Xm))) {
    nd <- tree
    while (!nd$leaf) {
      nd <- if (as.numeric(Xm[i, nd$feature]) < nd$threshold) nd$left
            else nd$right
    }
    out[i] <- nd$treatment
  }
  return(out)
}

#' trclrn_tree_rules
#'
#' Part of the trclrn_native implementation; see the file header for the
#' source it follows.
#'
#' @param tree See Usage.
#' @param names Defaults to \code{NULL}.
#' @param indent Defaults to \code{0}.
#' @return The value of \code{out}, as built in the body.
#' @export
trclrn_tree_rules <- function(tree, names = NULL, indent = 0) {
  pad <- paste(rep(" ", indent), collapse = "")
  if (tree$leaf) {
    return(sprintf("%streat with %s  (n = %d)", pad, tree$treatment,
                   tree$n))
  }
  nm <- if (is.null(names)) sprintf("x%d", tree$feature)
        else names[tree$feature]
  out <- c(sprintf("%sif %s < %.6g:", pad, nm, tree$threshold))
  out <- c(out, trclrn_tree_rules(tree$left, names, indent + 2L))
  out <- c(out, sprintf("%selse:", pad))
  out <- c(out, trclrn_tree_rules(tree$right, names, indent + 2L))
  return(out)
}

#' trclrn_cheatsheet
#'
#' Part of the trclrn_native implementation; see the file header for the
#' source it follows.
#'
#' @return A character value.
#' @export
trclrn_cheatsheet <- function() {
  return(paste0(
    "trclrn: tree-based ITR. Value V(pi) = mean of Y * ",
    "1{A = pi(X)} / p(A|X) -- only CONCORDANT subjects ",
    "contribute, reweighted. Search over rules representable ",
    "as a tree, so the winner is both value-maximal and ",
    "readable. Split score = gain from letting two children ",
    "choose different treatments (minimum-impurity decision ",
    "assignment). Positivity is Assumption 1 and is enforced, ",
    "not assumed. method='augmented' adds an outcome model ",
    "(Tao & Wang 2017) and survives either model being wrong."))
}

# compact alias per ledger/NAMING.md
trclrn_treeoptimalregime <- trclrn_fit_tree

# public names resolved by fn/_lazy_map.json
trclrn_tree_based_regime <- trclrn_fit_tree

# Entry point
morie_trclrn <- trclrn_fit_tree
