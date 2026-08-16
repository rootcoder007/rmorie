# morie.fn -- function file (rootcoder007/morie)
# Sequential super learning.
#
# Many algorithms are available and none is best everywhere; picking one
# in advance is a bet, and picking one after looking is selection whose
# cost is usually not paid. The super learner answers both by
# constructing the optimal weighted average of a whole library,
# selected by cross-validation against an a priori specified loss.
#
# The discrete super learner picks the library member with the
# smallest cross-validated risk,
#
#     k_hat(P_n) = argmin_k (1/V) sum_v int L(Q_hat_k(P^0_{n,v})) dP^1_{n,v},
#
# training on each training split and evaluating on the corresponding
# validation split. The ensemble super learner goes further and
# fits a convex combination of the library's predictions on the
# cross-validated output.
#
# The oracle inequality is what justifies it. Provided the loss is
# uniformly bounded, the cross-validation selector performs
# asymptotically as well as the oracle that knows which candidate is
# best -- and when no candidate attains a correctly specified parametric
# rate, the super learner attains the parametric rate up to a
# log n factor.
#
# Sequential super learning applies this to the iterated conditional
# expectations of a longitudinal problem: the outcome regression at the
# last time point is fitted, its prediction becomes the outcome for the
# previous time point, and so on backwards. Those same sequential
# regressions are the initial estimator LTMLE then targets.
#
# References
# ----------
# van der Laan, M. J. & Rose, S. (2018) Targeted Learning in Data
# Science, Springer, doi:10.1007/978-3-319-65304-4. Chap. 3.
#
# van der Laan, M. J., Polley, E. C. & Hubbard, A. E. (2007) "Super
# Learner", Statistical Applications in Genetics and Molecular Biology
# 6(1), Article 25, doi:10.2202/1544-6115.1309.
#
# Bang, H. & Robins, J. M. (2005) "Doubly robust estimation in missing
# data and causal inference models", Biometrics 61(4), 962-973,
# doi:10.1111/j.1541-0420.2005.00377.x.

.tlseqsl_EPS <- 1e-12
.tlseqsl_LOSSES <- c("squared", "log")

.tlseqsl_loss <- function(kind, y, p) {
  if (kind == "squared") {
    return((y - p) ^ 2)
  }
  q <- min(max(p, .tlseqsl_EPS), 1.0 - .tlseqsl_EPS)
  return(-(y * log(q) + (1.0 - y) * log(1.0 - q)))
}

#' morie_tlseqsl_cv_folds
#'
#' Part of the tlseqsl_native implementation; see the file header for
#' the source it follows.
#'
#' @param n See Usage.
#' @param V Defaults to \code{10}.
#' @param seed Defaults to \code{0}.
#' @return The value of \code{folds}, as built in the body.
#' @export
morie_tlseqsl_cv_folds <- function(n, V=10, seed=0) {
  if (V < 2 || V > n) {
    stop(sprintf("tlseqsl: V must lie in 2..%d, got %d", n, V))
  }
  rng <- .ghc_rng(seed)
  idx <- seq_len(n) - 1L
  if (n > 1) {
    for (i_1based in n:2) {
      i <- i_1based - 1L
      u <- .ghc_unif(rng, 1)
      j <- (as.integer(u * (i + 1))) %% (i + 1)
      tmp <- idx[i_1based]
      idx[i_1based] <- idx[j + 1L]
      idx[j + 1L] <- tmp
    }
  }
  folds <- vector("list", V)
  for (v_0based in 0:(V - 1)) {
    indices <- seq(v_0based + 1L, n, by = V)
    folds[[v_0based + 1L]] <- idx[indices]
  }
  folds
}

#' morie_tlseqsl_cv_risk
#'
#' Part of the tlseqsl_native implementation; see the file header for
#' the source it follows.
#'
#' @param X See Usage.
#' @param y See Usage.
#' @param algorithm See Usage.
#' @param V Defaults to \code{10}.
#' @param loss Defaults to \code{"squared"}.
#' @param seed Defaults to \code{0}.
#' @return A list with \code{risk}, \code{cv_predictions}, \code{V}, \code{loss}.
#' @export
morie_tlseqsl_cv_risk <- function(X, y, algorithm, V=10, loss="squared", seed=0) {
  if (!(loss %in% .tlseqsl_LOSSES)) {
    stop(sprintf("tlseqsl: loss must be one of %s, got %r",
                 paste(.tlseqsl_LOSSES, collapse = ", "), loss))
  }
  rows <- as.matrix(X)
  t_vec <- as.numeric(y)
  n <- length(t_vec)
  folds <- morie_tlseqsl_cv_folds(n, V, seed)
  tot <- 0.0
  m <- 0
  preds <- rep(0.0, n)
  for (f in folds) {
    tr <- setdiff(seq_len(n) - 1L, f)
    tr_1based <- tr + 1L
    fit <- algorithm(rows[tr_1based, , drop = FALSE], t_vec[tr_1based])
    for (i in f) {
      i_1based <- i + 1L
      p <- as.numeric(fit(rows[i_1based, ]))
      preds[i_1based] <- p
      tot <- tot + .tlseqsl_loss(loss, t_vec[i_1based], p)
      m <- m + 1
    }
  }
  list(risk = tot / m, cv_predictions = preds,
       V = as.integer(V), loss = loss)
}

#' morie_tlseqsl_discrete_super_learner
#'
#' Part of the tlseqsl_native implementation; see the file header for
#' the source it follows.
#'
#' @param X See Usage.
#' @param y See Usage.
#' @param library See Usage.
#' @param V Defaults to \code{10}.
#' @param loss Defaults to \code{"squared"}.
#' @param seed Defaults to \code{0}.
#' @return A list with \code{selected}, \code{risks}, \code{cv_predictions}, \code{note}.
#' @export
morie_tlseqsl_discrete_super_learner <- function(X, y, library, V=10,
                                                 loss="squared", seed=0) {
  if (length(library) == 0) {
    stop("tlseqsl: the library is empty")
  }
  risks <- list()
  cvp <- list()
  for (name in names(library)) {
    alg <- library[[name]]
    r <- morie_tlseqsl_cv_risk(X, y, alg, V, loss, seed)
    risks[[name]] <- r$risk
    cvp[[name]] <- r$cv_predictions
  }
  best <- NULL
  best_risk <- Inf
  for (name in names(risks)) {
    if (risks[[name]] < best_risk) {
      best_risk <- risks[[name]]
      best <- name
    }
  }
  list(selected = best, risks = risks, cv_predictions = cvp,
       note = "asymptotically as good as the oracle that knows which candidate is best")
}

#' morie_tlseqsl_ensemble_super_learner
#'
#' Part of the tlseqsl_native implementation; see the file header for
#' the source it follows.
#'
#' @param X See Usage.
#' @param y See Usage.
#' @param library See Usage.
#' @param V Defaults to \code{10}.
#' @param loss Defaults to \code{"squared"}.
#' @param seed Defaults to \code{0}.
#' @param grid Defaults to \code{21}.
#' @return A list with \code{estimate}, \code{weights}, \code{cv_risk}, \code{discrete_risks}, \code{discrete_choice}, \code{best_single}, \code{method}, \code{note}.
#' @export
morie_tlseqsl_ensemble_super_learner <- function(X, y, library, V=10,
                                                 loss="squared", seed=0,
                                                 grid=21) {
  d <- morie_tlseqsl_discrete_super_learner(X, y, library, V, loss, seed)
  names_vec <- sort(names(library))
  t_vec <- as.numeric(y)
  n <- length(t_vec)
  P <- vector("list", length(names_vec))
  for (i in seq_along(names_vec)) {
    P[[i]] <- d$cv_predictions[[names_vec[i]]]
  }
  if (length(names_vec) == 1) {
    w <- c(1.0)
  } else if (length(names_vec) == 2) {
    best <- NULL
    bw <- NULL
    for (gi in 0:(grid - 1)) {
      a <- gi / (grid - 1)
      r <- 0
      for (i in seq_len(n)) {
        r <- r + .tlseqsl_loss(loss, t_vec[i],
                               a * P[[1]][i] + (1 - a) * P[[2]][i])
      }
      r <- r / n
      if (is.null(best) || r < best) {
        best <- r
        bw <- c(a, 1 - a)
      }
    }
    w <- bw
  } else {
    inv <- numeric(length(names_vec))
    for (i in seq_along(names_vec)) {
      inv[i] <- 1.0 / max(d$risks[[names_vec[i]]], .tlseqsl_EPS)
    }
    s <- sum(inv)
    w <- inv / s
  }
  ens <- numeric(n)
  for (i in seq_len(n)) {
    s <- 0
    for (j in seq_along(names_vec)) {
      s <- s + w[j] * P[[j]][i]
    }
    ens[i] <- s
  }
  risk <- 0
  for (i in seq_len(n)) {
    risk <- risk + .tlseqsl_loss(loss, t_vec[i], ens[i])
  }
  risk <- risk / n

  weights_list <- as.list(w)
  names(weights_list) <- names_vec

  list(
    estimate = w,
    weights = weights_list,
    cv_risk = risk,
    discrete_risks = d$risks,
    discrete_choice = d$selected,
    best_single = min(unlist(d$risks)),
    method = "super learner; van der Laan, Polley & Hubbard (2007), van der Laan & Rose (2018) Chap. 3",
    note = "weights are non-negative and sum to 1 -- a weighted AVERAGE, not an unconstrained regression"
  )
}

#' morie_tlseqsl_sequential_super_learner
#'
#' Part of the tlseqsl_native implementation; see the file header for
#' the source it follows.
#'
#' @param histories See Usage.
#' @param outcomes See Usage.
#' @param library See Usage.
#' @param T See Usage.
#' @param V Defaults to \code{5}.
#' @param seed Defaults to \code{0}.
#' @return A list with \code{estimate}, \code{mean}, \code{sequential_fits}, \code{T}, \code{method}.
#' @export
morie_tlseqsl_sequential_super_learner <- function(histories, outcomes, library,
                                                    T, V=5, seed=0) {
  if (T < 1) {
    stop("tlseqsl: need at least one time point")
  }
  y_vec <- as.numeric(outcomes)
  fits <- list()
  current <- y_vec
  n <- length(histories)
  for (t in (T - 1):0) {
    t_len <- t + 1L
    X <- matrix(0, nrow = n, ncol = t_len)
    for (i in seq_len(n)) {
      X[i, ] <- histories[[i]][1:t_len]
    }
    sl <- morie_tlseqsl_ensemble_super_learner(X, current, library, V,
                                               "squared", seed)
    names_vec <- sort(names(library))
    d <- morie_tlseqsl_discrete_super_learner(X, current, library, V,
                                              "squared", seed)
    P <- vector("list", length(names_vec))
    for (i in seq_along(names_vec)) {
      P[[i]] <- d$cv_predictions[[names_vec[i]]]
    }
    w <- numeric(length(names_vec))
    for (i in seq_along(names_vec)) {
      w[i] <- sl$weights[[names_vec[i]]]
    }
    new_current <- numeric(length(current))
    for (i in seq_along(current)) {
      s <- 0
      for (j in seq_along(names_vec)) {
        s <- s + w[j] * P[[j]][i]
      }
      new_current[i] <- s
    }
    current <- new_current
    fits[[length(fits) + 1L]] <- list(t = t, weights = sl$weights,
                                      cv_risk = sl$cv_risk)
  }
  mean_val <- sum(current) / length(current)
  list(
    estimate = mean_val,
    mean = mean_val,
    sequential_fits = rev(fits),
    T = as.integer(T),
    method = "sequential super learning; van der Laan & Rose (2018) Chap. 3"
  )
}

#' morie_tlseqsl_cheatsheet
#'
#' Part of the tlseqsl_native implementation; see the file header for
#' the source it follows.
#'
#' @return A character value.
#' @export
morie_tlseqsl_cheatsheet <- function() {
  paste0(
    "tlseqsl: no algorithm is best everywhere, so choose by ",
    "CROSS-VALIDATION over a library and take the optimal ",
    "WEIGHTED AVERAGE. Discrete super learner = the CV ",
    "selector; the ensemble fits convex weights on the ",
    "cross-validated predictions. The oracle inequality (loss ",
    "bounded) says it does as well as the best candidate ",
    "asymptotically, and attains the parametric rate up to ",
    "log n when no candidate does. SEQUENTIAL super learning ",
    "runs this backwards through the iterated conditional ",
    "expectations -- the same regressions LTMLE then targets."
  )
}

# compact alias per ledger/NAMING.md
morie_tlseqsl_sequentialsuperlearner <- morie_tlseqsl_sequential_super_learner
