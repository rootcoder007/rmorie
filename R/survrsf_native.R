# morie.fn -- function file (rootcoder007/morie)
# Random survival forests.
#
# The algorithm, in the paper's five steps: draw B bootstrap samples
# (each leaving out about 37% of the data, the out-of-bag cases); grow
# a survival tree on each, choosing at every node p candidate
# variables at random and splitting on the one that maximises survival
# difference between the daughters; grow to full size subject to every
# terminal node holding at least d0 unique deaths; take the
# Nelson-Aalen estimator in each terminal node and average over trees;
# and estimate prediction error on the out-of-bag data.
#
# The terminal node estimator: H_h(t) = sum_{t_lh <= t} d_lh / Y_lh.
#
# Conservation of events (Lemma 1) is an exact identity: the hazard
# summed over the observed times -- censored included -- returns the
# number of deaths in the node exactly.
#
# Mortality M_i = sum_j H_e(T_j | x_i): a count of deaths, not a
# probability. Prediction error is 1 - C with Harrell's four-step
# concordance index.
#
# Four splitting rules: logrank, logrankrandom, logrankscore (Hothorn
# & Lausen's standardised statistic on Lausen's log-rank scores) and
# conserve (conservation of events, 1/(1+Conserve)).
#
# Variable importance (Sec. 7): drop the out-of-bag cases down their
# in-bag tree and, at any split on x, send the case to a daughter at
# random; VIMP is the resulting prediction error minus the original.
#
# References
# ----------
# Ishwaran, H., Kogalur, U. B., Blackstone, E. H. & Lauer, M. S.
# (2008) "Random Survival Forests", Annals of Applied Statistics 2(3),
# 841-860, doi:10.1214/08-AOAS169.
#
# Harrell, F. et al. (1982) "Evaluating the Yield of Medical Tests",
# JAMA 247(18), 2543-2546, doi:10.1001/jama.1982.03320430047030.
#
# Hothorn, T. & Lausen, B. (2003) "On the exact distribution of
# maximally selected rank statistics", Computational Statistics & Data
# Analysis 43(2), 121-137, doi:10.1016/S0167-9473(02)00225-6.
#
# Ishwaran, H. & Kogalur, U. B. (2007) "Random Survival Forests for
# R", R News 7(2), 25-31.

morie_survrsf_SPLIT_RULES <- c("logrank", "logrankrandom", "conserve",
                               "logrankscore")
.survrsf_AVAILABLE <- c("logrank", "logrankrandom", "logrankscore",
                        "conserve")
.survrsf_UNSOURCED <- list()

#' Which of the paper\'s four splitting rules are implemented
#'
#' Part of the survrsf_native implementation; see the file header for
#' the source it follows.
#'
#' @param rule Defaults to \code{NULL}.
#' @return A list with \code{rule}, \code{available}, \code{reason}.
#' @export
morie_survrsf_rule_status <- function(rule=NULL) {
  # Which of the paper's four splitting rules are implemented.
  if (is.null(rule)) {
    return(list(rules=morie_survrsf_SPLIT_RULES,
                available=.survrsf_AVAILABLE,
                unavailable=.survrsf_UNSOURCED))
  }
  if (!(rule %in% morie_survrsf_SPLIT_RULES)) {
    stop(sprintf("survrsf: rule must be one of %s, got %s",
                 paste(morie_survrsf_SPLIT_RULES, collapse=", "), rule))
  }
  reason <- .survrsf_UNSOURCED[[rule]]
  list(rule=rule, available=(rule %in% .survrsf_AVAILABLE),
       reason=if (is.null(reason)) "" else reason)
}

.survrsf_check_rule <- function(rule) {
  if (!(rule %in% morie_survrsf_SPLIT_RULES)) {
    stop(sprintf("survrsf: rule must be one of %s, got %s",
                 paste(morie_survrsf_SPLIT_RULES, collapse=", "), rule))
  }
  if (!(rule %in% .survrsf_AVAILABLE)) {
    stop(sprintf("survrsf: the %s splitting rule is not implemented",
                 rule))
  }
}

# Small deterministic generator: a float-safe 32-bit LCG. (Exact match
# to the Python 64-bit generator is not required -- the forest's
# randomness only needs to be reproducible.)
.survrsf_rng <- function(seed=0) {
  e <- new.env(parent=emptyenv())
  e$s <- as.numeric(seed) %% 2147483648
  if (e$s == 0) {
    e$s <- 1
  }
  # Python _Rng is the 64-bit MMIX LCG: s = s*6364136223846793005 +
  # 1442695040888963407 mod 2^64, output (s >> 11) / 2^53. Carried here
  # as (hi, lo) 32-bit words via the exact ghc limb helpers, seeded the
  # same way (one multiply-add applied to the seed itself).
  A64 <- c(1481765933, 1284865837)
  C64 <- list(hi = 335903614, lo = 4150755663)
  sd0 <- as.numeric(seed)
  st <- .ghc_add64(.ghc_mul64(list(hi = floor(sd0 / 4294967296),
                                   lo = sd0 %% 4294967296), A64), C64)
  e$w <- st
  e$next_ <- function() {
    e$w <- .ghc_add64(.ghc_mul64(e$w, A64), C64)
    e$w$hi / 4294967296 + floor(e$w$lo / 2048) / 9007199254740992
  }
  e$randint <- function(n) {
    as.integer(e$next_() * n) %% n
  }
  e$sample_ <- function(seq_, k) {
    pool <- as.list(seq_)
    out <- vector("list", 0)
    for (i in seq_len(min(k, length(pool)))) {
      pick <- e$randint(length(pool)) + 1L
      out <- c(out, pool[pick])
      pool <- pool[-pick]
    }
    unlist(out)
  }
  e
}

#' The terminal-node estimator of equation (3.1)
#'
#' Part of the survrsf_native implementation; see the file header for
#' the source it follows.
#'
#' @param times See Usage.
#' @param events See Usage.
#' @return A list with \code{time}, \code{chf}, \code{n}, \code{deaths}.
#' @export
morie_survrsf_nelson_aalen <- function(times, events) {
  # The terminal-node estimator of equation (3.1).
  n <- length(times)
  if (n != length(events)) {
    stop(sprintf("survrsf: %d times but %d event indicators", n,
                 length(events)))
  }
  if (n == 0L) {
    stop("survrsf: no observations")
  }
  order_ <- order(times)
  st <- times[order_]
  se <- events[order_]
  ts <- numeric(0)
  ds <- numeric(0)
  cum <- 0.0
  i <- 1L
  while (i <= n) {
    t <- st[i]
    j <- i
    d <- 0L
    while (j <= n && st[j] == t) {
      d <- d + as.integer(se[j] != 0)
      j <- j + 1L
    }
    at_risk <- n - i + 1L
    if (d > 0L) {
      cum <- cum + d / at_risk
      ts <- c(ts, t)
      ds <- c(ds, cum)
    }
    i <- j
  }
  list(time=ts, chf=ds, n=n, deaths=as.integer(sum(events != 0)))
}

.survrsf_chf_at <- function(na, t) {
  out <- 0.0
  tm <- na$time
  for (i in seq_along(tm)) {
    if (tm[i] <= t) {
      out <- na$chf[i]
    } else {
      break
    }
  }
  out
}

#' Lemma 1: the hazard summed over observed times is the deaths
#'
#' Censored times count too.
#'
#' @param times See Usage.
#' @param events See Usage.
#' @return A list with \code{sum_chf}, \code{deaths}, \code{difference}, \code{conserved}.
#' @export
morie_survrsf_conservation_check <- function(times, events) {
  # Lemma 1: the hazard summed over observed times is the deaths.
  # Censored times count too.
  na <- morie_survrsf_nelson_aalen(times, events)
  total <- sum(vapply(times, function(t) .survrsf_chf_at(na, t),
                      numeric(1)))
  deaths <- sum(events != 0)
  list(sum_chf=total, deaths=deaths, difference=total - deaths,
       conserved=abs(total - deaths) < 1e-9)
}

#' The two-sample log-rank statistic used for splitting
#'
#' Part of the survrsf_native implementation; see the file header for
#' the source it follows.
#'
#' @param times See Usage.
#' @param events See Usage.
#' @param group See Usage.
#' @return A numeric value.
#' @export
morie_survrsf_logrank_statistic <- function(times, events, group) {
  # The two-sample log-rank statistic used for splitting.
  n <- length(times)
  if (!(n == length(events) && n == length(group))) {
    stop("survrsf: times, events and group must have the same length")
  }
  order_ <- order(times)
  st <- times[order_]
  se <- events[order_]
  sg <- group[order_]
  num <- 0.0
  var_ <- 0.0
  i <- 1L
  while (i <= n) {
    t <- st[i]
    j <- i
    d <- 0L
    d1 <- 0L
    while (j <= n && st[j] == t) {
      if (se[j]) {
        d <- d + 1L
        if (sg[j]) {
          d1 <- d1 + 1L
        }
      }
      j <- j + 1L
    }
    at_risk <- n - i + 1L
    r1 <- sum(as.logical(sg[i:n]))
    if (d > 0L && at_risk > 1L) {
      num <- num + d1 - d * r1 / at_risk
      var_ <- var_ + (d * (r1 / at_risk) * (1.0 - r1 / at_risk) *
                      (at_risk - d) / (at_risk - 1L))
    } else if (d > 0L) {
      num <- num + d1 - d * r1 / at_risk
    }
    i <- j
  }
  if (var_ <= 0.0) {
    return(0.0)
  }
  abs(num) / sqrt(var_)
}

#' Lausen\'s log-rank scores, Hothorn & Lausen (2003) eq. (13)
#'
#' Without censoring or ties these are the Savage scores and sum to
#' zero.
#'
#' @param times See Usage.
#' @param events See Usage.
#' @return The value of \code{out}, as built in the body.
#' @export
morie_survrsf_logrank_scores <- function(times, events) {
  # Lausen's log-rank scores, Hothorn & Lausen (2003) eq. (13).
  # Without censoring or ties these are the Savage scores and sum to
  # zero.
  N <- length(times)
  if (N != length(events)) {
    stop("survrsf: times and events must have the same length")
  }
  if (N == 0L) {
    stop("survrsf: no observations")
  }
  gamma <- vapply(seq_len(N),
                  function(j) sum(times <= times[j]), numeric(1))
  out <- numeric(N)
  for (i in seq_len(N)) {
    s <- 0.0
    for (j in seq_len(N)) {
      if (times[j] <= times[i] && events[j]) {
        s <- s + 1.0 / (N - gamma[j] + 1)
      }
    }
    out[i] <- as.numeric(events[i] != 0) - s
  }
  out
}

#' morie_survrsf_logrank_score_statistic
#'
#' Part of the survrsf_native implementation; see the file header for
#' the source it follows.
#'
#' @param times See Usage.
#' @param events See Usage.
#' @param group See Usage.
#' @param scores Defaults to \code{NULL}.
#' @return A numeric value.
#' @export
morie_survrsf_logrank_score_statistic <- function(times, events, group,
                                                  scores=NULL) {
  # The standardised statistic of Hothorn & Lausen (2003) eqs (1)-(4),
  # with moments conditional on the scores.
  N <- length(times)
  a <- if (is.null(scores)) {
    morie_survrsf_logrank_scores(times, events)
  } else {
    as.numeric(scores)
  }
  if (!(N == length(group) && N == length(a))) {
    stop("survrsf: times, group and scores must have the same length")
  }
  m <- sum(!as.logical(group))
  n <- N - m
  if (m == 0L || n == 0L || N < 2L) {
    return(0.0)
  }
  T <- sum(a[!as.logical(group)])
  sa <- sum(a)
  sa2 <- sum(a * a)
  ET <- m * sa / N
  VT <- (m * n / (N * N * (N - 1))) * (N * sa2 - sa * sa)
  if (VT <= 0.0) {
    return(0.0)
  }
  abs(T - ET) / sqrt(VT)
}

#' The partial sums M_k of Ishwaran & Kogalur (2007) over the ordered
#'
#' times of a node. Lemma 1 forces M_n = 0.
#'
#' @param times See Usage.
#' @param events See Usage.
#' @return The value of \code{out}, as built in the body.
#' @export
morie_survrsf_conservation_residuals <- function(times, events) {
  # The partial sums M_k of Ishwaran & Kogalur (2007) over the ordered
  # times of a node. Lemma 1 forces M_n = 0.
  n <- length(times)
  if (n != length(events)) {
    stop("survrsf: times and events must have the same length")
  }
  if (n == 0L) {
    return(numeric(0))
  }
  na <- morie_survrsf_nelson_aalen(times, events)
  order_ <- order(times)
  out <- numeric(n)
  h_sum <- 0.0
  d_sum <- 0.0
  pos <- 1L
  for (i in order_) {
    h_sum <- h_sum + .survrsf_chf_at(na, times[i])
    d_sum <- d_sum + (if (events[i]) 1.0 else 0.0)
    out[pos] <- h_sum - d_sum
    pos <- pos + 1L
  }
  out
}

#' Conservation-of-events splitting, Ishwaran & Kogalur (2007)
#'
#' Returns the transform 1/(1 + Conserve).
#'
#' @param times See Usage.
#' @param events See Usage.
#' @param group See Usage.
#' @return A numeric value.
#' @export
morie_survrsf_conserve_statistic <- function(times, events, group) {
  # Conservation-of-events splitting, Ishwaran & Kogalur (2007).
  # Returns the transform 1/(1 + Conserve).
  n <- length(times)
  if (!(n == length(events) && n == length(group))) {
    stop("survrsf: times, events and group must have the same length")
  }
  total <- 0.0
  weight <- 0.0
  for (g in c(0L, 1L)) {
    idx <- which(as.integer(as.logical(group)) == g)
    if (length(idx) == 0L) {
      return(0.0)
    }
    m <- morie_survrsf_conservation_residuals(times[idx], events[idx])
    y1 <- length(idx)
    total <- total + y1 * sum(abs(m[-length(m)]))
    weight <- weight + y1
  }
  conserve <- if (weight > 0.0) total / weight else 0.0
  1.0 / (1.0 + conserve)
}

#' morie_survrsf_best_split
#'
#' Part of the survrsf_native implementation; see the file header for
#' the source it follows.
#'
#' @param X See Usage.
#' @param times See Usage.
#' @param events See Usage.
#' @param features See Usage.
#' @param min_deaths Defaults to \code{3}.
#' @param rule Defaults to \code{"logrank"}.
#' @param rng Defaults to \code{NULL}.
#' @return The value of \code{best}, as built in the body.
#' @export
morie_survrsf_best_split <- function(X, times, events, features,
                                     min_deaths=3, rule="logrank",
                                     rng=NULL) {
  # Search the candidate variables for the best split. features are
  # 0-based column indices; the returned left/right are 1-based row
  # positions into the node's cases.
  .survrsf_check_rule(rule)
  n <- length(times)
  best <- NULL
  scores <- if (rule == "logrankscore") {
    morie_survrsf_logrank_scores(times, events)
  } else {
    NULL
  }
  for (j in features) {
    col <- X[, j + 1L]
    vals <- sort(unique(col))
    if (length(vals) < 2L) {
      next
    }
    cuts <- (vals[-length(vals)] + vals[-1L]) / 2.0
    if (rule == "logrankrandom") {
      if (is.null(rng)) {
        rng <- .survrsf_rng(0)
      }
      cuts <- cuts[rng$randint(length(cuts)) + 1L]
    }
    for (c in cuts) {
      grp <- as.integer(col > c)
      left <- which(grp == 0L)
      right <- which(grp == 1L)
      if (sum(events[left] != 0) < min_deaths ||
          sum(events[right] != 0) < min_deaths) {
        next
      }
      stat <- if (rule == "logrankscore") {
        morie_survrsf_logrank_score_statistic(times, events, grp, scores)
      } else if (rule == "conserve") {
        morie_survrsf_conserve_statistic(times, events, grp)
      } else {
        morie_survrsf_logrank_statistic(times, events, grp)
      }
      if (is.null(best) || stat > best$statistic) {
        best <- list(variable=j, cut=c, statistic=stat,
                     left=left, right=right)
      }
    }
  }
  best
}

#' morie_survrsf_grow_tree
#'
#' Part of the survrsf_native implementation; see the file header for
#' the source it follows.
#'
#' @param X See Usage.
#' @param times See Usage.
#' @param events See Usage.
#' @param mtry Defaults to \code{NULL}.
#' @param min_deaths Defaults to \code{3}.
#' @param rule Defaults to \code{"logrank"}.
#' @param seed Defaults to \code{0}.
#' @param rng Defaults to \code{NULL}.
#' @return A list with \code{root}, \code{rule}, \code{mtry}, \code{min_deaths}, \code{n}.
#' @export
morie_survrsf_grow_tree <- function(X, times, events, mtry=NULL,
                                    min_deaths=3, rule="logrank",
                                    seed=0, rng=NULL) {
  # One survival tree, grown to saturation under d0.
  .survrsf_check_rule(rule)
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  n <- length(times)
  if (n == 0L) {
    stop("survrsf: no observations")
  }
  d <- ncol(X)
  mtry <- if (!is.null(mtry) && mtry) {
    as.integer(mtry)
  } else {
    max(1L, as.integer(sqrt(d)))
  }
  if (is.null(rng)) {
    rng <- .survrsf_rng(seed)
  }
  build <- function(idx, depth) {
    t <- times[idx]
    e <- events[idx]
    if (sum(e != 0) < 2L * min_deaths || depth > 40L) {
      return(list(leaf=TRUE, na=morie_survrsf_nelson_aalen(t, e),
                  n=length(idx), idx=idx))
    }
    feats <- rng$sample_(seq_len(d) - 1L, mtry)  # 0-based features
    sub <- X[idx, , drop=FALSE]
    sp <- morie_survrsf_best_split(sub, t, e, feats, min_deaths, rule,
                                   rng)
    if (is.null(sp)) {
      return(list(leaf=TRUE, na=morie_survrsf_nelson_aalen(t, e),
                  n=length(idx), idx=idx))
    }
    left <- idx[sp$left]
    right <- idx[sp$right]
    list(leaf=FALSE, variable=sp$variable, cut=sp$cut,
         statistic=sp$statistic,
         left=build(left, depth + 1L),
         right=build(right, depth + 1L))
  }
  list(root=build(seq_len(n), 0L), rule=rule, mtry=mtry,
       min_deaths=as.integer(min_deaths), n=n)
}

#' morie_survrsf_predict_tree
#'
#' Part of the survrsf_native implementation; see the file header for
#' the source it follows.
#'
#' @param tree See Usage.
#' @param x See Usage.
#' @param random_variable Defaults to \code{NULL}.
#' @param rng Defaults to \code{NULL}.
#' @return The value of \code{node}, as built in the body.
#' @export
morie_survrsf_predict_tree <- function(tree, x, random_variable=NULL,
                                       rng=NULL) {
  # Drop a case down the tree and return its terminal node.
  # random_variable (0-based) implements the Sec. 7 importance device:
  # at any split on that variable, the daughter is chosen at random.
  node <- tree$root
  while (!node$leaf) {
    if (!is.null(random_variable) && node$variable == random_variable) {
      r <- if (is.null(rng)) .survrsf_rng(0) else rng
      go_right <- r$next_() < 0.5
    } else {
      go_right <- x[node$variable + 1L] > node$cut
    }
    node <- if (go_right) node$right else node$left
  }
  node
}

#' morie_survrsf_forest
#'
#' Part of the survrsf_native implementation; see the file header for
#' the source it follows.
#'
#' @param X See Usage.
#' @param times See Usage.
#' @param events See Usage.
#' @param n_trees Defaults to \code{50}.
#' @param mtry Defaults to \code{NULL}.
#' @param min_deaths Defaults to \code{3}.
#' @param rule Defaults to \code{"logrank"}.
#' @param seed Defaults to \code{0}.
#' @return A list with \code{trees}, \code{inbag}, \code{n}, \code{rule}, \code{n_trees}, \code{oob_fraction}, \code{times}, \code{events}.
#' @export
morie_survrsf_forest <- function(X, times, events, n_trees=50, mtry=NULL,
                                 min_deaths=3, rule="logrank", seed=0) {
  # Grow the forest, keeping the out-of-bag membership.
  .survrsf_check_rule(rule)
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  n <- length(times)
  rng <- .survrsf_rng(seed)
  trees <- list()
  inbag <- list()
  for (b in seq_len(as.integer(n_trees))) {
    boot <- vapply(seq_len(n), function(z) rng$randint(n) + 1L, integer(1))
    used <- unique(boot)
    Xb <- X[boot, , drop=FALSE]
    tb <- times[boot]
    eb <- events[boot]
    if (sum(eb != 0) < 2L * min_deaths) {
      next
    }
    trees[[length(trees) + 1L]] <- morie_survrsf_grow_tree(Xb, tb, eb,
                                                           mtry, min_deaths,
                                                           rule, rng=rng)
    inbag[[length(inbag) + 1L]] <- used
  }
  if (length(trees) == 0L) {
    stop(sprintf(paste0("survrsf: no tree could be grown; the data hold ",
                        "too few deaths for min_deaths = %d"), min_deaths))
  }
  oob_fraction <- sum(vapply(inbag, function(u) n - length(u),
                             numeric(1))) / (length(inbag) * n)
  list(trees=trees, inbag=inbag, n=n, rule=rule, n_trees=length(trees),
       oob_fraction=oob_fraction, times=times, events=events)
}

#' morie_survrsf_ensemble_chf
#'
#' Part of the survrsf_native implementation; see the file header for
#' the source it follows.
#'
#' @param fit See Usage.
#' @param X See Usage.
#' @param t See Usage.
#' @param oob Defaults to \code{TRUE}.
#' @param random_variable Defaults to \code{NULL}.
#' @param seed Defaults to \code{1}.
#' @return The value of \code{out}, as built in the body.
#' @export
morie_survrsf_ensemble_chf <- function(fit, X, t, oob=TRUE,
                                       random_variable=NULL, seed=1) {
  # Equations (3.2) and (3.3): the out-of-bag or bootstrap ensemble.
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  rng <- .survrsf_rng(seed)
  out <- numeric(nrow(X))
  for (i in seq_len(nrow(X))) {
    vals <- 0.0
    count <- 0L
    for (b in seq_along(fit$trees)) {
      if (oob && (i %in% fit$inbag[[b]])) {
        next
      }
      node <- morie_survrsf_predict_tree(fit$trees[[b]], X[i, ],
                                         random_variable, rng)
      vals <- vals + .survrsf_chf_at(node$na, t)
      count <- count + 1L
    }
    out[i] <- if (count > 0L) vals / count else NaN
  }
  out
}

#' morie_survrsf_mortality
#'
#' Part of the survrsf_native implementation; see the file header for
#' the source it follows.
#'
#' @param fit See Usage.
#' @param X See Usage.
#' @param oob Defaults to \code{TRUE}.
#' @param random_variable Defaults to \code{NULL}.
#' @param seed Defaults to \code{1}.
#' @return The value of \code{out}, as built in the body.
#' @export
morie_survrsf_mortality <- function(fit, X, oob=TRUE,
                                    random_variable=NULL, seed=1) {
  # Sec. 4.1: the hazard summed over every observed time.
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  rng <- .survrsf_rng(seed)
  ts <- fit$times
  out <- numeric(nrow(X))
  for (i in seq_len(nrow(X))) {
    total <- 0.0
    count <- 0L
    for (b in seq_along(fit$trees)) {
      if (oob && (i %in% fit$inbag[[b]])) {
        next
      }
      node <- morie_survrsf_predict_tree(fit$trees[[b]], X[i, ],
                                         random_variable, rng)
      total <- total + sum(vapply(ts,
                                  function(t) .survrsf_chf_at(node$na, t),
                                  numeric(1)))
      count <- count + 1L
    }
    out[i] <- if (count > 0L) total / count else NaN
  }
  out
}

#' Harrell\'s C by the paper\'s four steps. predicted is a
#'
#' worse-outcome score: larger means the case is expected to fail
#' sooner.
#'
#' @param times See Usage.
#' @param events See Usage.
#' @param predicted See Usage.
#' @return A list with \code{c_index}, \code{concordance}, \code{permissible}, \code{prediction_error}.
#' @export
morie_survrsf_c_index <- function(times, events, predicted) {
  # Harrell's C by the paper's four steps. predicted is a
  # worse-outcome score: larger means the case is expected to fail
  # sooner.
  n <- length(times)
  if (!(n == length(events) && n == length(predicted))) {
    stop(paste0("survrsf: times, events and predictions must have the ",
                "same length"))
  }
  permissible <- 0.0
  concordance <- 0.0
  for (i in seq_len(n)) {
    for (j in seq.int(i + 1L, length.out=max(0L, n - i))) {
      ti <- times[i]
      tj <- times[j]
      ei <- events[i]
      ej <- events[j]
      if (ti < tj && !ei) {
        next
      }
      if (tj < ti && !ej) {
        next
      }
      if (ti == tj && !(ei || ej)) {
        next
      }
      permissible <- permissible + 1.0
      if (ti != tj) {
        if (ti < tj) {
          short <- i
          long_ <- j
        } else {
          short <- j
          long_ <- i
        }
        ps <- predicted[short]
        pl <- predicted[long_]
        if (ps > pl) {
          concordance <- concordance + 1.0
        } else if (ps == pl) {
          concordance <- concordance + 0.5
        }
      } else if (ei && ej) {
        concordance <- concordance + (if (predicted[i] == predicted[j]) 1.0 else 0.5)
      } else {
        dead <- if (ei) i else j
        other <- if (ei) j else i
        if (predicted[dead] > predicted[other]) {
          concordance <- concordance + 1.0
        } else {
          concordance <- concordance + 0.5
        }
      }
    }
  }
  if (permissible == 0.0) {
    stop(paste0("survrsf: no permissible pairs -- every pair has its ",
                "shorter time censored"))
  }
  list(c_index=concordance / permissible, concordance=concordance,
       permissible=permissible,
       prediction_error=1.0 - concordance / permissible)
}

#' Sec. 7: random daughter assignment at splits on x. Keys are
#'
#' 0-based variable indices to match the Python.
#'
#' @param fit See Usage.
#' @param X See Usage.
#' @param variables Defaults to \code{NULL}.
#' @param seed Defaults to \code{1}.
#' @return A list with \code{estimate}, \code{vimp}, \code{baseline_error}, \code{note}, \code{method}.
#' @export
morie_survrsf_vimp <- function(fit, X, variables=NULL, seed=1) {
  # Sec. 7: random daughter assignment at splits on x. Keys are
  # 0-based variable indices to match the Python.
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  base <- morie_survrsf_mortality(fit, X, oob=TRUE, seed=seed)
  base_pe <- morie_survrsf_c_index(fit$times, fit$events,
                                   base)$prediction_error
  vars <- if (is.null(variables)) {
    seq_len(ncol(X)) - 1L
  } else {
    as.integer(variables)
  }
  out <- list()
  for (v in vars) {
    m <- morie_survrsf_mortality(fit, X, oob=TRUE, random_variable=v,
                                 seed=seed)
    pe <- morie_survrsf_c_index(fit$times, fit$events,
                                m)$prediction_error
    out[[as.character(v)]] <- pe - base_pe
  }
  list(
    estimate=if (length(out) > 0L) max(unlist(out)) else 0.0,
    vimp=out, baseline_error=base_pe,
    note=paste0("VIMP is the change in error for a fresh case if x ",
                "were unavailable, NOT the change from regrowing the ",
                "forest without x"),
    method=paste0("variable importance by random daughter assignment; ",
                  "Ishwaran et al. (2008) Sec. 7")
  )
}

#' morie_survrsf_cheatsheet
#'
#' Part of the survrsf_native implementation; see the file header for
#' the source it follows.
#'
#' @return A character value.
#' @export
morie_survrsf_cheatsheet <- function() {
  paste0(
    "survrsf: bootstrap survival trees split on the log-rank ",
    "statistic, Nelson-Aalen in each terminal node, averaged ",
    "into an out-of-bag ensemble CHF. Conservation of events ",
    "(Lemma 1) is exact: the hazard summed over ALL observed ",
    "times, censored included, equals the number of deaths. ",
    "Mortality is that sum, a count of deaths, not a ",
    "probability. Error is 1 - C with Harrell's four-step ",
    "recipe. All four of the paper's splitting rules are ",
    "implemented."
  )
}

# compact alias per ledger/NAMING.md
morie_survrsf_random_survival_forest <- morie_survrsf_forest

#' @export
morie_survrsf <- morie_survrsf_forest
