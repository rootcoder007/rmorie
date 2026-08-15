# morie.fn -- function file (rootcoder007/morie)
# Random survival forests. See module docstring for full references.
# Implementation translated from the Python source.

SPLIT_RULES <- c("logrank", "logrankrandom", "conserve", "logrankscore")
_AVAILABLE <- c("logrank", "logrankrandom", "logrankscore", "conserve")
_UNSOURCED <- list()

morie_survrsf <- function(X, times, events,
                          n_trees = 50, mtry = NULL, min_deaths = 3,
                          rule = "logrank", seed = 0,
                          variables = NULL) {
  fit <- survrsf_forest(X, times, events, n_trees, mtry, min_deaths,
                        rule, seed)
  base_mort <- survrsf_mortality(fit, X, oob = TRUE, random_variable = NULL,
                                 seed = 1)
  base_err <- survrsf_c_index(fit$times, fit$events, base_mort)$prediction_error
  vi <- survrsf_vimp(fit, X, variables, seed = 1, base_mort = base_mort,
                     base_err = base_err)
  chf <- survrsf_ensemble_chf(fit, X, max(fit$times), oob = TRUE,
                              random_variable = NULL, seed = 1)
  list(forest = fit, mortality = base_mort, chf = chf, c_index = 1 - base_err,
       vimp = vi, base_error = base_err)
}

.survrsf_rule_status <- function(rule = NULL) {
  if (is.null(rule)) {
    return(list(rules = SPLIT_RULES, available = _AVAILABLE,
                unavailable = .ghc_unlist_named(_UNSOURCED)))
  }
  if (!(rule %in% SPLIT_RULES)) {
    stop(sprintf("survrsf: rule must be one of %s, got %r",
                 paste(SPLIT_RULES, collapse = ", "),
                 as.character(rule)))
  }
  list(rule = rule, available = rule %in% _AVAILABLE,
       reason = if (length(_UNSOURCED[[rule]])) _UNSOURCED[[rule]] else "")
}

.survrsf_check_rule <- function(rule) {
  if (!(rule %in% SPLIT_RULES)) {
    stop(sprintf("survrsf: rule must be one of %s, got %r",
                 paste(SPLIT_RULES, collapse = ", "),
                 as.character(rule)))
  }
  if (!(rule %in% _AVAILABLE)) {
    stop(sprintf("survrsf: the %r splitting rule is not implemented -- %s",
                 as.character(rule), _UNSOURCED[[rule]]))
  }
}

.survrsf_Rng <- function(seed = 0) {
  s <- (as.numeric(seed) * 6364136223846793005 + 1442695040888963407)
  s <- bitwAnd(as.integer(s), (bitwShiftL(1, 64) - 1))
  env <- new.env()
  env$s <- s
  env
}

.survrsf_Rng_next <- function(rng) {
  rng$s <- bitwAnd(rng$s * as.integer(6364136223846793005) +
                     as.integer(1442695040888963407),
                   bitwShiftL(1, 64) - 1)
  bitwShiftR(rng$s, 11) / as.numeric(bitwShiftL(1, 53))
}

.survrsf_Rng_randint <- function(rng, n) {
  as.integer(.survrsf_Rng_next(rng) * n) %% n
}

.survrsf_Rng_sample <- function(rng, seq, k) {
  pool <- as.list(seq)
  out <- vector("list", min(k, length(pool)))
  for (idx in seq_len(min(k, length(pool)))) {
    pos <- .survrsf_Rng_randint(rng, length(pool)) + 1
    out[[idx]] <- pool[[pos]]
    pool[[pos]] <- pool[[length(pool)]]
    pool[[length(pool)]] <- NULL
  }
  out
}

.survrsf_nelson_aalen <- function(times, events) {
  n <- length(times)
  if (n != length(events)) {
    stop(sprintf("survrsf: %d times but %d event indicators",
                 n, length(events)))
  }
  if (n == 0) {
    stop("survrsf: no observations")
  }
  ord <- order(times)
  ts <- numeric(0)
  ds <- numeric(0)
  cum <- 0.0
  i <- 1
  while (i <= n) {
    t <- times[ord[i]]
    j <- i
    d <- 0
    while (j <= n && times[ord[j]] == t) {
      if (events[ord[j]] != 0) d <- d + 1
      j <- j + 1
    }
    at_risk <- n - i + 1
    if (d > 0) {
      cum <- cum + d / as.numeric(at_risk)
      ts <- c(ts, as.numeric(t))
      ds <- c(ds, cum)
    }
    i <- j
  }
  list(time = ts, chf = ds, n = n,
       deaths = sum(1L * (events != 0)))
}

.survrsf_chf_at <- function(na, t) {
  out <- 0.0
  for (i in seq_along(na$time)) {
    if (na$time[i] <= t) {
      out <- na$chf[i]
    } else {
      break
    }
  }
  out
}

.survrsf_conservation_check <- function(times, events) {
  na <- .survrsf_nelson_aalen(times, events)
  total <- 0.0
  for (t in times) total <- total + .survrsf_chf_at(na, t)
  deaths <- as.numeric(sum(1L * (events != 0)))
  list(sum_chf = total, deaths = deaths,
       difference = total - deaths,
       conserved = abs(total - deaths) < 1e-9)
}

.survrsf_logrank_statistic <- function(times, events, group) {
  n <- length(times)
  if (!(n == length(events) && n == length(group))) {
    stop("survrsf: times, events and group must have the same length")
  }
  ord <- order(times)
  num <- 0.0
  var <- 0.0
  i <- 1
  while (i <= n) {
    t <- times[ord[i]]
    j <- i
    d <- 0
    d1 <- 0
    while (j <= n && times[ord[j]] == t) {
      if (events[ord[j]]) {
        d <- d + 1
        if (group[ord[j]]) d1 <- d1 + 1
      }
      j <- j + 1
    }
    at_risk <- n - i + 1
    r1 <- 0
    for (k in i:n) if (group[ord[k]]) r1 <- r1 + 1
    if (d > 0 && at_risk > 1) {
      num <- num + (d1 - d * r1 / as.numeric(at_risk))
      var <- var + (d * (r1 / as.numeric(at_risk)) *
                    (1.0 - r1 / as.numeric(at_risk)) *
                    (at_risk - d) / as.numeric(at_risk - 1))
    } else if (d > 0) {
      num <- num + (d1 - d * r1 / as.numeric(at_risk))
    }
    i <- j
  }
  if (var <= 0.0) return(0.0)
  abs(num) / sqrt(var)
}

.survrsf_logrank_scores <- function(times, events) {
  N <- length(times)
  if (N != length(events)) {
    stop("survrsf: times and events must have the same length")
  }
  if (N == 0) {
    stop("survrsf: no observations")
  }
  gamma <- integer(N)
  for (j in seq_len(N)) {
    cnt <- 0
    for (t in times) if (t <= times[j]) cnt <- cnt + 1
    gamma[j] <- cnt
  }
  out <- numeric(N)
  for (i in seq_len(N)) {
    s <- 0.0
    for (j in seq_len(N)) {
      if (times[j] <= times[i] && events[j]) {
        s <- s + 1.0 / as.numeric(N - gamma[j] + 1)
      }
    }
    out[i] <- as.numeric(events[i] != 0) - s
  }
  out
}

.survrsf_logrank_score_statistic <- function(times, events, group,
                                              scores = NULL) {
  N <- length(times)
  a <- if (is.null(scores)) .survrsf_logrank_scores(times, events)
       else as.numeric(scores)
  if (!(N == length(group) && N == length(a))) {
    stop("survrsf: times, group and scores must have the same length")
  }
  m <- sum(1L * (!as.logical(group)))
  n <- N - m
  if (m == 0 || n == 0 || N < 2) return(0.0)
  Tstat <- 0.0
  for (i in seq_len(N)) if (!group[i]) Tstat <- Tstat + a[i]
  sa <- sum(a)
  sa2 <- sum(a * a)
  ET <- m * sa / as.numeric(N)
  VT <- (m * n / as.numeric(N * N * (N - 1))) * (N * sa2 - sa * sa)
  if (VT <= 0.0) return(0.0)
  abs(Tstat - ET) / sqrt(VT)
}

.survrsf_conservation_residuals <- function(times, events) {
  n <- length(times)
  if (n != length(events)) {
    stop("survrsf: times and events must have the same length")
  }
  if (n == 0) return(numeric(0))
  na <- .survrsf_nelson_aalen(times, events)
  ord <- order(times)
  out <- numeric(n)
  h_sum <- 0.0
  d_sum <- 0.0
  for (k in seq_along(ord)) {
    i <- ord[k]
    h_sum <- h_sum + .survrsf_chf_at(na, times[i])
    d_sum <- d_sum + if (events[i]) 1.0 else 0.0
    out[k] <- h_sum - d_sum
  }
  out
}

.survrsf_conserve_statistic <- function(times, events, group) {
  n <- length(times)
  if (!(n == length(events) && n == length(group))) {
    stop("survrsf: times, events and group must have the same length")
  }
  total <- 0.0
  weight <- 0.0
  gvals <- as.integer(as.logical(group))
  for (g in c(0, 1)) {
    idx <- which(gvals == g)
    if (length(idx) == 0) return(0.0)
    t_sub <- times[idx]
    e_sub <- events[idx]
    m <- .survrsf_conservation_residuals(t_sub, e_sub)
    y1 <- as.numeric(length(idx))
    if (length(m) <= 1) {
      total <- total + y1 * 0.0
    } else {
      total <- total + y1 * sum(abs(m[seq_len(length(m) - 1)]))
    }
    weight <- weight + y1
  }
  conserve <- if (weight > 0.0) total / weight else 0.0
  1.0 / (1.0 + conserve)
}

.survrsf_best_split <- function(X, times, events, features, min_deaths = 3,
                                rule = "logrank", rng = NULL) {
  .survrsf_check_rule(rule)
  n <- length(times)
  best <- NULL
  scores <- if (rule == "logrankscore")
    .survrsf_logrank_scores(times, events) else NULL
  for (j in features) {
    vals_j <- unique(sapply(seq_len(n), function(i) X[[i]][j]))
    vals_j <- sort(vals_j)
    if (length(vals_j) < 2) next
    cuts <- sapply(seq_len(length(vals_j) - 1),
                   function(k) (vals_j[k] + vals_j[k + 1]) / 2.0)
    if (rule == "logrankrandom") {
      if (is.null(rng)) rng <- .survrsf_Rng(0)
      cuts <- cuts[.survrsf_Rng_randint(rng, length(cuts)) + 1]
    }
    for (c in cuts) {
      grp <- integer(n)
      for (i in seq_len(n)) grp[i] <- if (X[[i]][j] > c) 1L else 0L
      left <- which(grp == 0L)
      right <- which(grp == 1L)
      ld <- sum(1L * (events[left] != 0))
      rd <- sum(1L * (events[right] != 0))
      if (ld < min_deaths || rd < min_deaths) next
      if (rule == "logrankscore") {
        stat <- .survrsf_logrank_score_statistic(times, events, grp, scores)
      } else if (rule == "conserve") {
        stat <- .survrsf_conserve_statistic(times, events, grp)
      } else {
        stat <- .survrsf_logrank_statistic(times, events, grp)
      }
      if (is.null(best) || stat > best$statistic) {
        best <- list(variable = as.integer(j), cut = as.numeric(c),
                     statistic = as.numeric(stat), left = left,
                     right = right)
      }
    }
  }
  best
}

.survrsf_grow_tree <- function(X, times, events, mtry = NULL, min_deaths = 3,
                               rule = "logrank", seed = 0, rng = NULL) {
  .survrsf_check_rule(rule)
  n <- length(times)
  if (n == 0) stop("survrsf: no observations")
  d <- length(X[[1]])
  mtry <- if (is.null(mtry)) max(1L, as.integer(sqrt(d)))
          else as.integer(mtry)
  if (is.null(rng)) rng <- .survrsf_Rng(seed)

  build <- function(idx, depth) {
    t_ <- times[idx]
    e_ <- events[idx]
    nd <- sum(1L * (e_ != 0))
    if (nd < 2L * min_deaths || depth > 40) {
      na <- .survrsf_nelson_aalen(t_, e_)
      return(list(leaf = TRUE, na = na, n = length(idx),
                  idx = as.integer(idx)))
    }
    feats <- as.integer(unlist(.survrsf_Rng_sample(rng, seq_len(d), mtry)))
    sub <- lapply(idx, function(i) X[[i]])
    sp <- .survrsf_best_split(sub, t_, e_, feats, min_deaths, rule, rng)
    if (is.null(sp)) {
      na <- .survrsf_nelson_aalen(t_, e_)
      return(list(leaf = TRUE, na = na, n = length(idx),
                  idx = as.integer(idx)))
    }
    left <- as.integer(idx[sp$left])
    right <- as.integer(idx[sp$right])
    list(leaf = FALSE, variable = as.integer(sp$variable),
         cut = as.numeric(sp$cut), statistic = as.numeric(sp$statistic),
         left = build(left, depth + 1), right = build(right, depth + 1))
  }

  list(root = build(seq_len(n), 0), rule = rule, mtry = mtry,
       min_deaths = as.integer(min_deaths), n = n)
}

.survrsf_leaves <- function(node, out = NULL) {
  if (is.null(out)) out <- list()
  if (isTRUE(node$leaf)) {
    out[[length(out) + 1]] <- node
  } else {
    out <- .survrsf_leaves(node$left, out)
    out <- .survrsf_leaves(node$right, out)
  }
  out
}

.survrsf_predict_tree <- function(tree, x, random_variable = NULL,
                                  rng = NULL) {
  node <- tree$root
  if (is.null(rng)) rng <- .survrsf_Rng(0)
  while (!isTRUE(node$leaf)) {
    if (!is.null(random_variable) && node$variable == random_variable) {
      go_right <- .survrsf_Rng_next(rng) < 0.5
    } else {
      go_right <- x[node$variable] > node$cut
    }
    node <- if (go_right) node$right else node$left
  }
  node
}

survrsf_forest <- function(X, times, events, n_trees = 50, mtry = NULL,
                           min_deaths = 3, rule = "logrank", seed = 0) {
  .survrsf_check_rule(rule)
  n <- length(times)
  rng <- .survrsf_Rng(seed)
  trees <- list()
  inbag <- list()
  for (b in seq_len(as.integer(n_trees))) {
    boot <- sapply(seq_len(n), function(i) .survrsf_Rng_randint(rng, n))
    used <- unique(boot)
    Xb <- lapply(boot + 1, function(i) X[[i]])
    tb <- times[boot + 1]
    eb <- events[boot + 1]
    if (sum(1L * (eb != 0)) < 2L * min_deaths) next
    trees[[length(trees) + 1]] <- .survrsf_grow_tree(Xb, tb, eb, mtry,
                                                     min_deaths, rule,
                                                     rng = rng)
    inbag[[length(inbag) + 1]] <- as.integer(used)
  }
  if (length(trees) == 0) {
    stop(sprintf("survrsf: no tree could be grown; the data hold too few deaths for min_deaths = %d",
                 min_deaths))
  }
  oob_fraction <- sum(sapply(inbag, function(u) n - length(u))) /
    as.numeric(length(inbag) * n)
  list(trees = trees, inbag = inbag, n = n, rule = rule,
       n_trees = length(trees), oob_fraction = oob_fraction,
       times = as.numeric(times), events = as.numeric(events))
}

survrsf_ensemble_chf <- function(fit, X, t, oob = TRUE,
                                 random_variable = NULL, seed = 1) {
  rng <- .survrsf_Rng(seed)
  out <- numeric(length(X))
  for (i in seq_along(X)) {
    vals <- 0.0
    count <- 0L
    for (b in seq_along(fit$trees)) {
      if (oob && (i %in% fit$inbag[[b]])) next
      node <- .survrsf_predict_tree(fit$trees[[b]], X[[i]],
                                    random_variable, rng)
      vals <- vals + .survrsf_chf_at(node$na, t)
      count <- count + 1L
    }
    out[i] <- if (count > 0) vals / count else NA_real_
  }
  out
}

survrsf_mortality <- function(fit, X, oob = TRUE, random_variable = NULL,
                              seed = 1) {
  rng <- .survrsf_Rng(seed)
  ts <- fit$times
  out <- numeric(length(X))
  for (i in seq_along(X)) {
    total <- 0.0
    count <- 0L
    for (b in seq_along(fit$trees)) {
      if (oob && (i %in% fit$inbag[[b]])) next
      node <- .survrsf_predict_tree(fit$trees[[b]], X[[i]],
                                    random_variable, rng)
      s <- 0.0
      for (t in ts) s <- s + .survrsf_chf_at(node$na, t)
      total <- total + s
      count <- count + 1L
    }
    out[i] <- if (count > 0) total / count else NA_real_
  }
  out
}

survrsf_c_index <- function(times, events, predicted) {
  n <- length(times)
  if (!(n == length(events) && n == length(predicted))) {
    stop("survrsf: times, events and predictions must have the same length")
  }
  permissible <- 0.0
  concordance <- 0.0
  for (i in seq_len(n)) {
    for (j in (i + 1):n) {
      ti <- times[i]; tj <- times[j]
      ei <- events[i]; ej <- events[j]
      if (ti < tj && !ei) next
      if (tj < ti && !ej) next
      if (ti == tj && !(ei || ej)) next
      permissible <- permissible + 1.0
      pi <- predicted[i]; pj <- predicted[j]
      if (ti != tj) {
        if (ti < tj) { ps <- pi; pl <- pj } else { ps <- pj; pl <- pi }
        if (ps > pl) concordance <- concordance + 1.0
        else if (ps == pl) concordance <- concordance + 0.5
      } else if (ei && ej) {
        concordance <- concordance + if (pi == pj) 1.0 else 0.5
      } else {
        dead <- if (ei) i else j
        other <- if (ei) j else i
        concordance <- concordance + if (predicted[dead] > predicted[other])
                      1.0 else 0.5
      }
    }
  }
  if (permissible == 0.0) {
    stop("survrsf: no permissible pairs -- every pair has its shorter time censored")
  }
  list(c_index = concordance / permissible,
       concordance = concordance, permissible = permissible,
       prediction_error = 1.0 - concordance / permissible)
}

survrsf_vimp <- function(fit, X, variables = NULL, seed = 1,
                          base_mort = NULL, base_err = NULL) {
  if (is.null(base_mort)) {
    base_mort <- survrsf_mortality(fit, X, oob = TRUE,
                                   random_variable = NULL, seed = seed)
  }
  if (is.null(base_err)) {
    base_err <- survrsf_c_index(fit$times, fit$events, base_mort)$prediction_error
  }
  variables <- if (is.null(variables)) seq_len(length(X[[1]]))
               else as.integer(variables)
  out <- list()
  for (v in variables) {
    m <- survrsf_mortality(fit, X, oob = TRUE, random_variable = v,
                           seed = seed)
    pe <- survrsf_c_index(fit$times, fit$events, m)$prediction_error
    out[[as.character(v)]] <- pe - base_err
  }
  list(
    estimate = if (length(out)) max(unlist(out)) else 0.0,
    vimp = out,
    baseline_error = base_err,
    note = "VIMP is the change in error for a fresh case if x were unavailable, NOT the change from regrowing the forest without x",
    method = "variable importance by random daughter assignment; Ishwaran et al. (2008) Sec. 7"
  )
}

.ghc_unlist_named <- function(x) {
  if (length(x) == 0) return(list())
  out <- list()
  for (nm in names(x)) out[[nm]] <- x[[nm]]
  out
}
