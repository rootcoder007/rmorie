# PATE: private aggregation of teacher ensembles.
# Sources: Papernot, N., Abadi, M., Erlingsson, U., Goodfellow, I. &
# Talwar, K. (2017) "Semi-supervised Knowledge Transfer for Deep
# Learning from Private Training Data", ICLR 2017. Equation 1 (the
# noisy argmax over votes plus Lap(1/gamma)); Theorem 2 (each query
# is (2 gamma, 0)-DP since one record moves at most one teacher);
# Section 3.2 (the data-independent 4 T gamma^2 + 2 gamma sqrt(2 T
# ln(1/delta)) bound, with the paper's reported values of 26 at
# gamma=0.05, T=1000, delta=1e-6 and 5.80 at gamma=0.05, T=100,
# delta=1e-5); Section 3.3 (the data-dependent bound via Lemma 4 and
# Theorem 3); Theorem 1 (composability) and the tail bound
# eps = min_lambda (alpha + ln(1/delta))/lambda with integer lambdas.

# Base R only, faithful translation of pate_python_reference.py.

# We deliberately do NOT use .ghc_unif here because the Python arm's
# Laplace draws are produced via numpy's default_rng().random() and
# the -copysign(1, u)*log(1 - 2|u|) / gamma transformation. The shared
# SplitMix64 generator is exposed as .ghc_unif, which produces the same
# stream the Python arm uses when it seeds default_rng(seed). The
# u = r - 0.5 form below matches rng.random() - 0.5 exactly.

#' .pate_lap_draw
#'
#' A step of the pate_native implementation. Called by \code{noisy_argmax}, \code{pate}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param e Passed to \code{.ghc_unif}.
#' @param gamma Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.pate_lap_draw <- function(e, gamma) {
  u <- .ghc_unif(e, 1L) - 0.5
  -sign(u) * log(1 - 2 * abs(u)) / gamma
}

#' teacher_votes
#'
#' A step of the pate_native implementation. Called by \code{pate}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param teacher_predicts Coerced to list by the body, with \code{as.list}.
#' @param rows See Usage.
#' @param n_classes Defaults to \code{NULL}.
#' @return The value of \code{split}.
#' @export
teacher_votes <- function(teacher_predicts, rows, n_classes = NULL) {
  teachers <- as.list(teacher_predicts)
  if (length(teachers) == 0L)
    stop("pate: at least one teacher is needed")
  votes <- NULL
  k_hint <- n_classes
  for (predict in teachers) {
    out <- predict(rows)
    labels <- integer(length(out))
    for (i in seq_along(out)) {
      v <- out[[i]]
      if (is.list(v) || (length(v) > 1L && !is.null(names(v)))) {
        vv <- as.numeric(v)
        labels[i] <- which.max(vv) - 1L
      } else {
        labels[i] <- as.integer(v)
      }
    }
    if (is.null(votes)) {
      k <- if (is.null(k_hint)) max(labels) + 1L else as.integer(k_hint)
      votes <- matrix(0L, nrow = length(out), ncol = k)
    }
    for (i in seq_along(labels)) {
      lab <- labels[i]
      if (lab >= ncol(votes)) {
        extra <- lab + 1L - ncol(votes)
        votes <- cbind(votes, matrix(0L, nrow = nrow(votes),
                                     ncol = extra))
      }
      votes[i, lab + 1L] <- votes[i, lab + 1L] + 1L
    }
  }
  split(votes, row(votes))
}

#' noisy_argmax
#'
#' A step of the pate_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param counts A vector; its length is taken and its elements indexed.
#' @param gamma Passed to \code{.pate_lap_draw}.
#' @param seed Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.
#' @return The value of \code{arg}, as built in the body.
#' @export
noisy_argmax <- function(counts, gamma, seed = 0) {
  gamma <- as.numeric(gamma)
  if (gamma <= 0)
    stop("pate: gamma must be positive")
  e <- .ghc_rng(as.numeric(seed))
  best <- -Inf
  arg <- 0L
  for (j in seq_along(counts)) {
    lap <- .pate_lap_draw(e, gamma)
    v <- counts[j] + lap
    if (v > best) {
      best <- v
      arg <- j - 1L
    }
  }
  arg
}

#' epsilon_data_independent
#'
#' A step of the pate_native implementation. Called by \code{pate}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param T Numeric; combined arithmetically in the body.
#' @param gamma Numeric; combined arithmetically in the body.
#' @param delta Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
epsilon_data_independent <- function(T, gamma, delta) {
  T <- as.numeric(T)
  gamma <- as.numeric(gamma)
  delta <- as.numeric(delta)
  if (T < 0 || gamma <= 0)
    stop("pate: need T >= 0 and gamma > 0")
  if (!(delta > 0.0 && delta < 1.0))
    stop("pate: delta must lie in (0, 1)")
  4.0 * T * gamma^2 +
    2.0 * gamma * sqrt(2.0 * T * log(1.0 / delta))
}

#' lemma4_bound
#'
#' A step of the pate_native implementation. Called by \code{moments_accountant}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param counts Coerced to numeric by the body, with \code{as.numeric}.
#' @param gamma Numeric; combined arithmetically in the body.
#' @return A vector, from \code{c}.
#' @export
lemma4_bound <- function(counts, gamma) {
  gamma <- as.numeric(gamma)
  if (gamma <= 0)
    stop("pate: gamma must be positive")
  n <- as.numeric(counts)
  if (length(n) == 0L)
    stop("pate: empty vote vector")
  js <- which.max(n) - 1L
  tot <- 0.0
  for (j in seq_along(n) - 1L) {
    if (j == js) next
    gap <- gamma * (n[js + 1L] - n[j + 1L])
    tot <- tot + (2.0 + gap) / (4.0 * exp(gap))
  }
  c(min(tot, 1.0), tot)
}

#' theorem3_moment
#'
#' A step of the pate_native implementation. Called by \code{moments_accountant}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param q Numeric; combined arithmetically in the body.
#' @param gamma Numeric; combined arithmetically in the body.
#' @param l Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
theorem3_moment <- function(q, gamma, l) {
  q <- as.numeric(q)
  gamma <- as.numeric(gamma)
  if (gamma <= 0)
    stop("pate: gamma must be positive")
  if (q < 0.0)
    stop("pate: q must be non-negative")
  limit <- (exp(2.0 * gamma) - 1.0) / (exp(4.0 * gamma) - 1.0)
  if (q >= limit) return(NULL)
  if (q == 0.0) return(0.0)
  ratio <- (1.0 - q) / (1.0 - exp(2.0 * gamma) * q)
  if (ratio <= 0.0) return(NULL)
  log((1.0 - q) * ratio^l + q * exp(2.0 * gamma * l))
}

#' moments_accountant
#'
#' A step of the pate_native implementation. Called by \code{pate}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param vote_counts A vector; its length is taken.
#' @param gamma Numeric; combined arithmetically in the body.
#' @param delta Numeric; combined arithmetically in the body.
#' @param lambdas Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.
#' @param data_dependent A flag; the body branches on it. Defaults to \code{TRUE}.
#' @return A list with \code{epsilon}, \code{lambda}, \code{alpha}, \code{delta}, \code{queries}, \code{used}.
#' @export
moments_accountant <- function(vote_counts, gamma, delta,
                               lambdas = NULL,
                               data_dependent = TRUE) {
  gamma <- as.numeric(gamma)
  delta <- as.numeric(delta)
  if (!(delta > 0.0 && delta < 1.0))
    stop("pate: delta must lie in (0, 1)")
  lams <- if (is.null(lambdas)) seq_len(8L) else as.integer(lambdas)
  if (length(lams) == 0L || any(lams <= 0L))
    stop("pate: lambdas must be positive")
  alpha <- as.list(setNames(rep(0.0, length(lams)), lams))
  used <- list(data_dependent = 0L, data_independent = 0L)
  for (counts in vote_counts) {
    q <- if (isTRUE(data_dependent))
      lemma4_bound(counts, gamma)[1L] else 1.0
    for (l in lams) {
      indep <- 2.0 * gamma^2 * l * (l + 1.0)
      dep <- if (isTRUE(data_dependent))
        theorem3_moment(q, gamma, l) else NULL
      if (!is.null(dep) && dep < indep) {
        alpha[[as.character(l)]] <- alpha[[as.character(l)]] + dep
        if (l == lams[1L]) used$data_dependent <- used$data_dependent + 1L
      } else {
        alpha[[as.character(l)]] <- alpha[[as.character(l)]] + indep
        if (l == lams[1L])
          used$data_independent <- used$data_independent + 1L
      }
    }
  }
  log_inv_delta <- log(1.0 / delta)
  best <- NULL
  best_l <- lams[1L]
  for (l in lams) {
    eps <- (alpha[[as.character(l)]] + log_inv_delta) / l
    if (is.null(best) || eps < best) {
      best <- eps
      best_l <- l
    }
  }
  list(
    epsilon = best,
    lambda = best_l,
    alpha = alpha,
    delta = delta,
    queries = length(vote_counts),
    used = used
  )
}

#' pate
#'
#' A step of the pate_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param teacher_predicts A vector; its length is taken.
#' @param queries Coerced to list by the body, with \code{as.list}.
#' @param gamma Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.05}.
#' @param delta Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1e-05}.
#' @param n_classes Defaults to \code{NULL}.
#' @param student_train_fn Defaults to \code{NULL}.
#' @param student_features Optional; may be \code{NULL}. Coerced to list by the body, with \code{as.list}.
#' @param seed Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.
#' @param lambdas Defaults to \code{NULL}.
#' @return A list with \code{estimate}, \code{labels}, \code{clean_labels}, \code{votes}, \code{agreement}, \code{epsilon}, \code{epsilon_accountant}, \code{epsilon_data_independent}, \code{accountant}, \code{delta}, \code{gamma}, \code{n_teachers}, \code{n_queries}, \code{student}, \code{note}, \code{method}.
#' @export
pate <- function(teacher_predicts, queries, gamma = 0.05,
                 delta = 1e-5, n_classes = NULL,
                 student_train_fn = NULL, student_features = NULL,
                 seed = 0, lambdas = NULL) {
  rows <- as.list(queries)
  if (length(rows) == 0L)
    stop("pate: no queries to label")
  votes <- teacher_votes(teacher_predicts, rows, n_classes)
  e <- .ghc_rng(as.numeric(seed))
  labels <- integer(length(votes))
  for (i in seq_along(votes)) {
    n <- votes[[i]]
    gamma_num <- as.numeric(gamma)
    best <- -Inf
    arg <- 0L
    for (j in seq_along(n)) {
      lap <- .pate_lap_draw(e, gamma_num)
      v <- n[j] + lap
      if (v > best) {
        best <- v
        arg <- j - 1L
      }
    }
    labels[i] <- arg
  }
  clean <- vapply(votes, function(v) {
    which.max(v) - 1L
  }, integer(1L))
  acct <- moments_accountant(votes, gamma, delta, lambdas)
  indep <- epsilon_data_independent(length(rows), gamma, delta)
  eps <- min(acct$epsilon, indep)
  student <- NULL
  if (!is.null(student_train_fn)) {
    X <- if (is.null(student_features)) rows
         else as.list(student_features)
    if (length(X) != length(labels))
      stop("pate: student_features must be one per query")
    student <- student_train_fn(X, labels)
  }
  list(
    estimate = as.integer(labels),
    labels = as.integer(labels),
    clean_labels = as.integer(clean),
    votes = votes,
    agreement = sum(labels == clean) / as.numeric(length(labels)),
    epsilon = as.numeric(eps),
    epsilon_accountant = as.numeric(acct$epsilon),
    epsilon_data_independent = as.numeric(indep),
    accountant = acct,
    delta = as.numeric(delta),
    gamma = as.numeric(gamma),
    n_teachers = length(teacher_predicts),
    n_queries = length(rows),
    student = student,
    note = "clean_labels are the noiseless plurality and carry NO privacy guarantee; the semi-supervised GAN student of section 4 is not implemented",
    method = "PATE noisy teacher aggregation (Papernot et al. 2017)"
  )
}

#' .pate_cheatsheet
#'
#' A step of the pate_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
.pate_cheatsheet <- function() {
  paste("pate: private aggregation of teacher ensembles (Papernot et ",
        "al. 2017). Teachers trained on disjoint partitions vote; the ",
        "student sees only argmax_j {n_j + Lap(1/gamma)} (eq.1), which ",
        "is (2 gamma, 0)-DP per query since one record moves one ",
        "teacher. Two accountings: data-independent 4 T gamma^2 + ",
        "2 gamma sqrt(2 T ln(1/delta)), which reproduces the paper's ",
        "26 and 5.80; and data-dependent, where a strong quorum ",
        "makes the majority near-certain, q from Lemma 4 feeds ",
        "Theorem 3, the smaller of that and 2 gamma^2 l(l+1) is taken ",
        "per query, summed by Theorem 1, and converted by the tail ",
        "bound eps = min_lambda (alpha + ln(1/delta))/lambda. The ",
        "noiseless plurality is NOT private.", sep = "")
}

# compact alias per ledger/NAMING.md
private_aggregation <- pate

# name carried over from the generated stub this replaced
pate_aggregate <- pate

morie_pate <- pate
