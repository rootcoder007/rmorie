#' Symbolic regression with a Pareto front (PySR scheme)
#'
#' Regularized evolution of prefix expression trees over + - * / and chosen
#' unary operators: tournament selection on loss + parsimony x size (mean
#' squared error loss), subtree crossover or one mutation, rejection above
#' maxsize, replacement of the oldest member; a hall of fame keeps the best
#' expression of each size, its Pareto front carries PySR's score
#' -d log(loss) / d size and best is the top-scoring member within 1.5
#' times the smallest loss. Philox draws; identical to the Python arm
#' \code{morie.fn.symreg}.
#'
#' @param X Numeric matrix of predictors.
#' @param y Response vector.
#' @param unary_operators Character vector from sin, cos, exp, log, sqrt, square.
#' @param niterations Iterations (each of population_size steps).
#' @param population_size Population size.
#' @param maxsize Largest expression size.
#' @param tournament_size Tournament size.
#' @param crossover_probability Probability of crossover instead of mutation.
#' @param parsimony Size penalty in selection.
#' @param seed Philox seed.
#' @return List: equations (data frame of complexity, loss, equation, score),
#'   best (list) and prediction.
#' @references Cranmer, M. (2023). Interpretable machine learning for science
#'   with PySR and SymbolicRegression.jl. arXiv:2305.01582.
#'
#'   Real, E., Aggarwal, A., Huang, Y. and Le, Q. V. (2019). Regularized
#'   evolution for image classifier architecture search. AAAI 33, 4780-4789.
#' @examples
#' X <- matrix(seq(-2, 2, by = 0.25))
#' r <- PysrRegression(X, 2 * X[, 1]^2 + X[, 1], niterations = 40, seed = 3)
#' r$best$equation
#' @export
PysrRegression <- function(X, y, unary_operators = character(0), niterations = 30L, population_size = 60L,
                           maxsize = 20L, tournament_size = 8L, crossover_probability = 0.1, parsimony = 0.0032,
                           seed = 1) {
  X <- as.matrix(X) + 0
  y <- as.numeric(y)
  if (nrow(X) != length(y) || !length(y)) stop("X and y must be non-empty and of equal length")
  p <- ncol(X)
  un <- as.character(unary_operators)
  if (!all(un %in% c("sin", "cos", "exp", "log", "sqrt", "square"))) stop("unknown unary operator")
  names_ <- paste0("x", seq_len(p) - 1)
  U <- .sr_unif(seed)
  P <- population_size
  pop <- vector("list", P)
  losses <- numeric(P)
  for (i in seq_len(P)) {
    t <- .sr_grow(U, 1 + min(floor(U() * 3), 2), p, un)
    if (length(t$k) > maxsize) t <- .sr_terminal(U, p)
    pop[[i]] <- t
    losses[i] <- .sr_loss(t, X, y, un)
  }
  hof <- list()
  record <- function(t, loss) {
    key <- as.character(length(t$k))
    if (loss < Inf && (is.null(hof[[key]]) || loss < hof[[key]]$loss)) hof[[key]] <<- list(t = t, loss = loss)
  }
  for (i in seq_len(P)) record(pop[[i]], losses[i])
  tournament <- function() {
    best <- 0
    for (s in seq_len(tournament_size)) {
      i <- min(floor(U() * P), P - 1) + 1
      if (best == 0 || losses[i] + parsimony * length(pop[[i]]$k) < losses[best] + parsimony * length(pop[[best]]$k)) best <- i
    }
    best
  }
  oldest <- 1
  for (step in seq_len(niterations * P)) {
    a <- tournament()
    if (U() < crossover_probability) {
      b <- tournament()
      child <- .sr_crossover(U, pop[[a]], pop[[b]])
    } else {
      child <- .sr_mutate(U, pop[[a]], p, un)
    }
    if (length(child$k) > maxsize) child <- pop[[a]]
    cl <- .sr_loss(child, X, y, un)
    pop[[oldest]] <- child
    losses[oldest] <- cl
    oldest <- oldest %% P + 1
    record(child, cl)
  }
  cs <- sort(as.numeric(names(hof)))
  front <- list()
  for (cc in cs) {
    h <- hof[[as.character(cc)]]
    if (!length(front) || h$loss < front[[length(front)]]$loss) {
      front[[length(front) + 1]] <- list(complexity = cc, loss = h$loss, equation = .sr_string(h$t, un, names_), t = h$t)
    }
  }
  for (k in seq_along(front)) {
    if (k == 1) {
      front[[k]]$score <- 0
    } else {
      lo <- max(front[[k]]$loss, 1e-300)
      lp <- max(front[[k - 1]]$loss, 1e-300)
      front[[k]]$score <- -(log(lo) - log(lp)) / (front[[k]]$complexity - front[[k - 1]]$complexity)
    }
  }
  fl <- vapply(front, function(e) e$loss, 0)
  sc <- vapply(front, function(e) e$score, 0)
  cx <- vapply(front, function(e) e$complexity, 0)
  ok <- which(fl <= 1.5 * min(fl))
  bi <- ok[order(-sc[ok], cx[ok])][1]
  best <- front[[bi]]
  list(
    equations = data.frame(complexity = cx, loss = fl, equation = vapply(front, function(e) e$equation, ""), score = sc,
                           stringsAsFactors = FALSE),
    best = list(complexity = best$complexity, loss = best$loss, equation = best$equation, score = best$score),
    prediction = .sr_eval(best$t, X, un)
  )
}

.sr_unif <- function(seed) {
  env <- new.env()
  env$block <- 0
  env$buf <- numeric(0)
  env$pos <- 0
  function() {
    if (env$pos >= length(env$buf)) {
      env$buf <- .morie_random_uniform(4096, seed = seed, stream = env$block)
      env$block <- env$block + 1
      env$pos <- 0
    }
    env$pos <- env$pos + 1
    env$buf[env$pos]
  }
}

.sr_node <- function(k, v) list(k = k, v = v)
.sr_cat <- function(...) {
  parts <- list(...)
  list(k = unlist(lapply(parts, function(x) x$k)), v = unlist(lapply(parts, function(x) x$v)))
}
.sr_sub <- function(t, idx) list(k = t$k[idx], v = t$v[idx])

.sr_span <- function(k, i) {
  need <- 1
  j <- i
  while (need > 0) {
    need <- need + (if (k[j] == 2) 2 else if (k[j] == 3) 1 else 0) - 1
    j <- j + 1
  }
  j
}

.sr_terminal <- function(U, p) {
  if (U() < 0.4) return(.sr_node(0, 4 * U() - 2))
  .sr_node(1, min(floor(U() * p), p - 1))
}

.sr_grow <- function(U, d, p, un) {
  if (d <= 0 || U() < 0.3) return(.sr_terminal(U, p))
  if (length(un) && U() < 0.2) {
    op <- .sr_node(3, min(floor(U() * length(un)), length(un) - 1))
    return(.sr_cat(op, .sr_grow(U, d - 1, p, un)))
  }
  op <- .sr_node(2, min(floor(U() * 4), 3))
  left <- .sr_grow(U, d - 1, p, un)
  .sr_cat(op, left, .sr_grow(U, d - 1, p, un))
}

.sr_mutate <- function(U, t, p, un) {
  u <- U()
  n <- length(t$k)
  consts <- which(t$k == 0)
  vars <- which(t$k == 1)
  ops <- which(t$k >= 2)
  if (u < 0.3 && length(consts)) {
    i <- consts[min(floor(U() * length(consts)), length(consts) - 1) + 1]
    t$v[i] <- t$v[i] + 2 * (U() - 0.5)
    return(t)
  }
  if (u >= 0.3 && u < 0.5 && length(ops)) {
    i <- ops[min(floor(U() * length(ops)), length(ops) - 1) + 1]
    m <- if (t$k[i] == 2) 4 else length(un)
    t$v[i] <- min(floor(U() * m), m - 1)
    return(t)
  }
  if (u >= 0.5 && u < 0.65 && length(vars)) {
    i <- vars[min(floor(U() * length(vars)), length(vars) - 1) + 1]
    t$v[i] <- min(floor(U() * p), p - 1)
    return(t)
  }
  i <- min(floor(U() * n), n - 1) + 1
  j <- .sr_span(t$k, i)
  head <- .sr_sub(t, seq_len(i - 1))
  mid <- .sr_sub(t, i:(j - 1))
  tail <- .sr_sub(t, if (j <= n) j:n else integer(0))
  if (u >= 0.85) {
    op <- .sr_node(2, min(floor(U() * 4), 3))
    return(.sr_cat(head, op, mid, .sr_terminal(U, p), tail))
  }
  .sr_cat(head, .sr_grow(U, 2, p, un), tail)
}

.sr_crossover <- function(U, a, b) {
  na <- length(a$k)
  nb <- length(b$k)
  i <- min(floor(U() * na), na - 1) + 1
  j <- min(floor(U() * nb), nb - 1) + 1
  ea <- .sr_span(a$k, i)
  eb <- .sr_span(b$k, j)
  .sr_cat(.sr_sub(a, seq_len(i - 1)), .sr_sub(b, j:(eb - 1)), .sr_sub(a, if (ea <= na) ea:na else integer(0)))
}

.sr_un <- function(name, a) {
  suppressWarnings(switch(name,
    sin = sin(a),
    cos = cos(a),
    exp = exp(a),
    log = log(a),
    sqrt = sqrt(a),
    square = a * a
  ))
}

.sr_eval <- function(t, X, un) {
  st <- new.env()
  st$pos <- 1
  rec <- function() {
    k <- t$k[st$pos]
    v <- t$v[st$pos]
    st$pos <- st$pos + 1
    if (k == 0) return(rep(v, nrow(X)))
    if (k == 1) return(X[, v + 1])
    if (k == 3) return(.sr_un(un[v + 1], rec()))
    a <- rec()
    b <- rec()
    switch(v + 1, a + b, a - b, a * b, a / b)
  }
  rec()
}

.sr_loss <- function(t, X, y, un) {
  pred <- .sr_eval(t, X, un)
  s <- 0
  for (i in seq_along(y)) {
    d <- pred[i] - y[i]
    s <- s + d * d
  }
  s <- s / length(y)
  if (is.finite(s)) s else Inf
}

.sr_string <- function(t, un, names_) {
  st <- new.env()
  st$pos <- 1
  rec <- function() {
    k <- t$k[st$pos]
    v <- t$v[st$pos]
    st$pos <- st$pos + 1
    if (k == 0) return(sprintf("%.6g", v))
    if (k == 1) return(names_[v + 1])
    if (k == 3) return(paste0(un[v + 1], "(", rec(), ")"))
    a <- rec()
    b <- rec()
    paste0("(", a, " ", c("+", "-", "*", "/")[v + 1], " ", b, ")")
  }
  rec()
}
