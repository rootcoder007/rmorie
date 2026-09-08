# R arm of karpV -- genetic programming (Koza 1992).
#
# Programs are expression trees over a function set and a terminal set,
# and they are bred rather than written: evaluate every tree against the
# fitness cases, select parents in proportion to how well they did, cross
# them by swapping subtrees, repeat. The representation is the point --
# because the genome IS a program, crossover yields something runnable
# rather than something that has to be decoded.
#
# Everything here follows the book:
#
#   initialisation  ramped half-and-half. Depths spread evenly over
#                   2..max_depth_init, half grown (a branch may stop
#                   early at a terminal) and half full (every branch runs
#                   to the depth). That is what gives the initial
#                   population a spread of shapes rather than one shape
#                   repeated.
#   fitness         raw fitness is the sum of absolute errors over the
#                   fitness cases; adjusted fitness is 1/(1+raw), and
#                   selection uses it because it compresses the
#                   difference between bad individuals and magnifies it
#                   between good ones.
#   selection       fitness-proportionate over adjusted fitness.
#   crossover       swap subtrees at a chosen node in each parent, with
#                   Koza's 90/10 bias towards an internal node --
#                   uniform choice picks a leaf almost every time in any
#                   sizeable tree and crossover degenerates into
#                   swapping constants.
#   operators       crossover, reproduction and mutation, plus a depth
#                   cap; a child that exceeds it is replaced by its
#                   first parent, which is the book's rule.
#
# Protected division returns 1 for a zero divisor, also from the book:
# otherwise one division by zero throws away an otherwise good program.
#
# The random stream is a 32-bit xorshift written out here rather than
# taken from R's generator, because R and Python do not share one and a
# bred population has to reproduce in both arms.
#
# Reference
#   Koza, J.R. (1992) "Genetic Programming: On the Programming of
#     Computers by Means of Natural Selection." MIT Press, Cambridge MA.
#     Chapters 6 and 7: the tableau, ramped half-and-half, adjusted
#     fitness, fitness-proportionate selection, the 90/10 crossover
#     point bias, and protected division.

.KARPV_2_32 <- 4294967296

# 32-bit operations on doubles. R's bitwXor is defined over the signed
# 32-bit range and overflows above 2^31, so the words are split into two
# 16-bit halves; every value stays well under 2^53 and the arithmetic is
# exact.
#' 32-bit operations on doubles. R\'s bitwXor is defined over the signed
#'
#' 32-bit range and overflows above 2^31, so the words are split into
#' two 16-bit halves; every value stays well under 2^53 and the
#' arithmetic is exact.
#'
#' @param a Numeric; combined arithmetically in the body.
#' @param b Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
#' @examples
#' A <- matrix(c(4, 1, 0.5, 1, 3, 0.8, 0.5, 0.8, 2), nrow = 3)
#' b <- c(1.5, 2.5, 3.5)
#' res <- .karpv_xor32(a = A, b = b)
#' res
.karpv_xor32 <- function(a, b) {
  ah <- a %/% 65536
  al <- a %% 65536
  bh <- b %/% 65536
  bl <- b %% 65536
  bitwXor(ah, bh) * 65536 + bitwXor(al, bl)
}

#' .karpv_shl
#'
#' A step of the karpV_native implementation. Called by \code{.karpv_u32}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a Numeric; combined arithmetically in the body.
#' @param k Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
#' @examples
#' A <- matrix(c(4, 1, 0.5, 1, 3, 0.8, 0.5, 0.8, 2), nrow = 3)
#' res <- .karpv_shl(a = A, k = A)
#' res
.karpv_shl <- function(a, k) (a * 2^k) %% .KARPV_2_32
#' .karpv_shr
#'
#' A step of the karpV_native implementation. Called by \code{.karpv_u32}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a Numeric; combined arithmetically in the body.
#' @param k Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
#' @examples
#' A <- matrix(c(4, 1, 0.5, 1, 3, 0.8, 0.5, 0.8, 2), nrow = 3)
#' res <- .karpv_shr(a = A, k = A)
#' res
.karpv_shr <- function(a, k) a %/% 2^k

#' .karpv_rng
#'
#' A step of the karpV_native implementation. Called by \code{morie_karpV}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param seed Numeric; combined arithmetically in the body.
#' @return The value of \code{e}, as built in the body.
#' @export
#' @examples
#' res <- .karpv_rng(seed = 1L)
#' res
.karpv_rng <- function(seed) {
  s <- seed %% .KARPV_2_32
  if (s == 0) s <- 2463534242
  e <- new.env(parent = emptyenv())
  e$s <- s
  e
}

#' .karpv_u32
#'
#' A step of the karpV_native implementation. Called by \code{.karpv_below}, \code{.karpv_unit}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param e A list; the body reads \code{$s} from it.
#' @return The value of \code{$}.
#' @export
.karpv_u32 <- function(e) {
  s <- e$s
  s <- .karpv_xor32(s, .karpv_shl(s, 13))
  s <- .karpv_xor32(s, .karpv_shr(s, 17))
  s <- .karpv_xor32(s, .karpv_shl(s, 5))
  e$s <- s %% .KARPV_2_32
  e$s
}

# 32 bits over 2^32, so the draw is in [0,1) with no rounding surprise
# and the same bits in both arms.
#' 32 bits over 2^32, so the draw is in [0,1) with no rounding surprise
#'
#' and the same bits in both arms.
#'
#' @param e Passed to \code{.karpv_u32}.
#' @return A numeric value.
#' @export
.karpv_unit <- function(e) .karpv_u32(e) / .KARPV_2_32

# A whole number in 0..n-1 by rejection, so the range is exact. A
# remainder would bias the low end, and the bias changes with n -- the
# kind of thing that never shows up in one language alone.
#' A whole number in 0..n-1 by rejection, so the range is exact. A
#'
#' remainder would bias the low end, and the bias changes with n -- the
#' kind of thing that never shows up in one language alone.
#'
#' @param e Passed to \code{.karpv_u32}.
#' @param n Numeric; combined arithmetically in the body.
#' @return The value of \code{repeat}.
#' @export
.karpv_below <- function(e, n) {
  if (n <= 1) return(0)
  mask <- .KARPV_2_32 - 1
  limit <- mask - (mask %% n)
  repeat {
    v <- .karpv_u32(e)
    if (v <= limit) return(v %% n)
  }
}

# ---------------------------------------------------------------- trees

#' .karpv_fnode
#'
#' A step of the karpV_native implementation. Called by \code{.karpv_copy},
#' \code{.karpv_grow}, \code{.karpv_replace}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param op Carried through into a list the body builds.
#' @param args Carried through into a list the body builds.
#' @return A list with \code{op}, \code{args}.
#' @export
.karpv_fnode <- function(op, args) list(op = op, args = args)
#' .karpv_tnode
#'
#' A step of the karpV_native implementation. Called by \code{.karpv_copy},
#' \code{.karpv_random_terminal}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param term Carried through into a list the body builds.
#' @return A list with \code{term}.
#' @export
.karpv_tnode <- function(term) list(term = term)
#' .karpv_is_term
#'
#' A step of the karpV_native implementation. Called by \code{.karpv_collect},
#' \code{.karpv_copy}, \code{.karpv_pick_point} and 4 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param node A list; the body reads \code{$term} from it.
#' @return A logical value.
#' @export
.karpv_is_term <- function(node) !is.null(node$term)

.KARPV_FUNCTIONS <- list(list("+", 2L), list("-", 2L), list("*", 2L),
                         list("%", 2L))

#' .karpv_apply
#'
#' A step of the karpV_native implementation. Called by \code{morie_karpV_evaluate}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param op One of \code{"-"}, \code{"*"}, \code{"\%\%"}, \code{"+"}.
#' @param vals A vector; indexed elementwise.
#' @return Nothing; this branch always raises.
#' @export
.karpv_apply <- function(op, vals) {
  if (op == "+") return(vals[1] + vals[2])
  if (op == "-") return(vals[1] - vals[2])
  if (op == "*") return(vals[1] * vals[2])
  if (op == "%") {
    # Koza's protected division: a zero divisor yields 1, so one bad
    # division does not throw away an otherwise good program.
    return(if (vals[2] == 0) 1 else vals[1] / vals[2])
  }
  stop(sprintf("karpV: unknown function %s", op), call. = FALSE)
}

#' Evaluate an expression tree
#'
#' @param node the tree.
#' @param env named list of variable values.
#' @return the numeric value.
#' @export
morie_karpV_evaluate <- function(node, env) {
  if (.karpv_is_term(node)) {
    t <- node$term
    if (is.character(t)) {
      if (!is.null(env[[t]])) return(as.numeric(env[[t]]))
      return(as.numeric(t))
    }
    return(as.numeric(t))
  }
  .karpv_apply(node$op,
               vapply(node$args, morie_karpV_evaluate, numeric(1), env))
}

#' Depth of an expression tree
#'
#' @param node the tree.
#' @return the depth, a leaf counting as 1.
#' @export
morie_karpV_depth <- function(node) {
  if (.karpv_is_term(node)) return(1L)
  1L + max(vapply(node$args, morie_karpV_depth, integer(1)))
}

#' Node count of an expression tree
#'
#' @param node the tree.
#' @return the number of nodes.
#' @export
morie_karpV_size <- function(node) {
  if (.karpv_is_term(node)) return(1L)
  1L + sum(vapply(node$args, morie_karpV_size, integer(1)))
}

#' Print an expression tree
#'
#' @param node the tree.
#' @return a prefix string.
#' @export
morie_karpV_to_string <- function(node) {
  if (.karpv_is_term(node)) {
    t <- node$term
    # An ephemeral constant is printed through sprintf, not through
    # either language's own float formatting, so both arms produce the
    # same expression text.
    if (is.numeric(t)) return(sprintf("%.17g", t))
    return(as.character(t))
  }
  paste0("(", node$op, " ",
         paste(vapply(node$args, morie_karpV_to_string, character(1)),
               collapse = " "), ")")
}

#' .karpv_random_terminal
#'
#' A step of the karpV_native implementation. Called by \code{.karpv_grow}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param e Passed to \code{.karpv_below}.
#' @param terminals A vector; its length is taken and its elements indexed.
#' @param erc Optional; may be \code{NULL}. A vector; indexed elementwise.
#' @return The value of \code{.karpv_tnode}.
#' @export
.karpv_random_terminal <- function(e, terminals, erc) {
  n <- length(terminals) + (if (is.null(erc)) 0L else 1L)
  i <- .karpv_below(e, n)
  if (i < length(terminals)) return(.karpv_tnode(terminals[i + 1L]))
  .karpv_tnode(erc[1] + (erc[2] - erc[1]) * .karpv_unit(e))
}

#' .karpv_grow
#'
#' A step of the karpV_native implementation. Called by \code{morie_karpV},
#' \code{morie_karpV_ramped}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param e Passed to \code{.karpv_random_terminal}.
#' @param functions A vector; its length is taken and its elements indexed.
#' @param terminals A vector; its length is taken.
#' @param erc Optional; may be \code{NULL}. Passed to \code{.karpv_random_terminal}.
#' @param d Numeric; combined arithmetically in the body.
#' @param full A flag; the body branches on it.
#' @return The value of \code{.karpv_fnode}.
#' @export
.karpv_grow <- function(e, functions, terminals, erc, d, full) {
  if (d <= 1) return(.karpv_random_terminal(e, terminals, erc))
  if (!full) {
    total <- length(functions) + length(terminals) +
      (if (is.null(erc)) 0L else 1L)
    if (.karpv_below(e, total) >= length(functions))
      return(.karpv_random_terminal(e, terminals, erc))
  }
  f <- functions[[.karpv_below(e, length(functions)) + 1L]]
  .karpv_fnode(f[[1]], lapply(seq_len(f[[2]]), function(k)
    .karpv_grow(e, functions, terminals, erc, d - 1L, full)))
}

#' Ramped half-and-half initialisation
#'
#' @param e the generator, from the module's own xorshift.
#' @param n population size.
#' @param functions list of name and arity pairs.
#' @param terminals character vector of variable names.
#' @param erc length-two range for ephemeral constants, or NULL.
#' @param max_depth deepest initial tree.
#' @return a list of trees with a spread of depths and shapes.
#' @export
morie_karpV_ramped <- function(e, n, functions, terminals, erc, max_depth) {
  span <- max_depth - 1L
  lapply(seq_len(n), function(i) {
    i0 <- i - 1L
    d <- if (span > 0L) 2L + (i0 %% span) else 2L
    .karpv_grow(e, functions, terminals, erc, d,
                ((i0 %/% max(span, 1L)) %% 2L) == 1L)
  })
}

# ---------------------------------------------------------------- nodes

#' .karpv_collect
#'
#' A step of the karpV_native implementation. Called by \code{.karpv_pick_point}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param node A list; the body reads \code{$args} from it.
#' @param path Carried through into a list the body builds.
#' @param out A vector; its length is taken and its elements indexed.
#' @return The value of \code{out}, as built in the body.
#' @export
.karpv_collect <- function(node, path, out) {
  out[[length(out) + 1L]] <- list(path = path, node = node)
  if (!.karpv_is_term(node))
    for (k in seq_along(node$args))
      out <- .karpv_collect(node$args[[k]], c(path, k), out)
  out
}

#' .karpv_pick_point
#'
#' A step of the karpV_native implementation. Called by \code{morie_karpV}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param e Passed to \code{.karpv_unit}.
#' @param node Passed to \code{.karpv_collect}.
#' @param internal_bias Passed to \code{<}.
#' @return The value of \code{$}.
#' @export
.karpv_pick_point <- function(e, node, internal_bias) {
  nodes <- .karpv_collect(node, integer(0), list())
  is_t <- vapply(nodes, function(p) .karpv_is_term(p$node), logical(1))
  internal <- nodes[!is_t]
  leaves <- nodes[is_t]
  want <- length(internal) > 0L && .karpv_unit(e) < internal_bias
  pool <- if (want) internal else if (length(leaves)) leaves else internal
  pool[[.karpv_below(e, length(pool)) + 1L]]$path
}

#' .karpv_get
#'
#' A step of the karpV_native implementation. Called by \code{morie_karpV}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param node A list; the body reads \code{$args} from it.
#' @param path See Usage.
#' @return The value of \code{node}, as built in the body.
#' @export
.karpv_get <- function(node, path) {
  for (k in path) node <- node$args[[k]]
  node
}

#' .karpv_copy
#'
#' A step of the karpV_native implementation. Called by \code{.karpv_replace}, \code{morie_karpV}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param node A list; the body reads \code{$args}, \code{$op}, \code{$term} from it.
#' @return The value of \code{.karpv_fnode}.
#' @export
.karpv_copy <- function(node) {
  if (.karpv_is_term(node)) return(.karpv_tnode(node$term))
  .karpv_fnode(node$op, lapply(node$args, .karpv_copy))
}

#' .karpv_replace
#'
#' A step of the karpV_native implementation. Called by \code{morie_karpV}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param node A list; the body reads \code{$args}, \code{$op} from it.
#' @param path A vector; its length is taken and its elements indexed.
#' @param new Passed to \code{.karpv_copy}.
#' @return The value of \code{out}, as built in the body.
#' @export
.karpv_replace <- function(node, path, new) {
  if (!length(path)) return(.karpv_copy(new))
  out <- .karpv_fnode(node$op, node$args)
  out$args[[path[1]]] <- .karpv_replace(out$args[[path[1]]], path[-1], new)
  out
}

# Compensated accumulation, so both arms agree bit for bit: R's sum()
# accumulates in long double, CPython's compensates, and neither is the
# plain loop the other one is.
#' Compensated accumulation, so both arms agree bit for bit: R\'s sum()
#'
#' accumulates in long double, CPython\'s compensates, and neither is
#' the plain loop the other one is.
#'
#' @param v A vector; its length is taken and its elements indexed.
#' @return A numeric value.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' res <- .karpv_csum(v = x)
#' res
.karpv_csum <- function(v) {
  s <- 0
  cc <- 0
  for (i in seq_along(v)) {
    t <- v[i]
    u <- s + t
    if (abs(s) >= abs(t)) cc <- cc + ((s - u) + t) else cc <- cc + ((t - u) + s)
    s <- u
  }
  s + cc
}

#' Raw fitness: sum of absolute errors over the fitness cases
#'
#' @param node the tree.
#' @param cases list of list(inputs, target).
#' @param terminals variable names, matched positionally to inputs.
#' @return the summed absolute error, or Inf if the tree misbehaves.
#' @export
morie_karpV_raw_fitness <- function(node, cases, terminals) {
  errs <- numeric(length(cases))
  for (i in seq_along(cases)) {
    cs <- cases[[i]]
    env <- as.list(stats::setNames(as.numeric(cs[[1]]), terminals))
    got <- try(morie_karpV_evaluate(node, env), silent = TRUE)
    if (inherits(got, "try-error") || !is.finite(got)) return(Inf)
    errs[i] <- abs(got - as.numeric(cs[[2]]))
  }
  .karpv_csum(errs)
}

#' Adjusted fitness
#'
#' @param raw raw fitness, smaller being better.
#' @return 1/(1+raw), which is what selection uses.
#' @export
morie_karpV_adjusted <- function(raw) if (!is.finite(raw)) 0 else 1 / (1 + raw)

#' .karpv_roulette
#'
#' A step of the karpV_native implementation. Called by \code{morie_karpV}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param e Passed to \code{.karpv_below}.
#' @param adj A vector; its length is taken and its elements indexed.
#' @param total Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.karpv_roulette <- function(e, adj, total) {
  if (total <= 0) return(.karpv_below(e, length(adj)))
  r <- .karpv_unit(e) * total
  acc <- 0
  for (i in seq_along(adj)) {
    acc <- acc + adj[i]
    if (r < acc) return(i - 1L)
  }
  length(adj) - 1L
}

#' Genetic programming
#'
#' @param fitness function of a tree returning raw fitness, or NULL to
#'   use the sum of absolute errors over cases.
#' @param ops alias for functions, for the older call shape.
#' @param gens generations after the initial one.
#' @param cases list of list(inputs, target).
#' @param terminals variable names, matched positionally to inputs.
#' @param functions list of name and arity pairs; defaults to plus,
#'   minus, times and protected division.
#' @param erc length-two range for ephemeral constants, or NULL.
#' @param pop_size population size.
#' @param max_depth_init deepest initial tree.
#' @param max_depth cap on offspring depth; a child that exceeds it is
#'   replaced by its first parent.
#' @param p_crossover crossover probability.
#' @param p_mutation mutation probability.
#' @param internal_bias probability of an internal crossover point.
#' @param seed seeds the module's own generator, so a run reproduces in
#'   both language arms.
#' @param elitism how many best individuals to carry over unchanged.
#' @return a list with best, best_string, best_raw, best_adjusted,
#'   best_size, best_depth, generation_found, history, evaluations,
#'   generations, pop_size, seed and method.
#' @export
morie_karpV <- function(fitness = NULL, ops = NULL, gens = 20L,
                        cases = NULL, terminals = "x", functions = NULL,
                        erc = c(-5, 5), pop_size = 100L,
                        max_depth_init = 6L, max_depth = 17L,
                        p_crossover = 0.9, p_mutation = 0,
                        internal_bias = 0.9, seed = 1L, elitism = 0L) {
  if (is.null(functions))
    functions <- if (!is.null(ops)) ops else .KARPV_FUNCTIONS
  terminals <- as.character(terminals)
  if (is.null(fitness)) {
    if (is.null(cases) || !length(cases))
      stop("karpV: give either a fitness function or fitness cases",
           call. = FALSE)
    fitness <- function(tree) morie_karpV_raw_fitness(tree, cases, terminals)
  }
  if (max_depth_init < 2L)
    stop(sprintf(paste("karpV: max_depth_init = %d; ramped half-and-half",
                       "needs at least 2"), max_depth_init), call. = FALSE)

  e <- .karpv_rng(seed)
  pop <- morie_karpV_ramped(e, as.integer(pop_size), functions, terminals,
                            erc, as.integer(max_depth_init))
  evals <- 0L
  best <- NULL
  best_raw <- Inf
  best_gen <- 0L
  hist <- list()

  for (g in 0:as.integer(gens)) {
    raws <- vapply(pop, fitness, numeric(1))
    evals <- evals + length(pop)
    adj <- vapply(raws, morie_karpV_adjusted, numeric(1))
    total <- .karpv_csum(adj)
    for (i in seq_along(raws)) if (raws[i] < best_raw) {
      best_raw <- raws[i]
      best <- .karpv_copy(pop[[i]])
      best_gen <- as.integer(g)
    }
    fin <- raws[is.finite(raws)]
    hist[[length(hist) + 1L]] <- c(
      min(raws),
      if (length(fin)) .karpv_csum(fin) / length(fin) else Inf,
      morie_karpV_size(pop[[which.min(raws)]]))
    if (g == as.integer(gens)) break

    nxt <- list()
    if (elitism > 0L) {
      ord <- order(raws)
      for (i in ord[seq_len(as.integer(elitism))])
        nxt[[length(nxt) + 1L]] <- .karpv_copy(pop[[i]])
    }
    while (length(nxt) < length(pop)) {
      r <- .karpv_unit(e)
      if (r < p_crossover && length(pop) > 1L) {
        a <- pop[[.karpv_roulette(e, adj, total) + 1L]]
        b <- pop[[.karpv_roulette(e, adj, total) + 1L]]
        pa <- .karpv_pick_point(e, a, internal_bias)
        pb <- .karpv_pick_point(e, b, internal_bias)
        child <- .karpv_replace(a, pa, .karpv_get(b, pb))
        if (morie_karpV_depth(child) > max_depth) child <- .karpv_copy(a)
        nxt[[length(nxt) + 1L]] <- child
      } else if (r < p_crossover + p_mutation) {
        a <- pop[[.karpv_roulette(e, adj, total) + 1L]]
        pa <- .karpv_pick_point(e, a, internal_bias)
        sub <- .karpv_grow(e, functions, terminals, erc,
                           as.integer(max_depth_init), FALSE)
        child <- .karpv_replace(a, pa, sub)
        if (morie_karpV_depth(child) > max_depth) child <- .karpv_copy(a)
        nxt[[length(nxt) + 1L]] <- child
      } else {
        nxt[[length(nxt) + 1L]] <-
          .karpv_copy(pop[[.karpv_roulette(e, adj, total) + 1L]])
      }
    }
    pop <- nxt[seq_along(pop)]
  }

  list(best = best,
       best_string = if (is.null(best)) NULL else morie_karpV_to_string(best),
       best_raw = best_raw,
       best_adjusted = morie_karpV_adjusted(best_raw),
       best_size = if (is.null(best)) 0L else morie_karpV_size(best),
       best_depth = if (is.null(best)) 0L else morie_karpV_depth(best),
       generation_found = best_gen,
       history = hist,
       evaluations = as.integer(evals),
       generations = as.integer(gens),
       pop_size = as.integer(pop_size),
       seed = as.integer(seed),
       method = sprintf(paste("genetic programming (Koza 1992): ramped",
                              "half-and-half over depths 2..%d,",
                              "fitness-proportionate selection on adjusted",
                              "fitness, %g crossover with a %g",
                              "internal-node bias, depth cap %d"),
                        max_depth_init, p_crossover, internal_bias,
                        max_depth))
}
