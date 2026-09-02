# SPDX-License-Identifier: AGPL-3.0-or-later
# R arm of alfqud -- AlphaDev's AssemblyGame. Mirrors
# src/morie/fn/alfqud.py operation for operation.
#
# The trick of the paper is the reframing. Do not ask a model to write a
# sorting algorithm. Ask it to play a one-player game whose moves are
# single machine instructions and whose position is the program written
# so far together with what that program does to a set of test inputs
# when you actually run it. A move that makes the outputs more sorted is
# a good move. What comes out is not a proof and not a transcript of
# reasoning; it is a program, and it either sorts or it does not.
#
#   THE MACHINE. Registers and memory, and four instructions: an
#   unconditional move, a compare that sets a flag, and two moves
#   conditional on that flag. The semantics are written out in
#   morie_alfqud_step and nothing there is a heuristic. This is what
#   makes the exercise falsifiable: a candidate program is not scored by
#   resemblance to a sorting algorithm, it is EXECUTED.
#
#   THE REWARD. Correctness counts the memory slots holding the value
#   they should hold, summed over the test inputs; the score subtracts a
#   weight times the instruction count, which is the paper's latency
#   proxy. The weight is a parameter because the trade it governs is the
#   caller's to make, and because at weight zero the search is a pure
#   correctness search, the honest baseline.
#
#   THE SEARCH. Two routes, and the second keeps the first honest.
#     mcts  PUCT tree search, the method the paper uses. AlphaDev guides
#           it with a trained policy and value network; there are no
#           weights here, so the prior is uniform and a position is
#           valued by the correctness it has actually reached. That is a
#           weaker search than the paper's, and it is labelled as such.
#     bfs   Exhaustive enumeration up to the length limit. Exponential,
#           so only usable on small action spaces -- but it returns the
#           PROVABLE optimum, and a tree search that beats a provable
#           optimum is a tree search with a bug.
#
# Both routes return the best program they saw, not the last one.
#
# References
#   Mankowitz, D.J. et al. (2023) "Faster sorting algorithms discovered
#     using deep reinforcement learning." Nature 618, 257-263.
#     doi:10.1038/s41586-023-06004-9.
#   Silver, D. et al. (2017) "Mastering the game of Go without human
#     knowledge." Nature 550, 354-359. The PUCT selection rule.
#   Rosin, C.D. (2011) "Multi-armed bandits with episode context."
#     Annals of Mathematics and Artificial Intelligence 61(3), 203-230.

.alfqud_ops <- c("mov", "cmp", "cmovl", "cmovg")

#' .alfqud_read
#'
#' A step of the alfqud_native implementation. Called by \code{morie_alfqud_step}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param st A list; the body reads \code{$mem}, \code{$reg} from it.
#' @param loc A vector; indexed elementwise.
#' @return Nothing; this branch always raises.
#' @export
.alfqud_read <- function(st, loc) {
  bank <- loc[[1]]
  idx <- as.integer(loc[[2]])
  if (identical(bank, "M")) {
    if (idx < 0L || idx >= length(st$mem)) {
      stop("the instruction reads outside memory")
    }
    return(st$mem[idx + 1L])
  }
  if (identical(bank, "R")) {
    if (idx < 0L || idx >= length(st$reg)) {
      stop("the instruction reads a register that is not there")
    }
    return(st$reg[idx + 1L])
  }
  stop("a location is in memory or in a register, nothing else")
}

#' .alfqud_write
#'
#' A step of the alfqud_native implementation. Called by \code{morie_alfqud_step}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param st A list; the body reads \code{$mem}, \code{$reg} from it.
#' @param loc A vector; indexed elementwise.
#' @param v See Usage.
#' @return The value of \code{st}, as built in the body.
#' @export
.alfqud_write <- function(st, loc, v) {
  bank <- loc[[1]]
  idx <- as.integer(loc[[2]])
  if (identical(bank, "M")) {
    if (idx < 0L || idx >= length(st$mem)) {
      stop("the instruction writes outside memory")
    }
    st$mem[idx + 1L] <- v
  } else if (identical(bank, "R")) {
    if (idx < 0L || idx >= length(st$reg)) {
      stop("the instruction writes a register that is not there")
    }
    st$reg[idx + 1L] <- v
  } else {
    stop("a location is in memory or in a register, nothing else")
  }
  st
}

#' Execute one instruction
#'
#' Deterministic, and the whole semantics. \code{mov src dst} gives dst
#' the value of src; \code{cmp a b} sets the flag to minus one, zero or
#' one as a is below, equal to or above b and moves nothing;
#' \code{cmovl} and \code{cmovg} move only when the flag is negative or
#' positive respectively.
#'
#' The conditional moves are the reason this instruction set is worth
#' searching at all: they let a program reorder two values without a
#' branch, which is what the discovered routines exploit.
#'
#' @param st A list with mem, reg and flag.
#' @param instr A list of the operation and its two locations.
#' @return The state after the instruction.
#' @export
morie_alfqud_step <- function(st, instr) {
  op <- instr[[1]]
  if (!(op %in% .alfqud_ops)) stop("unknown instruction: ", op)
  a <- instr[[2]]
  b <- instr[[3]]
  if (identical(op, "mov")) {
    st <- .alfqud_write(st, b, .alfqud_read(st, a))
  } else if (identical(op, "cmp")) {
    x <- .alfqud_read(st, a)
    y <- .alfqud_read(st, b)
    st$flag <- if (x < y) -1L else if (x > y) 1L else 0L
  } else if (identical(op, "cmovl")) {
    if (st$flag < 0L) st <- .alfqud_write(st, b, .alfqud_read(st, a))
  } else {
    if (st$flag > 0L) st <- .alfqud_write(st, b, .alfqud_read(st, a))
  }
  st
}

#' Run a program on one input vector
#'
#' Memory starts holding the input, the registers start at zero and the
#' flag starts cleared, so a program's behaviour depends on nothing but
#' the instructions it contains.
#'
#' @param program A list of instructions.
#' @param x The input vector.
#' @param n_reg How many registers.
#' @return The memory the program leaves behind.
#' @export
morie_alfqud_run <- function(program, x, n_reg) {
  st <- list(
    mem = as.numeric(x), reg = rep(0, as.integer(n_reg)),
    flag = 0L
  )
  for (instr in program) st <- morie_alfqud_step(st, instr)
  st$mem
}

#' How many memory slots end up holding the value they should
#'
#' Summed over the test inputs, so a program that sorts two of three
#' cases scores strictly between one that sorts none and one that sorts
#' all. The paper's alternative measure -- the squared distance from the
#' target -- rewards being close; this one rewards being right, and
#' being right is what a sorting routine has to be.
#'
#' @param program A list of instructions.
#' @param inputs The test inputs.
#' @param targets What each input should become.
#' @param n_reg How many registers.
#' @return An integer count.
#' @export
morie_alfqud_correctness <- function(program, inputs, targets, n_reg) {
  got <- 0L
  for (q in seq_along(inputs)) {
    out <- morie_alfqud_run(program, inputs[[q]], n_reg)
    t <- targets[[q]]
    for (k in seq_along(t)) if (out[k] == t[k]) got <- got + 1L
  }
  got
}

#' Every legal instruction over the given memory and registers
#'
#' Ordered so the enumeration is reproducible: by operation in the order
#' they are defined, then by source, then by destination. A compare of a
#' location with itself is dropped -- its flag is always zero, so it can
#' only waste an instruction.
#'
#' @param n_mem How many memory slots.
#' @param n_reg How many registers.
#' @return A list of instructions.
#' @export
morie_alfqud_actions <- function(n_mem, n_reg) {
  locs <- list()
  for (i in 0:(as.integer(n_mem) - 1L)) {
    locs[[length(locs) + 1L]] <-
      list("M", i)
  }
  if (as.integer(n_reg) > 0L) {
    for (i in 0:(as.integer(n_reg) - 1L)) {
      locs[[length(locs) + 1L]] <-
        list("R", i)
    }
  }
  out <- list()
  for (op in .alfqud_ops) {
    for (a in locs) {
      for (b in locs) {
        if (identical(a[[1]], b[[1]]) && a[[2]] == b[[2]]) next
        out[[length(out) + 1L]] <- list(op, a, b)
      }
    }
  }
  out
}

#' A program as one readable line per instruction
#'
#' @param program A list of instructions.
#' @return A character scalar.
#' @export
morie_alfqud_text <- function(program) {
  if (!length(program)) {
    return("")
  }
  paste(
    vapply(program, function(i) {
      sprintf(
        "%s %s%d %s%d", i[[1]], i[[2]][[1]], as.integer(i[[2]][[2]]),
        i[[3]][[1]], as.integer(i[[3]][[2]])
      )
    }, character(1)),
    collapse = "\n"
  )
}

#' .alfqud_score
#'
#' A step of the alfqud_native implementation. Called by \code{.alfqud_bfs}, \code{.alfqud_mcts}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param program A vector; its length is taken.
#' @param inputs Passed to \code{morie_alfqud_correctness}.
#' @param targets Passed to \code{morie_alfqud_correctness}.
#' @param n_reg Passed to \code{morie_alfqud_correctness}.
#' @param lw Coerced to numeric by the body, with \code{as.numeric}.
#' @param rf Optional; may be \code{NULL}. Passed to \code{is.null}.
#' @return A vector, from \code{c}.
#' @export
.alfqud_score <- function(program, inputs, targets, n_reg, lw, rf) {
  cc <- if (is.null(rf)) {
    morie_alfqud_correctness(
      program, inputs,
      targets, n_reg
    )
  } else {
    as.numeric(rf(program, inputs, targets, n_reg))
  }
  c(as.numeric(cc) - as.numeric(lw) * length(program), as.numeric(cc))
}

#' .alfqud_bfs
#'
#' A step of the alfqud_native implementation. Called by \code{morie_alfqud}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param inputs Passed to \code{.alfqud_score}.
#' @param targets Passed to \code{.alfqud_score}.
#' @param acts See Usage.
#' @param n_reg Passed to \code{.alfqud_score}.
#' @param max_len Coerced to integer by the body, with \code{as.integer}.
#' @param lw Passed to \code{.alfqud_score}.
#' @param rf Passed to \code{.alfqud_score}.
#' @return A list with \code{prog}, \code{s}, \code{c}, \code{seen}.
#' @export
.alfqud_bfs <- function(inputs, targets, acts, n_reg, max_len, lw, rf) {
  best <- list()
  z <- .alfqud_score(list(), inputs, targets, n_reg, lw, rf)
  best_s <- z[1]
  best_c <- z[2]
  frontier <- list(list())
  seen <- 1L
  for (d in seq_len(as.integer(max_len))) {
    nxt <- list()
    for (prog in frontier) {
      for (act in acts) {
        cand <- c(prog, list(act))
        seen <- seen + 1L
        z <- .alfqud_score(cand, inputs, targets, n_reg, lw, rf)
        if (z[1] > best_s) {
          best_s <- z[1]
          best_c <- z[2]
          best <- cand
        }
        nxt[[length(nxt) + 1L]] <- cand
      }
    }
    frontier <- nxt
  }
  list(prog = best, s = best_s, c = best_c, seen = seen)
}

#' .alfqud_mcts
#'
#' A step of the alfqud_native implementation. Called by \code{morie_alfqud}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param inputs Passed to \code{.alfqud_score}.
#' @param targets Passed to \code{.alfqud_score}.
#' @param acts A vector; its length is taken and its elements indexed.
#' @param n_reg Passed to \code{.alfqud_score}.
#' @param max_len Coerced to integer by the body, with \code{as.integer}.
#' @param lw Passed to \code{.alfqud_score}.
#' @param rf Passed to \code{.alfqud_score}.
#' @param n_sim Coerced to integer by the body, with \code{as.integer}.
#' @param c_puct Numeric; combined arithmetically in the body.
#' @return A list with \code{prog}, \code{s}, \code{c}, \code{seen}.
#' @export
.alfqud_mcts <- function(inputs, targets, acts, n_reg, max_len, lw, rf,
                         n_sim, c_puct) {
  # Nodes are keyed by the program that reaches them, so the tree is a
  # tree and not a graph -- two different instruction orders reaching the
  # same machine state are different nodes, which is what the game says.
  # Priors are uniform because there is no trained policy here, and a
  # leaf is valued by the correctness it has reached, normalised to the
  # unit interval so the exploration constant means the same thing
  # whatever the test set is.
  a <- length(acts)
  full <- sum(vapply(targets, length, integer(1)))
  N <- new.env(hash = TRUE, parent = emptyenv())
  W <- new.env(hash = TRUE, parent = emptyenv())
  # Prefixed because R will not take the empty string as a name, and the
  # root of the tree is the empty program.
  key <- function(v) paste0("k", paste(v, collapse = ","))
  best <- list()
  z <- .alfqud_score(list(), inputs, targets, n_reg, lw, rf)
  best_s <- z[1]
  best_c <- z[2]
  nodes <- 0L
  for (it in seq_len(as.integer(n_sim))) {
    node <- integer(0)
    path <- list()
    repeat {
      k0 <- key(node)
      if (!exists(k0, envir = N, inherits = FALSE)) break
      if (length(node) >= as.integer(max_len)) break
      nv <- get(k0, envir = N)
      wv <- get(k0, envir = W)
      sq <- sqrt(sum(nv))
      bi <- 1L
      bv <- NULL
      for (k in seq_len(a)) {
        nk <- nv[k]
        q <- if (nk > 0) wv[k] / nk else 0
        u <- q + c_puct * (1 / a) * sq / (1 + nk)
        if (is.null(bv) || u > bv) {
          bv <- u
          bi <- k
        }
      }
      path[[length(path) + 1L]] <- list(k0, bi)
      node <- c(node, bi)
    }
    k0 <- key(node)
    if (!exists(k0, envir = N, inherits = FALSE)) {
      assign(k0, rep(0, a), envir = N)
      assign(k0, rep(0, a), envir = W)
      nodes <- nodes + 1L
    }
    prog <- if (!length(node)) list() else lapply(node, function(k) acts[[k]])
    z <- .alfqud_score(prog, inputs, targets, n_reg, lw, rf)
    if (z[1] > best_s) {
      best_s <- z[1]
      best_c <- z[2]
      best <- prog
    }
    v <- if (full > 0) z[2] / full else 0
    for (pp in path) {
      kk <- pp[[1]]
      ii <- pp[[2]]
      nv <- get(kk, envir = N)
      nv[ii] <- nv[ii] + 1
      assign(kk, nv, envir = N)
      wv <- get(kk, envir = W)
      wv[ii] <- wv[ii] + v
      assign(kk, wv, envir = W)
    }
  }
  list(prog = best, s = best_s, c = best_c, seen = nodes)
}

#' Search for a machine program that sorts the given inputs
#'
#' @param target The test inputs. What each one should become is its own
#'   values in ascending order -- that is the specification of a sort,
#'   and making it derived rather than supplied means the two cannot
#'   drift apart.
#' @param action_space The instructions the search may use, or NULL for
#'   every legal instruction over the memory and registers.
#' @param reward_fn A function of program, inputs, targets and n_reg
#'   returning the correctness, or NULL for the count of correctly
#'   placed elements.
#' @param n_reg How many registers the machine has.
#' @param max_len The longest program the search will consider.
#' @param latency_weight Instructions charged against correctness. Zero
#'   searches for correctness alone.
#' @param search Either mcts or bfs; see the file header on why both.
#' @param n_sim Simulations for the tree search.
#' @param c_puct The PUCT exploration constant.
#' @param seed Unused by this search, kept so the signature matches the
#'   Python arm.
#' @return A list with the best program, its score, its correctness, and
#'   how much of the space was looked at.
#' @export
morie_alfqud <- function(target, action_space = NULL, reward_fn = NULL,
                         n_reg = 2L, max_len = 3L, latency_weight = 0,
                         search = "mcts", n_sim = 400L, c_puct = 1.25,
                         seed = 0) {
  inputs <- lapply(target, as.numeric)
  if (!length(inputs)) {
    stop(
      "a search with no test input cannot tell a sorting routine ",
      "from any other program"
    )
  }
  n_mem <- length(inputs[[1]])
  for (x in inputs) {
    if (length(x) != n_mem) {
      stop("every test input must be the same length")
    }
  }
  targets <- lapply(inputs, sort)
  n_reg <- as.integer(n_reg)
  acts <- if (is.null(action_space)) {
    morie_alfqud_actions(n_mem, n_reg)
  } else {
    action_space
  }
  if (!length(acts)) {
    stop("a search with no legal move has nothing to do")
  }
  r <- if (identical(search, "bfs")) {
    .alfqud_bfs(
      inputs, targets, acts, n_reg, max_len, latency_weight,
      reward_fn
    )
  } else if (identical(search, "mcts")) {
    .alfqud_mcts(
      inputs, targets, acts, n_reg, max_len, latency_weight,
      reward_fn, n_sim, c_puct
    )
  } else {
    stop("the search is mcts or bfs")
  }
  full <- sum(vapply(targets, length, integer(1)))
  outs <- lapply(inputs, function(x) morie_alfqud_run(r$prog, x, n_reg))
  list(
    program = r$prog, text = morie_alfqud_text(r$prog),
    length = length(r$prog), score = r$s, correct = r$c,
    max_correct = full, solved = r$c == full,
    outputs = outs, targets = targets, nodes = r$seen,
    n_actions = length(acts), n_mem = n_mem, n_reg = n_reg,
    max_len = as.integer(max_len),
    latency_weight = as.numeric(latency_weight), search = search,
    method = "AlphaDev AssemblyGame instruction search"
  )
}

#' One-line summary of the alfqud module
#'
#' @return A character scalar.
#' @export
morie_alfqud_cheatsheet <- function() {
  paste0(
    "alfqud: AlphaDev AssemblyGame. Programs of mov/cmp/cmovl/",
    "cmovg searched by PUCT tree search or exhaustively, scored ",
    "by executing them on test inputs"
  )
}
