# morie.fn -- function file (rootcoder007/morie)
# LL(1) parsing: the table, and what top-down analysis cannot do.
#
# The decision. A top-down parser expands the leftmost nonterminal.
# Sitting at A with one lookahead token a, it must pick a production
# A -> alpha before seeing what alpha derives. That choice is possible
# for every (A, a) exactly when the grammar is LL(1), and the two
# ingredients are FIRST(alpha) and FOLLOW(A).
#
# Production A -> alpha is entered under every a in FIRST(alpha), and
# additionally under every a in FOLLOW(A) when alpha can derive the
# empty string. Two productions landing in one cell is a conflict,
# and the grammar is not LL(1) -- this is reported with the offending
# pair rather than silently resolved, because silently resolving it
# is how a parser comes to accept a language nobody wrote down.
#
# The structural obstruction. Left recursion A -> A alpha puts
# FIRST(A alpha) subseteq FIRST(A), so the recursive production and
# the base production compete on the same lookahead, always. No
# amount of lookahead fixes it: a top-down parser expanding A would
# expand A again with no input consumed. This is the price top-down
# analysis pays, and the standard transformation to right recursion
# is provided -- it changes the parse tree's shape, so the
# associativity a left-recursive grammar encoded has to be recovered
# some other way.
#
# Two routes, one answer. The table-driven parser runs an explicit
# stack; the recursive-descent parser uses the call stack and reads
# like the grammar. Both are implemented and must produce identical
# trees, which the anchor checks -- one of them being wrong is much
# easier than both being wrong the same way.
#
# References
# ----------
# Knuth, D. E. (1971) "Top-down syntax analysis", Acta Informatica
# 1(2), 79-110, doi:10.1007/BF00289517. Top-down (LL) analysis, the
# role of one-symbol lookahead, and the failure of top-down methods on
# left-recursive grammars.
#
# Knuth, D. E. (1965) "On the translation of languages from left to
# right", Information and Control 8(6), 607-639,
# doi:10.1016/S0019-9958(65)90426-2, for the LR(k) classes against
# which the LL(1) restriction is measured.

.prsLL_EPSILON <- ""
.prsLL_END <- "$"
.prsLL_ROUTES <- c("table", "recursive_descent")

# ----- Set operations on character vectors -----
#' Set operations on character vectors -----
#'
#' A step of the prsLL_native implementation. Called by \code{.prsLL_first_seq},
#' \code{.prsLL_first_sets}, \code{.prsLL_follow_sets} and 2 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a Passed to \code{c}.
#' @param b Passed to \code{c}.
#' @return The value of \code{unique}.
#' @export
#' @examples
#' A <- matrix(c(4, 1, 0.5, 1, 3, 0.8, 0.5, 0.8, 2), nrow = 3)
#' b <- c(1.5, 2.5, 3.5)
#' res <- .prsLL_union(a = A, b = b)
#' res
.prsLL_union <- function(a, b) unique(c(a, b))
#' .prsLL_setdiff
#'
#' A step of the prsLL_native implementation. Called by \code{.prsLL_first_seq},
#' \code{.prsLL_follow_sets}, \code{.prsLL_ll1_table}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a A vector; indexed elementwise.
#' @param b Passed to \code{\%in\%}.
#' @return The value of \code{[}.
#' @export
#' @examples
#' A <- matrix(c(4, 1, 0.5, 1, 3, 0.8, 0.5, 0.8, 2), nrow = 3)
#' b <- c(1.5, 2.5, 3.5)
#' res <- .prsLL_setdiff(a = A, b = b)
#' res
.prsLL_setdiff <- function(a, b) a[!(a %in% b)]
#' .prsLL_subset
#'
#' A step of the prsLL_native implementation. Called by \code{.prsLL_first_sets},
#' \code{.prsLL_follow_sets}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a Passed to \code{\%in\%}.
#' @param b Passed to \code{\%in\%}.
#' @return A logical value.
#' @export
#' @examples
#' A <- matrix(c(4, 1, 0.5, 1, 3, 0.8, 0.5, 0.8, 2), nrow = 3)
#' b <- c(1.5, 2.5, 3.5)
#' res <- .prsLL_subset(a = A, b = b)
#' res
.prsLL_subset <- function(a, b) all(a %in% b)

# ----- Grammar construction and validation -----
#' Grammar construction and validation -----
#'
#' A step of the prsLL_native implementation. Called by
#' \code{.prsLL_remove_left_recursion}, \code{morie_prsLL}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param rules See Usage.
#' @param start Optional; may be \code{NULL}. Coerced to character by the body, with
#' \code{as.character}.
#' @return The value of \code{g}, as built in the body.
#' @export
.prsLL_grammar <- function(rules, start = NULL) {
  R <- list()
  for (item in rules) {
    lhs <- item[[1]]
    rhs <- item[[2]]
    if (!is.character(lhs) || length(lhs) == 0L || lhs == "") {
      stop(sprintf("prsLL: a left-hand side must be a non-empty symbol, got %s",
                   deparse(lhs)))
    }
    seq <- as.character(rhs)
    if (any(seq == "")) {
      stop("prsLL: write the empty production as an empty right-hand side, not as \"\"")
    }
    if (.prsLL_END %in% seq || lhs == .prsLL_END) {
      stop(sprintf("prsLL: %s is reserved for end of input", .prsLL_END))
    }
    R[[length(R) + 1L]] <- list(lhs, seq)
  }
  if (length(R) == 0L) {
    stop("prsLL: the grammar has no productions")
  }
  S <- if (is.null(start)) R[[1]][[1]] else as.character(start)
  lhs_list <- vapply(R, function(x) x[[1]], character(1))
  if (!(S %in% lhs_list)) {
    stop(sprintf("prsLL: the start symbol %s has no production", S))
  }
  g <- list(rules = R, start = S)
  nts <- .prsLL_nonterminals(g)
  unreachable <- setdiff(nts, .prsLL_reachable(g))
  if (length(unreachable) > 0L) {
    stop(sprintf("prsLL: nonterminal(s) %s cannot be reached from the start symbol",
                 paste(sort(unreachable), collapse = ", ")))
  }
  g
}

#' .prsLL_reachable
#'
#' A step of the prsLL_native implementation. Called by \code{.prsLL_grammar}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param g A list; the body reads \code{$rules}, \code{$start} from it.
#' @return The value of \code{seen}, as built in the body.
#' @export
.prsLL_reachable <- function(g) {
  nts <- .prsLL_nonterminals(g)
  seen <- c(g$start)
  stack <- c(g$start)
  while (length(stack) > 0L) {
    A <- stack[length(stack)]
    stack <- stack[-length(stack)]
    for (rule in g$rules) {
      if (rule[[1]] != A) next
      for (s in rule[[2]]) {
        if (s %in% nts && !(s %in% seen)) {
          seen <- c(seen, s)
          stack <- c(stack, s)
        }
      }
    }
  }
  seen
}

#' .prsLL_nonterminals
#'
#' A step of the prsLL_native implementation. Called by \code{.prsLL_first_of},
#' \code{.prsLL_first_sets}, \code{.prsLL_follow_sets} and 8 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param g A list; the body reads \code{$rules} from it.
#' @return The value of \code{out}, as built in the body.
#' @export
.prsLL_nonterminals <- function(g) {
  out <- character(0)
  for (rule in g$rules) {
    lhs <- rule[[1]]
    if (!(lhs %in% out)) {
      out <- c(out, lhs)
    }
  }
  out
}

#' .prsLL_terminals
#'
#' A step of the prsLL_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param g A list; the body reads \code{$rules} from it.
#' @return The value of \code{out}, as built in the body.
#' @export
.prsLL_terminals <- function(g) {
  nts <- .prsLL_nonterminals(g)
  out <- character(0)
  for (rule in g$rules) {
    for (s in rule[[2]]) {
      if (!(s %in% nts) && !(s %in% out)) {
        out <- c(out, s)
      }
    }
  }
  out
}

# ----- FIRST and FOLLOW sets -----
#' FIRST and FOLLOW sets -----
#'
#' A step of the prsLL_native implementation. Called by \code{.prsLL_first_of},
#' \code{.prsLL_follow_sets}, \code{.prsLL_left_recursive} and 1 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param g A list; the body reads \code{$rules} from it.
#' @return The value of \code{first}, as built in the body.
#' @export
.prsLL_first_sets <- function(g) {
  nts <- .prsLL_nonterminals(g)
  first <- list()
  for (A in nts) first[[A]] <- character(0)
  changed <- TRUE
  while (changed) {
    changed <- FALSE
    for (rule in g$rules) {
      A <- rule[[1]]
      rhs <- rule[[2]]
      add <- .prsLL_first_seq(rhs, first, nts)
      if (!.prsLL_subset(add, first[[A]])) {
        first[[A]] <- .prsLL_union(first[[A]], add)
        changed <- TRUE
      }
    }
  }
  first
}

#' .prsLL_first_seq
#'
#' A step of the prsLL_native implementation. Called by \code{.prsLL_first_of},
#' \code{.prsLL_first_sets}, \code{.prsLL_follow_sets} and 1 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param seq See Usage.
#' @param first A vector; indexed elementwise.
#' @param nts Passed to \code{\%in\%}.
#' @return The value of \code{out}, as built in the body.
#' @export
.prsLL_first_seq <- function(seq, first, nts) {
  out <- character(0)
  for (s in seq) {
    if (!(s %in% nts)) {
      out <- c(out, s)
      return(out)
    }
    out <- .prsLL_union(out, .prsLL_setdiff(first[[s]], .prsLL_EPSILON))
    if (!(.prsLL_EPSILON %in% first[[s]])) {
      return(out)
    }
  }
  out <- c(out, .prsLL_EPSILON)
  out
}

#' .prsLL_first_of
#'
#' A step of the prsLL_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param seq Coerced to character by the body, with \code{as.character}.
#' @param g Passed to \code{.prsLL_first_sets}.
#' @param first Optional; may be \code{NULL}. Passed to \code{is.null}.
#' @return The value of \code{.prsLL_first_seq}.
#' @export
.prsLL_first_of <- function(seq, g, first = NULL) {
  f <- if (is.null(first)) .prsLL_first_sets(g) else first
  .prsLL_first_seq(as.character(seq), f, .prsLL_nonterminals(g))
}

#' .prsLL_follow_sets
#'
#' A step of the prsLL_native implementation. Called by \code{.prsLL_ll1_table}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param g A list; the body reads \code{$rules}, \code{$start} from it.
#' @param first Optional; may be \code{NULL}. Passed to \code{is.null}.
#' @return The value of \code{follow}, as built in the body.
#' @export
.prsLL_follow_sets <- function(g, first = NULL) {
  nts <- .prsLL_nonterminals(g)
  f <- if (is.null(first)) .prsLL_first_sets(g) else first
  follow <- list()
  for (A in nts) follow[[A]] <- character(0)
  follow[[g$start]] <- c(follow[[g$start]], .prsLL_END)
  changed <- TRUE
  while (changed) {
    changed <- FALSE
    for (rule in g$rules) {
      A <- rule[[1]]
      rhs <- rule[[2]]
      n <- length(rhs)
      for (i in seq_len(n)) {
        s <- rhs[i]
        if (!(s %in% nts)) next
        rest <- if (i < n) .prsLL_first_seq(rhs[(i + 1L):n], f, nts) else character(0)
        add <- .prsLL_setdiff(rest, .prsLL_EPSILON)
        if (.prsLL_EPSILON %in% rest || i == n) {
          add <- .prsLL_union(add, follow[[A]])
        }
        if (!.prsLL_subset(add, follow[[s]])) {
          follow[[s]] <- .prsLL_union(follow[[s]], add)
          changed <- TRUE
        }
      }
    }
  }
  follow
}

# ----- LL(1) table construction -----
#' LL(1) table construction -----
#'
#' A step of the prsLL_native implementation. Called by \code{.prsLL_is_ll1}, \code{.prsLL_parse}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param g A list; the body reads \code{$rules} from it.
#' @return A list with \code{table}, \code{conflicts}, \code{first}, \code{follow}.
#' @export
.prsLL_ll1_table <- function(g) {
  first <- .prsLL_first_sets(g)
  follow <- .prsLL_follow_sets(g, first)
  nts <- .prsLL_nonterminals(g)
  table <- list()
  conflicts <- list()
  for (i in seq_along(g$rules)) {
    rule <- g$rules[[i]]
    A <- rule[[1]]
    rhs <- rule[[2]]
    look <- .prsLL_first_seq(rhs, first, nts)
    cells <- .prsLL_setdiff(look, .prsLL_EPSILON)
    if (.prsLL_EPSILON %in% look) {
      cells <- .prsLL_union(cells, follow[[A]])
    }
    for (a in cells) {
      key <- paste(A, a, sep = "\r")
      if (!is.null(table[[key]]) && table[[key]] != i) {
        conflicts[[length(conflicts) + 1L]] <- list(
          nonterminal = A,
          lookahead = a,
          rules = c(table[[key]], i)
        )
      } else {
        table[[key]] <- i
      }
    }
  }
  list(table = table, conflicts = conflicts, first = first, follow = follow)
}

#' .prsLL_is_ll1
#'
#' A step of the prsLL_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param g Passed to \code{.prsLL_ll1_table}.
#' @return A list with \code{estimate}, \code{ll1}, \code{conflicts}, \code{table},
#' \code{first}, \code{follow}, \code{left_recursive}, \code{method}.
#' @export
.prsLL_is_ll1 <- function(g) {
  t <- .prsLL_ll1_table(g)
  list(
    estimate = length(t$conflicts) == 0L,
    ll1 = length(t$conflicts) == 0L,
    conflicts = t$conflicts,
    table = t$table,
    first = t$first,
    follow = t$follow,
    left_recursive = .prsLL_left_recursive(g),
    method = "Knuth (1971): FIRST/FOLLOW table, one production per (nonterminal, lookahead) cell"
  )
}

# ----- Left recursion detection and removal -----
#' Left recursion detection and removal -----
#'
#' A step of the prsLL_native implementation. Called by \code{.prsLL_is_ll1}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param g A list; the body reads \code{$rules} from it.
#' @return The value of \code{out}, as built in the body.
#' @export
.prsLL_left_recursive <- function(g) {
  nts <- .prsLL_nonterminals(g)
  first <- .prsLL_first_sets(g)
  edges <- list()
  for (A in nts) edges[[A]] <- character(0)
  for (rule in g$rules) {
    A <- rule[[1]]
    rhs <- rule[[2]]
    for (s in rhs) {
      if (!(s %in% nts)) break
      edges[[A]] <- .prsLL_union(edges[[A]], s)
      if (!(.prsLL_EPSILON %in% first[[s]])) break
    }
  }
  out <- character(0)
  for (A in nts) {
    seen <- character(0)
    stack <- c(A)
    while (length(stack) > 0L) {
      B <- stack[length(stack)]
      stack <- stack[-length(stack)]
      found <- FALSE
      for (C in edges[[B]]) {
        if (C == A) {
          out <- c(out, A)
          found <- TRUE
          break
        }
        if (!(C %in% seen)) {
          seen <- c(seen, C)
          stack <- c(stack, C)
        }
      }
      if (found) break
    }
  }
  out
}

#' .prsLL_remove_left_recursion
#'
#' A step of the prsLL_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param g A list; the body reads \code{$rules}, \code{$start} from it.
#' @return The value of \code{.prsLL_grammar}.
#' @export
.prsLL_remove_left_recursion <- function(g) {
  rules <- list()
  nts <- .prsLL_nonterminals(g)
  for (A in nts) {
    prods <- list()
    for (rule in g$rules) {
      if (rule[[1]] == A) {
        prods[[length(prods) + 1L]] <- rule[[2]]
      }
    }
    rec <- list()
    for (p in prods) {
      if (length(p) > 0L && p[1] == A) {
        rec[[length(rec) + 1L]] <- p[-1L]
      }
    }
    base <- list()
    for (p in prods) {
      if (!(length(p) > 0L && p[1] == A)) {
        base[[length(base) + 1L]] <- p
      }
    }
    if (length(rec) == 0L) {
      for (p in prods) {
        rules[[length(rules) + 1L]] <- list(A, p)
      }
      next
    }
    if (length(base) == 0L) {
      stop(sprintf("prsLL: %s is left-recursive with no base production, so it derives nothing", A))
    }
    tail <- paste0(A, "'")
    while (tail %in% nts) {
      tail <- paste0(tail, "'")
    }
    for (p in base) {
      rules[[length(rules) + 1L]] <- list(A, c(p, tail))
    }
    for (p in rec) {
      rules[[length(rules) + 1L]] <- list(tail, c(p, tail))
    }
    rules[[length(rules) + 1L]] <- list(tail, character(0))
  }
  .prsLL_grammar(rules, g$start)
}

# ----- Parse tree nodes -----
#' Parse tree nodes -----
#'
#' A step of the prsLL_native implementation. Called by \code{.prsLL_parse_rd},
#' \code{.prsLL_parse_table}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param sym Carried through into a list the body builds.
#' @return A list with \code{symbol}, \code{children}.
#' @export
.prsLL_leaf <- function(sym) {
  list(symbol = sym, children = NULL)
}

#' .prsLL_node
#'
#' A step of the prsLL_native implementation. Called by \code{.prsLL_parse_rd},
#' \code{.prsLL_parse_table}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param sym Carried through into a list the body builds.
#' @param kids Carried through into a list the body builds.
#' @return A list with \code{symbol}, \code{children}.
#' @export
.prsLL_node <- function(sym, kids) {
  list(symbol = sym, children = kids)
}

# ----- Parsing -----
#' Parsing -----
#'
#' A step of the prsLL_native implementation. Called by \code{.prsLL_parse_rd},
#' \code{.prsLL_parse_table}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param table A vector; indexed elementwise.
#' @param A Passed to \code{paste}.
#' @param a Passed to \code{paste}.
#' @return The value of \code{[[}.
#' @export
.prsLL_pick <- function(table, A, a) {
  key <- paste(A, a, sep = "\r")
  if (is.null(table[[key]])) {
    stop(sprintf("prsLL: no production for %s on lookahead %s", A, a))
  }
  table[[key]]
}

#' .prsLL_parse_rd
#'
#' A step of the prsLL_native implementation. Called by \code{.prsLL_parse}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param g A list; the body reads \code{$rules} from it.
#' @param table Passed to \code{.prsLL_pick}.
#' @param toks A vector; indexed elementwise.
#' @param A Passed to \code{.prsLL_pick}.
#' @param pos Numeric; combined arithmetically in the body.
#' @return The value of \code{list}.
#' @export
.prsLL_parse_rd <- function(g, table, toks, A, pos) {
  i <- .prsLL_pick(table, A, toks[pos + 1L])
  rhs <- g$rules[[i]][[2]]
  nts <- .prsLL_nonterminals(g)
  kids <- list()
  for (s in rhs) {
    if (s %in% nts) {
      result <- .prsLL_parse_rd(g, table, toks, s, pos)
      sub <- result[[1]]
      pos <- result[[2]]
      kids[[length(kids) + 1L]] <- sub
    } else {
      if (toks[pos + 1L] != s) {
        stop(sprintf("prsLL: expected %s but found %s at token %d",
                     s, toks[pos + 1L], pos))
      }
      kids[[length(kids) + 1L]] <- .prsLL_leaf(s)
      pos <- pos + 1L
    }
  }
  list(.prsLL_node(A, kids), pos)
}

#' .prsLL_parse_table
#'
#' A step of the prsLL_native implementation. Called by \code{.prsLL_parse}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param g A list; the body reads \code{$rules}, \code{$start} from it.
#' @param table Passed to \code{.prsLL_pick}.
#' @param toks A vector; indexed elementwise.
#' @return The value of \code{list}.
#' @export
.prsLL_parse_table <- function(g, table, toks) {
  nts <- .prsLL_nonterminals(g)
  root <- .prsLL_node(g$start, list())
  stack <- list(list(g$start, root))
  pos <- 0L
  while (length(stack) > 0L) {
    item <- stack[[length(stack)]]
    stack[[length(stack)]] <- NULL
    sym <- item[[1]]
    node <- item[[2]]
    if (sym %in% nts) {
      i <- .prsLL_pick(table, sym, toks[pos + 1L])
      rhs <- g$rules[[i]][[2]]
      kids <- list()
      for (s in rhs) {
        if (s %in% nts) {
          kids[[length(kids) + 1L]] <- .prsLL_node(s, list())
        } else {
          kids[[length(kids) + 1L]] <- .prsLL_leaf(s)
        }
      }
      node$children <- kids
      for (k in rev(seq_along(rhs))) {
        stack[[length(stack) + 1L]] <- list(rhs[k], kids[[k]])
      }
    } else {
      if (toks[pos + 1L] != sym) {
        stop(sprintf("prsLL: expected %s but found %s at token %d",
                     sym, toks[pos + 1L], pos))
      }
      pos <- pos + 1L
    }
  }
  list(root, pos)
}

#' .prsLL_parse
#'
#' A step of the prsLL_native implementation. Called by \code{morie_prsLL}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param g A list; the body reads \code{$start} from it.
#' @param tokens Coerced to character by the body, with \code{as.character}.
#' @param route Compared against \code{"table"}. Defaults to \code{"table"}.
#' @return The value of \code{tree}, as built in the body.
#' @export
.prsLL_parse <- function(g, tokens, route = "table") {
  if (!(route %in% .prsLL_ROUTES)) {
    stop(sprintf("prsLL: route must be one of %s, got %s",
                 paste(.prsLL_ROUTES, collapse = ", "), route))
  }
  t <- .prsLL_ll1_table(g)
  if (length(t$conflicts) > 0L) {
    stop(sprintf("prsLL: the grammar is not LL(1) -- %d conflict(s), first at (%s, %s)",
                 length(t$conflicts),
                 t$conflicts[[1]]$nonterminal,
                 t$conflicts[[1]]$lookahead))
  }
  toks <- c(as.character(tokens), .prsLL_END)
  if (route == "table") {
    result <- .prsLL_parse_table(g, t$table, toks)
  } else {
    result <- .prsLL_parse_rd(g, t$table, toks, g$start, 0L)
  }
  tree <- result[[1]]
  pos <- result[[2]]
  if (pos != length(toks) - 1L) {
    stop(sprintf("prsLL: input not consumed -- stopped at token %d (%s)",
                 pos, toks[pos + 1L]))
  }
  tree
}

# ----- Tree linearisation -----
#' Tree linearisation -----
#'
#' A step of the prsLL_native implementation. Called by \code{morie_prsLL}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param tree A list; the body reads \code{$children}, \code{$symbol} from it.
#' @return The value of \code{out}, as built in the body.
#' @export
.prsLL_linearise <- function(tree) {
  if (is.null(tree$children)) {
    return(c(tree$symbol))
  }
  out <- character(0)
  for (k in tree$children) {
    out <- c(out, .prsLL_linearise(k))
  }
  out
}

# ----- Entry point -----
#' Entry point -----
#'
#' A step of the prsLL_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param grammar_ A list; the body reads \code{$rules}, \code{$start} from it.
#' @param tokens Passed to \code{.prsLL_parse}.
#' @param route Passed to \code{.prsLL_parse}. Defaults to \code{"table"}.
#' @return A list with \code{estimate}, \code{tree}, \code{route}, \code{tokens},
#' \code{yield}, \code{method}.
#' @export
morie_prsLL <- function(grammar_, tokens, route = "table") {
  if (is.list(grammar_) && !is.null(grammar_$rules) && !is.null(grammar_$start)) {
    g <- grammar_
  } else {
    g <- .prsLL_grammar(grammar_)
  }
  tree <- .prsLL_parse(g, tokens, route)
  list(
    estimate = tree,
    tree = tree,
    route = route,
    tokens = as.character(tokens),
    yield = .prsLL_linearise(tree),
    method = "Knuth (1971) top-down analysis with one token of lookahead"
  )
}
