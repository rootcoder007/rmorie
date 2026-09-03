# morie.fn -- function file (rootcoder007/morie)
# LR parsing: deciding a reduction from the left, with lookahead.
#
# **The idea.** A bottom-up parser reads left to right and keeps a
# stack of what it has seen. At each point it must decide: shift the
# next token, or reduce a handle already on the stack -- and if reduce,
# by which production. Knuth's result is that for an LR(k) grammar this
# decision is a function of the stack contents and the next k
# tokens alone, so a finite automaton over *items* decides it.
#
# **Items and states.** An item [A -> alpha . beta, a] records a
# partially matched production together with a lookahead that would
# justify reducing it. The closure of a set of items adds, for
# every [A -> alpha . B beta, a] and every production B -> gamma,
# the items [B -> . gamma, b] for b in FIRST(beta a). States are
# closed item sets; the transition on a symbol shifts the dot past it.
# That is the whole construction.
#
# **Three ways to get the lookaheads, and they differ.**
#
# lr1 is Knuth's canonical construction: the lookahead is carried
# per item, so a production reduced in one context can have a different
# lookahead set than the same production reduced elsewhere.
#
# slr1 (DeRemer 1971) throws that away and reduces A -> alpha on
# all of FOLLOW(A). Far fewer states, but the FOLLOW set pools every
# context, so it can call for a reduction in a context where the
# reduction is wrong.
#
# lalr1 merges canonical states that share a core, unioning their
# lookaheads -- almost always as small as SLR and almost always as
# strong as LR(1). "Almost": merging can create reduce/reduce conflicts
# that neither parent had.
#
# **Conflicts are reported, never resolved.**
#
# **What LR buys over LL.** Left recursion is fine here.
#
# References
# ----------
# Knuth, D. E. (1965) "On the translation of languages from left to
# right", Information and Control 8(6), 607-639,
# doi:10.1016/S0019-9958(65)90426-2.
#
# DeRemer, F. L. (1971) "Simple LR(k) grammars", Communications of the
# ACM 14(7), 453-460, doi:10.1145/362619.362625.

# ---- Private constants --------------------------------------------------

.prsLR_END     <- "END"
.prsLR_EPSILON <- "<epsilon>"
.prsLR_METHODS <- c("lr1", "slr1", "lalr1")
.prsLR_AUG     <- "S'"

# ---- Private grammar utilities ----------------------------------------

#' .prsLR_grammar
#'
#' A step of the prsLR_native implementation. Called by \code{morie_prsLR}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param g A list; the body reads \code{$rules}, \code{$start} from it.
#' @return Nothing; this branch always raises.
#' @export
.prsLR_grammar <- function(g) {
  if (is.list(g) && !is.null(g$rules) && !is.null(g$start)) return(g)
  stop("prsLR: grammar must be a list with $rules and $start")
}

#' .prsLR_nonterminals
#'
#' A step of the prsLR_native implementation. Called by \code{.prsLR_augment},
#' \code{.prsLR_canonical_collection}, \code{.prsLR_first_sets} and 2 others in the
#' module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param g A list; the body reads \code{$rules} from it.
#' @return The value of \code{unique}.
#' @export
.prsLR_nonterminals <- function(g) {
  unique(sapply(g$rules, function(r) r[[1]]))
}

#' .prsLR_terminals
#'
#' A step of the prsLR_native implementation. Called by \code{.prsLR_canonical_collection}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param g A list; the body reads \code{$rules} from it.
#' @return The value of \code{setdiff}.
#' @export
.prsLR_terminals <- function(g) {
  nt_set <- .prsLR_nonterminals(g)
  terms  <- unique(unlist(lapply(g$rules, function(r) r[[2]])))
  setdiff(terms, nt_set)
}

#' .prsLR_first_seq
#'
#' A step of the prsLR_native implementation. Called by \code{.prsLR_closure},
#' \code{.prsLR_first_sets}, \code{.prsLR_follow_sets}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param seq A vector; its length is taken.
#' @param first A vector; indexed elementwise.
#' @param nts Passed to \code{\%in\%}.
#' @return The value of \code{unique}.
#' @export
.prsLR_first_seq <- function(seq, first, nts) {
  out <- character(0)
  if (length(seq) == 0) return(c(.prsLR_EPSILON))
  for (s in seq) {
    if (!(s %in% nts)) {
      out <- c(out, s)
      return(unique(out))
    }
    out <- union(out, setdiff(first[[s]], .prsLR_EPSILON))
    if (!(.prsLR_EPSILON %in% first[[s]])) {
      return(unique(out))
    }
  }
  out <- c(out, .prsLR_EPSILON)
  unique(out)
}

#' .prsLR_first_sets
#'
#' A step of the prsLR_native implementation. Called by \code{.prsLR_canonical_collection}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param g A list; the body reads \code{$rules} from it.
#' @return The value of \code{first}, as built in the body.
#' @export
.prsLR_first_sets <- function(g) {
  nts   <- .prsLR_nonterminals(g)
  first <- list()
  for (nt in nts) first[[nt]] <- character(0)
  changed <- TRUE
  while (changed) {
    changed <- FALSE
    for (rule in g$rules) {
      lhs    <- rule[[1]]
      rhs    <- rule[[2]]
      add    <- .prsLR_first_seq(rhs, first, nts)
      newset <- unique(c(first[[lhs]], add))
      if (length(newset) != length(first[[lhs]])) {
        first[[lhs]] <- newset
        changed <- TRUE
      }
    }
  }
  first
}

#' .prsLR_follow_sets
#'
#' A step of the prsLR_native implementation. Called by \code{.prsLR_build_tables}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param g A list; the body reads \code{$rules}, \code{$start} from it.
#' @param first Passed to \code{.prsLR_first_seq}.
#' @return The value of \code{follow}, as built in the body.
#' @export
.prsLR_follow_sets <- function(g, first) {
  nts    <- .prsLR_nonterminals(g)
  follow <- list()
  for (nt in nts) follow[[nt]] <- character(0)
  follow[[g$start]] <- .prsLR_END
  changed <- TRUE
  while (changed) {
    changed <- FALSE
    for (rule in g$rules) {
      lhs <- rule[[1]]
      rhs <- rule[[2]]
      n   <- length(rhs)
      for (i in seq_len(n)) {
        B <- rhs[[i]]
        if (!(B %in% nts)) next
        tail <- if (i < n) rhs[(i + 1):n] else list()
        add  <- .prsLR_first_seq(tail, first, nts)
        newset <- unique(c(follow[[B]], setdiff(add, .prsLR_EPSILON)))
        if (.prsLR_EPSILON %in% add) {
          newset <- unique(c(newset, follow[[lhs]]))
        }
        if (length(newset) != length(follow[[B]])) {
          follow[[B]] <- newset
          changed <- TRUE
        }
      }
    }
  }
  follow
}

#' .prsLR_linearise
#'
#' A step of the prsLR_native implementation. Called by \code{morie_prsLR}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param tree A list; the body reads \code{$children}, \code{$symbol} from it.
#' @return A character value.
#' @export
.prsLR_linearise <- function(tree) {
  if (is.null(tree$children)) return(tree$symbol)
  paste(sapply(tree$children, .prsLR_linearise), collapse = " ")
}

# ---- Core LR machinery -------------------------------------------------

#' .prsLR_augment
#'
#' A step of the prsLR_native implementation. Called by \code{.prsLR_build_tables},
#' \code{morie_augment}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param g A list; the body reads \code{$rules}, \code{$start} from it.
#' @return A list with \code{rules}, \code{start}, \code{original_start}.
#' @export
.prsLR_augment <- function(g) {
  tag <- .prsLR_AUG
  nts <- .prsLR_nonterminals(g)
  while (tag %in% nts) tag <- paste0(tag, "'")
  new_rule <- list(list(tag, list(g$start)))
  new_rules <- c(new_rule, g$rules)
  list(rules = new_rules, start = tag, original_start = g$start)
}

#' .prsLR_closure
#'
#' A step of the prsLR_native implementation. Called by
#' \code{.prsLR_canonical_collection}, \code{.prsLR_goto}, \code{morie_closure}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param items Coerced to character by the body, with \code{as.character}.
#' @param ag A list; the body reads \code{$rules} from it.
#' @param first Passed to \code{.prsLR_first_seq}.
#' @param nts Passed to \code{.prsLR_first_seq}.
#' @param k Passed to \code{==}.
#' @return A vector, from \code{sort}.
#' @export
.prsLR_closure <- function(items, ag, first, nts, k) {
  out <- unique(as.character(items))
  changed <- TRUE
  while (changed) {
    changed <- FALSE
    for (it in out) {
      parts <- strsplit(it, ":", fixed = TRUE)[[1]]
      i   <- as.integer(parts[1])
      dot <- as.integer(parts[2])
      rhs <- ag$rules[[i]][[2]]
      if (dot >= length(rhs) || !(rhs[[dot + 1]] %in% nts)) next
      B <- rhs[[dot + 1]]
      if (k == 0) {
        looks <- NA_character_
      } else {
        look <- parts[3]
        tail <- if (dot + 1 < length(rhs)) rhs[(dot + 2):length(rhs)] else list()
        fs   <- .prsLR_first_seq(tail, first, nts)
        looks <- setdiff(fs, .prsLR_EPSILON)
        if (.prsLR_EPSILON %in% fs || length(tail) == 0) {
          looks <- union(looks, look)
        }
      }
      for (j in seq_along(ag$rules)) {
        if (ag$rules[[j]][[1]] != B) next
        if (k == 0) {
          new <- paste0(j, ":0")
          if (!(new %in% out)) {
            out <- c(out, new)
            changed <- TRUE
          }
        } else {
          for (b in looks) {
            new <- paste0(j, ":0:", b)
            if (!(new %in% out)) {
              out <- c(out, new)
              changed <- TRUE
            }
          }
        }
      }
    }
  }
  sort(unique(out))
}

#' .prsLR_goto
#'
#' A step of the prsLR_native implementation. Called by
#' \code{.prsLR_canonical_collection}, \code{morie_goto}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param state See Usage.
#' @param sym Passed to \code{==}.
#' @param ag A list; the body reads \code{$rules} from it.
#' @param first Passed to \code{.prsLR_closure}.
#' @param nts Passed to \code{.prsLR_closure}.
#' @param k Passed to \code{.prsLR_closure}.
#' @return The value of \code{.prsLR_closure}.
#' @export
.prsLR_goto <- function(state, sym, ag, first, nts, k) {
  moved <- character(0)
  for (it in state) {
    parts <- strsplit(it, ":", fixed = TRUE)[[1]]
    i   <- as.integer(parts[1])
    dot <- as.integer(parts[2])
    rhs <- ag$rules[[i]][[2]]
    if (dot < length(rhs) && rhs[[dot + 1]] == sym) {
      if (k == 0) {
        moved <- c(moved, paste0(i, ":", dot + 1))
      } else {
        look <- parts[3]
        moved <- c(moved, paste0(i, ":", dot + 1, ":", look))
      }
    }
  }
  if (length(moved) == 0) return(character(0))
  .prsLR_closure(moved, ag, first, nts, k)
}

#' .prsLR_core
#'
#' A step of the prsLR_native implementation. Called by \code{.prsLR_build_tables}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param state See Usage.
#' @return A vector, from \code{sort}.
#' @export
.prsLR_core <- function(state) {
  cores <- character(0)
  for (it in state) {
    parts <- strsplit(it, ":", fixed = TRUE)[[1]]
    cores <- c(cores, paste(parts[1], parts[2], sep = ":"))
  }
  sort(unique(cores))
}

#' .prsLR_canonical_collection
#'
#' A step of the prsLR_native implementation. Called by \code{.prsLR_build_tables},
#' \code{morie_canonical_collection}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param ag A list; the body reads \code{$rules}, \code{$start} from it.
#' @param k Passed to \code{.prsLR_closure}.
#' @return A list with \code{states}, \code{index}, \code{transitions}, \code{first},
#' \code{nonterminals}.
#' @export
.prsLR_canonical_collection <- function(ag, k) {
  g0 <- list(rules = ag$rules, start = ag$start)
  first <- .prsLR_first_sets(g0)
  nts <- .prsLR_nonterminals(g0)
  syms <- c(nts, .prsLR_terminals(g0))
  start_item <- if (k == 0) "1:0" else paste0("1:0:", .prsLR_END)
  I0 <- .prsLR_closure(start_item, ag, first, nts, k)
  states <- list(I0)
  index <- list()
  index[[paste(I0, collapse = "|")]] <- 0
  trans <- list()
  q <- list(I0)
  while (length(q) > 0) {
    I <- q[[1]]
    q <- q[-1]
    I_idx <- index[[paste(I, collapse = "|")]]
    for (X in syms) {
      J <- .prsLR_goto(I, X, ag, first, nts, k)
      if (length(J) == 0) next
      J_key <- paste(J, collapse = "|")
      if (is.null(index[[J_key]])) {
        index[[J_key]] <- length(states)
        states <- c(states, list(J))
        q <- c(q, list(J))
      }
      trans_key <- paste(I_idx, X, sep = ":")
      trans[[trans_key]] <- index[[J_key]]
    }
  }
  list(states = states, index = index, transitions = trans,
       first = first, nonterminals = nts)
}

#' .prsLR_build_tables
#'
#' A step of the prsLR_native implementation. Called by \code{.prsLR_parse},
#' \code{morie_build_tables}, \code{morie_conflicts} and 1 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param g Passed to \code{.prsLR_augment}.
#' @param method One of \code{"lalr1"}, \code{"slr1"}.
#' @return A list with \code{action}, \code{goto}, \code{states}, \code{n_states},
#' \code{conflicts}, \code{rules}, \code{augmented}, \code{method}.
#' @export
.prsLR_build_tables <- function(g, method) {
  if (!(method %in% .prsLR_METHODS)) {
    stop(sprintf("prsLR: method must be one of %s, got %s",
                 paste(.prsLR_METHODS, collapse = ", "), method))
  }
  ag <- .prsLR_augment(g)
  k <- if (method == "slr1") 0 else 1
  col <- .prsLR_canonical_collection(ag, k)
  states <- col$states
  trans_raw <- col$transitions
  trans <- list()
  for (key in names(trans_raw)) {
    parts <- strsplit(key, ":", fixed = TRUE)[[1]]
    s <- as.integer(parts[1])
    X <- parts[2]
    new_key <- paste(s, X, sep = ":")
    trans[[new_key]] <- trans_raw[[key]]
  }
  nts <- col$nonterminals
  follow <- if (method == "slr1") {
    .prsLR_follow_sets(list(rules = ag$rules, start = ag$start), col$first)
  } else NULL

  if (method == "lalr1") {
    groups <- list()
    for (n in seq_along(states)) {
      core <- paste(.prsLR_core(states[[n]]), collapse = "|")
      if (is.null(groups[[core]])) groups[[core]] <- integer(0)
      groups[[core]] <- c(groups[[core]], n - 1)
    }
    remap <- integer(0)
    merged <- list()
    for (core in names(groups)) {
      members <- groups[[core]]
      new <- length(merged)
      union_items <- character(0)
      for (n in members) {
        remap <- c(remap, setNames(new, as.character(n)))
        union_items <- c(union_items, states[[n + 1]])
      }
      merged <- c(merged, list(sort(unique(union_items))))
    }
    states <- merged
    new_trans <- list()
    for (key in names(trans)) {
      parts <- strsplit(key, ":", fixed = TRUE)[[1]]
      s <- as.integer(parts[1])
      X <- parts[2]
      t <- trans[[key]]
      new_s <- remap[as.character(s)]
      new_t <- remap[as.character(t)]
      new_key <- paste(new_s, X, sep = ":")
      new_trans[[new_key]] <- new_t
    }
    trans <- new_trans
  }

  action <- list()
  gotos <- list()
  confl <- list()

  put_action <- function(s, a, act) {
    key <- paste(s, a, sep = ":")
    if (!is.null(action[[key]])) {
      existing <- action[[key]]
      if (!identical(existing, act)) {
        kind <- if ("shift" %in% c(existing[1], act[1])) "shift/reduce" else "reduce/reduce"
        confl[[length(confl) + 1]] <<- list(
          state = s, lookahead = a,
          existing = existing, proposed = act, kind = kind
        )
      }
    } else {
      action[[key]] <<- act
    }
  }

  for (key in names(trans)) {
    parts <- strsplit(key, ":", fixed = TRUE)[[1]]
    s <- as.integer(parts[1])
    X <- parts[2]
    t <- trans[[key]]
    if (X %in% nts) {
      gotos[[key]] <- t
    } else {
      put_action(s, X, c("shift", as.character(t)))
    }
  }
  for (s_idx in seq_along(states)) {
    s <- s_idx - 1
    st <- states[[s_idx]]
    for (it in st) {
      parts <- strsplit(it, ":", fixed = TRUE)[[1]]
      i <- as.integer(parts[1])
      dot <- as.integer(parts[2])
      rule <- ag$rules[[i]]
      lhs <- rule[[1]]
      rhs <- rule[[2]]
      if (dot != length(rhs)) next
      if (i == 1) {
        put_action(s, .prsLR_END, c("accept", NA_character_))
        next
      }
      if (method == "slr1") {
        looks <- follow[[lhs]]
      } else {
        looks <- parts[3]
      }
      for (a in looks) {
        put_action(s, a, c("reduce", as.character(i)))
      }
    }
  }
  list(action = action, goto = gotos, states = states,
       n_states = length(states), conflicts = confl,
       rules = ag$rules, augmented = ag, method = method)
}

#' .prsLR_leaf
#'
#' A step of the prsLR_native implementation. Called by \code{.prsLR_parse}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param sym Carried through into a list the body builds.
#' @return A list with \code{symbol}, \code{children}.
#' @export
.prsLR_leaf <- function(sym) {
  list(symbol = sym, children = NULL)
}

#' .prsLR_parse
#'
#' A step of the prsLR_native implementation. Called by \code{morie_parse}, \code{morie_prsLR}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param g Passed to \code{.prsLR_build_tables}.
#' @param tokens Coerced to character by the body, with \code{as.character}.
#' @param method Passed to \code{.prsLR_build_tables}.
#' @param tables Optional; may be \code{NULL}. Passed to \code{is.null}.
#' @return Nothing; this branch always raises.
#' @export
.prsLR_parse <- function(g, tokens, method, tables) {
  t <- if (!is.null(tables)) tables else .prsLR_build_tables(g, method)
  if (length(t$conflicts) > 0) {
    c <- t$conflicts[[1]]
    stop(sprintf("prsLR: the grammar is not %s -- %d conflict(s), first a %s in state %d on %s",
                 t$method, length(t$conflicts), c$kind, c$state, c$lookahead))
  }
  toks <- c(as.character(tokens), .prsLR_END)
  stack <- c(0L)
  trees <- list()
  pos <- 1L
  for (iter in seq_len(100000)) {
    if (pos > length(toks)) break
    a <- toks[pos]
    key <- paste(stack[length(stack)], a, sep = ":")
    act <- t$action[[key]]
    if (is.null(act)) {
      stop(sprintf("prsLR: syntax error at token %d (%s) in state %d",
                   pos - 1L, a, stack[length(stack)]))
    }
    op <- act[1]
    arg <- act[2]
    if (op == "shift") {
      stack <- c(stack, as.integer(arg))
      trees <- c(trees, list(.prsLR_leaf(a)))
      pos <- pos + 1L
    } else if (op == "reduce") {
      i <- as.integer(arg)
      rule <- t$rules[[i]]
      lhs <- rule[[1]]
      rhs <- rule[[2]]
      n <- length(rhs)
      kids <- list()
      for (j in seq_len(n)) {
        stack <- stack[-length(stack)]
        kids <- c(kids, list(trees[[length(trees)]]))
        trees <- trees[-length(trees)]
      }
      kids <- rev(kids)
      node <- list(symbol = lhs, children = kids)
      goto_key <- paste(stack[length(stack)], lhs, sep = ":")
      nxt <- t$goto[[goto_key]]
      if (is.null(nxt)) {
        stop(sprintf("prsLR: no goto for %s in state %d", lhs, stack[length(stack)]))
      }
      stack <- c(stack, nxt)
      trees <- c(trees, list(node))
    } else {
      if (length(trees) != 1 || pos != length(toks)) {
        stop(sprintf("prsLR: accepted with %d trees and %d tokens left",
                     length(trees), length(toks) - pos))
      }
      return(trees[[1]])
    }
  }
  stop("prsLR: the parser did not terminate")
}

# ---- Public API --------------------------------------------------------

#' morie_augment
#'
#' A step of the prsLR_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param g Passed to \code{.prsLR_augment}.
#' @return The value of \code{.prsLR_augment}.
#' @export
morie_augment <- function(g) {
  .prsLR_augment(g)
}

#' morie_closure
#'
#' A step of the prsLR_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param items Passed to \code{.prsLR_closure}.
#' @param ag Passed to \code{.prsLR_closure}.
#' @param first Passed to \code{.prsLR_closure}.
#' @param nts Passed to \code{.prsLR_closure}.
#' @param k Passed to \code{.prsLR_closure}. Defaults to \code{1}.
#' @return The value of \code{.prsLR_closure}.
#' @export
morie_closure <- function(items, ag, first, nts, k = 1) {
  .prsLR_closure(items, ag, first, nts, k)
}

#' morie_goto
#'
#' A step of the prsLR_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param state Passed to \code{.prsLR_goto}.
#' @param sym Passed to \code{.prsLR_goto}.
#' @param ag Passed to \code{.prsLR_goto}.
#' @param first Passed to \code{.prsLR_goto}.
#' @param nts Passed to \code{.prsLR_goto}.
#' @param k Passed to \code{.prsLR_goto}. Defaults to \code{1}.
#' @return The value of \code{.prsLR_goto}.
#' @export
morie_goto <- function(state, sym, ag, first, nts, k = 1) {
  .prsLR_goto(state, sym, ag, first, nts, k)
}

#' morie_canonical_collection
#'
#' A step of the prsLR_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param ag Passed to \code{.prsLR_canonical_collection}.
#' @param k Passed to \code{.prsLR_canonical_collection}. Defaults to \code{1}.
#' @return The value of \code{.prsLR_canonical_collection}.
#' @export
morie_canonical_collection <- function(ag, k = 1) {
  .prsLR_canonical_collection(ag, k)
}

#' morie_build_tables
#'
#' A step of the prsLR_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param g Passed to \code{.prsLR_build_tables}.
#' @param method Passed to \code{.prsLR_build_tables}. Defaults to \code{"lr1"}.
#' @return The value of \code{.prsLR_build_tables}.
#' @export
morie_build_tables <- function(g, method = "lr1") {
  .prsLR_build_tables(g, method)
}

#' morie_conflicts
#'
#' A step of the prsLR_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param g Passed to \code{.prsLR_build_tables}.
#' @param method Passed to \code{.prsLR_build_tables}. Defaults to \code{"lr1"}.
#' @return A list with \code{estimate}, \code{conflicts}, \code{n_conflicts},
#' \code{method}, \code{n_states}, \code{ok}.
#' @export
morie_conflicts <- function(g, method = "lr1") {
  t <- .prsLR_build_tables(g, method)
  list(
    estimate = t$conflicts,
    conflicts = t$conflicts,
    n_conflicts = length(t$conflicts),
    method = method,
    n_states = t$n_states,
    ok = length(t$conflicts) == 0
  )
}

#' morie_parse
#'
#' A step of the prsLR_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param g Passed to \code{.prsLR_parse}.
#' @param tokens Passed to \code{.prsLR_parse}.
#' @param method Passed to \code{.prsLR_parse}. Defaults to \code{"lr1"}.
#' @param tables Passed to \code{.prsLR_parse}.
#' @return The value of \code{.prsLR_parse}.
#' @export
morie_parse <- function(g, tokens, method = "lr1", tables = NULL) {
  .prsLR_parse(g, tokens, method, tables)
}

#' morie_prsLR
#'
#' A step of the prsLR_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param grammar_ Passed to \code{.prsLR_grammar}.
#' @param tokens Passed to \code{.prsLR_parse}.
#' @param method Passed to \code{.prsLR_build_tables}. Defaults to \code{"lr1"}.
#' @return A list with \code{estimate}, \code{tree}, \code{method}, \code{n_states},
#' \code{conflicts}, \code{tokens}, \code{yield}.
#' @export
morie_prsLR <- function(grammar_, tokens, method = "lr1") {
  g <- .prsLR_grammar(grammar_)
  t <- .prsLR_build_tables(g, method)
  tree <- .prsLR_parse(g, tokens, method, t)
  list(
    estimate = tree,
    tree = tree,
    method = method,
    n_states = t$n_states,
    conflicts = t$conflicts,
    tokens = as.character(tokens),
    yield = .prsLR_linearise(tree)
  )
}
