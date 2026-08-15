```r
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

.prsLR_grammar <- function(g) {
  if (is.list(g) && !is.null(g$rules) && !is.null(g$start)) return(g)
  stop("prsLR: grammar must be a list with $rules and $start")
}

.prsLR_nonterminals <- function(g) {
  unique(sapply(g$rules, function(r) r[[1]]))
}

.prsLR_terminals <- function(g) {
  nt_set <- .prsLR_nonterminals(g)
  terms  <- unique(unlist(lapply(g$rules, function(r) r[[2]])))
  setdiff(terms, nt_set)
}

.prsLR_first_seq <- function(seq, first, nts) {
  out <- character(0)
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

.prsLR_linearise <- function(tree) {
  if (is.null(tree$children)) return(tree$symbol)
  paste(sapply(tree$children, .prsLR_linearise), collapse = " ")
}

# ---- Core LR machinery -------------------------------------------------

.prsLR_augment <- function(g) {
  tag <- .prsLR_AUG
  nts <- .prsLR_nonterminals(g)
  while (tag %in% nts) tag <- paste0(tag, "'")
  new_rule <- list(list(tag, list(g$start)))
  new_rules <- c(new_rule, g$rules)
  list(rules = new_rules, start = tag, original_start = g$start)
}

.prsLR_closure <- function(items, ag, first, nts, k) {
  out <- as.character(items)
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
        looks <- list(NA_character_)
      } else {
        look <- parts[3]
        tail <- if (dot + 1 < length(rhs)) rhs[(dot + 2):length(rhs)] else list()
        fs   <- .prsLR_first_seq(tail, first, nts)
        looks_vec <- setdiff(fs, .prsLR_EPSILON)
        if (.prsLR_EPSILON %in% fs || length(tail) == 0) {
          looks_vec <- union(looks_vec, look)
        }
        looks <- as.list(looks_vec)
      }
      for (j in seq_along(ag$rules)) {
        if (ag$rules[[j]][[1]] != B) next
        for (b in looks) {
          if (k == 0) {
            new <- paste0(j, ":", 0)
          } else {
            new <- paste0(j, ":", 0, ":", b)
          }
          if (!(new %in% out)) {
            out <- c(out, new)
            changed <- TRUE
          }
        }
      }
    }
  }
  unique(out)
}

.prsLR_goto <- function(state, sym, ag, first, nts, k) {
  moved <- character(0)
  for (it in state) {
    parts <- strsplit(it, ":", fixed = TRUE)[[1]]
    i   <- as.integer(parts[1])
    dot <- as.integer(parts[2])
    rhs <- ag$rules[[i]][[2]]
    if (dot < length(rhs) && rhs[[dot + 1]] == sym) {
      if (k == 0) {
        new <- paste0(i, ":", dot + 1)
      } else {
        look <- parts[3]
        new  <- paste0(i, ":", dot + 1, ":", look)
      }
      moved <- c(moved, new)
    }
  }
  if (length(moved) == 0) return(character(0))
  .prsLR_closure(unique(moved), ag, first, nts, k)
}

.prsLR_core <- function(state) {
  s
