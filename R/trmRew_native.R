# morie.fn -- function file (rootcoder007/morie)
# Term rewriting: normal forms, confluence, and completion.
#
# Rewriting. A rule l -> r applies to a term when some subterm matches
# l -- matching, not unification: the rule's variables may be bound,
# the term's may not. Replace that subterm by r under the same binding
# and repeat. A term with no applicable rule is in normal form.
#
# Two things can go wrong, independently. Termination: f(x) -> f(f(x))
# never stops; a reduction order (the lexicographic path order, LPO)
# proves termination when every rule strictly decreases. Confluence:
# a -> b and a -> c both apply and never meet again, so the normal
# form depends on which rule was picked.
#
# Non-confluence hides where two left-hand sides overlap. Superimpose
# l2 on a non-variable subterm of l1 by unification; the two ways of
# rewriting the overlap give a critical pair. Knuth & Bendix's
# Critical Pair Lemma: a system is locally confluent exactly when
# every critical pair is joinable. Newman's lemma upgrades local
# confluence to confluence for terminating systems.
#
# Completion. When a critical pair is not joinable, orient it into a
# new rule with the reduction order and add it; its own critical pairs
# join the queue. On success the result is convergent -- terminating
# and confluent. The procedure may loop forever or stop unable to
# orient a pair (commutativity cannot be oriented); both outcomes are
# reported.
#
# Terms are those of morie.fn.unifAlg.
#
# References
# ----------
# Knuth, D. E. & Bendix, P. B. (1970) "Simple word problems in
# universal algebras", in J. Leech (ed.) Computational Problems in
# Abstract Algebra, Pergamon Press, 263-297,
# doi:10.1016/B978-0-08-012975-4.50028-X.
#
# Newman, M. H. A. (1942) "On theories with a combinatorial definition
# of 'equivalence'", Annals of Mathematics 43(2), 223-243,
# doi:10.2307/1968867.
#
# Baader, F. & Nipkow, T. (1998) Term Rewriting and All That,
# Cambridge University Press, ISBN 978-0-521-77920-3. Chs. 2, 5, 7.

.trmRew_STRATEGIES <- c("innermost", "outermost")

.trmRew_is_var <- function(t) morie_unifAlg_is_var(t)

.trmRew_app <- function(symbol, args) {
  do.call(morie_unifAlg_app, c(list(symbol), args))
}

#' A rewrite rule, checked for the two conditions rules need
#'
#' Part of the trmRew_native implementation; see the file header for the
#' source it follows.
#'
#' @param lhs See Usage.
#' @param rhs See Usage.
#' @return The value of \code{list}.
#' @export
morie_trmRew_rule <- function(lhs, rhs) {
  # A rewrite rule, checked for the two conditions rules need.
  if (.trmRew_is_var(lhs)) {
    stop(paste0("trmRew: a rule cannot have a bare variable on the ",
                "left -- it would match everything"))
  }
  extra <- setdiff(morie_unifAlg_variables(rhs),
                   morie_unifAlg_variables(lhs))
  if (length(extra) > 0L) {
    stop(sprintf(paste0("trmRew: the right-hand side introduces the ",
                        "unbound variable(s) %s"),
                 paste(sort(extra), collapse=", ")))
  }
  list(lhs, rhs)
}

#' Every position in a term, as a 0-based integer vector of argument
#'
#' indices. The empty position is integer(0).
#'
#' @param t See Usage.
#' @return The value of \code{out}, as built in the body.
#' @export
morie_trmRew_positions <- function(t) {
  # Every position in a term, as a 0-based integer vector of argument
  # indices. The empty position is integer(0).
  out <- list(integer(0))
  if (!.trmRew_is_var(t)) {
    args <- t[[3]]
    for (i in seq_along(args)) {
      for (p in morie_trmRew_positions(args[[i]])) {
        out <- c(out, list(c(i - 1L, p)))
      }
    }
  }
  out
}

#' The subterm at a position
#'
#' Part of the trmRew_native implementation; see the file header for the
#' source it follows.
#'
#' @param t See Usage.
#' @param pos See Usage.
#' @return The value of \code{cur}, as built in the body.
#' @export
morie_trmRew_subterm_at <- function(t, pos) {
  # The subterm at a position.
  cur <- t
  for (i in pos) {
    if (.trmRew_is_var(cur) || i >= length(cur[[3]])) {
      stop(sprintf("trmRew: position does not exist in the term"))
    }
    cur <- cur[[3]][[i + 1L]]
  }
  cur
}

#' The term with the subterm at pos replaced
#'
#' Part of the trmRew_native implementation; see the file header for the
#' source it follows.
#'
#' @param t See Usage.
#' @param pos See Usage.
#' @param new See Usage.
#' @return The value of \code{.trmRew_app}.
#' @export
morie_trmRew_replace_at <- function(t, pos, new) {
  # The term with the subterm at pos replaced.
  if (length(pos) == 0L) {
    return(new)
  }
  if (.trmRew_is_var(t) || pos[1L] >= length(t[[3]])) {
    stop("trmRew: position does not exist in the term")
  }
  args <- t[[3]]
  args[[pos[1L] + 1L]] <- morie_trmRew_replace_at(args[[pos[1L] + 1L]],
                                                  pos[-1L], new)
  .trmRew_app(t[[2]], args)
}

#' One rewrite, or NULL when the term is in normal form. Innermost
#'
#' reduces arguments before the term above them; outermost the other way
#' round.
#'
#' @param t See Usage.
#' @param rules See Usage.
#' @param strategy Defaults to \code{"innermost"}.
#' @return Nothing; the function is called for its effect.
#' @export
morie_trmRew_rewrite_step <- function(t, rules, strategy="innermost") {
  # One rewrite, or NULL when the term is in normal form. Innermost
  # reduces arguments before the term above them; outermost the other
  # way round.
  if (!(strategy %in% .trmRew_STRATEGIES)) {
    stop(sprintf("trmRew: strategy must be one of %s, got %s",
                 paste(.trmRew_STRATEGIES, collapse=", "), strategy))
  }
  pos <- morie_trmRew_positions(t)
  lens <- vapply(pos, length, integer(1))
  ord <- if (strategy == "innermost") {
    order(-lens, seq_along(pos))
  } else {
    order(lens, seq_along(pos))
  }
  pos <- pos[ord]
  for (p in pos) {
    s <- morie_trmRew_subterm_at(t, p)
    if (.trmRew_is_var(s)) {
      next
    }
    for (i in seq_along(rules)) {
      l <- rules[[i]][[1L]]
      r <- rules[[i]][[2L]]
      m <- morie_unifAlg_match(l, s)
      if (!is.null(m)) {
        return(list(term=morie_trmRew_replace_at(t, p,
                                                 morie_unifAlg_substitute(r, m)),
                    position=p, rule=i - 1L, binding=m))
      }
    }
  }
  NULL
}

#' morie_trmRew_normal_form
#'
#' Part of the trmRew_native implementation; see the file header for the
#' source it follows.
#'
#' @param t See Usage.
#' @param rules See Usage.
#' @param strategy Defaults to \code{"innermost"}.
#' @param max_steps Defaults to \code{10000}.
#' @return Nothing; this branch always raises.
#' @export
morie_trmRew_normal_form <- function(t, rules, strategy="innermost",
                                     max_steps=10000) {
  # Rewrite to exhaustion. Raises if the step budget runs out.
  cur <- t
  trace <- list()
  for (k in seq_len(as.integer(max_steps))) {
    st <- morie_trmRew_rewrite_step(cur, rules, strategy)
    if (is.null(st)) {
      return(list(normal_form=cur, steps=length(trace), trace=trace))
    }
    trace <- c(trace, list(list(st$rule, st$position)))
    cur <- st$term
  }
  stop(sprintf(paste0("trmRew: no normal form after %d steps -- the ",
                      "system does not terminate on this term"),
               as.integer(max_steps)))
}

.trmRew_prec <- function(precedence, sym) {
  if (!is.null(precedence) && sym %in% names(precedence)) {
    as.numeric(precedence[[sym]])
  } else {
    0
  }
}

#' The lexicographic path order, s >_lpo t
#'
#' Part of the trmRew_native implementation; see the file header for the
#' source it follows.
#'
#' @param s See Usage.
#' @param t See Usage.
#' @param precedence See Usage.
#' @return A logical value.
#' @export
morie_trmRew_lpo_greater <- function(s, t, precedence) {
  # The lexicographic path order, s >_lpo t.
  if (identical(s, t)) {
    return(FALSE)
  }
  if (.trmRew_is_var(t)) {
    return(!.trmRew_is_var(s) && (t[[2]] %in% morie_unifAlg_variables(s)))
  }
  if (.trmRew_is_var(s)) {
    return(FALSE)
  }
  for (a in s[[3]]) {
    if (identical(a, t) || morie_trmRew_lpo_greater(a, t, precedence)) {
      return(TRUE)
    }
  }
  ps <- .trmRew_prec(precedence, s[[2]])
  pt <- .trmRew_prec(precedence, t[[2]])
  if (ps > pt) {
    return(all(vapply(t[[3]],
                      function(b) morie_trmRew_lpo_greater(s, b, precedence),
                      logical(1))))
  }
  if (ps < pt) {
    return(FALSE)
  }
  if (length(s[[3]]) != length(t[[3]])) {
    return(length(s[[3]]) > length(t[[3]]))
  }
  sa <- s[[3]]
  ta <- t[[3]]
  for (idx in seq_along(sa)) {
    a <- sa[[idx]]
    b <- ta[[idx]]
    if (identical(a, b)) {
      next
    }
    return(morie_trmRew_lpo_greater(a, b, precedence) &&
           all(vapply(ta,
                      function(c) morie_trmRew_lpo_greater(s, c, precedence),
                      logical(1))))
  }
  FALSE
}

#' Whether every rule strictly decreases in the LPO. Sufficient, not
#'
#' necessary. Returns 0-based indices of unoriented rules.
#'
#' @param rules See Usage.
#' @param precedence See Usage.
#' @return A list with \code{terminating}, \code{unoriented}, \code{method}.
#' @export
morie_trmRew_is_terminating <- function(rules, precedence) {
  # Whether every rule strictly decreases in the LPO. Sufficient, not
  # necessary. Returns 0-based indices of unoriented rules.
  bad <- integer(0)
  for (i in seq_along(rules)) {
    if (!morie_trmRew_lpo_greater(rules[[i]][[1L]], rules[[i]][[2L]],
                                  precedence)) {
      bad <- c(bad, i - 1L)
    }
  }
  list(terminating=(length(bad) == 0L), unoriented=bad,
       method=paste0("lexicographic path order (Baader & Nipkow 1998 ",
                     "Ch. 5); sufficient, not necessary"))
}

.trmRew_rename <- function(t, tag) {
  if (.trmRew_is_var(t)) {
    return(morie_unifAlg_var(paste0(t[[2]], tag)))
  }
  .trmRew_app(t[[2]], lapply(t[[3]], function(a) .trmRew_rename(a, tag)))
}

.trmRew_overlap <- function(ra, rb, same) {
  # Superpose rb's left-hand side on ra's, at every non-variable
  # position, and rewrite the overlap both ways.
  l1 <- ra[[1L]]
  r1 <- ra[[2L]]
  L2 <- .trmRew_rename(rb[[1L]], "#2")
  R2 <- .trmRew_rename(rb[[2L]], "#2")
  out <- list()
  for (p in morie_trmRew_positions(l1)) {
    if (same && length(p) == 0L) {
      next
    }
    sub_t <- morie_trmRew_subterm_at(l1, p)
    if (.trmRew_is_var(sub_t)) {
      next
    }
    u <- morie_unifAlg_unify(sub_t, L2)
    if (!isTRUE(u$unified)) {
      next
    }
    sig <- u$mgu
    a <- morie_unifAlg_apply_subst(r1, sig)
    b <- morie_unifAlg_apply_subst(morie_trmRew_replace_at(l1, p, R2), sig)
    if (!identical(a, b)) {
      out <- c(out, list(list(left=a, right=b, position=p)))
    }
  }
  out
}

#' Every overlap between two left-hand sides
#'
#' Part of the trmRew_native implementation; see the file header for the
#' source it follows.
#'
#' @param rules See Usage.
#' @return The value of \code{out}, as built in the body.
#' @export
morie_trmRew_critical_pairs <- function(rules) {
  # Every overlap between two left-hand sides.
  out <- list()
  for (i in seq_along(rules)) {
    for (j in seq_along(rules)) {
      for (c in .trmRew_overlap(rules[[i]], rules[[j]], i == j)) {
        c$rules <- c(i - 1L, j - 1L)
        out <- c(out, list(c))
      }
    }
  }
  out
}

#' Whether two terms reach a common normal form
#'
#' Part of the trmRew_native implementation; see the file header for the
#' source it follows.
#'
#' @param a See Usage.
#' @param b See Usage.
#' @param rules See Usage.
#' @param max_steps Defaults to \code{10000}.
#' @return The value of \code{res}, as built in the body.
#' @export
morie_trmRew_joinable <- function(a, b, rules, max_steps=10000) {
  # Whether two terms reach a common normal form.
  res <- tryCatch({
    na <- morie_trmRew_normal_form(a, rules, max_steps=max_steps)$normal_form
    nb <- morie_trmRew_normal_form(b, rules, max_steps=max_steps)$normal_form
    identical(na, nb)
  }, error=function(e) FALSE)
  res
}

#' The Critical Pair Lemma, applied
#'
#' Part of the trmRew_native implementation; see the file header for the
#' source it follows.
#'
#' @param rules See Usage.
#' @param max_steps Defaults to \code{10000}.
#' @return A list with \code{estimate}, \code{locally_confluent}, \code{n_critical_pairs}, \code{unjoinable}, \code{method}.
#' @export
morie_trmRew_is_locally_confluent <- function(rules, max_steps=10000) {
  # The Critical Pair Lemma, applied.
  cps <- morie_trmRew_critical_pairs(rules)
  bad <- list()
  for (c in cps) {
    if (!morie_trmRew_joinable(c$left, c$right, rules, max_steps)) {
      bad <- c(bad, list(c))
    }
  }
  list(estimate=(length(bad) == 0L), locally_confluent=(length(bad) == 0L),
       n_critical_pairs=length(cps), unjoinable=bad,
       method="Knuth & Bendix (1970) Critical Pair Lemma")
}

#' Confluence via Newman\'s lemma: terminating and locally confluent
#'
#' Part of the trmRew_native implementation; see the file header for the
#' source it follows.
#'
#' @param rules See Usage.
#' @param precedence See Usage.
#' @param max_steps Defaults to \code{10000}.
#' @return A list with \code{estimate}, \code{confluent}, \code{terminating}, \code{locally_confluent}, \code{n_critical_pairs}, \code{unjoinable}, \code{method}.
#' @export
morie_trmRew_is_confluent <- function(rules, precedence, max_steps=10000) {
  # Confluence via Newman's lemma: terminating and locally confluent.
  term <- morie_trmRew_is_terminating(rules, precedence)
  lc <- morie_trmRew_is_locally_confluent(rules, max_steps)
  yes <- term$terminating && lc$locally_confluent
  list(estimate=yes, confluent=yes, terminating=term$terminating,
       locally_confluent=lc$locally_confluent,
       n_critical_pairs=lc$n_critical_pairs, unjoinable=lc$unjoinable,
       method=paste0("Newman (1942): terminating + locally confluent ",
                     "implies confluent"))
}

.trmRew_incomplete <- function(rules, why, pair) {
  list(estimate=NULL, rules=rules, complete=FALSE, reason=why, pair=pair,
       n_rules=length(rules),
       method=paste0("Knuth & Bendix (1970) completion, oriented by the ",
                     "lexicographic path order"))
}

.trmRew_canonical <- function(rules) {
  # Rename variables to x0, x1, ... so completion output does not carry
  # the bookkeeping suffixes renaming apart introduced.
  out <- list()
  for (rl in rules) {
    l <- rl[[1L]]
    r <- rl[[2L]]
    names_ <- morie_unifAlg_variables(l)
    sub <- list()
    for (i in seq_along(names_)) {
      sub[[names_[i]]] <- morie_unifAlg_var(sprintf("x%d", i - 1L))
    }
    out <- c(out, list(list(morie_unifAlg_substitute(l, sub),
                            morie_unifAlg_substitute(r, sub))))
  }
  out
}

.trmRew_interreduce <- function(rules, precedence) {
  out <- rules
  changed <- TRUE
  while (changed) {
    changed <- FALSE
    for (i in seq_along(out)) {
      rest <- out[-i]
      l <- out[[i]][[1L]]
      r <- out[[i]][[2L]]
      nr <- if (length(rest) > 0L) {
        morie_trmRew_normal_form(r, rest)$normal_form
      } else {
        r
      }
      if (length(rest) > 0L &&
          !is.null(morie_trmRew_rewrite_step(l, rest))) {
        out <- rest
        changed <- TRUE
        break
      }
      if (!identical(nr, r)) {
        out[[i]] <- list(l, nr)
        changed <- TRUE
        break
      }
    }
  }
  .trmRew_canonical(out)
}

#' morie_trmRew_complete
#'
#' Part of the trmRew_native implementation; see the file header for the
#' source it follows.
#'
#' @param equations See Usage.
#' @param precedence See Usage.
#' @param max_rules Defaults to \code{60}.
#' @param max_steps Defaults to \code{10000}.
#' @param max_iter Defaults to \code{4000}.
#' @return The value of \code{.trmRew_incomplete}.
#' @export
morie_trmRew_complete <- function(equations, precedence, max_rules=60,
                                  max_steps=10000, max_iter=4000) {
  # Knuth-Bendix completion of a set of equations (Huet's form: rules
  # are kept interreduced and only a new rule's critical pairs are
  # queued). Reports success, an unorientable pair, or exhaustion of
  # the rule budget.
  rules <- list()
  queue <- lapply(equations, function(eq) list(eq[[1L]], eq[[2L]]))
  for (it in seq_len(as.integer(max_iter))) {
    if (length(queue) == 0L) {
      rules <- .trmRew_interreduce(rules, precedence)
      return(list(estimate=rules, rules=rules, complete=TRUE, reason=NULL,
                  n_rules=length(rules),
                  method=paste0("Knuth & Bendix (1970) completion, ",
                                "oriented by the lexicographic path order")))
    }
    pr <- queue[[1L]]
    queue <- queue[-1L]
    s_ <- morie_trmRew_normal_form(pr[[1L]], rules,
                                   max_steps=max_steps)$normal_form
    t_ <- morie_trmRew_normal_form(pr[[2L]], rules,
                                   max_steps=max_steps)$normal_form
    if (identical(s_, t_)) {
      next
    }
    if (morie_trmRew_lpo_greater(s_, t_, precedence)) {
      new <- morie_trmRew_rule(s_, t_)
    } else if (morie_trmRew_lpo_greater(t_, s_, precedence)) {
      new <- morie_trmRew_rule(t_, s_)
    } else {
      return(.trmRew_incomplete(rules,
                                if (length(rules) == 0L) {
                                  "unorientable equation"
                                } else {
                                  "unorientable critical pair"
                                },
                                list(s_, t_)))
    }
    # Collapse: a rule whose left-hand side the new rule can rewrite is
    # no longer a rule; it goes back into the queue.
    keep <- list()
    for (rl in rules) {
      l <- rl[[1L]]
      r <- rl[[2L]]
      if (!is.null(morie_trmRew_rewrite_step(l, list(new)))) {
        queue <- c(queue, list(list(l, r)))
      } else {
        keep <- c(keep, list(list(l,
                                  morie_trmRew_normal_form(r, list(new),
                                                           max_steps=max_steps)$normal_form)))
      }
    }
    rules <- c(keep, list(new))
    if (length(rules) > as.integer(max_rules)) {
      return(.trmRew_incomplete(rules, "rule budget exhausted", NULL))
    }
    new_idx <- length(rules)
    for (oi in seq_along(rules)) {
      other <- rules[[oi]]
      for (c in .trmRew_overlap(new, other, oi == new_idx)) {
        queue <- c(queue, list(list(c$left, c$right)))
      }
      if (oi != new_idx) {
        for (c in .trmRew_overlap(other, new, FALSE)) {
          queue <- c(queue, list(list(c$left, c$right)))
        }
      }
    }
  }
  .trmRew_incomplete(rules, "iteration budget exhausted", NULL)
}

#' Whether two terms are equal in the theory the rules present. Sound
#'
#' only for a convergent system.
#'
#' @param s See Usage.
#' @param t See Usage.
#' @param rules See Usage.
#' @param max_steps Defaults to \code{10000}.
#' @return A list with \code{equal}, \code{left}, \code{right}.
#' @export
morie_trmRew_decides <- function(s, t, rules, max_steps=10000) {
  # Whether two terms are equal in the theory the rules present. Sound
  # only for a convergent system.
  a <- morie_trmRew_normal_form(s, rules, max_steps=max_steps)$normal_form
  b <- morie_trmRew_normal_form(t, rules, max_steps=max_steps)$normal_form
  list(equal=identical(a, b), left=a, right=b)
}

#' morie_trmRew_term_rewriting
#'
#' Part of the trmRew_native implementation; see the file header for the
#' source it follows.
#'
#' @param term See Usage.
#' @param rules See Usage.
#' @param strategy Defaults to \code{"innermost"}.
#' @param max_steps Defaults to \code{10000}.
#' @return A list with \code{estimate}, \code{normal_form}, \code{steps}, \code{trace}, \code{strategy}, \code{method}.
#' @export
morie_trmRew_term_rewriting <- function(term, rules, strategy="innermost",
                                        max_steps=10000) {
  # Entry point: reduce term under rules.
  nf <- morie_trmRew_normal_form(term, rules, strategy, max_steps)
  list(estimate=nf$normal_form, normal_form=nf$normal_form,
       steps=nf$steps, trace=nf$trace, strategy=strategy,
       method=sprintf("leftmost-%s rewriting to normal form", strategy))
}

#' morie_trmRew_cheatsheet
#'
#' Part of the trmRew_native implementation; see the file header for the
#' source it follows.
#'
#' @return A character value.
#' @export
morie_trmRew_cheatsheet <- function() {
  paste0(
    "trmRew: a rule l -> r rewrites a subterm that MATCHES l (rule ",
    "vars bind, term vars do not). Normal form = no rule applies. ",
    "Termination is proved by a reduction order (LPO); confluence by ",
    "Knuth & Bendix's Critical Pair Lemma -- overlap two left-hand ",
    "sides, and the system is locally confluent iff every critical ",
    "pair is joinable; Newman lifts that to confluence for ",
    "terminating systems. Completion orients unjoinable pairs into ",
    "new rules until convergent, or reports an unorientable pair."
  )
}

# compact alias per ledger/NAMING.md
morie_trmRew <- morie_trmRew_term_rewriting
