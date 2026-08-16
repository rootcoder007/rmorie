# morie.fn -- function file (rootcoder007/morie)
# Robinson unification, and the check that is usually left out.
#
# **The problem.** Two terms unify when some substitution makes them
# identical. Robinson's contribution was not that such a substitution
# can be searched for, but that when one exists there is a *most
# general* one, and that it is computable: every other unifier factors
# through it.
#
# **The disagreement set.** Robinson's algorithm walks the two terms in
# parallel and, at the first position where they differ, extracts the
# pair of subterms sitting there -- the disagreement set. If neither is
# a variable the terms cannot be unified. If one is a variable
# :math:`v` and the other a term :math:`t`, bind :math:`v \mapsto t`,
# apply the binding everywhere, and repeat. The algorithm terminates
# because each round removes one variable from the problem.
#
# **The occurs check.** Binding :math:`v \mapsto t` is legitimate only
# when :math:`v` does not occur inside :math:`t`. Unifying :math:`x`
# with :math:`f(x)` would otherwise produce the "solution"
# :math:`x \mapsto f(x)`, whose repeated application never reaches a
# finite term -- :math:`f(f(f(\ldots)))`. Robinson's Sec. 5 has the
# check; most Prolog systems omit it for speed and are unsound as a
# result, so ``occurs_check=False`` is offered and documented rather
# than hidden.
#
# **Most general, and what that buys.** If :math:`\sigma` is the
# returned unifier and :math:`\theta` any other, there is a
# :math:`\delta` with :math:`\theta = \delta \circ \sigma`. So nothing
# is decided prematurely: resolution can commit to :math:`\sigma` and
# still reach every conclusion reachable through any other unifier.
# ``factor_through`` computes that :math:`\delta` and the anchor uses
# it, rather than asserting generality on faith.
#
# **Terms.** A variable is ``("VAR", name)``; an application is
# ``("APP", symbol, (arg, ...))``. Constants are applications of
# arity zero. Build them with :func:`var`, :func:`app` and
# :func:`const` rather than by hand.
#
# References
# ----------
# Robinson, J. A. (1965) "A Machine-Oriented Logic Based on the
# Resolution Principle", *Journal of the ACM* 12(1), 23-41,
# doi:10.1145/321250.321253. Sec. 5 (the Unification Theorem: any
# unifiable set has a most general unifier), the disagreement-set
# algorithm, the occurs check, and the factorisation
# :math:`\theta = \delta \circ \sigma` reproduced above.

.VAR <- "VAR"
.APP <- "APP"

# ---- private helpers (prefixed .unifAlg_ to avoid env collisions) ----

#' .unifAlg_is_var
#'
#' Part of the unifAlg_native implementation; see the file header for
#' the source it follows.
#'
#' @param t See Usage.
#' @return A logical value.
#' @export
.unifAlg_is_var <- function(t) {
  is.list(t) && length(t) == 2L && identical(t[[1]], .VAR)
}

#' .unifAlg_check
#'
#' Part of the unifAlg_native implementation; see the file header for
#' the source it follows.
#'
#' @param t See Usage.
#' @return Nothing; this branch always raises.
#' @export
.unifAlg_check <- function(t) {
  if (.unifAlg_is_var(t)) {
    return(t)
  }
  if (is.list(t) && length(t) == 3L && identical(t[[1]], .APP) &&
      is.character(t[[2]]) && length(t[[2]]) == 1L) {
    args <- t[[3]]
    new_args <- lapply(args, .unifAlg_check)
    return(list(.APP, t[[2]], new_args))
  }
  stop(sprintf("unifAlg: not a term: %s -- build terms with var(), app() or const()",
               deparse(t)))
}

#' .unifAlg_apply_once
#'
#' Part of the unifAlg_native implementation; see the file header for
#' the source it follows.
#'
#' @param t See Usage.
#' @param subst See Usage.
#' @return The value of \code{list}.
#' @export
.unifAlg_apply_once <- function(t, subst) {
  if (.unifAlg_is_var(t)) {
    nm <- t[[2]]
    if (nm %in% names(subst)) {
      return(.unifAlg_check(subst[[nm]]))
    }
    return(t)
  }
  new_args <- lapply(t[[3]], function(a) .unifAlg_apply_once(a, subst))
  list(.APP, t[[2]], new_args)
}

#' .unifAlg_fail
#'
#' Part of the unifAlg_native implementation; see the file header for
#' the source it follows.
#'
#' @param sub See Usage.
#' @param why See Usage.
#' @param oc See Usage.
#' @return A list with \code{estimate}, \code{unified}, \code{mgu}, \code{reason}, \code{occurs_check}, \code{partial}, \code{n_bindings}, \code{method}.
#' @export
.unifAlg_fail <- function(sub, why, oc) {
  list(
    estimate = FALSE,
    unified = FALSE,
    mgu = NULL,
    reason = why,
    occurs_check = as.logical(oc),
    partial = sub,
    n_bindings = 0L,
    method = "Robinson (1965) Sec. 5 disagreement-set unification"
  )
}

# ---- public API ----

#' morie_unifAlg_var
#'
#' Part of the unifAlg_native implementation; see the file header for
#' the source it follows.
#'
#' @param name See Usage.
#' @return The value of \code{list}.
#' @export
morie_unifAlg_var <- function(name) {
  list(.VAR, as.character(name))
}

#' morie_unifAlg_app
#'
#' Part of the unifAlg_native implementation; see the file header for
#' the source it follows.
#'
#' @param symbol See Usage.
#' @param ... Passed through.
#' @return The value of \code{list}.
#' @export
morie_unifAlg_app <- function(symbol, ...) {
  args <- list(...)
  list(.APP, as.character(symbol), args)
}

#' morie_unifAlg_const
#'
#' Part of the unifAlg_native implementation; see the file header for
#' the source it follows.
#'
#' @param symbol See Usage.
#' @return The value of \code{list}.
#' @export
morie_unifAlg_const <- function(symbol) {
  list(.APP, as.character(symbol), list())
}

#' morie_unifAlg_is_var
#'
#' Part of the unifAlg_native implementation; see the file header for
#' the source it follows.
#'
#' @param t See Usage.
#' @return The value of \code{.unifAlg_is_var}.
#' @export
morie_unifAlg_is_var <- function(t) {
  .unifAlg_is_var(t)
}

#' morie_unifAlg_variables
#'
#' Part of the unifAlg_native implementation; see the file header for
#' the source it follows.
#'
#' @param t See Usage.
#' @return The value of \code{$}.
#' @export
morie_unifAlg_variables <- function(t) {
  t <- .unifAlg_check(t)
  env <- new.env()
  env$out <- character(0)
  env$seen <- character(0)

  walk <- function(x) {
    if (.unifAlg_is_var(x)) {
      nm <- x[[2]]
      if (!(nm %in% env$seen)) {
        env$seen <- c(env$seen, nm)
        env$out <- c(env$out, nm)
      }
    } else {
      for (a in x[[3]]) {
        walk(a)
      }
    }
  }

  walk(t)
  env$out
}

#' morie_unifAlg_occurs
#'
#' Part of the unifAlg_native implementation; see the file header for
#' the source it follows.
#'
#' @param name See Usage.
#' @param t See Usage.
#' @return The value of \code{%in%}.
#' @export
morie_unifAlg_occurs <- function(name, t) {
  as.character(name) %in% morie_unifAlg_variables(t)
}

#' morie_unifAlg_apply_subst
#'
#' Part of the unifAlg_native implementation; see the file header for
#' the source it follows.
#'
#' @param t See Usage.
#' @param subst See Usage.
#' @return Nothing; this branch always raises.
#' @export
morie_unifAlg_apply_subst <- function(t, subst) {
  cur <- .unifAlg_check(t)
  for (i in seq_len(64L)) {
    nxt <- .unifAlg_apply_once(cur, subst)
    if (identical(nxt, cur)) {
      return(cur)
    }
    cur <- nxt
  }
  stop(sprintf("unifAlg: the substitution %s does not reach a fixed point -- it binds a variable to a term containing itself",
               deparse(subst)))
}

#' morie_unifAlg_substitute
#'
#' Part of the unifAlg_native implementation; see the file header for
#' the source it follows.
#'
#' @param t See Usage.
#' @param subst See Usage.
#' @return The value of \code{.unifAlg_apply_once}.
#' @export
morie_unifAlg_substitute <- function(t, subst) {
  .unifAlg_apply_once(.unifAlg_check(t), subst)
}

#' morie_unifAlg_compose
#'
#' Part of the unifAlg_native implementation; see the file header for
#' the source it follows.
#'
#' @param outer See Usage.
#' @param inner See Usage.
#' @return The value of \code{[}.
#' @export
morie_unifAlg_compose <- function(outer, inner) {
  out <- list()
  for (nm in names(inner)) {
    v <- inner[[nm]]
    out[[nm]] <- .unifAlg_apply_once(.unifAlg_check(v), outer)
  }
  for (nm in names(outer)) {
    if (!(nm %in% names(inner))) {
      out[[nm]] <- .unifAlg_check(outer[[nm]])
    }
  }
  keep <- vapply(names(out), function(k) {
    v <- out[[k]]
    !(is.list(v) && length(v) == 2L && identical(v[[1]], .VAR) && identical(v[[2]], k))
  }, logical(1))
  out[keep]
}

#' morie_unifAlg_disagreement
#'
#' Part of the unifAlg_native implementation; see the file header for
#' the source it follows.
#'
#' @param t1 See Usage.
#' @param t2 See Usage.
#' @return Nothing; the function is called for its effect.
#' @export
morie_unifAlg_disagreement <- function(t1, t2) {
  a <- .unifAlg_check(t1)
  b <- .unifAlg_check(t2)
  if (identical(a, b)) {
    return(NULL)
  }
  if (.unifAlg_is_var(a) || .unifAlg_is_var(b)) {
    return(list(a, b))
  }
  if (!identical(a[[2]], b[[2]]) || length(a[[3]]) != length(b[[3]])) {
    return(list(a, b))
  }
  for (i in seq_along(a[[3]])) {
    d <- morie_unifAlg_disagreement(a[[3]][[i]], b[[3]][[i]])
    if (!is.null(d)) {
      return(d)
    }
  }
  NULL
}

#' morie_unifAlg_unify
#'
#' Part of the unifAlg_native implementation; see the file header for
#' the source it follows.
#'
#' @param t1 See Usage.
#' @param t2 See Usage.
#' @param occurs_check Defaults to \code{TRUE}.
#' @return Nothing; this branch always raises.
#' @export
morie_unifAlg_unify <- function(t1, t2, occurs_check = TRUE) {
  a <- .unifAlg_check(t1)
  b <- .unifAlg_check(t2)
  sub <- list()

  for (i in seq_len(4096L)) {
    da <- morie_unifAlg_apply_subst(a, sub)
    db <- morie_unifAlg_apply_subst(b, sub)
    d <- morie_unifAlg_disagreement(da, db)
    if (is.null(d)) {
      return(list(
        estimate = TRUE,
        unified = TRUE,
        mgu = sub,
        reason = NULL,
        occurs_check = as.logical(occurs_check),
        cyclic = FALSE,
        n_bindings = length(sub),
        method = "Robinson (1965) Sec. 5 disagreement-set unification"
      ))
    }
    x <- d[[1]]
    y <- d[[2]]
    if (.unifAlg_is_var(y) && !.unifAlg_is_var(x)) {
      x <- d[[2]]
      y <- d[[1]]
    }
    if (!.unifAlg_is_var(x)) {
      why <- sprintf("symbol clash: %s/%d against %s/%d",
                     x[[2]], length(x[[3]]), y[[2]], length(y[[3]]))
      return(.unifAlg_fail(sub, why, occurs_check))
    }
    if (morie_unifAlg_occurs(x[[2]], y)) {
      if (occurs_check) {
        why <- sprintf("occurs check: %s occurs in the term it would be bound to",
                       x[[2]])
        return(.unifAlg_fail(sub, why, occurs_check))
      }
      cyc <- sub
      cyc[[x[[2]]]] <- y
      return(list(
        estimate = TRUE,
        unified = TRUE,
        mgu = cyc,
        reason = NULL,
        occurs_check = FALSE,
        cyclic = TRUE,
        n_bindings = length(cyc),
        method = "Robinson (1965) Sec. 5 disagreement-set unification, occurs check suppressed"
      ))
    }
    sub <- morie_unifAlg_compose(setNames(list(y), x[[2]]), sub)
  }
  stop("unifAlg: unification did not terminate")
}

#' morie_unifAlg_match
#'
#' Part of the unifAlg_native implementation; see the file header for
#' the source it follows.
#'
#' @param pattern See Usage.
#' @param subject See Usage.
#' @return The value of \code{sub}, as built in the body.
#' @export
morie_unifAlg_match <- function(pattern, subject) {
  p <- .unifAlg_check(pattern)
  s <- .unifAlg_check(subject)
  sub <- list()
  stack <- list(list(p, s))

  while (length(stack) > 0L) {
    pair <- stack[[length(stack)]]
    stack <- stack[-length(stack)]
    x <- pair[[1]]
    y <- pair[[2]]

    if (.unifAlg_is_var(x)) {
      nm <- x[[2]]
      if (nm %in% names(sub)) {
        if (!identical(sub[[nm]], y)) {
          return(NULL)
        }
      } else {
        sub[[nm]] <- y
      }
    } else if (.unifAlg_is_var(y)) {
      return(NULL)
    } else if (!identical(x[[2]], y[[2]]) || length(x[[3]]) != length(y[[3]])) {
      return(NULL)
    } else {
      new_pairs <- Map(function(a, b) list(a, b), x[[3]], y[[3]])
      stack <- c(stack, new_pairs)
    }
  }
  sub
}

#' morie_unifAlg_factor_through
#'
#' Part of the unifAlg_native implementation; see the file header for
#' the source it follows.
#'
#' @param general See Usage.
#' @param other See Usage.
#' @param over See Usage.
#' @return The value of \code{delta}, as built in the body.
#' @export
morie_unifAlg_factor_through <- function(general, other, over) {
  delta <- list()
  for (name in over) {
    img <- morie_unifAlg_apply_subst(morie_unifAlg_var(name), general)
    tgt <- morie_unifAlg_apply_subst(morie_unifAlg_var(name), other)
    m <- morie_unifAlg_match(img, tgt)
    if (is.null(m)) {
      return(NULL)
    }
    for (k in names(m)) {
      v <- m[[k]]
      if (k %in% names(delta) && !identical(delta[[k]], v)) {
        return(NULL)
      }
      delta[[k]] <- v
    }
  }
  delta
}

# Main entry point
morie_unifAlg <- morie_unifAlg_unify

# compact alias per ledger/NAMING.md
morie_unifAlg_unification <- morie_unifAlg_unify
